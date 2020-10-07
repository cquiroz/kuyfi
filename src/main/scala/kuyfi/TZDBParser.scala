package kuyfi

import atto._, Atto.{ char => chr, _ }
import atto.ParseResult.{ Done, Fail }
import cats._
import cats.effect._
import cats.syntax.all._
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.format.TextStyle
import java.time.{ DayOfWeek, LocalTime, Month }
import java.util.Locale
import mouse.boolean._
import scala.jdk.CollectionConverters._
import TZDB._

/** Defines atto parsers to read tzdb files
  */
object TZDBParser {
  // Useful Monoid
  implicit def parserListMonoid[A]: Monoid[ParseResult[List[A]]] =
    new Monoid[ParseResult[List[A]]] {
      def empty = Done("", List.empty[A])
      def combine(a: ParseResult[List[A]], b: ParseResult[List[A]]): ParseResult[List[A]] =
        (a, b) match {
          case (Done(u, x), Done(v, y)) => Done(u + v, x ::: y)
          case _                        => Fail("", Nil, "Can only handle full response ")
        }
    }

  implicit class Parser2Coproduct[A](val a: Parser[A]) extends AnyVal {
    def liftC[C <: shapeless.Coproduct](implicit
      inj: shapeless.ops.coproduct.Inject[C, A]
    ): Parser[C] = a.map(_.liftC[C])
  }

  private val space      = chr(' ')
  private val semicolon  = chr(':')
  private val tab        = chr('\t')
  private val nl         = chr('\n')
  private val identifier = stringOf1(noneOf(" \t\n"))

  private val whitespace: Parser[Char]          = tab | space
  private val linkSeparator: Parser[List[Char]] = many(whitespace)

  private val months: List[(String, Month)]   =
    Month.values().map(m => (m.getDisplayName(TextStyle.SHORT, Locale.ENGLISH), m)).toList
  private val days: List[(String, DayOfWeek)] =
    DayOfWeek.values().map(m => (m.getDisplayName(TextStyle.SHORT, Locale.ENGLISH), m)).toList

  val from: Parser[String] =
    stringOf1(digit) |
      string("minimum") |
      string("maximum")

  val fromParser: Parser[RuleYear] = {
    stringOf1(digit).map(y => GivenYear(y.toInt)) |
      string("minimum").map(_ => Minimum: RuleYear) |
      string("maximum").map(_ => Maximum: RuleYear) |
      string("max").map(_ => Maximum: RuleYear) |
      string("min").map(_ => Minimum: RuleYear)
  }

  val toParser: Parser[RuleYear] = {
    stringOf1(digit).map(y => GivenYear(y.toInt)) |
      string("minimum").map(_ => Minimum: RuleYear) |
      string("maximum").map(_ => Maximum: RuleYear) |
      string("max").map(_ => Maximum: RuleYear) |
      string("min").map(_ => Minimum: RuleYear) |
      string("only").map(_ => Only: RuleYear)
  }

  def parseOneOf[A](items: List[(String, A)], msg: String): Parser[A] = {
    val p = items.map { case (i, v) =>
      string(i).map(_ => v)
    }

    p.foldRight(err[A](msg))(_ | _)
  }

  val monthParser: Parser[Month] = parseOneOf(months, "unknown month")

  val dayParser: Parser[DayOfWeek] = parseOneOf(days, "unknown day")

  val afterWeekdayParser: Parser[On] =
    for {
      d <- opt(space) ~> dayParser <~ string(">=")
      a <- int
    } yield AfterWeekday(d, a)

  val beforeWeekdayParser: Parser[On] =
    for {
      d <- opt(space) ~> dayParser <~ string("<=")
      a <- int
    } yield BeforeWeekday(d, a)

  val lastWeekdayParser: Parser[On] =
    for {
      _ <- string("last")
      d <- dayParser
    } yield LastWeekday(d)

  val onParser: Parser[On] =
    (opt(space) ~> int.map(DayOfTheMonth.apply)) |
      afterWeekdayParser |
      beforeWeekdayParser |
      lastWeekdayParser

  val timePartParser: Parser[Char] =
    digit | semicolon

  val hourMinParser: Parser[(Boolean, Int, Int)] =
    for {
      _ <- opt(space)
      n <- opt(chr('-'))
      h <- int <~ semicolon
      m <- int
    } yield (n.isDefined, h, m)

  val hourMinParserLT: Parser[(Int, Boolean, LocalTime)] = hourMinParser.map { case (_, h, m) =>
    val (endOfDay, hours) = fixHourRange(h)
    (if (h > 24) h % 24 else 0, endOfDay, LocalTime.of(hours, m))
  }

  val hourMinParserOf: Parser[GmtOffset] = hourMinParser.map {
    case (n, h, m) if n => GmtOffset(-h, -m, 0)
    case (_, h, m)      => GmtOffset(h, m, 0)
  }

  val hourMinSecParser: Parser[(Boolean, Int, Int, Int)] =
    for {
      _ <- opt(space)
      n <- opt(chr('-'))
      _ <- opt(space)
      h <- int <~ semicolon
      m <- int <~ semicolon
      s <- int
    } yield (n.isDefined, h, m, s)

  val hourMinSecParserLT: Parser[(Int, Boolean, LocalTime)] = hourMinSecParser.map {
    case (_, h, m, s) =>
      val (endOfDay, hours) = fixHourRange(h)
      (if (h > 24) h % 24 else 0, endOfDay, LocalTime.of(hours, m, s))
  }

  val hourMinSecParserOf: Parser[GmtOffset] = hourMinSecParser.map {
    case (neg, h, m, s) if neg => GmtOffset(-h, -m, -s)
    case (_, h, m, s)          => GmtOffset(h, m, s)
  }

  val timeParser: Parser[(Int, Boolean, LocalTime)] =
    opt(many(whitespace)) ~>
      hourMinSecParserLT |
      hourMinParserLT |
      int.map(fixHourRange).map(h => (0, h._1, LocalTime.of(h._2, 0)))

  private def fixHourRange(h: Int): (Boolean, Int) =
    (h >= 24, (h >= 24).fold(h - 24, h))

  val gmtOffsetParser: Parser[GmtOffset] =
    hourMinSecParserOf |
      hourMinParserOf |
      int.map(h => GmtOffset(h, 0, 0))

  val atParser: Parser[At]      =
    (timeParser ~ chr('w')).map { case ((r, e, t), _) => AtWallTime(t, e, r): At } |
      (timeParser ~ chr('s')).map { case ((r, e, t), _) => AtStandardTime(t, e, r): At } |
      (timeParser ~ oneOf("zgu")).map { case ((r, e, t), _) => AtUniversalTime(t, e, r): At } |
      (opt(whitespace) ~> timeParser).map { case (r, e, t) => AtWallTime(t, e, r): At }

  val saveParser: Parser[Save]  =
    (opt(chr('-')) ~ timeParser).map { case (s, (_, _, l)) => Save(s.isEmpty, l) }

  val toEndLine: Parser[String] = takeWhile(_ =!= '\n') <~ opt(nl)

  val letterParser: Parser[Letter] =
    for {
      l <- takeWhile(c => c.isUpper || c === '-')
      _ <- toEndLine
    } yield Letter(l)

  val ruleParser: Parser[Rule] =
    for {
      _      <- string("Rule") <~ whitespace
      name   <- identifier <~ whitespace
      from   <- fromParser <~ whitespace
      to     <- toParser <~ whitespace
      _      <- chr('-') <~ whitespace
      month  <- monthParser <~ whitespace
      on     <- onParser <~ whitespace
      at     <- atParser <~ whitespace
      save   <- saveParser <~ whitespace
      letter <- letterParser
    } yield Rule(name, from, to, month, on, at, save, letter).adjustRoll

  val linkParser: Parser[Link] =
    for {
      _    <- string("Link") <~ linkSeparator
      from <- identifier <~ linkSeparator
      to   <- identifier
      _    <- toEndLine
    } yield Link(from, to)

  val untilParser: Parser[Until] =
    for {
      year  <- int <~ opt(whitespace)
      month <- opt(monthParser) <~ many(whitespace)
      on    <- opt(onParser) <~ many(whitespace)
      at    <- opt(atParser)
      _     <- toEndLine
    } yield Until(year, month, on, at)

  val commentParser: Parser[Comment] =
    chr('#') ~> toEndLine.map(Comment.apply)

  val zoneRuleParser: Parser[ZoneRule] =
    gmtOffsetParser.map(d => FixedOffset(d): ZoneRule) | (chr('-') <~ opt(whitespace)).map(_ =>
      NullRule: ZoneRule
    ) | identifier.map(RuleId.apply)

  val zoneTransitionParser: Parser[ZoneTransition] =
    for {
      gmtOff   <- many(whitespace) ~> gmtOffsetParser <~ whitespace
      zoneRule <- zoneRuleParser <~ many(whitespace)
      format   <- identifier <~ many(whitespace)
      until    <- opt(untilParser)
      _        <- opt(many(commentParser))
    } yield ZoneTransition(gmtOff, zoneRule, format, until)

  val continuationZoneTransitionParser: Parser[ZoneTransition] =
    for {
      _        <- manyN(3, whitespace) <~ opt(whitespace)
      gmtOff   <- gmtOffsetParser <~ whitespace
      zoneRule <- opt(whitespace) ~> zoneRuleParser <~ many(whitespace)
      format   <- identifier <~ many(whitespace)
      until    <- opt(untilParser)
      _        <- opt(many(many(whitespace) ~> commentParser))
    } yield ZoneTransition(gmtOff, zoneRule, format, until)

  val zoneTransitionListParser: Parser[List[ZoneTransition]] =
    (zoneTransitionParser ~ many(continuationZoneTransitionParser)).map { case (a, b) => a :: b }

  val zoneParser: Parser[Zone]                               =
    for {
      _     <- string("Zone") <~ whitespace
      name  <- identifier <~ whitespace
      trans <- zoneTransitionListParser
    } yield Zone(name, trans)

  val zoneParserNl: Parser[Zone] =
    zoneParser <~ opt(nl)

  val blankLine: Parser[BlankLine] =
    nl.map(_ => BlankLine(""))

  val fileParser: Parser[List[Row]] =
    for {
      c <- many(
             commentParser.liftC[Row] | ruleParser.liftC[Row] | zoneParserNl.liftC[Row] | linkParser
               .liftC[Row] | blankLine.liftC[Row]
           )
    } yield c

  def parseFile(text: String): ParseResult[List[Row]] =
    fileParser.parseOnly(text)

  val tzdbFiles: List[String] = List(
    "africa",
    "antarctica",
    "asia",
    "australasia",
    "backward",
    "etcetera",
    "europe",
    "northamerica",
    "pacificnew",
    "southamerica",
    "systemv"
  )

  /** Parse the version
    */
  def parseVersion(dir: File): IO[Option[TzdbVersion]] =
    IO {
      dir match {
        case x if x.isDirectory =>
          val files = x.list
          files
            .filter(_ === "version")
            .map(f =>
              Files
                .readAllLines(new File(dir, f).toPath, StandardCharsets.UTF_8)
                .asScala
                .mkString("\n")
            )
            .map(TzdbVersion.apply)
            .toList
            .headOption
        case _                  => None
      }
    }

  /** Entry point. Takes a dir with the TZDB files and parses them into Rows
    */
  def parseAll(dir: File): IO[List[Row]] =
    IO {
      dir match {
        case x if x.isDirectory =>
          val files  = x.list
          val parsed =
            files
              .filter(f => tzdbFiles.contains(f))
              .map(f =>
                Files
                  .readAllLines(new File(dir, f).toPath, StandardCharsets.UTF_8)
                  .asScala
                  .mkString("\n")
              )
              .map(parseFile)
          val rows   = parsed.toList.combineAll match {
            case Done(_, v) => v
            case _          => Nil
          }
          rows
        case _                  => Nil
      }
    }
}
