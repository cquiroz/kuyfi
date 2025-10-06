package kuyfi

import cats.parse.{ Parser, Parser0 }
import cats.parse.Rfc5234.{ digit => digitParser }
import cats._
import cats.syntax.all._
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.format.TextStyle
import java.time.{ DayOfWeek, LocalTime, Month }
import java.util.Locale
import scala.jdk.CollectionConverters._
import TZDB._

/** Defines cats-parse parsers to read tzdb files
  */
object TZDBParser {
  type ParseResult[A] = Either[Parser.Error, A]

  // Useful Monoid
  implicit def parserListMonoid[A]: Monoid[ParseResult[List[A]]] =
    new Monoid[ParseResult[List[A]]] {
      def empty                                                                           = Right(List.empty[A])
      def combine(a: ParseResult[List[A]], b: ParseResult[List[A]]): ParseResult[List[A]] =
        (a, b) match {
          case (Right(x), Right(y)) => Right(x ::: y)
          case (Left(e), _)         => Left(e)
          case (_, Left(e))         => Left(e)
        }
    }

  implicit class Parser2Coproduct[A](val a: Parser[A]) extends AnyVal {
    def liftC[C <: shapeless.Coproduct](implicit
      inj: shapeless.ops.coproduct.Inject[C, A]
    ): Parser[C] = a.map(_.liftC[C])
  }

  private val space      = Parser.char(' ')
  private val semicolon  = Parser.char(':')
  private val nl         = Parser.char('\n')
  private val identifier = Parser.charWhere(c => c != ' ' && c != '\t' && c != '\n').rep.string

  private val whitespace: Parser[Char]     = Parser.charIn(" \t")
  private val linkSeparator: Parser0[Unit] = whitespace.rep0.void

  private val int: Parser[Int] =
    digitParser.rep.string.map(_.toInt)

  private val months: List[(String, Month)]   =
    Month.values().map(m => (m.getDisplayName(TextStyle.SHORT, Locale.ENGLISH), m)).toList
  private val days: List[(String, DayOfWeek)] =
    DayOfWeek.values().map(m => (m.getDisplayName(TextStyle.SHORT, Locale.ENGLISH), m)).toList

  val from: Parser[String] =
    digitParser.rep.string |
      Parser.string("minimum").string |
      Parser.string("maximum").string

  val fromParser: Parser[RuleYear] =
    digitParser.rep.string.map(y => GivenYear(y.toInt)) |
      Parser.string("minimum").as(Minimum: RuleYear) |
      Parser.string("maximum").as(Maximum: RuleYear) |
      Parser.string("max").as(Maximum: RuleYear) |
      Parser.string("min").as(Minimum: RuleYear)

  val toParser: Parser[RuleYear] =
    digitParser.rep.string.map(y => GivenYear(y.toInt)) |
      Parser.string("minimum").as(Minimum: RuleYear) |
      Parser.string("maximum").as(Maximum: RuleYear) |
      Parser.string("max").as(Maximum: RuleYear) |
      Parser.string("min").as(Minimum: RuleYear) |
      Parser.string("only").as(Only: RuleYear)

  def parseOneOf[A](items: List[(String, A)]): Parser[A] = {
    val p = items.map { case (i, v) =>
      Parser.string(i).as(v)
    }

    p.reduceLeft(_ | _)
  }

  val monthParser: Parser[Month] = parseOneOf(months)

  val dayParser: Parser[DayOfWeek] = parseOneOf(days)

  val afterWeekdayParser: Parser[On] =
    ((space.?.with1 *> dayParser) ~ (space.? *> Parser.string(">=") *> int)).map { case (d, a) =>
      AfterWeekday(d, a)
    }

  val beforeWeekdayParser: Parser[On] =
    ((space.?.with1 *> dayParser) ~ (space.? *> Parser.string("<=") *> int)).map { case (d, a) =>
      BeforeWeekday(d, a)
    }

  val lastWeekdayParser: Parser[On] =
    (Parser.string("last") *> dayParser).map(d => LastWeekday(d))

  val onParser: Parser[On] =
    afterWeekdayParser.backtrack |
      beforeWeekdayParser.backtrack |
      lastWeekdayParser.backtrack |
      (space *> int).backtrack.map(DayOfTheMonth.apply) |
      int.map(DayOfTheMonth.apply)

  val hourMinParser: Parser[(Boolean, Int, Int)] =
    (Parser.char('-') *> int ~ (semicolon *> int)).map { case (h, m) => (true, h, m) } |
      (space *> Parser.char('-') *> int ~ (semicolon *> int)).map { case (h, m) => (true, h, m) } |
      (space *> int ~ (semicolon *> int)).map { case (h, m) => (false, h, m) } |
      (int ~ (semicolon *> int)).map { case (h, m) => (false, h, m) }

  private val backtrackHourMinParser = hourMinParser.backtrack

  val hourMinParserLT: Parser[(Int, Boolean, LocalTime)] = hourMinParser.map { case (_, h, m) =>
    val (endOfDay, hours) = fixHourRange(h)
    (if (h > 24) h % 24 else 0, endOfDay, LocalTime.of(hours, m))
  }

  val hourMinParserOf: Parser[GmtOffset] = hourMinParser.map {
    case (n, h, m) if n => GmtOffset(-h, -m, 0)
    case (_, h, m)      => GmtOffset(h, m, 0)
  }

  val hourMinSecParser: Parser[(Boolean, Int, Int, Int)] =
    (Parser.char('-') *> int ~ (semicolon *> int) ~ (semicolon *> int)).map { case ((h, m), s) =>
      (true, h, m, s)
    } |
      (Parser.char('-') *> space *> int ~ (semicolon *> int) ~ (semicolon *> int)).map {
        case ((h, m), s) => (true, h, m, s)
      } |
      (space *> Parser.char('-') *> int ~ (semicolon *> int) ~ (semicolon *> int)).map {
        case ((h, m), s) => (true, h, m, s)
      } |
      (space *> Parser.char('-') *> space *> int ~ (semicolon *> int) ~ (semicolon *> int)).map {
        case ((h, m), s) => (true, h, m, s)
      } |
      (space *> int ~ (semicolon *> int) ~ (semicolon *> int)).map { case ((h, m), s) =>
        (false, h, m, s)
      } |
      (int ~ (semicolon *> int) ~ (semicolon *> int)).map { case ((h, m), s) => (false, h, m, s) }

  private val backtrackHourMinSecParser = hourMinSecParser.backtrack

  val hourMinSecParserLT: Parser[(Int, Boolean, LocalTime)] = hourMinSecParser.map {
    case (_, h, m, s) =>
      val (endOfDay, hours) = fixHourRange(h)
      (if (h > 24) h % 24 else 0, endOfDay, LocalTime.of(hours, m, s))
  }

  val hourMinSecParserOf: Parser[GmtOffset] = hourMinSecParser.map {
    case (neg, h, m, s) if neg => GmtOffset(-h, -m, -s)
    case (_, h, m, s)          => GmtOffset(h, m, s)
  }

  val timeParser: Parser[(Int, Boolean, LocalTime)] = {
    val time: Parser[(Int, Boolean, LocalTime)] =
      backtrackHourMinSecParser.map { case (_, h, m, s) =>
        val (endOfDay, hours) = fixHourRange(h)
        (if (h > 24) h % 24 else 0, endOfDay, LocalTime.of(hours, m, s))
      } |
        backtrackHourMinParser.map { case (_, h, m) =>
          val (endOfDay, hours) = fixHourRange(h)
          (if (h > 24) h % 24 else 0, endOfDay, LocalTime.of(hours, m))
        } |
        int.map(fixHourRange).map(h => (0, h._1, LocalTime.of(h._2, 0)))

    (whitespace.rep.with1 *> time) | time
  }

  private def fixHourRange(h: Int): (Boolean, Int) =
    (h >= 24, if (h >= 24) h - 24 else h)

  val gmtOffsetParser: Parser[GmtOffset] =
    hourMinSecParserOf.backtrack |
      hourMinParserOf.backtrack |
      (Parser.char('-') *> int).backtrack.map(h => GmtOffset(-h, 0, 0)) |
      int.map(h => GmtOffset(h, 0, 0))

  val atParser: Parser[At] =
    (timeParser <* Parser.char('w')).backtrack.map { case (r, e, t) => AtWallTime(t, e, r): At } |
      (timeParser <* Parser.char('s')).backtrack.map { case (r, e, t) =>
        AtStandardTime(t, e, r): At
      } |
      (timeParser <* Parser.charIn("zgu")).backtrack.map { case (r, e, t) =>
        AtUniversalTime(t, e, r): At
      } |
      timeParser.map { case (r, e, t) => AtWallTime(t, e, r): At }

  val saveParser: Parser[Save] =
    ((Parser.char('-') *> whitespace.?).backtrack *> timeParser).map { case (_, _, l) =>
      Save(false, l)
    } |
      timeParser.map { case (_, _, l) => Save(true, l) }

  val toEndLine: Parser0[String] = Parser.charsWhile0(_ != '\n')

  val letterParser: Parser[Letter] =
    for {
      l <- Parser.charsWhile(c => c != '\n' && c != '#').string
      _ <- toEndLine
    } yield Letter(l.trim)

  val ruleParser: Parser[Rule] =
    for {
      name   <- Parser.string("Rule") *> whitespace *> identifier <* whitespace
      from   <- fromParser <* whitespace
      to     <- toParser <* whitespace
      month  <- Parser.char('-') *> whitespace *> monthParser <* whitespace
      on     <- onParser <* whitespace
      at     <- atParser <* whitespace
      save   <- saveParser <* whitespace.rep0
      letter <- letterParser <* nl.?
    } yield Rule(name, from, to, month, on, at, save, letter).adjustRoll

  val linkParser: Parser[Link] =
    for {
      from <- Parser.string("Link") *> linkSeparator *> identifier
      to   <- linkSeparator *> identifier <* toEndLine <* nl.?
    } yield Link(from, to)

  val untilParser: Parser[Until] =
    for {
      year  <- int <* whitespace.?
      month <- monthParser.? <* whitespace.rep0
      on    <- onParser.? <* whitespace.rep0
      at    <- atParser.? <* toEndLine
    } yield Until(year, month, on, at)

  val commentParser: Parser[Comment] =
    (Parser.char('#') *> toEndLine).map(Comment.apply) |
      (whitespace.rep *> Parser.char('#') *> toEndLine).map(Comment.apply)

  val zoneRuleParser: Parser[ZoneRule] =
    gmtOffsetParser.map(d => FixedOffset(d): ZoneRule) |
      (Parser.char('-') <* whitespace.?).as(NullRule: ZoneRule) |
      identifier.map(RuleId.apply)

  val zoneTransitionParser: Parser[ZoneTransition] = {
    val transition =
      for {
        gmtOff   <- gmtOffsetParser <* whitespace
        zoneRule <- zoneRuleParser <* whitespace.rep0
        format   <- identifier <* whitespace.rep0
        until    <- untilParser.?
      } yield ZoneTransition(gmtOff, zoneRule, format, until)

    transition | (whitespace.rep *> transition)
  }

  val continuationZoneTransitionParser: Parser[ZoneTransition] =
    for {
      gmtOff   <- nl *> whitespace.rep(1) *> gmtOffsetParser <* whitespace.rep(1)
      zoneRule <- zoneRuleParser <* whitespace.rep0
      format   <- identifier <* whitespace.rep0
      until    <- untilParser.? <* toEndLine
    } yield ZoneTransition(gmtOff, zoneRule, format, until)

  val zoneWithCommentParser: Parser[Option[ZoneTransition]] =
    ((nl *> commentParser).backtrack.as(None: Option[ZoneTransition])) |
      continuationZoneTransitionParser.map(Some(_))

  val zoneTransitionListParser: Parser[List[ZoneTransition]] =
    (zoneTransitionParser ~ zoneWithCommentParser.backtrack.rep0).map { case (a, b) =>
      a :: b.flatten
    }

  val zoneParser: Parser[Zone] =
    for {
      name  <- Parser.string("Zone") *> whitespace *> identifier <* whitespace
      trans <- zoneTransitionListParser
    } yield Zone(name, trans)

  val zoneParserNl: Parser[Zone] =
    zoneParser <* nl.?

  val blankLine: Parser[BlankLine] =
    nl.as(BlankLine(""))

  val commentParserNl: Parser[Comment] =
    commentParser <* nl.?

  val fileParser: Parser[List[Row]] =
    (
      commentParserNl.backtrack.liftC[Row] |
        ruleParser.backtrack.liftC[Row] |
        zoneParserNl.backtrack.liftC[Row] |
        linkParser.backtrack.liftC[Row] |
        blankLine.liftC[Row]
    ).rep.map(_.toList)

  def parseFile(text: String): ParseResult[List[Row]] =
    fileParser.parseAll(text)

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
  def parseVersion(dir: File): Option[TzdbVersion] =
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

  /** Entry point. Takes a dir with the TZDB files and parses them into Rows
    */
  def parseAll(dir: File): List[Row] =
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
          case Right(v) => v
          case Left(_)  => Nil
        }
        rows
      case _                  => Nil
    }
}
