package kuyfi

import java.time.{DayOfWeek, LocalTime, Month}
import java.time.format.TextStyle
import java.util.Locale

import atto.ParseResult.{Done, Fail}
import better.files.File
import shapeless.{Coproduct, _}
import shapeless.ops.coproduct.Inject

import scalaz.effect.IO

/**
  * Model of the TimeZone Database
  */
object TZDB {

  /**
    * Definition of timestamps
    */
  sealed trait At extends Product with Serializable {
    def time: LocalTime
  }
  case class AtWallTime(time: LocalTime) extends At
  case class AtStandardTime(time: LocalTime) extends At
  case class AtUniversalTime(time: LocalTime) extends At

  /**
    * Model for Zone entries on TZDB
    */
  case class GmtOffset(h: Int, m: Int, s: Int)
  case class Until(y: Int, m: Option[Month], d: Option[DayOfTheMonth], at: Option[At])
  case class ZoneTransition(at: GmtOffset, rules: String, format: String, until: Option[Until])
  case class Zone(name: String, transitions: List[ZoneTransition])  extends Product with Serializable

  /**
    * Model for Rule Entries
    */
  case class Letter(letter: String)
  case class Save(time: LocalTime)

  sealed trait On extends Product with Serializable
  case class DayOfTheMonth(i: Int) extends On
  case class LastWeekday(d: DayOfWeek) extends On
  case class AfterWeekday(d: DayOfWeek, day: Int) extends On
  case class BeforeWeekday(d: DayOfWeek, day: Int) extends On

  sealed trait Year extends Product with Serializable
  case class GivenYear(year: Int) extends Year
  case object Minimum extends Year
  case object Maximum extends Year
  case object Only extends Year
  case class Rule(name: String, from: Year, to: Year, month: Month, on: On, at: At, save: Save, letter: Letter) extends Product with Serializable

  /**
    * Model for Link entries
    */
  case class Link(from: String, to: String) extends Product with Serializable

  /**
    * Comments and blank lines
    */
  case class Comment(comment: String) extends Product with Serializable
  case class BlankLine(line: String) extends Product with Serializable

  /**
    * Coproduct for the content of lines on the parsed files
    */
  type Row = Comment :+: BlankLine :+: Link :+: Rule :+: Zone :+: CNil

  implicit class ToCoproduct[A](val a: A) extends AnyVal {
    def liftC[C <: Coproduct](implicit inj: Inject[C, A]): C = Coproduct[C](a)
  }

}

/**
  * Defines atto parsers to read tzdb files
  */
object TZDBParser {
  import TZDB._
  import scalaz._
  import scalaz.effect._
  import Scalaz._
  import atto._, Atto.{char => chr, _}, compat.scalaz._
  import better.files._

  // Useful Monoid
  implicit def parserListMonoid[A]: Monoid[ParseResult[List[A]]] =
    Monoid.instance[ParseResult[List[A]]]((a, b) => (a, b) match {
      case (Done(u, x), Done(v, y)) => Done(u + v, x ::: y)
      case _                        => Fail("", Nil, "Can only handle full response ")
    }, Done("", List.empty[A]))

  implicit class Parser2Coproduct[A](val a: Parser[A]) extends AnyVal {
    def liftC[C <: shapeless.Coproduct](implicit inj: shapeless.ops.coproduct.Inject[C, A]): Parser[C] = a.map(_.liftC[C])
  }

  private val space = chr(' ')
  private val semicolon = chr(':')
  private val tab = chr('\t')
  private val nl = chr('\n')
  private val identifier = stringOf1(noneOf(" \t\n"))

  private val whitespace: Parser[Char] = tab | space
  private val linkSeparator: Parser[List[Char]] = many(whitespace)

  private val months: List[(String, Month)] = Month.values().map(m => (m.getDisplayName(TextStyle.SHORT, Locale.ENGLISH), m)).toList
  private val days: List[(String, DayOfWeek)] = DayOfWeek.values().map(m => (m.getDisplayName(TextStyle.SHORT, Locale.ENGLISH), m)).toList

  val from: Parser[String] =
    stringOf1(digit) |
    string("minimum") |
    string("maximum")

  val fromParser: Parser[Year] = {
    stringOf1(digit).map(y => GivenYear(y.toInt)) |
    string("minimum").map(_ => Minimum: Year) |
    string("maximum").map(_ => Maximum: Year) |
    string("max").map(_ => Maximum: Year) |
    string("min").map(_ => Minimum: Year)
  }

  val toParser: Parser[Year] = {
    stringOf1(digit).map(y => GivenYear(y.toInt)) |
    string("minimum").map(_ => Minimum: Year) |
    string("maximum").map(_ => Maximum: Year) |
    string("max").map(_ => Maximum: Year) |
    string("min").map(_ => Minimum: Year) |
    string("only").map(_ => Only: Year)
  }

  def parseOneOf[A](items: List[(String, A)], msg: String): Parser[A] = {
    val p = items.map {
      case (i, v) => string(i).map(_ => v)
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

  val hourMinParserLT: Parser[LocalTime] = hourMinParser.map {
    case (_, h, m) => LocalTime.of(fixHourRange(h), m)
  }

  val hourMinParserOf: Parser[GmtOffset] = hourMinParser.map {
    case (n, h, m) if n => GmtOffset(-h, -m, 0)
    case (_, h, m)      => GmtOffset( h,  m, 0)
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

  val hourMinSecParserLT: Parser[LocalTime] = hourMinSecParser.map {
      case (_, h, m, s) => LocalTime.of(fixHourRange(h), m, s)
    }

  val hourMinSecParserOf: Parser[GmtOffset] = hourMinSecParser.map {
      case (neg, h, m, s) if neg => GmtOffset(-h, -m, -s)
      case (_  , h, m, s)        => GmtOffset( h,  m,  s)
    }

  val timeParser: Parser[LocalTime] =
    opt(many(whitespace)) ~>
    hourMinSecParserLT |
    hourMinParserLT |
    int.map(h => LocalTime.of(fixHourRange(h), 0))

  private def fixHourRange(h: Int) = (h === 24) ? 0 | h

  val gmtOffsetParser: Parser[GmtOffset] =
    hourMinSecParserOf |
    hourMinParserOf |
    int.map(h => GmtOffset(h, 0, 0))

  val atParser: Parser[At] =
    (timeParser ~ chr('w')).map(x => AtWallTime(x._1): At) |
    (timeParser ~ chr('s')).map(x => AtStandardTime(x._1): At) |
    (timeParser ~ chr('u')).map(x => AtUniversalTime(x._1): At) |
    timeParser.map(x => AtWallTime(x): At)

  val saveParser: Parser[Save] =
    timeParser.map(x => Save(x))

  val toEndLine: Parser[String] = takeWhile(_ =/= '\n') <~ opt(nl)

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
    } yield Rule(name, from, to, month, on, at, save, letter)

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
      day   <- opt(int.map(DayOfTheMonth.apply)) <~ many(whitespace)
      at    <- opt(atParser)
      _     <- toEndLine
    } yield Until(year, month, day, at)

  val commentParser: Parser[Comment] =
    chr('#') ~> toEndLine.map(Comment.apply)

  val zoneTransitionParser: Parser[ZoneTransition] =
    for {
      gmtOff <- many(whitespace) ~> gmtOffsetParser <~ whitespace
      rules  <- identifier <~ many(whitespace)
      format <- identifier <~ many(whitespace)
      until  <- opt(untilParser)
      _      <- opt(many(commentParser))
    } yield ZoneTransition(gmtOff, rules, format, until)

  val continuationZoneTransitionParser: Parser[ZoneTransition] =
    for {
      _      <- manyN(3, whitespace)
      gmtOff <- gmtOffsetParser <~ whitespace
      rules  <- opt(whitespace) ~> identifier <~ many(whitespace)
      format <- identifier <~ many(whitespace)
      until  <- opt(untilParser)
      _      <- opt(many(many(whitespace) ~> commentParser))
    } yield ZoneTransition(gmtOff, rules, format, until)

  val zoneTransitionListParser: Parser[List[ZoneTransition]] =
    (zoneTransitionParser ~ many(continuationZoneTransitionParser)).map { case (a, b) => a :: b }

  val zoneParser: Parser[Zone] =
    for {
      _      <- string("Zone") <~ whitespace
      name   <- identifier <~ whitespace
      trans  <- zoneTransitionListParser
    } yield Zone(name, trans)

  val blankLine: Parser[BlankLine] =
    nl.map(_ => BlankLine(""))
    //many(whitespace <~ nl).map(c => BlankLine(c.mkString))

  val fileParser: Parser[List[Row]] =
    for {
      c <- many(commentParser.liftC[Row] | ruleParser.liftC[Row] | zoneParser.liftC[Row] | linkParser.liftC[Row] | blankLine.liftC[Row])
    } yield c

  def parseFile(text: String): ParseResult[List[Row]] = {
    fileParser parseOnly text
  }

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

  /**
    * Entry point. Takes a dir with the TZDB files and parses them into Rows
    */
  def parseAll(dir: File): IO[List[Row]] = IO {
    dir match {
      case File.Type.SymbolicLink(_) => Nil
      case File.Type.Directory(files) =>
        val parsed = files.filter(f => tzdbFiles.contains(f.name)).map(f => parseFile(f.contentAsString))
        parsed.toList.suml match {
          case Done(_, v) => v
          case _          => Nil
        }
      case File.Type.RegularFile(_) => Nil
      case _ => Nil
    }
  }
}


