package kuyfi

import java.io.File
import java.nio.file.Files
import java.time.{DayOfWeek, LocalTime, Month}
import java.time.format.TextStyle
import java.util.Locale

import scalaz._
import Scalaz._
import atto._
import atto.Atto.{char => chr, _}
import compat.scalaz._

import scala.collection.JavaConverters._

object TZDBParser {
  case class GmtOffset(h: Int, m: Int, s: Int)
  case class Until(y: Int, m: Option[Month], d: Option[DayOfTheMonth], at: Option[RuleAt])
  case class ZoneTransition(at: GmtOffset, rules: String, format: String, until: Option[Until])
  case class Zone(name: String, transitions: List[ZoneTransition])

  case class RuleLetter(letter: String)
  case class RuleSave(time: LocalTime)

  sealed trait RuleAt extends Product with Serializable {
    def time: LocalTime
  }
  case class AtWallTime(time: LocalTime) extends RuleAt
  case class AtStandardTime(time: LocalTime) extends RuleAt
  case class AtUniversalTime(time: LocalTime) extends RuleAt

  sealed trait RuleOn extends Product with Serializable
  case class DayOfTheMonth(i: Int) extends RuleOn
  case class LastWeekday(d: DayOfWeek) extends RuleOn
  case class AfterWeekday(d: DayOfWeek, day: Int) extends RuleOn
  case class BeforeWeekday(d: DayOfWeek, day: Int) extends RuleOn

  sealed trait RuleYear extends Product with Serializable
  case class GivenYear(year: Int) extends RuleYear
  case object Minimum extends RuleYear
  case object Maximum extends RuleYear
  case object Only extends RuleYear

  case class Rule(name: String, from: RuleYear, to: RuleYear, month: Month, on: RuleOn, at: RuleAt, save: RuleSave, letter: RuleLetter) extends Product with Serializable
  case class Comment(comment: String) extends Product with Serializable
  case class BlankLine(line: String) extends Product with Serializable

  case class Link(from: String, to: String)

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

  val fromParser: Parser[RuleYear] = {
    stringOf1(digit).map(y => GivenYear(y.toInt)) |
    string("minimum").map(_ => Minimum: RuleYear) |
    string("maximum").map(_ => Maximum: RuleYear) |
    string("max").map(_ => Maximum: RuleYear)
  }

  val toParser: Parser[RuleYear] = {
    stringOf1(digit).map(y => GivenYear(y.toInt)) |
    string("minimum").map(_ => Minimum: RuleYear) |
    string("maximum").map(_ => Maximum: RuleYear) |
    string("max").map(_ => Maximum: RuleYear) |
    string("only").map(_ => Only: RuleYear)
  }

  def parseOneOf[A](items: List[(String, A)], msg: String): Parser[A] = {
    val p = items.map {
      case (i, v) => string(i).map(_ => v)
    }

    p.foldRight(err[A](msg))(_ | _)
  }

  val monthParser: Parser[Month] = parseOneOf(months, "unknown month")

  val dayParser: Parser[DayOfWeek] = parseOneOf(days, "unknown day")

  val afterWeekdayParser: Parser[RuleOn] =
    for {
      d <- dayParser <~ string(">=")
      a <- int
    } yield AfterWeekday(d, a)

  val beforeWeekdayParser: Parser[RuleOn] =
    for {
      d <- dayParser <~ string("<=")
      a <- int
    } yield BeforeWeekday(d, a)

  val lastWeekdayParser: Parser[RuleOn] =
    for {
      _ <- string("last")
      d <- dayParser
    } yield LastWeekday(d)

  val onParser: Parser[RuleOn] =
    (opt(space) ~> int.map(DayOfTheMonth.apply)) |
    afterWeekdayParser |
    beforeWeekdayParser |
    lastWeekdayParser

  val timePartParser: Parser[Char] =
    digit | semicolon

  val hourMinParser: Parser[(Int, Int)] =
    for {
      _ <- opt(space)
      h <- int <~ semicolon
      m <- int
    } yield (h, m)

  val hourMinParserLT: Parser[LocalTime] = hourMinParser.map {
    case (h, m) => LocalTime.of((h === 24) ? 0 | h, m)
  }

  val hourMinParserOf: Parser[GmtOffset] = hourMinParser.map {
    case (h, m) => GmtOffset(h, m, 0)
  }

  val hourMinSecParser: Parser[(Boolean, Int, Int, Int)] =
    for {
      n <- opt(chr('-'))
      _ <- opt(space)
      h <- int <~ semicolon
      m <- int <~ semicolon
      s <- int
    } yield (n.isDefined, h, m, s)

  val hourMinSecParserLT: Parser[LocalTime] = hourMinSecParser.map {
      case (_, h, m, s) => LocalTime.of((h === 24) ? 0 | h, m, s)
    }

  val hourMinSecParserOf: Parser[GmtOffset] = hourMinSecParser.map {
      case (n, h, m, s) => GmtOffset(n ? -h | h, (n && h === 0) ? -m | m, s)
    }

  val timeParser: Parser[LocalTime] =
    opt(many(whitespace)) ~>
    hourMinSecParserLT |
    hourMinParserLT |
    int.map(h => LocalTime.of((h === 24) ? 0 | h, 0))

  val gmtOffsetParser: Parser[GmtOffset] =
    hourMinSecParserOf |
    hourMinParserOf |
    int.map(h => GmtOffset(h, 0, 0))

  val atParser: Parser[RuleAt] =
    (timeParser ~ chr('w')).map(x => AtWallTime(x._1): RuleAt) |
    (timeParser ~ chr('s')).map(x => AtStandardTime(x._1): RuleAt) |
    (timeParser ~ chr('u')).map(x => AtUniversalTime(x._1): RuleAt) |
    timeParser.map(x => AtWallTime(x): RuleAt)

  val saveParser: Parser[RuleSave] =
    timeParser.map(x => RuleSave(x))

  val toEndLine: Parser[String] = takeWhile(_ =/= '\n') <~ opt(nl)

  val letterParser: Parser[RuleLetter] =
    for {
      l <- takeWhile(c => c.isUpper || c === '-')
      _ <- toEndLine
    } yield RuleLetter(l)

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
      rules  <- identifier <~ many(whitespace)
      format <- identifier <~ many(whitespace)
      until  <- opt(untilParser)
      _      <- opt(many(commentParser))
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

  val fileParser: Parser[Any] =
    for {
      c <- many(commentParser || ruleParser || zoneParser || linkParser || blankLine)
    } yield c

  val files: List[String] = List(
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

  def parseFile(text: String): ParseResult[Any] = {
    fileParser parseOnly text
  }

  def main(args: Array[String]): Unit = {
    //generateTZDataSources(new File("src/main/scala/zone/data"), new File("jvm/src/main/resources/tzdb"))
  }
}
