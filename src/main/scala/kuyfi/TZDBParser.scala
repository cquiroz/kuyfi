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
import scala.collection.breakOut

object TZDBParser {
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

  case class Rule(name: String, from: RuleYear, to: RuleYear, month: Month, on: RuleOn, at: RuleAt, save: RuleSave, letter: RuleLetter)

  val tab: Parser[Char] = chr('\t') | chr(' ')
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

  private val months: List[(String, Month)] = Month.values().map(m => (m.getDisplayName(TextStyle.SHORT, Locale.ENGLISH), m)).toList
  private val days: List[(String, DayOfWeek)] = DayOfWeek.values().map(m => (m.getDisplayName(TextStyle.SHORT, Locale.ENGLISH), m)).toList

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
    (opt(chr(' ')) ~> int.map(DayOfTheMonth.apply)) |
    afterWeekdayParser |
    beforeWeekdayParser |
    lastWeekdayParser
  val timePartParser: Parser[Char] =
    digit | chr(':')
  val hourMinParser: Parser[LocalTime] =
    for {
      _ <- opt(chr(' '))
      h <- int <~ chr(':')
      m <- int
    } yield LocalTime.of((h === 24) ? 0 | h, m)
  val hourMinSecParser: Parser[LocalTime] =
    for {
      h <- int <~ chr(':')
      m <- int <~ chr(':')
      s <- int
    } yield LocalTime.of((h === 24) ? 0 | h, m, s)
  val timeParser: Parser[LocalTime] =
    hourMinSecParser |
    hourMinParser |
    int.map(h => LocalTime.of((h === 24) ? 0 | h, 0))
  val atParser: Parser[RuleAt] =
    (timeParser ~ chr('w')).map(x => AtWallTime(x._1): RuleAt) |
    (timeParser ~ chr('s')).map(x => AtStandardTime(x._1): RuleAt) |
    (timeParser ~ chr('u')).map(x => AtUniversalTime(x._1): RuleAt) |
    timeParser.map(x => AtWallTime(x): RuleAt)
  val saveParser: Parser[RuleSave] =
    timeParser.map(x => RuleSave(x))
  val letterParser: Parser[RuleLetter] =
    for {
      l <- takeWhile(c => c.isUpper || c === '-')
      _ <- takeWhile(_ =/= '\n')
    } yield RuleLetter(l)

  val ruleParser: Parser[Rule] = for {
    _      <- string("Rule") <~ tab
    name   <- stringOf1(notChar('\t')) <~ tab
    from   <- fromParser <~ tab
    to     <- toParser <~ tab
    _      <- chr('-') <~ tab
    month  <- monthParser <~ tab
    on     <- onParser <~ tab
    at     <- atParser <~ tab
    save   <- saveParser <~ tab
    letter <- letterParser
  } yield Rule(name, from, to, month, on, at, save, letter)

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

  def parseRule = {
    string("Rule") ~ tab ~ stringOf1(notChar('\t')) ~ tab
  }

  def toSource(f: File) = {


    string("RowOnly") parseOnly "123abc"
  }

  def generateTZDataSources(base: File, data: File): Seq[File] = {
    val r = Files.list(data.toPath).iterator().asScala.collect {
      case f if files.contains(f.toFile.getName) => toSource(f.toFile)
    }
    println(parseRule)
    Nil
  }

  def main(args: Array[String]): Unit = {
    generateTZDataSources(new File("src/main/scala/zone/data"), new File("jvm/src/main/resources/tzdb"))
  }
}
