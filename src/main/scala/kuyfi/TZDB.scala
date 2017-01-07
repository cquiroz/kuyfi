package kuyfi

import java.time._
import java.time.zone.ZoneOffsetTransitionRule.TimeDefinition

import shapeless._
import shapeless.ops.coproduct.Inject

import scalaz.Order
import scalaz.Ordering

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

  object At {
    // TODO move to an aux class
    def toTimeDefinition(at: At): TimeDefinition = at match {
      case AtWallTime(_)      => TimeDefinition.WALL
      case AtStandardTime(_)  => TimeDefinition.STANDARD
      case AtUniversalTime(_) => TimeDefinition.UTC
    }
  }

  /**
    * Model for Zone entries on TZDB
    */
  case class GmtOffset(h: Int, m: Int, s: Int) {
    // TODO move to an aux class
    def toZoneOffset: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(h, m, s)
  }

  object GmtOffset {
    val zero: GmtOffset = GmtOffset(0, 0, 0)
  }

  case class Until(y: Int, m: Option[Month], d: Option[DayOfTheMonth], at: Option[At]) {
    // TODO move to an aux class
    def toDateTime: LocalDateTime = {
      val month = m.getOrElse(Month.JANUARY)
      val date: LocalDate = d.fold {
        val dayOfMonth = month.length(Year.isLeap(y))
        LocalDate.of(y, month, dayOfMonth)
      } { dayOfTheMonth =>
        LocalDate.of(y, month, dayOfTheMonth.i)
      }
      LocalDateTime.of(date, LocalTime.MIDNIGHT)
    }

  }

  sealed trait ZoneRule extends Product with Serializable {
    def fixedOffset: Option[Int] = None
  }

  case object NullRule extends ZoneRule {
    override val fixedOffset: Option[Int] = Some(0)
  }
  case class FixedOffset(offset: GmtOffset) extends ZoneRule {
    override val fixedOffset: Option[Int] = Some(Duration.ofHours(offset.h).plusMinutes(offset.m).plusSeconds(offset.s).getSeconds.toInt)
  }
  case class RuleId(id: String) extends ZoneRule

  case class ZoneTransition(offset: GmtOffset, ruleId: ZoneRule, format: String, until: Option[Until])
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

  sealed trait RuleYear extends Product with Serializable
  case class GivenYear(year: Int) extends RuleYear
  case object Minimum extends RuleYear
  case object Maximum extends RuleYear
  case object Only extends RuleYear

  object RuleYear {
    implicit val order: Order[RuleYear] = Order.order { (a, b) => (a, b) match {
        case (GivenYear(x), GivenYear(y)) => Ordering.fromInt(x.compareTo(y))
        case (Maximum, Maximum) => Ordering.EQ
        case (Minimum, Minimum) => Ordering.EQ
        case (Only, Only)       => Ordering.EQ
        case (Only, _)          => Ordering.LT
        case (_, Only)          => Ordering.GT
        case (_, Maximum)       => Ordering.LT
        case (Maximum, _)       => Ordering.GT
        case (Minimum, _)       => Ordering.LT
        case (_, Minimum)       => Ordering.GT
        case _                  => Ordering.EQ
      }
    }
  }

  case class Rule(name: String, from: RuleYear, to: RuleYear, month: Month, on: On, at: At, save: Save, letter: Letter) extends Product with Serializable {
    private def toInt(y: RuleYear, defaultY: Int): Int =
      y match {
        case GivenYear(x) => x
        case Maximum      => Year.MAX_VALUE
        case Minimum      => Year.MIN_VALUE
        case Only         => defaultY
      }

    val startYear: Int = toInt(from, 0)
    val endYear: Int   = toInt(to, startYear)

    def adjustForwards: Rule = on match {
      case BeforeWeekday(weekDay, dayOfMonth) =>
        val adjustedDate: LocalDate = LocalDate.of(2004, this.month, dayOfMonth).minusDays(6)
        val before = BeforeWeekday(weekDay, adjustedDate.getDayOfMonth)
        val month = adjustedDate.getMonth
        copy(month = month, on = before)
      case _                   => this
    }
  }

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
