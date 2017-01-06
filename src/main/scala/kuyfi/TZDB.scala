package kuyfi

import java.time._
import java.time.zone.ZoneOffsetTransitionRule.TimeDefinition

import shapeless._
import shapeless.ops.coproduct.Inject

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
      //adjustToFowards(year)
      //var date: LocalDate = null
      val month = m.getOrElse(Month.JANUARY)
      val date: LocalDate = d.fold {
        val dayOfMonth = month.length(Year.isLeap(y))
        LocalDate.of(y, month, dayOfMonth)
      } { dayOfTheMonth =>
        LocalDate.of(y, month, dayOfTheMonth.i)
      }
      /*if (dayOfMonth == -1) {
        dayOfMonth = month.length(Year.isLeap(year))
        date = LocalDate.of(year, month, dayOfMonth)
        if (dayOfWeek != null)
          date = date.`with`(TemporalAdjusters.previousOrSame(dayOfWeek))(/
      } else {
        date = LocalDate.of(year, month, dayOfMonth)
        if (dayOfWeek != null)
          date = date.`with`(TemporalAdjusters.nextOrSame(dayOfWeek))
      }*/
      //date = deduplicate(date)
      LocalDateTime.of(date, LocalTime.MIDNIGHT)
      /*
      if (endOfDay)
        ldt = ldt.plusDays(1)
      ldt*/
    }

  }

  sealed trait ZoneRule extends Product with Serializable

  case object NullRule extends ZoneRule
  case class FixedOffset(offset: GmtOffset) extends ZoneRule
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
