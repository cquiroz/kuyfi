package kuyfi

import java.time._
import java.time.chrono.IsoChronology
import java.time.zone.ZoneOffsetTransitionRule.TimeDefinition
import java.time.temporal.TemporalAdjusters
import java.time.zone.{ZoneOffsetTransition, ZoneRules, ZoneOffsetTransitionRule}

import shapeless._
import shapeless.ops.coproduct.Inject

import cats.Order
import mouse.all._

/**
  * Model of the TimeZone Database
  */
object TZDB {

  /**
    * Definition of timestamps
    */
  sealed trait At extends Product with Serializable {
    def time: LocalTime
    def endOfDay: Boolean
    def noEndOfDay: At
    def timeDefinition: TimeDefinition
    def rollOver: Int
    def adjustDateForEndOfDay(d: LocalDate): LocalDate = endOfDay.fold(d.plusDays(1), d)
  }
  final case class AtWallTime(time: LocalTime, endOfDay: Boolean, rollOver: Int) extends At {
    override def noEndOfDay = copy(endOfDay = false)
    val timeDefinition = TimeDefinition.WALL
  }
  object AtWallTime {
    def apply(time: LocalTime): At = AtWallTime(time, time.getHour == 24 && time.getMinute == 0 && time.getSecond == 0, rollOver = 0)
  }

  final case class AtStandardTime(time: LocalTime, endOfDay: Boolean, rollOver: Int) extends At {
    override def noEndOfDay = copy(endOfDay = false)
    val timeDefinition = TimeDefinition.STANDARD
  }
  object AtStandardTime {
    def apply(time: LocalTime): At = this(time, time.getHour == 24 && time.getMinute == 0 && time.getSecond == 0, rollOver = 0)
  }

  final case class AtUniversalTime(time: LocalTime, endOfDay: Boolean, rollOver: Int) extends At{
    override def noEndOfDay = copy(endOfDay = false)
    val timeDefinition = TimeDefinition.UTC
  }
  object AtUniversalTime {
    def apply(time: LocalTime): At = this(time, time.getHour == 24 && time.getMinute == 0 && time.getSecond == 0, rollOver = 0)
  }

  object At {
    implicit val order: Order[At] = Order.from { (a, b) => a.time.compareTo(b.time) }
  }

  /**
    * Model for Zone entries on TZDB
    */
  final case class GmtOffset(h: Int, m: Int, s: Int) {
    def toZoneOffset: ZoneOffset = ZoneOffset.ofHoursMinutesSeconds(h, m, s)
  }

  object GmtOffset {
    val zero: GmtOffset = GmtOffset(0, 0, 0)
  }

  final case class Until(y: Int, m: Option[Month], on: Option[On], at: Option[At]) {

    def toDateTime: LocalDateTime = {
      val month = m.getOrElse(Month.JANUARY)
      val date = on.map(_.dateTimeInContext(y, month)).getOrElse(LocalDate.of(y, month, 1))
      val time = at.map(_.time).getOrElse(LocalTime.MIDNIGHT)
      val adjustedDate = at.map(_.adjustDateForEndOfDay(date)).getOrElse(date)
      LocalDateTime.of(adjustedDate, time)
    }
  }

  sealed trait ZoneRule extends Product with Serializable {
    def fixedOffset: Option[Int] = None
  }

  case object NullRule extends ZoneRule {
    override val fixedOffset: Option[Int] = Some(0)
  }
  final case class FixedOffset(offset: GmtOffset) extends ZoneRule {
    override val fixedOffset: Option[Int] = Some(Duration.ofHours(offset.h.toLong).plusMinutes(offset.m.toLong).plusSeconds(offset.s.toLong).getSeconds.toInt)
  }
  final case class RuleId(id: String) extends ZoneRule

  final case class ZoneTransition(offset: GmtOffset, ruleId: ZoneRule, format: String, until: Option[Until])
  final case class Zone(name: String, transitions: List[ZoneTransition])  extends Product with Serializable {
    def scalaSafeName: String = name.replace("-", "_minus_").replace("+", "_plus_").replaceAll("/|-|\\+", "_")
    def scalaGroup(z: Int): String = name.substring(0, z).toLowerCase + name.substring(name.length - z, name.length - 1).toLowerCase
  }

  /**
    * Model for Rule Entries
    */
  final case class Letter(letter: String)
  final case class Save(positive: Boolean, time: LocalTime) {
    val seconds: Int = {
      val factor = if (positive) 1 else -1
      factor * (time.getHour * 3600 + time.getMinute * 60 + time.getSecond)
    }
  }

  sealed trait On extends Product with Serializable {
    def dayOfMonthIndicator: Option[Int] = None
    def dayOnYear(y: Int, m: Month): Int
    def dayOfWeek: Option[DayOfWeek] = None
    def onDay(d: Int): On = this
    def dateTimeInContext(y: Int, month: Month): LocalDate = {
      val lastDay = month.length(Year.isLeap(y.toLong))
      (dayOfMonthIndicator, dayOfWeek) match {
        case (None, Some(dw)) => LocalDate.of(y, month, lastDay).`with`(TemporalAdjusters.lastInMonth(dw))
        case (None, None)     => LocalDate.of(y, month, lastDay)
        case (Some(_), _)     => LocalDate.of(y, month, dayOnYear(y, month))
      }
    }
    def adjustRoll(hrs: Int): On =
      this match {
        case d: DayOfTheMonth => DayOfTheMonth(d.i + (1 + (hrs / 24)))
        case d: LastWeekday => LastWeekday(d.d.plus(((1 + (hrs / 24))).toLong))
        case d: AfterWeekday => d.copy(day = d.day + (1 + (hrs / 24)))
        case d: BeforeWeekday => d.copy(day = d.day + (1 + (hrs / 24)))
      }
  }
  final case class DayOfTheMonth(i: Int) extends On {
    override val dayOfMonthIndicator = Some(i)
    override def onDay(d: Int) = DayOfTheMonth(d)
    override def dayOnYear(y: Int, m: Month): Int = i
  }
  final case class LastWeekday(d: DayOfWeek) extends On {
    override def dayOfWeek: Option[DayOfWeek] = Some(d)
    override def dayOnYear(y: Int, m: Month): Int = {
      val lastDay = m.length(Year.isLeap(y.toLong))
      LocalDate.of(y, m, lastDay).`with`(TemporalAdjusters.previousOrSame(d)).getDayOfMonth
    }
  }
  final case class AfterWeekday(d: DayOfWeek, day: Int) extends On {
    override val dayOfMonthIndicator = Some(day)
    override def onDay(i: Int) = AfterWeekday(d.plus(1), i)
    override def dayOfWeek: Option[DayOfWeek] = Some(d)
    override def dayOnYear(y: Int, m: Month): Int = LocalDate.of(y, m, day).`with`(TemporalAdjusters.nextOrSame(d)).getDayOfMonth
  }
  final case class BeforeWeekday(d: DayOfWeek, day: Int) extends On {
    override val dayOfMonthIndicator = Some(day)
    override def onDay(i: Int) = BeforeWeekday(d.plus(1), i)
    override def dayOfWeek: Option[DayOfWeek] = Some(d)
    override def dayOnYear(y: Int, m: Month): Int = LocalDate.of(y, m, day).`with`(TemporalAdjusters.previousOrSame(d)).getDayOfMonth
  }

  sealed trait RuleYear extends Product with Serializable
  final case class GivenYear(year: Int) extends RuleYear
  case object Minimum extends RuleYear
  case object Maximum extends RuleYear
  case object Only extends RuleYear

  object RuleYear {
    implicit val order: Order[RuleYear] = Order.from { (a, b) => (a, b) match {
        case (GivenYear(x), GivenYear(y)) => x.compareTo(y)
        case (Maximum, Maximum)           => 0
        case (Minimum, Minimum)           => 0
        case (Only, Only)                 => 0
        case (Only, _)                    => -1
        case (_, Only)                    => 1
        case (_, Maximum)                 => -1
        case (Maximum, _)                 => 1
        case (Minimum, _)                 => -1
        case (_, Minimum)                 => 1
        case _                            => 0
      }
    }
  }

  final case class ZoneOffsetParams(transition: LocalDateTime, offsetBefore: ZoneOffset, offsetAfter: ZoneOffset) {
    def toEpochSecond: Long = transition.toEpochSecond(offsetBefore)
    def toZoneOffsetTransition: ZoneOffsetTransitionParams = ZoneOffsetTransitionParams(transition, offsetBefore, offsetAfter)
  }

  final case class Rule(name: String, from: RuleYear, to: RuleYear, month: Month, on: On, at: At, save: Save, letter: Letter) extends Product with Serializable {
    private def toInt(y: RuleYear, defaultY: Int): Int =
      y match {
        case GivenYear(x) => x
        case Maximum      => Year.MAX_VALUE
        case Minimum      => Year.MIN_VALUE
        case Only         => defaultY
      }

    val startYear: Int = toInt(from, 0)
    val endYear: Int   = toInt(to, startYear)

    def adjustRoll: Rule =
      if (at.rollOver > 0) {
        copy(on = on.adjustRoll(at.rollOver))
      } else {
        this
      }

    def adjustForwards: Rule = on match {
      case BeforeWeekday(weekDay, dayOfMonth) =>
        val adjustedDate = LocalDate.of(2004, this.month, dayOfMonth).minusDays(6)
        val before = BeforeWeekday(weekDay, adjustedDate.getDayOfMonth)
        val month = adjustedDate.getMonth
        copy(month = month, on = before)
      case _                   => this
    }

    def toLocalDate: LocalDate = {
      val date = on match {
        case LastWeekday(d)              =>
          val monthLen: Int = month.length(IsoChronology.INSTANCE.isLeapYear(startYear.toLong))
          LocalDate.of(startYear, month, monthLen).`with`(TemporalAdjusters.previousOrSame(d))
        case DayOfTheMonth(d)            =>
          LocalDate.of(startYear, month, d)
        case AfterWeekday(dayOfWeek, d)  =>
          LocalDate.of(startYear, month, d).`with`(TemporalAdjusters.nextOrSame(dayOfWeek))
        case BeforeWeekday(dayOfWeek, d) =>
          LocalDate.of(startYear, month, d).`with`(TemporalAdjusters.nextOrSame(dayOfWeek))
      }
      at.adjustDateForEndOfDay(date)
    }

    def toTransitionParams(standardOffset: ZoneOffset, savingsBeforeSecs: Int): ZoneOffsetParams = {
      val ldt = LocalDateTime.of(toLocalDate, at.time)
      val wallOffset = ZoneOffset.ofTotalSeconds(standardOffset.getTotalSeconds + savingsBeforeSecs)
      val dt = at.timeDefinition.createDateTime(ldt, standardOffset, wallOffset)
      val offsetAfter = ZoneOffset.ofTotalSeconds(standardOffset.getTotalSeconds + save.seconds)
      ZoneOffsetParams(dt, wallOffset, offsetAfter)
    }

    def toTransitionRule(standardOffset: ZoneOffset, savingsBeforeSecs: Int): (ZoneOffsetTransitionRule, Rule) = {
      def transitionRule(dayOfMonthIndicator: Int, dayOfWeek: DayOfWeek, endOfDay: Boolean) = {
        val trans = toTransitionParams(standardOffset, savingsBeforeSecs)
        ZoneOffsetTransitionRule.of(month, dayOfMonthIndicator, dayOfWeek, at.time, endOfDay, at.timeDefinition, standardOffset, trans.offsetBefore, trans.offsetAfter)
      }

      val dayOfMonth = on.dayOfMonthIndicator.orElse((month != Month.FEBRUARY) option month.maxLength - 6)
      dayOfMonth.fold((transitionRule(-1, on.dayOfWeek.orNull, at.endOfDay), this)){ d =>
        if (at.endOfDay && !(d == 28 && (month == Month.FEBRUARY))) {
          val date = LocalDate.of(2004, month, d).plusDays(1)
          (transitionRule(date.getDayOfMonth, on.dayOfWeek.map(_.plus(1)).orNull, endOfDay = false), copy(on = on.onDay(date.getDayOfMonth), month = date.getMonth, at = at.noEndOfDay))
        } else {
          (transitionRule(d, on.dayOfWeek.orNull, at.endOfDay), this)
        }
      }
    }

  }

  /**
    * Model for Link entries
    */
  final case class Link(from: String, to: String) extends Product with Serializable

  /**
    * Comments and blank lines
    */
  final case class Comment(comment: String) extends Product with Serializable
  final case class BlankLine(line: String) extends Product with Serializable

  final case class ZoneOffsetTransitionParams(transition: LocalDateTime,
                                        offsetBefore: ZoneOffset,
                                        offsetAfter: ZoneOffset) {
    def toOffsetTransition: ZoneOffsetTransition =
      ZoneOffsetTransition.of(transition, offsetBefore, offsetAfter)
  }

  sealed trait ZoneRulesParams {
    def toZoneRules: ZoneRules
  }
  final case class FixedZoneRulesParams(baseStandardOffset: ZoneOffset,
                             baseWallOffset: ZoneOffset,
                             standardOffsetTransitionList: List[ZoneOffsetTransitionParams],
                             transitionList: List[ZoneOffsetTransitionParams],
                             lastRules: List[ZoneOffsetTransitionRule]) extends ZoneRulesParams {
    import scala.collection.JavaConverters._
    def toZoneRules = ZoneRules.of(baseStandardOffset, baseWallOffset, standardOffsetTransitionList.map(_.toOffsetTransition).asJava, transitionList.map(_.toOffsetTransition).asJava, lastRules.asJava)
  }
  final case class StandardRulesParams(baseStandardOffset: ZoneOffset,
                             baseWallOffset: ZoneOffset,
                             standardOffsetTransitionList: List[ZoneOffsetTransitionParams],
                             transitionList: List[ZoneOffsetTransitionParams],
                             lastRules: List[ZoneOffsetTransitionRule]) extends ZoneRulesParams {
    import scala.collection.JavaConverters._
    def toZoneRules = ZoneRules.of(baseStandardOffset, baseWallOffset, standardOffsetTransitionList.map(_.toOffsetTransition).asJava, transitionList.map(_.toOffsetTransition).asJava, lastRules.asJava)
  }

  final case class TzdbVersion(ver: String)
  val DefaultTzdbVersion: TzdbVersion = TzdbVersion("Unknown")

  /**
    * Coproduct for the content of lines on the parsed files
    */
  type Row = Comment :+: BlankLine :+: Link :+: Rule :+: Zone :+: CNil

  implicit class ToCoproduct[A](val a: A) extends AnyVal {
    def liftC[C <: Coproduct](implicit inj: Inject[C, A]): C = Coproduct[C](a)
  }

}
