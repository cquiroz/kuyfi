package kuyfi

import java.time.zone.ZoneOffsetTransitionRule.TimeDefinition
import java.time.{ LocalDateTime, Year, ZoneOffset }
import kuyfi.TZDB.{ At, GivenYear, GmtOffset, Maximum, Rule, RuleYear }

object TimeZoneWindow {
  sealed trait TimeZoneWindow {
    def standardOffset: GmtOffset
    def windowEnd: LocalDateTime
    def timeDefinition: TimeDefinition
    def fixedSavingAmountSeconds: Option[Int]
    def createWallOffset(savingsSecs: Int): ZoneOffset =
      ZoneOffset.ofTotalSeconds(standardOffset.toZoneOffset.getTotalSeconds + savingsSecs)
    def createDateTimeEpochSecond(savingsSecs: Int): Long = {
      val wallOffset = createWallOffset(savingsSecs)
      val ldt        = timeDefinition.createDateTime(windowEnd, standardOffset.toZoneOffset, wallOffset)
      ldt.toEpochSecond(wallOffset)
    }
    def tidy(windowStartYear:         Int): TimeZoneWindow
  }

  def ruleOrderings(f: Rule => RuleYear): scala.Ordering[Rule] =
    new scala.Ordering[Rule]() {

      private val ruleOrdering = RuleYear.order.toOrdering
      private val atOrdering   = At.order.toOrdering
      override def compare(x: Rule, y: Rule): Int = {
        val rulesCmp = ruleOrdering.compare(f(x), f(y))
        if (rulesCmp == 0) {
          val monthCmp = x.month.compareTo(y.month)
          if (monthCmp == 0) {
            val thisDate  = x.toLocalDate
            val otherDate = y.toLocalDate
            val dateCmp   = thisDate.compareTo(otherDate)
            if (dateCmp == 0)
              atOrdering.compare(x.at, y.at)
            else
              dateCmp
          } else
            monthCmp
        } else
          rulesCmp
      }
    }

  val ruleOrdering: scala.Ordering[Rule] = ruleOrderings(_.from)

  val ruleOrderingLast: scala.Ordering[Rule] = ruleOrderings(_.to)

  final case class WindowRules(
    lastRuleList:         List[Rule],
    maxLastRuleStartYear: Int,
    ruleList:             List[Rule]
  ) {
    def sorted: WindowRules =
      // Note the rules are sorted different with respect to the year
      copy(lastRuleList = lastRuleList.sorted(ruleOrderingLast),
           ruleList = ruleList.sorted(ruleOrdering)
      )
  }

  object WindowRules {
    val zero: WindowRules = WindowRules(Nil, Year.MIN_VALUE, Nil)
  }

  final case class FixedTimeZoneWindow(
    standardOffset:           GmtOffset,
    windowEnd:                LocalDateTime,
    timeDefinition:           TimeDefinition,
    fixedSavingAmountSeconds: Option[Int]
  ) extends TimeZoneWindow {
    override def tidy(windowStartYear: Int): TimeZoneWindow = this
  }
  final case class RulesTimeZoneWindow private (
    standardOffset:           GmtOffset,
    windowEnd:                LocalDateTime,
    timeDefinition:           TimeDefinition,
    fixedSavingAmountSeconds: Option[Int],
    windowRules:              WindowRules
  ) extends TimeZoneWindow {

    override def tidy(windowStartYear: Int): TimeZoneWindow = {
      val newWindowRules     = if (windowEnd == LocalDateTime.MAX) {
        val maxLastRuleStartYear = Math.max(windowRules.maxLastRuleStartYear, windowStartYear) + 1
        val lastRules            = windowRules.lastRuleList.map(_.copy(to = GivenYear(maxLastRuleStartYear)))
        val lastRulesPlus1       =
          windowRules.lastRuleList.map(_.copy(to = GivenYear(maxLastRuleStartYear + 1)))
        val sr                   = RulesTimeZoneWindow
          .addRules(windowRules, lastRules)
          .copy(lastRuleList = lastRulesPlus1, maxLastRuleStartYear = Year.MAX_VALUE)
        if (maxLastRuleStartYear == Year.MAX_VALUE)
          sr.copy(lastRuleList = Nil, maxLastRuleStartYear = maxLastRuleStartYear)
        else
          sr.copy(maxLastRuleStartYear = maxLastRuleStartYear + 1)
      } else {
        val endYear   = windowEnd.getYear
        val lastRules = windowRules.lastRuleList.map(_.copy(to = GivenYear(endYear + 1)))
        RulesTimeZoneWindow
          .addRules(windowRules, lastRules)
          .copy(lastRuleList = Nil, maxLastRuleStartYear = Year.MAX_VALUE)
      }
      val newFixedAmountSecs =
        if (newWindowRules.ruleList.nonEmpty || fixedSavingAmountSeconds.isDefined)
          fixedSavingAmountSeconds
        else
          None
      RulesTimeZoneWindow(standardOffset,
                          windowEnd,
                          timeDefinition,
                          newFixedAmountSecs,
                          newWindowRules.sorted
      )
    }
  }

  object RulesTimeZoneWindow {

    private def addRules(rules: List[Rule]): WindowRules = addRules(WindowRules.zero, rules)

    private def addRules(originalRules: WindowRules, rules: List[Rule]): WindowRules =
      rules.foldLeft(originalRules) { (acc, rule) =>
        val (lastRule, endYear) =
          if (rule.to == Maximum)
            (true, rule.startYear)
          else
            (false, rule.endYear)
        (rule.startYear to endYear).foldLeft(acc) { (ac, year) =>
          val intermediateRule = rule.copy(from = GivenYear(year))
          if (lastRule)
            ac.copy(lastRuleList = ac.lastRuleList :+ intermediateRule,
                    maxLastRuleStartYear = Math.max(rule.startYear, ac.maxLastRuleStartYear)
            )
          else
            ac.copy(ruleList = ac.ruleList :+ intermediateRule)
        }
      }

    def apply(
      standardOffset: GmtOffset,
      windowEnd:      LocalDateTime,
      timeDefinition: TimeDefinition,
      fixedOffsetSec: Option[Int],
      rules:          List[Rule]
    ): RulesTimeZoneWindow =
      RulesTimeZoneWindow(standardOffset,
                          windowEnd,
                          timeDefinition,
                          fixedOffsetSec,
                          addRules(rules)
      )
  }

}
