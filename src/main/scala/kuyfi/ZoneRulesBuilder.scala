/*
 * Copyright (c) 2007-present, Stephen Colebourne & Michael Nascimento Santos
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of JSR-310 nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package kuyfi

import java.time.temporal.ChronoField.YEAR
import java.time.temporal.TemporalAdjusters.nextOrSame
import java.time.temporal.TemporalAdjusters.previousOrSame
import java.util.{Collections, Objects}
import java.time._
import java.time.chrono.IsoChronology
import java.time.zone.{ZoneOffsetTransition, ZoneOffsetTransitionRule, ZoneRules}
import java.time.zone.ZoneOffsetTransitionRule.TimeDefinition

import kuyfi.TZDB._
import kuyfi.ZoneRulesBuilder.{FixedTimeZoneWindow, TimeZoneWindow}
import shapeless.Poly1

import scalaz._
import Scalaz._
import scala.annotation.tailrec

object ZoneRulesBuilder {

  sealed trait TimeZoneWindow {
    def standardOffset: GmtOffset
    def windowEnd: LocalDateTime
    def timeDefinition: ZoneOffsetTransitionRule.TimeDefinition
    def fixedSavingAmountSeconds: Option[Int]
    def createWallOffset(savingsSecs: Int): ZoneOffset =
      ZoneOffset.ofTotalSeconds(standardOffset.toZoneOffset.getTotalSeconds + savingsSecs)
    def createDateTimeEpochSecond(savingsSecs: Int): Long = {
      val wallOffset = createWallOffset(savingsSecs)
      val ldt = timeDefinition.createDateTime(windowEnd, standardOffset.toZoneOffset, wallOffset)
      println("CDT " + windowEnd)
      println("CDT " + standardOffset.toZoneOffset)
      println("CDT " + wallOffset)
      ldt.toEpochSecond(wallOffset)
    }
    def tidy(windowStartYear: Int): TimeZoneWindow = {
      println("TIDY Rule " + windowStartYear)
      this
    }
  }

  implicit val ruleOrdering: scala.Ordering[Rule] = new scala.Ordering[Rule]() {
    val ruleOrdering = RuleYear.order.toScalaOrdering
    val atOrdering = At.order.toScalaOrdering
    override def compare(x: Rule, y: Rule): Int = {
      val rulesCmp = ruleOrdering.compare(x.from, y.from)
      if (rulesCmp == 0) {
        val monthCmp = x.month.compareTo(y.month)
        if (monthCmp == 0) {
          val thisDate = x.toLocalDate
          val otherDate = y.toLocalDate
          val dateCmp = thisDate.compareTo(otherDate)
          if (dateCmp == 0) {
            atOrdering.compare(x.at, y.at)
          } else {
            dateCmp
          }
        } else {
          monthCmp
        }
      } else {
        rulesCmp
      }
    }
  }


  case class SplitRules(lastRuleList: List[Rule], maxLastRuleStartYear: Int, ruleList: List[Rule]) {
    def sorted: SplitRules = copy(lastRuleList = lastRuleList.sorted, ruleList = ruleList.sorted)
  }

  object SplitRules {
    val zero: SplitRules = SplitRules(Nil, Year.MIN_VALUE, Nil)
  }

  case class FixedTimeZoneWindow(standardOffset: GmtOffset, windowEnd: LocalDateTime, timeDefinition: ZoneOffsetTransitionRule.TimeDefinition, fixedSavingAmountSeconds: Option[Int]) extends TimeZoneWindow {
    override def tidy(windowStartYear: Int): TimeZoneWindow = {
      println("TIDY Fixed " + windowEnd)
      this
    }
  }
  case class RulesTimeZoneWindow private (standardOffset: GmtOffset, windowEnd: LocalDateTime, timeDefinition: ZoneOffsetTransitionRule.TimeDefinition, fixedSavingAmountSeconds: Option[Int], splitRules: SplitRules) extends TimeZoneWindow {

    override def tidy(windowStartYear: Int): TimeZoneWindow = {
      //println("TIDY " + windowEnd)
      val newSplitRules = if (windowEnd == LocalDateTime.MAX) {
        val maxLastRuleStartYear = Math.max(splitRules.maxLastRuleStartYear, windowStartYear) + 1
        val lastRules = splitRules.lastRuleList
        val sr = RulesTimeZoneWindow.acc(splitRules, lastRules.map(_.copy(to = GivenYear(maxLastRuleStartYear)))).copy(lastRuleList = lastRules.map(_.copy(to = GivenYear(maxLastRuleStartYear + 1))),  maxLastRuleStartYear = Year.MAX_VALUE)
        if (maxLastRuleStartYear == Year.MAX_VALUE) {
          sr.copy(lastRuleList = Nil, maxLastRuleStartYear = maxLastRuleStartYear)
        } else {
          sr.copy(maxLastRuleStartYear = maxLastRuleStartYear + 1)
        }
      } else {
        val endYear: Int = windowEnd.getYear
        val lastRules = splitRules.lastRuleList
        RulesTimeZoneWindow.acc(splitRules, lastRules.map(_.copy(to = GivenYear(endYear + 1)))).copy(lastRuleList = Nil,  maxLastRuleStartYear = Year.MAX_VALUE)
      }
      val newFixedAmountSecs = if (splitRules.ruleList.isEmpty && fixedSavingAmountSeconds.isEmpty) {
        None
      } else {
        fixedSavingAmountSeconds
      }
      println("TIDY Rule " + windowStartYear)
      newSplitRules.sorted.lastRuleList.foreach(x => println(x.from + " " + x.month + " " + x.on + " " + x.at))
      println("MAX " + newSplitRules.sorted.maxLastRuleStartYear)
      newSplitRules.sorted.ruleList.foreach(x => println(x.from + " " + x.month + " " + x.on + " " + x.at))
      println("COMPPPPPPPPPPP")
      RulesTimeZoneWindow(standardOffset, windowEnd, timeDefinition, newFixedAmountSecs, newSplitRules.sorted)
    }
  }

  object RulesTimeZoneWindow {

    private def acc(rules: List[Rule]): SplitRules = acc(SplitRules.zero, rules)

    private def acc(originalRules: SplitRules, rules: List[Rule]): SplitRules = rules.foldLeft(originalRules) { (acc, rule) =>
      val (lastRule, endYear) = if (rule.to == Maximum) {
          (true, rule.startYear)
        } else {
          (false, rule.endYear)
        }
      val p = (rule.startYear to endYear).foldLeft(acc) { (ac, year) =>
        val intermediateRule = rule.copy(from = GivenYear(year))
        if (lastRule) {
          ac.copy(lastRuleList = ac.lastRuleList :+ intermediateRule, maxLastRuleStartYear = Math.max(rule.startYear, ac.maxLastRuleStartYear))
        } else {
          ac.copy(ruleList = ac.ruleList :+ intermediateRule)
        }
      }
      /*println("ADDDDDDDDDDDDD Rule " + rule.startYear)
      p.lastRuleList.foreach(x => println(x.from + " " + x.month + " " + x.on + " " + x.at))
      println("MAX " + p.maxLastRuleStartYear)
      p.ruleList.foreach(x => println(x.from + " " + x.month + " " +  x.on + " " + x.at))
      println("COMPPPPPPPPPPP")*/
      p
    }

    def apply(standardOffset: GmtOffset, windowEnd: LocalDateTime, timeDefinition: ZoneOffsetTransitionRule.TimeDefinition, fixedOffsetSec: Option[Int], rules: List[Rule]): RulesTimeZoneWindow =
      RulesTimeZoneWindow(standardOffset, windowEnd, timeDefinition, fixedOffsetSec, acc(rules))
  }

  type RulesById = Map[String, List[Rule]]

  case class WindowsCollector(rules: RulesById, zoneWindows: Map[Zone, List[TimeZoneWindow]]) {
    def toRules: Map[Zone, ZoneRules] = {
      //println("WINDOWS")
      //windows.foreach(println)
      zoneWindows.map { case (zone, windows) =>
        val zoneRules: Option[ZoneRules] = windows.headOption.map { firstWindow =>
          val loopSavings = firstWindow.fixedSavingAmountSeconds.getOrElse(0)
          // NOTE should windows be a NonEmptyList?
          val loopStandardOffset: ZoneOffset = firstWindow.standardOffset.toZoneOffset
          val firstWallOffset: ZoneOffset = ZoneOffset.ofTotalSeconds(loopStandardOffset.getTotalSeconds + loopSavings)
          val loopWindowStart: LocalDateTime = LocalDateTime.of(Year.MIN_VALUE, 1, 1, 0, 0)
          val loopWindowOffset: ZoneOffset = firstWallOffset
          println("TO RULES")
          println(firstWindow.standardOffset.toZoneOffset)
          println(firstWindow.windowEnd)
          println(firstWindow.timeDefinition)
          println(loopSavings)

          println(firstWallOffset)
          println(loopWindowStart)
          println(loopWindowOffset)
          println("BEGIN LOOP")
          windows.map(w => println(w.windowEnd))


          val q = windows.foldLeft((loopWindowStart, loopWindowOffset, loopStandardOffset, loopSavings, List.empty[ZoneOffsetTransition], List.empty[ZoneOffsetTransition], List.empty[ZoneOffsetTransitionRule])) { case ((lws, lwo, lso, ls, standardTransitions, transitionList, transitionRules), timeZoneWindow) =>
            println("WINDOW " + timeZoneWindow.windowEnd + " LWS " + lws.getYear)
            println("------------------------")
            val tzw = timeZoneWindow.tidy(lws.getYear)

            val effectiveSavings: Int = tzw.fixedSavingAmountSeconds.getOrElse {
              tzw match {
                case RulesTimeZoneWindow(_, _, _, _, splitRules) =>
                  @tailrec
                  def go(savings: Int, rule: List[Rule]): Int = rule match {
                    case h :: rest =>
                      //println("offsets XXX")
                      //println(lso)
                      //println(ls)
                      val trans = h.toTransition2(lso, ls)
                      if (trans.toEpochSecond > loopWindowStart.toEpochSecond(lwo)) {
                        savings
                      } else {
                        go(h.save.seconds, rest)
                      }
                    case Nil => 0
                  }

                  go(0, splitRules.ruleList)
                case _ => 0
              }
            }
            println("EFF Savings " + effectiveSavings)
            val (newStdTransitions, newLso) =
              if (lso != tzw.standardOffset.toZoneOffset) {
                //println("New std " + lws)
                //println("New std " + lwo)
                //println("New std " + lws.toEpochSecond(lwo))
                //println("New std " + lso)
                //println("New std " + LocalDateTime.ofEpochSecond(lws.toEpochSecond(lwo), 0, lso))
                //println("New std " + lso)
                //println("New std " + tzw.standardOffset.toZoneOffset)
                (List(ZoneOffsetTransition.of(LocalDateTime.ofEpochSecond(lws.toEpochSecond(lwo), 0, lso), lso, tzw.standardOffset.toZoneOffset)), tzw.standardOffset.toZoneOffset)
              } else {
                (Nil, lso)
              }
            println("new " + (standardTransitions ::: newStdTransitions) + " " + newLso)
            val effectiveWallOffset: ZoneOffset = ZoneOffset.ofTotalSeconds(newLso.getTotalSeconds + effectiveSavings)
            val newTransitions = if (lwo != effectiveWallOffset) {
              List(ZoneOffsetTransition.of(lws, lwo, effectiveWallOffset))
            } else {
              Nil
            }
            println("new2 " + (transitionList ::: newTransitions))
            val (newLs, moreTransitions) = tzw match {
              case RulesTimeZoneWindow(_, _, _, _, splitRules) =>
                println("RULEZ " + splitRules.ruleList.size + " " + newTransitions.size)
                var i = 0
                splitRules.ruleList.foldLeft((effectiveSavings, newTransitions)) { case ((savings, transitions), r) =>
                  println("offsets " + i + " " + tzw.windowEnd)
                  i = i + 1
                  //println(newLso)
                  //println(savings)

                  val transition = r.toTransition2(newLso, savings)

                  if ((transition.toEpochSecond >= lws.toEpochSecond(lwo)) && (transition.toEpochSecond < tzw.createDateTimeEpochSecond(savings)) && (transition.offsetBefore != transition.offsetAfter)) {
                    //println("TO OUT")
                    (r.save.seconds, transitions :+ transition.toZoneOffsetTransition)
                  } else {
                    (savings, transitions)
                  }
                }
              case _ =>
                (effectiveSavings, newTransitions)
            }

            println("new3 " + newLs + " ")
            (transitionList ::: moreTransitions).foreach(println)

            println("NEW 4 ")
            val (finalLs, finalRules) = tzw match {
              case RulesTimeZoneWindow(_, _, _, _, splitRules) =>
                println("NEW 4 " + splitRules.lastRuleList.size)
                splitRules.lastRuleList.foldLeft((newLs, List.empty[ZoneOffsetTransitionRule])) { case ((savings, tr), r) =>
                  println(savings)
                  val transitionRule = r.toTransitionRule(lso, savings)

                  (r.save.seconds, tr :+ transitionRule._1)
                }
              case _ =>
                (newLs, Nil)
            }

            val newLoopWindowOffset = tzw.createWallOffset(finalLs)
            val newLoopWindowStart = LocalDateTime.ofEpochSecond(tzw.createDateTimeEpochSecond(finalLs), 0, newLoopWindowOffset)
            println("New WS " + tzw.createDateTimeEpochSecond(finalLs))
            println("New WS FS " + finalLs)
            println("New WS " + newLoopWindowOffset)
            println("New WS " + newLoopWindowStart)
            (newLoopWindowStart, newLoopWindowOffset, newLso, finalLs, standardTransitions ::: newStdTransitions, transitionList ::: moreTransitions, transitionRules ::: finalRules)
          }
          import scala.collection.JavaConverters._
          ZoneRules.of(firstWindow.standardOffset.toZoneOffset, firstWallOffset, q._5.asJava, q._6.asJava, q._7.asJava)
          //println(q)
        }
        zone -> zoneRules
      } collect {
        case (z, Some(r)) => z -> r
      }
    }
  }

  object toWindows extends Poly1 {
    type U = WindowsCollector => WindowsCollector

    implicit val caseItem1: Case.Aux[Comment, U] = at[Comment](_ => identity)
    implicit val caseItem2: Case.Aux[BlankLine, U] = at[BlankLine](_ => identity)
    implicit val caseItem3: Case.Aux[Link, U] = at[Link](_ => identity)
    implicit val caseItem4: Case.Aux[Rule, U] = at[Rule](_ => identity)
    implicit val caseItem5: Case.Aux[Zone, U] = at[Zone]{zone => (c: WindowsCollector) =>
      val newWindows = zone.transitions.foldLeft(List.empty[TimeZoneWindow]) { (windows, transition) =>
        println("######################")
        println(transition)

        def windowForever(offset: GmtOffset): TimeZoneWindow = FixedTimeZoneWindow(offset, LocalDateTime.MAX, TimeDefinition.WALL, transition.ruleId.fixedOffset)

        def windowWithFixedOffset(offset: GmtOffset)(until: Until): TimeZoneWindow = {
          println("FIXED UNTIL " + until.toDateTime)
          FixedTimeZoneWindow(offset, until.toDateTime, until.at.map(At.toTimeDefinition).getOrElse(TimeDefinition.WALL), transition.ruleId.fixedOffset)
        }

        val w: TimeZoneWindow = transition.ruleId match {
          // Fixed offset at 0
          case r @ NullRule            =>
            transition.until.fold(windowForever(GmtOffset.zero))(windowWithFixedOffset(transition.offset))
          // Fixed offset
          case FixedOffset(offset) =>
            transition.until.fold(windowForever(GmtOffset.zero))(windowWithFixedOffset(offset))
          // Offset determined by rules
          case RuleId(ruleId)      =>
            // TODO should this halt if a rule is not found
            val rules: List[Rule] = c.rules.getOrElse(ruleId, Nil)
            val adjustedRules = rules.map(_.adjustForwards)
            transition.until.fold(RulesTimeZoneWindow(transition.offset, LocalDateTime.MAX, TimeDefinition.WALL, transition.ruleId.fixedOffset, adjustedRules)) { until =>
              println("CREATED " + until)
              println("CREATED " + until.toDateTime)
              RulesTimeZoneWindow(transition.offset, until.toDateTime, until.at.map(At.toTimeDefinition).getOrElse(TimeDefinition.WALL), transition.ruleId.fixedOffset, adjustedRules)
            }
        }

        windows :+ w
      }
      c.copy(zoneWindows = c.zoneWindows + (zone -> newWindows))
    }
  }

  object collectRules extends Poly1 {
    type U = List[Rule] => List[Rule]

    implicit val caseComment: Case.Aux[Comment, U] = at[Comment](_ => identity)
    implicit val caseBlank: Case.Aux[BlankLine, U] = at[BlankLine](_ => identity)
    implicit val caseLink: Case.Aux[Link, U] = at[Link](_ => identity)
    implicit val caseRule: Case.Aux[Rule, U] = at[Rule](i => r => i :: r)
    implicit val caseZone: Case.Aux[Zone, U] = at[Zone](_ => identity)

  }

  def calculateTransitions(rows: List[Row]): Map[Zone, ZoneRules] = {
    val rulesByName: RulesById = rows.flatMap(_.fold(collectRules).apply(Nil)).groupBy(_.name)
    val k: List[WindowsCollector] = rows.map(_.fold(toWindows).apply(WindowsCollector(rulesByName, Map.empty)))
    k.filter(_.zoneWindows.nonEmpty).flatMap(_.toRules).toMap
  }
}
