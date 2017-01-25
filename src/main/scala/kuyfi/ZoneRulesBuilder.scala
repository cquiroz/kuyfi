package kuyfi

import java.time._
import java.time.zone.{ZoneOffsetTransition, ZoneOffsetTransitionRule, ZoneRules}
import java.time.zone.ZoneOffsetTransitionRule.TimeDefinition

import kuyfi.TZDB._
import kuyfi.TimeZoneWindow._
import shapeless.Poly1

import scalaz._
import scala.annotation.tailrec

object ZoneRulesBuilder {

  type RulesById = Map[String, List[Rule]]

  case class WindowsCollector(rules: RulesById, zoneWindows: Map[Zone, List[TimeZoneWindow]]) {
    case class TransitionsAccumulator(lws: LocalDateTime,
                                      lwo: ZoneOffset,
                                      lso: ZoneOffset,
                                      ls: Int,
                                      standardTransitions: List[ZoneOffsetTransition],
                                      transitionsList: List[ZoneOffsetTransition],
                                      transitionRules: List[ZoneOffsetTransitionRule])

    def toRules: Map[Zone, ZoneRules] = {
      zoneWindows.map { case (zone, windows) =>
        val zoneRules: Option[ZoneRules] = windows.headOption.map { firstWindow =>
          val loopSavings = firstWindow.fixedSavingAmountSeconds.getOrElse(0)
          val loopStandardOffset = firstWindow.standardOffset.toZoneOffset
          val firstWallOffset = ZoneOffset.ofTotalSeconds(loopStandardOffset.getTotalSeconds + loopSavings)
          val loopWindowStart = LocalDateTime.of(Year.MIN_VALUE, 1, 1, 0, 0)
          val loopWindowOffset = firstWallOffset

          val start = TransitionsAccumulator(loopWindowStart, loopWindowOffset, loopStandardOffset, loopSavings, List.empty[ZoneOffsetTransition], List.empty[ZoneOffsetTransition], List.empty[ZoneOffsetTransitionRule])
          val accumulator = windows.foldLeft(start) { case (TransitionsAccumulator(lws, lwo, lso, ls, standardTransitions, transitionList, transitionRules), timeZoneWindow) =>
            val tzw = timeZoneWindow.tidy(lws.getYear)

            val effectiveSavings = tzw.fixedSavingAmountSeconds.getOrElse {
              tzw match {
                case RulesTimeZoneWindow(_, _, _, _, windowRules) =>
                  @tailrec
                  def go(savings: Int, rule: List[Rule]): Int = rule match {
                    case h :: rest =>
                      val trans = h.toTransitionParams(lso, ls)
                      if (trans.toEpochSecond > lws.toEpochSecond(lwo)) {
                        savings
                      } else {
                        go(h.save.seconds, rest)
                      }
                    case Nil => 0
                  }

                  go(0, windowRules.ruleList)
                case _ => 0
              }
            }
            val (newStdTransitions, newLso) =
              if (lso != tzw.standardOffset.toZoneOffset) {
                (List(ZoneOffsetTransition.of(LocalDateTime.ofEpochSecond(lws.toEpochSecond(lwo), 0, lso), lso, tzw.standardOffset.toZoneOffset)), tzw.standardOffset.toZoneOffset)
              } else {
                (Nil, lso)
              }
            val effectiveWallOffset: ZoneOffset = ZoneOffset.ofTotalSeconds(newLso.getTotalSeconds + effectiveSavings)
            val newTransitions = if (lwo != effectiveWallOffset) {
              List(ZoneOffsetTransition.of(lws, lwo, effectiveWallOffset))
            } else {
              Nil
            }
            val (newLs, moreTransitions) = tzw match {
              case RulesTimeZoneWindow(_, _, _, _, windowRules) =>
                windowRules.ruleList.foldLeft((effectiveSavings, newTransitions)) { case ((savings, transitions), r) =>
                  val transition = r.toTransitionParams(newLso, savings)

                  if ((transition.toEpochSecond >= lws.toEpochSecond(lwo)) && (transition.toEpochSecond < tzw.createDateTimeEpochSecond(savings)) && (transition.offsetBefore != transition.offsetAfter)) {
                    (r.save.seconds, transitions :+ transition.toZoneOffsetTransition)
                  } else {
                    (savings, transitions)
                  }
                }
              case _ =>
                (effectiveSavings, newTransitions)
            }

            val (finalLs, finalRules) = tzw match {
              case RulesTimeZoneWindow(_, _, _, _, splitRules) =>
                splitRules.lastRuleList.foldLeft((newLs, List.empty[ZoneOffsetTransitionRule])) { case ((savings, tr), r) =>
                  val transitionRule = r.toTransitionRule(newLso, savings)
                  (r.save.seconds, tr :+ transitionRule._1)
                }
              case _ =>
                (newLs, Nil)
            }

            val newLoopWindowOffset = tzw.createWallOffset(finalLs)
            val newLoopWindowStart = LocalDateTime.ofEpochSecond(tzw.createDateTimeEpochSecond(finalLs), 0, newLoopWindowOffset)
            TransitionsAccumulator(newLoopWindowStart, newLoopWindowOffset, newLso, finalLs, standardTransitions ::: newStdTransitions, transitionList ::: moreTransitions, transitionRules ::: finalRules)
          }
          import scala.collection.JavaConverters._
          ZoneRules.of(firstWindow.standardOffset.toZoneOffset, firstWallOffset, accumulator.standardTransitions.asJava, accumulator.transitionsList.asJava, accumulator.transitionRules.asJava)
        }
        zone -> zoneRules
      } collect {
        case (z, Some(r)) => z -> r
      }
    }
  }

  /**
    * Creat timezone windows from the zone transitions
    */
  object toWindows extends Poly1 {
    type U = WindowsCollector => WindowsCollector

    implicit val caseItem1: Case.Aux[Comment, U] = at[Comment](_ => identity)
    implicit val caseItem2: Case.Aux[BlankLine, U] = at[BlankLine](_ => identity)
    implicit val caseItem3: Case.Aux[Link, U] = at[Link](_ => identity)
    implicit val caseItem4: Case.Aux[Rule, U] = at[Rule](_ => identity)
    implicit val caseItem5: Case.Aux[Zone, U] = at[Zone]{zone => (c: WindowsCollector) =>
      val newWindows = zone.transitions.foldLeft(List.empty[TimeZoneWindow]) { (windows, transition) =>

        def windowForever(offset: GmtOffset): TimeZoneWindow = FixedTimeZoneWindow(offset, LocalDateTime.MAX, TimeDefinition.WALL, transition.ruleId.fixedOffset)

        def windowWithFixedOffset(offset: GmtOffset)(until: Until): TimeZoneWindow =
          FixedTimeZoneWindow(offset, until.toDateTime, until.at.map(_.timeDefinition).getOrElse(TimeDefinition.WALL), transition.ruleId.fixedOffset)

        val w: TimeZoneWindow = transition.ruleId match {
          case NullRule            =>
            transition.until.fold(windowForever(transition.offset))(windowWithFixedOffset(transition.offset))
          case FixedOffset(offset) =>
            transition.until.fold(windowForever(offset))(windowWithFixedOffset(transition.offset))
          case RuleId(ruleId)      =>
            val rules: List[Rule] = c.rules.getOrElse(ruleId, Nil)
            val adjustedRules = rules.map(_.adjustForwards)
            transition.until.fold(RulesTimeZoneWindow(transition.offset, LocalDateTime.MAX, TimeDefinition.WALL, transition.ruleId.fixedOffset, adjustedRules)) { until =>
              RulesTimeZoneWindow(transition.offset, until.toDateTime, until.at.map(_.timeDefinition).getOrElse(TimeDefinition.WALL), transition.ruleId.fixedOffset, adjustedRules)
            }
        }

        windows :+ w
      }
      c.copy(zoneWindows = c.zoneWindows + (zone -> newWindows))
    }
  }

  /**
    * Collects all the rules
    */
  object collectRules extends Poly1 {
    type U = List[Rule] => List[Rule]

    implicit val caseComment: Case.Aux[Comment, U] = at[Comment](_ => identity)
    implicit val caseBlank: Case.Aux[BlankLine, U] = at[BlankLine](_ => identity)
    implicit val caseLink: Case.Aux[Link, U]       = at[Link](_ => identity)
    implicit val caseRule: Case.Aux[Rule, U]       = at[Rule](i => r => i :: r)
    implicit val caseZone: Case.Aux[Zone, U]       = at[Zone](_ => identity)
  }

  /**
    * Calculates all the zone rules for the rows
    */
  def calculateTransitions(rows: List[Row]): Map[Zone, ZoneRules] = {
    val rulesByName: RulesById = rows.flatMap(_.fold(collectRules).apply(Nil)).groupBy(_.name)
    val collectedRules: List[WindowsCollector] = rows.map(_.fold(toWindows).apply(WindowsCollector(rulesByName, Map.empty)))
    collectedRules.filter(_.zoneWindows.nonEmpty).flatMap(_.toRules).toMap
  }

  /**
    * Calculates all the zone rules for the rows
    * and adds copies for the linked rules
    */
  def calculateTransitionsWithLinks(rows: List[Row]): Map[String, ZoneRules] = {
    val rules = calculateTransitions(rows).map(x => (x._1.name, x._2))
    val links = rows.flatMap(_.select[Link])

    def go(toResolve: List[Link]): Map[String, ZoneRules] = toResolve match {
      case Nil => Map.empty
      case r   =>
        val (in, out) = r.partition(l => rules.exists(x => x._1 == l.from))
        val found = in.flatMap { link =>
          rules.get(link.from).map(u => link.to -> u)
        }

        val rel = out.flatMap { link =>
          val recursive = links.find(x => x.to == link.from)
          recursive.map(l => link.copy(from = l.from))
        }
        found.toMap ++ go(rel)
    }
    rules ++ go(links)
  }
}
