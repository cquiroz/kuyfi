package kuyfi

import java.time.zone.ZoneOffsetTransitionRule
import java.time.zone.ZoneOffsetTransitionRule.TimeDefinition
import java.time.{ DayOfWeek, LocalDateTime, LocalTime, Month, ZoneOffset }
import kuyfi.TZDB._

class TZDBCodeGeneratorSpec extends munit.FunSuite {
  import TZDBCodeGenerator.PureTreeGenerator._

  val zone1 = Zone(
    "Europe/Belfast",
    List(
      ZoneTransition(GmtOffset(0, -23, -40),
                     NullRule,
                     "LMT",
                     Some(Until(1880, Some(Month.AUGUST), Some(DayOfTheMonth(2)), None))
      ),
      ZoneTransition(GmtOffset(0, -25, -21),
                     NullRule,
                     "DMT",
                     Some(
                       Until(1916,
                             Some(Month.MAY),
                             Some(DayOfTheMonth(21)),
                             Some(AtWallTime(LocalTime.of(2, 0)))
                       )
                     )
      ),
      ZoneTransition(
        GmtOffset(0, -25, -21),
        FixedOffset(GmtOffset(1, 0, 0)),
        "IST",
        Some(
          Until(1916,
                Some(Month.OCTOBER),
                Some(DayOfTheMonth(1)),
                Some(AtStandardTime(LocalTime.of(2, 0)))
          )
        )
      ),
      ZoneTransition(GmtOffset(0, 0, 0),
                     RuleId("GB-Eire"),
                     "%s",
                     Some(Until(1968, Some(Month.OCTOBER), Some(DayOfTheMonth(27)), None))
      ),
      ZoneTransition(GmtOffset(1, 0, 0),
                     NullRule,
                     "BST",
                     Some(
                       Until(1971,
                             Some(Month.OCTOBER),
                             Some(DayOfTheMonth(31)),
                             Some(AtUniversalTime(LocalTime.of(2, 0)))
                       )
                     )
      ),
      ZoneTransition(GmtOffset(0, 0, 0),
                     RuleId("GB-Eire"),
                     "%s",
                     Some(Until(1996, None, None, None))
      ),
      ZoneTransition(GmtOffset(0, 0, 0), RuleId("EU"), "GMT/BST", None)
    )
  )

  val zoneFixed =
    Zone("Etc/GMT+1", List(ZoneTransition(GmtOffset(-1, 0, 0), NullRule, "-01", None)))

  val zone2 = Zone(
    "Africa/Tripoli",
    List(
      ZoneTransition(GmtOffset(0, 52, 44), NullRule, "LMT", Some(Until(1920, None, None, None))),
      ZoneTransition(GmtOffset(1, 0, 0),
                     RuleId("Libya"),
                     "CE%sT",
                     Some(Until(1959, None, None, None))
      ),
      ZoneTransition(GmtOffset(2, 0, 0), NullRule, "EET", Some(Until(1982, None, None, None))),
      ZoneTransition(GmtOffset(1, 0, 0),
                     RuleId("Libya"),
                     "CE%sT",
                     Some(Until(1990, Some(Month.MAY), Some(DayOfTheMonth(4)), None))
      ),
      ZoneTransition(GmtOffset(2, 0, 0),
                     NullRule,
                     "EET",
                     Some(Until(1996, Some(Month.SEPTEMBER), Some(DayOfTheMonth(30)), None))
      ),
      ZoneTransition(GmtOffset(1, 0, 0),
                     RuleId("Libya"),
                     "CE%sT",
                     Some(Until(1997, Some(Month.OCTOBER), Some(DayOfTheMonth(4)), None))
      ),
      ZoneTransition(GmtOffset(2, 0, 0),
                     NullRule,
                     "EET",
                     Some(
                       Until(2012,
                             Some(Month.NOVEMBER),
                             Some(DayOfTheMonth(10)),
                             Some(AtWallTime(LocalTime.of(2, 0)))
                       )
                     )
      ),
      ZoneTransition(GmtOffset(1, 0, 0),
                     RuleId("Libya"),
                     "CE%sT",
                     Some(
                       Until(2013,
                             Some(Month.OCTOBER),
                             Some(DayOfTheMonth(25)),
                             Some(AtWallTime(LocalTime.of(2, 0)))
                       )
                     )
      ),
      ZoneTransition(GmtOffset(2, 0, 0), NullRule, "EET", None)
    )
  )

  val link1 = Link("Europe/Belfast", "Europe/Ireland")
  val link2 = Link("America/Curacao", "America/Aruba")

  import TZDBCodeGenerator._
  import treehugger.forest._

  test("generate a name from a Zone") {
    assertEquals(treeToString(
                   TreeGenerator[Zone].generateTree(zone1)
                 ),
                 "(\"Europe/Belfast\", Europe_Belfast)"
    )
  }
  test("generate a name from a Fixed offset Zone") {
    assertEquals(treeToString(
                   TreeGenerator[Zone].generateTree(zoneFixed)
                 ),
                 "(\"Etc/GMT+1\", Etc_GMT_plus_1)"
    )
  }
  test("generate a tuple from a Link") {
    assertEquals(treeToString(
                   TreeGenerator[Link].generateTree(link2)
                 ),
                 "(\"America/Aruba\", \"America/Curacao\")"
    )
  }
  test("clean dangling links") {
    val rows1 = link1.liftC[Row] :: link2.liftC[Row] :: Nil
    assert(cleanLinks(rows1).isEmpty)

    val rows2 = link1.liftC[Row] :: link2.liftC[Row] :: zone1.liftC[Row] :: Nil

    assertEquals(cleanLinks(rows2).length, 2)
  }
  test("generate an object from a List of Zones") {
    assertEquals(
      treeToString(
        TreeGenerator[List[Zone]].generateTree(List(zone1, zone2))
      ),
      "lazy val allZones: Map[String, ZoneRules] = Map((\"Europe/Belfast\", Europe_Belfast), (\"Africa/Tripoli\", Africa_Tripoli))"
    )
  }
  test("generate from zone offset transition rule") {
    val rule = ZoneOffsetTransitionRule.of(Month.JANUARY,
                                           3,
                                           DayOfWeek.MONDAY,
                                           LocalTime.of(12, 0),
                                           false,
                                           TimeDefinition.UTC,
                                           ZoneOffset.ofHours(0),
                                           ZoneOffset.ofHours(1),
                                           ZoneOffset.ofHours(2)
    )
    assertEquals(
      treeToString(
        TreeGenerator[ZoneOffsetTransitionRule].generateTree(rule)
      ),
      s"ZoneOffsetTransitionRule.of(Month.JANUARY, 3, DayOfWeek.MONDAY, LocalTime.of(12, 0, 0, 0), false, ZoneOffsetTransitionRule.TimeDefinition.UTC, ZoneOffset.ofTotalSeconds(0), ZoneOffset.ofTotalSeconds(3600), ZoneOffset.ofTotalSeconds(7200))"
    )
  }
  test("generate from zone offset transition") {
    assertEquals(
      treeToString(
        TreeGenerator[ZoneOffsetTransitionParams].generateTree(
          ZoneOffsetTransitionParams(LocalDateTime.of(2017, Month.FEBRUARY, 1, 10, 15),
                                     ZoneOffset.ofHours(1),
                                     ZoneOffset.ofHours(2)
          )
        )
      ),
      s"ZoneOffsetTransition.of(LocalDateTime.of(2017, 2, 1, 10, 15, 0, 0), ZoneOffset.ofTotalSeconds(3600), ZoneOffset.ofTotalSeconds(7200))"
    )
  }
  test("generate from Month") {
    assertEquals(treeToString(TreeGenerator[Month].generateTree(Month.JANUARY)), s"Month.JANUARY")
    assertEquals(treeToString(TreeGenerator[Month].generateTree(Month.DECEMBER)), s"Month.DECEMBER")
  }
  test("generate from LocalDateTime") {
    assertEquals(
      treeToString(
        TreeGenerator[LocalDateTime]
          .generateTree(LocalDateTime.of(2017, Month.FEBRUARY, 1, 10, 15, 25))
      ),
      s"LocalDateTime.of(2017, 2, 1, 10, 15, 25, 0)"
    )
  }
  test("generate from offset") {
    assertEquals(treeToString(
                   TreeGenerator[ZoneOffset].generateTree(ZoneOffset.ofHoursMinutesSeconds(1, 2, 3))
                 ),
                 s"ZoneOffset.ofTotalSeconds(${1 * 3600 + 2 * 60 + 3})"
    )
  }
  test("import a top level package") {
    assert(
      treeToString(
        exportTzdb(TzdbVersion("2018e"),
                   "org.threeten.bp",
                   "org.threeten.bp",
                   link1.liftC[Row] :: link2.liftC[Row] :: zone1.liftC[Row] :: Nil,
                   _ => true
        )
      ).contains("import org.threeten.bp.zone._")
    )
  }
  test("generate from zone rules param") {
    val standardTransitions = List(
      ZoneOffsetTransitionParams(LocalDateTime.of(2017, Month.FEBRUARY, 1, 10, 15),
                                 ZoneOffset.ofHours(1),
                                 ZoneOffset.ofHours(2)
      )
    )
    val transitions         = List(
      ZoneOffsetTransitionParams(LocalDateTime.of(2005, Month.NOVEMBER, 3, 0, 0),
                                 ZoneOffset.ofHours(0),
                                 ZoneOffset.ofHours(2)
      )
    )
    val rule                = List(
      ZoneOffsetTransitionRule.of(Month.JANUARY,
                                  3,
                                  DayOfWeek.MONDAY,
                                  LocalTime.of(12, 0),
                                  false,
                                  TimeDefinition.UTC,
                                  ZoneOffset.ofHours(0),
                                  ZoneOffset.ofHours(1),
                                  ZoneOffset.ofHours(2)
      )
    )
    val params              = StandardRulesParams(ZoneOffset.ofHours(1),
                                     ZoneOffset.ofHours(0),
                                     standardTransitions,
                                     transitions,
                                     rule
    )
    assertEquals(
      treeToString(TreeGenerator[ZoneRulesParams].generateTree(params)).trim,
      s"""{
      |  val bso: ZoneOffset = ZoneOffset.ofTotalSeconds(3600)
      |  val bwo: ZoneOffset = ZoneOffset.ofTotalSeconds(0)
      |  val standardTransitions: List[ZoneOffsetTransition] = List(ZoneOffsetTransition.of(LocalDateTime.of(2017, 2, 1, 10, 15, 0, 0), ZoneOffset.ofTotalSeconds(3600), ZoneOffset.ofTotalSeconds(7200)))
      |  val transitionList: List[ZoneOffsetTransition] = List(ZoneOffsetTransition.of(LocalDateTime.of(2005, 11, 3, 0, 0, 0, 0), ZoneOffset.ofTotalSeconds(0), ZoneOffset.ofTotalSeconds(7200)))
      |  val lastRules: List[ZoneOffsetTransitionRule] = List(ZoneOffsetTransitionRule.of(Month.JANUARY, 3, DayOfWeek.MONDAY, LocalTime.of(12, 0, 0, 0), false, ZoneOffsetTransitionRule.TimeDefinition.UTC, ZoneOffset.ofTotalSeconds(0), ZoneOffset.ofTotalSeconds(3600), ZoneOffset.ofTotalSeconds(7200)))
      |  ZoneRules.of(bso, bwo, standardTransitions asJava, transitionList asJava, lastRules asJava)
      |}""".stripMargin
    )

    val r = treeToString(
      exportTzdb(TzdbVersion("2018e"),
                 "java.time",
                 "java.time",
                 link1.liftC[Row] :: link2.liftC[Row] :: zone1.liftC[Row] :: Nil,
                 _ => true
      )
    )
    assert(r.contains("import java.time.zone._"))
  }
}
