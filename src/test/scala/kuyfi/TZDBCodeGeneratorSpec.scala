package kuyfi

import java.time.{LocalTime, Month}

import kuyfi.TZDB.{AtStandardTime, AtUniversalTime, AtWallTime, DayOfTheMonth, GmtOffset, Link, Row, Until, Zone, ZoneTransition}
import org.scalatest.{FlatSpec, Matchers}

class TZDBCodeGeneratorSpec extends FlatSpec with Matchers {

  val zone1 = Zone("Europe/Belfast", List(
                ZoneTransition(GmtOffset(0, -23, 40), "-",       "LMT",     Some(Until(1880, Some(Month.AUGUST),  Some(DayOfTheMonth(2)),  None))),
                ZoneTransition(GmtOffset(0, -25, 21), "-",       "DMT",     Some(Until(1916, Some(Month.MAY),     Some(DayOfTheMonth(21)), Some(AtWallTime(LocalTime.of(2, 0)))))),
                ZoneTransition(GmtOffset(0, -25, 21), "1:00",    "IST",     Some(Until(1916, Some(Month.OCTOBER), Some(DayOfTheMonth(1)),  Some(AtStandardTime(LocalTime.of(2, 0)))))),
                ZoneTransition(GmtOffset(0,   0,  0), "GB-Eire", "%s",      Some(Until(1968, Some(Month.OCTOBER), Some(DayOfTheMonth(27)), None))),
                ZoneTransition(GmtOffset(1,   0,  0), "-",       "BST",     Some(Until(1971, Some(Month.OCTOBER), Some(DayOfTheMonth(31)), Some(AtUniversalTime(LocalTime.of(2, 0)))))),
                ZoneTransition(GmtOffset(0,   0,  0), "GB-Eire", "%s",      Some(Until(1996, None,                None,                    None))),
                ZoneTransition(GmtOffset(0,   0,  0), "EU",      "GMT/BST", None)
            ))

  val zone2 = Zone("Africa/Tripoli", List(
      ZoneTransition(GmtOffset( 0, 52, 44), "-",     "LMT",   Some(Until(1920, None,                  None,                    None))),
      ZoneTransition(GmtOffset( 1,  0,  0), "Libya", "CE%sT", Some(Until(1959, None,                  None,                    None))),
      ZoneTransition(GmtOffset( 2,  0,  0), "-",     "EET",   Some(Until(1982, None,                  None,                    None))),
      ZoneTransition(GmtOffset( 1,  0,  0), "Libya", "CE%sT", Some(Until(1990, Some(Month.MAY),       Some(DayOfTheMonth(4)),  None))),
      ZoneTransition(GmtOffset( 2,  0,  0), "-",     "EET",   Some(Until(1996, Some(Month.SEPTEMBER), Some(DayOfTheMonth(30)), None))),
      ZoneTransition(GmtOffset( 1,  0,  0), "Libya", "CE%sT", Some(Until(1997, Some(Month.OCTOBER),   Some(DayOfTheMonth(4)),  None))),
      ZoneTransition(GmtOffset( 2,  0,  0), "-",     "EET",   Some(Until(2012, Some(Month.NOVEMBER),  Some(DayOfTheMonth(10)), Some(AtWallTime(LocalTime.of(2, 0)))))),
      ZoneTransition(GmtOffset( 1,  0,  0), "Libya", "CE%sT", Some(Until(2013, Some(Month.OCTOBER),   Some(DayOfTheMonth(25)), Some(AtWallTime(LocalTime.of(2, 0)))))),
      ZoneTransition(GmtOffset( 2,  0,  0), "-",     "EET",   None)
  ))

  val link1 = Link("Europe/Belfast", "Europe/Ireland")
  val link2 = Link("America/Curacao", "America/Aruba")

  import TZDBCodeGenerator._
  import treehugger.forest._

  "TZDB Code generator" should
    "generate a name from a Zone" in {
      treeToString(TreeGenerator[Zone].generateTree(zone1)) shouldBe "(\"Europe/Belfast\", ZoneRules.of(ZoneOffset.ofHoursMinutesSeconds(0, -23, -40)))"
    }
    it should "generate a name from a Fixed offset Zone" in {
      treeToString(TreeGenerator[Zone].generateTree(zoneFixed)) shouldBe "(\"Etc/GMT+1\", ZoneRules.of(ZoneOffset.ofHoursMinutesSeconds(-1, 0, 0)))"
    }
    it should "generate a tuple from a Link" in {
      treeToString(TreeGenerator[Link].generateTree(link2)) shouldBe "(\"America/Aruba\", \"America/Curacao\")"
    }
    it should "clean dangling links" in {
      val rows1 = link1.liftC[Row] :: link2.liftC[Row] :: Nil
      cleanLinks(rows1) shouldBe empty

      val rows2 = link1.liftC[Row] :: link2.liftC[Row] :: zone1.liftC[Row] :: Nil

      cleanLinks(rows2) should have size 2
    }
    it should "generate an object from a List of Zones" in {
      treeToString(TreeGenerator[List[Zone]].generateTree(List(zone1, zone2))) shouldBe "lazy val allZones: Map[String, ZoneRules] = Map((\"Europe/Belfast\", ZoneRules.of(ZoneOffset.ofHoursMinutesSeconds(0, -23, -40))), (\"Africa/Tripoli\", ZoneRules.of(ZoneOffset.ofHoursMinutesSeconds(0, 52, 44))))"
    }
    it should "import a top level package" in {
      treeToString(exportAll("org.threeten.bp", link1.liftC[Row] :: link2.liftC[Row] :: zone1.liftC[Row] :: Nil)) should include ("import org.threeten.bp._")
    }
}
