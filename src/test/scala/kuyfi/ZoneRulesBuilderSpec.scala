package kuyfi

import org.scalatest.{FlatSpec, Matchers}
import atto.ParseResult.{Done, Fail}
import atto.ParseResult
import java.time.zone.{ZoneRules, ZoneRulesProvider}

import kuyfi.TZDB.Zone

class ZoneRulesBuilderSpec extends FlatSpec with Matchers {
  import better.files._

  val r = file"src/test/resources/"
  val rows = TZDBParser.parseAll(r).map(ZoneRulesBuilder.calculateTransitions).unsafePerformIO()

  def compareZoneRules(calculated: Option[ZoneRules], target: String) = {
    val platformRules = ZoneRulesProvider.getRules(target, false)

    calculated.map(_.getTransitionRules.size) shouldBe Some(platformRules.getTransitionRules.size)
    calculated.map(_.getTransitionRules) shouldBe Some(platformRules.getTransitionRules)
    calculated.map(_.getTransitions.size) shouldBe Some(platformRules.getTransitions.size)
    calculated.map(_.getTransitions) shouldBe Some(platformRules.getTransitions)
    calculated.map(_.isFixedOffset) shouldBe Some(platformRules.isFixedOffset)
    calculated.map(_.toString) shouldBe Some(platformRules.toString)
  }

  "ZoneRulesBuilder" should
    "do a full calculation for all tzdb" in {
      rows.foreach(println)
      rows.size shouldBe 382
    }
    it should "calculate the transitions for europe/london" in {
      val calculatedRules = rows.find(_._1.name == "Europe/London").map(_._2)
      compareZoneRules(calculatedRules, "Europe/London")
    }
    it should "calculate the transitions for America/New York" in {
      val calculatedRules = rows.find(_._1.name == "America/New_York").map(_._2)
      compareZoneRules(calculatedRules, "America/New_York")
    }
    it should "construct the transition zones for London" in {
      val londonRules = ZoneRulesProvider.getRules("Europe/London", false)

      val text = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/europe_london"), "UTF-8").mkString

      val parsedZoneRules: Option[Map[Zone, ZoneRules]] = TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).option
      parsedZoneRules.map(_.size) shouldBe Some(1)
      val calculatedLondonRules = parsedZoneRules.flatMap(_.find(_._1.name == "Europe/London")).map(_._2)
      calculatedLondonRules.map(_.getTransitionRules.size) shouldBe Some(londonRules.getTransitionRules.size)
      calculatedLondonRules.map(_.getTransitionRules) shouldBe Some(londonRules.getTransitionRules)
      calculatedLondonRules.map(_.getTransitions.size) shouldBe Some(londonRules.getTransitions.size)
      calculatedLondonRules.map(_.getTransitions) shouldBe Some(londonRules.getTransitions)
      calculatedLondonRules.map(_.isFixedOffset) shouldBe Some(londonRules.isFixedOffset)
      calculatedLondonRules.map(_.toString) shouldBe Some(londonRules.toString)
    }

}
