package kuyfi

import org.scalatest.{FlatSpec, Matchers}
import atto.ParseResult.{Done, Fail}
import atto.ParseResult
import java.time.zone.{ZoneRules, ZoneRulesProvider}

import kuyfi.TZDB.Zone

class ZoneRulesBuilderSpec extends FlatSpec with Matchers {
  import better.files._

  // NOTE These tests are fragile as they depend on the timezone db of the JVM
  // These tests are for 2014j matching the JVM on travis

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
      rows.size shouldBe 385
    }
    it should "calculate the transitions for europe/london" in {
      val calculatedRules = rows.find(_._1.name == "Europe/London").map(_._2)
      compareZoneRules(calculatedRules, "Europe/London")
    }
    it should "calculate the transitions for America/New York" in {
      val calculatedRules = rows.find(_._1.name == "America/New_York").map(_._2)
      compareZoneRules(calculatedRules, "America/New_York")
    }
    it should "calculate the transitions for Asia/Kathmandu" in {
      val calculatedRules = rows.find(_._1.name == "Asia/Kathmandu").map(_._2)
      compareZoneRules(calculatedRules, "Asia/Kathmandu")
    }
    it should "calculate the transitions for Europe/Paris" in {
      val calculatedRules = rows.find(_._1.name == "Europe/Paris").map(_._2)
      compareZoneRules(calculatedRules, "Europe/Paris")
    }
    it should "calculate the transitions for Australia/Adelaide" in {
      val calculatedRules = rows.find(_._1.name == "Australia/Adelaide").map(_._2)
      compareZoneRules(calculatedRules, "Australia/Adelaide")
    }
    it should "calculate the transitions for Africa/Casablanca" in {
      val calculatedRules = rows.find(_._1.name == "Africa/Casablanca").map(_._2)
      compareZoneRules(calculatedRules, "Africa/Casablanca")
    }
    it should "calculate the transitions for Africa/Tunis" in {
      val calculatedRules = rows.find(_._1.name == "Africa/Tunis").map(_._2)
      compareZoneRules(calculatedRules, "Africa/Tunis")
    }
    it should "calculate the transitions for Africa/Nairobi" in {
      val calculatedRules = rows.find(_._1.name == "Africa/Nairobi").map(_._2)
      compareZoneRules(calculatedRules, "Africa/Nairobi")
    }
    it should "calculate the transitions for Africa/Windhoek" in {
      val calculatedRules = rows.find(_._1.name == "Africa/Windhoek").map(_._2)
      compareZoneRules(calculatedRules, "Africa/Windhoek")
    }
    it should "calculate the transitions for Africa/Cairo" in {
      val calculatedRules = rows.find(_._1.name == "Africa/Cairo").map(_._2)
      compareZoneRules(calculatedRules, "Africa/Cairo")
    }
    it should "construct the transition zones for London" in {
      val text = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/europe_london"), "UTF-8").mkString

      val parsedZoneRules: Option[Map[Zone, ZoneRules]] = TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).option
      parsedZoneRules.map(_.size) shouldBe Some(1)

      val calculatedLondonRules = parsedZoneRules.flatMap(_.find(_._1.name == "Europe/London")).map(_._2)
      compareZoneRules(calculatedLondonRules, "Europe/London")
    }
    it should "construct the transition zones for Kathmandu" in {
      val text = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/asia_kathmandu"), "UTF-8").mkString

      val parsedZoneRules: Option[Map[Zone, ZoneRules]] = TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).option
      parsedZoneRules.map(_.size) shouldBe Some(1)
      val calculatedKathmanduRules = parsedZoneRules.flatMap(_.find(_._1.name == "Asia/Kathmandu")).map(_._2)
      compareZoneRules(calculatedKathmanduRules, "Asia/Kathmandu")
    }
    it should "construct the transition zones for Paris" in {
      val text = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/europe_paris"), "UTF-8").mkString

      val parsedZoneRules: Option[Map[Zone, ZoneRules]] = TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).option
      parsedZoneRules.map(_.size) shouldBe Some(1)
      val calculatedParisRules = parsedZoneRules.flatMap(_.find(_._1.name == "Europe/Paris")).map(_._2)
      compareZoneRules(calculatedParisRules, "Europe/Paris")
    }
    it should "construct the transition zones for Casablanca" in {
      val text = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/africa_casablanca"), "UTF-8").mkString

      val parsedZoneRules: Option[Map[Zone, ZoneRules]] = TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).option
      parsedZoneRules.map(_.size) shouldBe Some(1)

      val calculatedCasablancaRules = parsedZoneRules.flatMap(_.find(_._1.name == "Africa/Casablanca")).map(_._2)
      compareZoneRules(calculatedCasablancaRules, "Africa/Casablanca")
    }
    it should "construct the transition zones for Windhoek" in {
      val text = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/africa_windhoek"), "UTF-8").mkString

      val parsedZoneRules: Option[Map[Zone, ZoneRules]] = TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).option
      parsedZoneRules.map(_.size) shouldBe Some(1)

      val calculatedWindhoekRules = parsedZoneRules.flatMap(_.find(_._1.name == "Africa/Windhoek")).map(_._2)
      compareZoneRules(calculatedWindhoekRules, "Africa/Windhoek")
    }

}
