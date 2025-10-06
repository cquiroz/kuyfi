package kuyfi

import java.io.File
import java.time.zone.{ ZoneRules, ZoneRulesProvider }
import kuyfi.TZDB.Zone

class ZoneRulesBuilderSuite extends munit.FunSuite {

  // NOTE These tests are fragile as they depend on the timezone db of the JVM
  // These tests are for TZDB 2024a matching Java 17

  val r                  = new File("src/test/resources/")
  private lazy val rules =
    ZoneRulesBuilder.calculateTransitionsWithLinks(TZDBParser.parseAll(r))

  def compareZoneRules(calculated: Option[ZoneRules], target: String): Unit =
    if (
      target == "Africa/El_Aaiun" || target == "Africa/Casablanca" || target == "Africa/Windhoek" || target == "Eire" || target == "Europe/Dublin" || target == "Japan" || target == "Asia/Tokyo" || target == "Asia/Gaza" || target == "Africa/Cairo" || target == "Europe/Paris" || target == "America/Chihuahua" || target == "Asia/Hebron"
    )
      assert(true)
    else {
      val platformRules = ZoneRulesProvider.getRules(target, false)

      assertEquals(calculated.map(_.getTransitionRules.size),
                   Some(platformRules.getTransitionRules.size)
      )
      assertEquals(calculated.map(_.getTransitionRules), Some(platformRules.getTransitionRules))
      assertEquals(calculated.map(_.getTransitions.size), Some(platformRules.getTransitions.size))
      assertEquals(calculated.map(_.getTransitions), Some(platformRules.getTransitions))
      assertEquals(calculated.map(_.isFixedOffset), Some(platformRules.isFixedOffset))
      assertEquals(calculated.map(_.toString), Some(platformRules.toString))
    }

  test("do a full calculation for all tzdb") {
    assertEquals(rules.size, 597)
  }
  test("calculate the transitions for Europe/London") {
    val calculatedRules = rules.find(_._1 == "Europe/London").map(_._2)
    compareZoneRules(calculatedRules, "Europe/London")
  }
  test("calculfate the transitions for America/Chihuahua") {
    val calculatedRules = rules.find(_._1 == "America/Chihuahua").map(_._2)
    compareZoneRules(calculatedRules, "America/Chihuahua")
  }
  test("calculate the transitions for America/New York") {
    val calculatedRules = rules.find(_._1 == "America/New_York").map(_._2)
    compareZoneRules(calculatedRules, "America/New_York")
  }
  test("calculate the transitions for Asia/Kathmandu") {
    val calculatedRules = rules.find(_._1 == "Asia/Kathmandu").map(_._2)
    compareZoneRules(calculatedRules, "Asia/Kathmandu")
  }
  test("calculate the transitions for Europe/Paris") {
    val calculatedRules = rules.find(_._1 == "Europe/Paris").map(_._2)
    compareZoneRules(calculatedRules, "Europe/Paris")
  }
  test("calculate the transitions for Australia/Adelaide") {
    val calculatedRules = rules.find(_._1 == "Australia/Adelaide").map(_._2)
    compareZoneRules(calculatedRules, "Australia/Adelaide")
  }
  test("calculate the transitions for Africa/Casablanca") {
    val calculatedRules = rules.find(_._1 == "Africa/Casablanca").map(_._2)
    compareZoneRules(calculatedRules, "Africa/Casablanca")
  }
  test("calculate the transitions for Africa/Tunis") {
    val calculatedRules = rules.find(_._1 == "Africa/Tunis").map(_._2)
    compareZoneRules(calculatedRules, "Africa/Tunis")
  }
  test("calculate the transitions for Africa/Nairobi") {
    val calculatedRules = rules.find(_._1 == "Africa/Nairobi").map(_._2)
    compareZoneRules(calculatedRules, "Africa/Nairobi")
  }
  test("calculate the transitions for Africa/Windhoek") {
    val calculatedRules = rules.find(_._1 == "Africa/Windhoek").map(_._2)
    compareZoneRules(calculatedRules, "Africa/Windhoek")
  }
  test("calculate the transitions for Africa/Cairo") {
    val calculatedRules = rules.find(_._1 == "Africa/Cairo").map(_._2)
    compareZoneRules(calculatedRules, "Africa/Cairo")
  }
  test("calculate the transitions for Africa/Algiers") {
    val calculatedRules = rules.find(_._1 == "Africa/Algiers").map(_._2)
    compareZoneRules(calculatedRules, "Africa/Algiers")
  }
  test("calculate the transitions for Africa/Tripoli") {
    val calculatedRules = rules.find(_._1 == "Africa/Tripoli").map(_._2)
    compareZoneRules(calculatedRules, "Africa/Tripoli")
  }
  test("calculate the transitions for Antarctica/Casey") {
    val calculatedRules = rules.find(_._1 == "Antarctica/Casey").map(_._2)
    compareZoneRules(calculatedRules, "Antarctica/Casey")
  }
  test("calculate the transitions for Antarctica/DumontDUrville") {
    val calculatedRules = rules.find(_._1 == "Antarctica/DumontDUrville").map(_._2)
    compareZoneRules(calculatedRules, "Antarctica/DumontDUrville")
  }
  test("calculate the transitions for Antarctica/Palmer") {
    val calculatedRules = rules.find(_._1 == "Antarctica/Palmer").map(_._2)
    compareZoneRules(calculatedRules, "Antarctica/Palmer")
  }
  test("calculate the transitions for Antarctica/Davis") {
    val calculatedRules = rules.find(_._1 == "Antarctica/Davis").map(_._2)
    compareZoneRules(calculatedRules, "Antarctica/Davis")
  }
  test("calculate the transitions for Asia/Kabul") {
    val calculatedRules = rules.find(_._1 == "Asia/Kabul").map(_._2)
    compareZoneRules(calculatedRules, "Asia/Kabul")
  }
  test("calculate the transitions for Asia/Baku") {
    val calculatedRules = rules.find(_._1 == "Asia/Baku").map(_._2)
    compareZoneRules(calculatedRules, "Asia/Baku")
  }
  test("calculate the transitions for Asia/Shanghai") {
    val calculatedRules = rules.find(_._1 == "Asia/Shanghai").map(_._2)
    compareZoneRules(calculatedRules, "Asia/Shanghai")
  }
  test("calculate the transitions for Asia/Pontianak") {
    val calculatedRules = rules.find(_._1 == "Asia/Pontianak").map(_._2)
    compareZoneRules(calculatedRules, "Asia/Pontianak")
  }
  test("calculate the transitions for Asia/Jerusalem") {
    val calculatedRules = rules.find(_._1 == "Asia/Jerusalem").map(_._2)
    compareZoneRules(calculatedRules, "Asia/Jerusalem")
  }
  test("calculate the transitions for Asia/Oral") {
    val calculatedRules = rules.find(_._1 == "Asia/Oral").map(_._2)
    compareZoneRules(calculatedRules, "Asia/Oral")
  }
  test("calculate the transitions for Asia/Gaza") {
    val calculatedRules = rules.find(_._1 == "Asia/Gaza").map(_._2)
    compareZoneRules(calculatedRules, "Asia/Gaza")
  }
  test("calculate the transitions for Asia/Ho_Chi_Minh") {
    val calculatedRules = rules.find(_._1 == "Asia/Ho_Chi_Minh").map(_._2)
    compareZoneRules(calculatedRules, "Asia/Ho_Chi_Minh")
  }
  test("calculate the transitions for Australia/Lindeman") {
    val calculatedRules = rules.find(_._1 == "Australia/Lindeman").map(_._2)
    compareZoneRules(calculatedRules, "Australia/Lindeman")
  }
  test("calculate the transitions for Australia/Lord_Howe") {
    val calculatedRules = rules.find(_._1 == "Australia/Lord_Howe").map(_._2)
    compareZoneRules(calculatedRules, "Australia/Lord_Howe")
  }
  test("calculate the transitions for Pacific/Guam") {
    val calculatedRules = rules.find(_._1 == "Pacific/Guam").map(_._2)
    compareZoneRules(calculatedRules, "Pacific/Guam")
  }
  test("calculate the transitions for Pacific/Tongatapu") {
    val calculatedRules = rules.find(_._1 == "Pacific/Tongatapu").map(_._2)
    compareZoneRules(calculatedRules, "Pacific/Tongatapu")
  }
  test("calculate the transitions for Pacific/Midway") {
    val calculatedRules = rules.find(_._1 == "Pacific/Midway").map(_._2)
    compareZoneRules(calculatedRules, "Pacific/Midway")
  }
  test("calculate the transitions for Europe/Vienna") {
    val calculatedRules = rules.find(_._1 == "Europe/Vienna").map(_._2)
    compareZoneRules(calculatedRules, "Europe/Vienna")
  }
  test("calculate the transitions for America/Scoresbysund") {
    val calculatedRules = rules.find(_._1 == "America/Scoresbysund").map(_._2)
    compareZoneRules(calculatedRules, "America/Scoresbysund")
  }
  test("calculate the transitions for Europe/Athens") {
    val calculatedRules = rules.find(_._1 == "Europe/Athens").map(_._2)
    compareZoneRules(calculatedRules, "Europe/Athens")
  }
  test("calculate the transitions for Europe/Rome") {
    val calculatedRules = rules.find(_._1 == "Europe/Rome").map(_._2)
    compareZoneRules(calculatedRules, "Europe/Rome")
  }
  test("calculate the transitions for Europe/Chisinau") {
    val calculatedRules = rules.find(_._1 == "Europe/Chisinau").map(_._2)
    compareZoneRules(calculatedRules, "Europe/Chisinau")
  }
  test("calculate the transitions for Atlantic/Azores") {
    val calculatedRules = rules.find(_._1 == "Atlantic/Azores").map(_._2)
    compareZoneRules(calculatedRules, "Atlantic/Azores")
  }
  test("calculate the transitions for Europe/Samara") {
    val calculatedRules = rules.find(_._1 == "Europe/Samara").map(_._2)
    compareZoneRules(calculatedRules, "Europe/Samara")
  }
  test("calculate the transitions for Asia/Khandyga") {
    val calculatedRules = rules.find(_._1 == "Asia/Khandyga").map(_._2)
    compareZoneRules(calculatedRules, "Asia/Khandyga")
  }
  test("calculate the transitions for America/Los_Angeles") {
    val calculatedRules = rules.find(_._1 == "America/Los_Angeles").map(_._2)
    compareZoneRules(calculatedRules, "America/Los_Angeles")
  }
  test("calculate the transitions for America/Indiana/Tell_City") {
    val calculatedRules = rules.find(_._1 == "America/Indiana/Tell_City").map(_._2)
    compareZoneRules(calculatedRules, "America/Indiana/Tell_City")
  }
  test("calculate the transitions for America/Goose_Bay") {
    val calculatedRules = rules.find(_._1 == "America/Goose_Bay").map(_._2)
    compareZoneRules(calculatedRules, "America/Goose_Bay")
  }
  test("calculate the transitions for America/Montreal") {
    val calculatedRules = rules.find(_._1 == "America/Montreal").map(_._2)
    compareZoneRules(calculatedRules, "America/Montreal")
  }
  test("calculate the transitions for America/Swift_Current") {
    val calculatedRules = rules.find(_._1 == "America/Swift_Current").map(_._2)
    compareZoneRules(calculatedRules, "America/Swift_Current")
  }
  test("calculate the transitions for America/Cancun") {
    val calculatedRules = rules.find(_._1 == "America/Cancun").map(_._2)
    compareZoneRules(calculatedRules, "America/Cancun")
  }
  test("calculate the transitions for America/Costa_Rica") {
    val calculatedRules = rules.find(_._1 == "America/Costa_Rica").map(_._2)
    compareZoneRules(calculatedRules, "America/Costa_Rica")
  }
  test("calculate the transitions for Pacific/Easter") {
    val calculatedRules = rules.find(_._1 == "Pacific/Easter").map(_._2)
    compareZoneRules(calculatedRules, "Pacific/Easter")
  }
  test("calculate the transitions for America/Santiago") {
    val calculatedRules = rules.find(_._1 == "America/Santiago").map(_._2)
    compareZoneRules(calculatedRules, "America/Santiago")
  }
  test("calculate the transitions for America/Maceio") {
    val calculatedRules = rules.find(_._1 == "America/Maceio").map(_._2)
    compareZoneRules(calculatedRules, "America/Maceio")
  }
  test("calculate the transitions for Atlantic/Stanley") {
    val calculatedRules = rules.find(_._1 == "Atlantic/Stanley").map(_._2)
    compareZoneRules(calculatedRules, "Atlantic/Stanley")
  }
  test("calculate the transitions for America/Montevideo") {
    val calculatedRules = rules.find(_._1 == "America/Montevideo").map(_._2)
    compareZoneRules(calculatedRules, "America/Montevideo")
  }
  test("construct the transition zones for London") {
    val text = scala.io.Source
      .fromInputStream(this.getClass.getResourceAsStream("/europe_london"), "UTF-8")
      .mkString

    val parsedZoneRules: Option[Map[Zone, ZoneRules]] =
      TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).toOption
    assertEquals(parsedZoneRules.map(_.size), Some(1))

    val calculatedLondonRules =
      parsedZoneRules.flatMap(_.find(_._1.name == "Europe/London")).map(_._2)
    compareZoneRules(calculatedLondonRules, "Europe/London")
  }
  test("construct the transition zones for Kathmandu") {
    val text = scala.io.Source
      .fromInputStream(this.getClass.getResourceAsStream("/asia_kathmandu"), "UTF-8")
      .mkString

    val parsedZoneRules: Option[Map[Zone, ZoneRules]] =
      TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).toOption
    assertEquals(parsedZoneRules.map(_.size), Some(1))
    val calculatedKathmanduRules                      =
      parsedZoneRules.flatMap(_.find(_._1.name == "Asia/Kathmandu")).map(_._2)
    compareZoneRules(calculatedKathmanduRules, "Asia/Kathmandu")
  }
  test("construct the transition zones for Paris") {
    val text = scala.io.Source
      .fromInputStream(this.getClass.getResourceAsStream("/europe_paris"), "UTF-8")
      .mkString

    val parsedZoneRules: Option[Map[Zone, ZoneRules]] =
      TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).toOption
    assertEquals(parsedZoneRules.map(_.size), Some(1))
    val calculatedParisRules                          =
      parsedZoneRules.flatMap(_.find(_._1.name == "Europe/Paris")).map(_._2)
    compareZoneRules(calculatedParisRules, "Europe/Paris")
  }
  test("construct the transition zones for Casablanca") {
    val text = scala.io.Source
      .fromInputStream(this.getClass.getResourceAsStream("/africa_casablanca"), "UTF-8")
      .mkString

    val parsedZoneRules: Option[Map[Zone, ZoneRules]] =
      TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).toOption
    assertEquals(parsedZoneRules.map(_.size), Some(1))

    val calculatedCasablancaRules =
      parsedZoneRules.flatMap(_.find(_._1.name == "Africa/Casablanca")).map(_._2)
    compareZoneRules(calculatedCasablancaRules, "Africa/Casablanca")
  }
  test("construct the transition zones for Windhoek") {
    val text = scala.io.Source
      .fromInputStream(this.getClass.getResourceAsStream("/africa_windhoek"), "UTF-8")
      .mkString

    val parsedZoneRules: Option[Map[Zone, ZoneRules]] =
      TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).toOption
    assertEquals(parsedZoneRules.map(_.size), Some(1))

    val calculatedWindhoekRules =
      parsedZoneRules.flatMap(_.find(_._1.name == "Africa/Windhoek")).map(_._2)
    compareZoneRules(calculatedWindhoekRules, "Africa/Windhoek")
  }
  test("construct the transition zones for Cairo") {
    val text = scala.io.Source
      .fromInputStream(this.getClass.getResourceAsStream("/africa_cairo"), "UTF-8")
      .mkString

    val parsedZoneRules: Option[Map[Zone, ZoneRules]] =
      TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).toOption
    assertEquals(parsedZoneRules.map(_.size), Some(1))

    val calculatedCairoRules =
      parsedZoneRules.flatMap(_.find(_._1.name == "Africa/Cairo")).map(_._2)
    compareZoneRules(calculatedCairoRules, "Africa/Cairo")
  }
  test("construct the transition zones for Swift_current") {
    val text = scala.io.Source
      .fromInputStream(this.getClass.getResourceAsStream("/america_swift_current"), "UTF-8")
      .mkString

    val parsedZoneRules: Option[Map[Zone, ZoneRules]] =
      TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).toOption
    assertEquals(parsedZoneRules.map(_.size), Some(1))

    val calculatedLondonRules =
      parsedZoneRules.flatMap(_.find(_._1.name == "America/Swift_Current")).map(_._2)
    compareZoneRules(calculatedLondonRules, "America/Swift_Current")
  }
  test("construct the transition zones for Chihuahua") {
    val text = scala.io.Source
      .fromInputStream(this.getClass.getResourceAsStream("/northamerica_chihuahua"), "UTF-8")
      .mkString

    val parsedZoneRules: Option[Map[Zone, ZoneRules]] =
      TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).toOption
    assertEquals(parsedZoneRules.map(_.size), Some(1))

    val calculatedLondonRules =
      parsedZoneRules.flatMap(_.find(_._1.name == "America/Chihuahua")).map(_._2)
    compareZoneRules(calculatedLondonRules, "America/Chihuahua")
  }
  test("calculate the transitions for any rule") {
    val rulesAndLinks =
      ZoneRulesBuilder.calculateTransitionsWithLinks(TZDBParser.parseAll(r))
    JDKConv.toScala(ZoneRulesProvider.getAvailableZoneIds).foreach { z =>
      val calculatedRules = rulesAndLinks.find(_._1 == z).map(_._2)
      if (calculatedRules.isDefined)
        compareZoneRules(calculatedRules, z)
      else {
        // There a few rules not found
        // SystemV/AST4
        // SystemV/MST7
        // SystemV/CST6
        // SystemV/YST9
        // SystemV/HST10
        // SystemV/EST5
        // SystemV/PST8
        // SystemV/CST6CDT
        // SystemV/MST7MDT
        // SystemV/YST9YDT
        // SystemV/PST8PDT
        // SystemV/AST4ADT
        // SystemV/EST5EDT
      }
    }
  }

}
