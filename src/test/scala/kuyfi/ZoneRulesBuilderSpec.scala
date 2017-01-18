package kuyfi

import org.scalatest.{FlatSpec, Matchers}
import atto.ParseResult.{Done, Fail}
import atto.ParseResult
import java.time.zone.{ZoneRules, ZoneRulesProvider}

class ZoneRulesBuilderSpec extends FlatSpec with Matchers {
  "ZoneRulesBuilder" should
    "construct the transition zones for London" in {
      val londonRules = ZoneRulesProvider.getRules("Europe/London", false)

      val text = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/europe_london"), "UTF-8").mkString

      val parsedZoneRules: Option[List[ZoneRules]] = TZDBParser.parseFile(text).map(ZoneRulesBuilder.calculateTransitions).option
      parsedZoneRules.map(_.size) shouldBe Some(1)
      val calculatedLondonRules = parsedZoneRules.flatMap(_.headOption)
      calculatedLondonRules.map(_.getTransitionRules.size) shouldBe Some(londonRules.getTransitionRules.size)
      calculatedLondonRules.map(_.getTransitionRules) shouldBe Some(londonRules.getTransitionRules)
      calculatedLondonRules.map(_.getTransitions.size) shouldBe Some(londonRules.getTransitions.size)
      calculatedLondonRules.map(_.getTransitions) shouldBe Some(londonRules.getTransitions)
      calculatedLondonRules.map(_.isFixedOffset) shouldBe Some(londonRules.isFixedOffset)
      calculatedLondonRules.map(_.toString) shouldBe Some(londonRules.toString)
    }
}
