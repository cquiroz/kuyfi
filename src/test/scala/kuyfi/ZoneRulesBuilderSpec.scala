package kuyfi

import org.scalatest.{FlatSpec, Matchers}

class ZoneRulesBuilderSpec extends FlatSpec with Matchers {
  "ZoneRulesBuilder" should
    "construct the transition zones for London" in {
      val text = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/europe_london"), "UTF-8").mkString
      val r = TZDBParser.parseFile(text)
      r.map(ZoneRulesBuilder.calculateTransitions)//.map(println)
    }
}
