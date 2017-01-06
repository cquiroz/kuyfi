package kuyfi

import kuyfi.TZDB.{GivenYear, Maximum, Minimum, Only, RuleYear}
import org.scalatest.{FlatSpec, Matchers}

import scalaz._
import Scalaz._

class TZDBSpec extends FlatSpec with Matchers {
  "Year ordering" should
    "orders given years" in {
      (GivenYear(2001): RuleYear) <= (GivenYear(2002): RuleYear) shouldBe true
      (GivenYear(2004): RuleYear) >= (GivenYear(2002): RuleYear) shouldBe true
      (GivenYear(2004): RuleYear) == (GivenYear(2004): RuleYear) shouldBe true
    }
    it should "make everything less than Maximum" in {
      (GivenYear(2001): RuleYear) <= (Maximum: RuleYear) shouldBe true
      (Minimum: RuleYear) <= (Maximum: RuleYear) shouldBe true
      (Only: RuleYear) <= (Maximum: RuleYear) shouldBe true
      (Maximum: RuleYear) >= (Maximum: RuleYear) && (Maximum: RuleYear) <= (Maximum: RuleYear) shouldBe true
    }
    it should "make everything more than Maximum" in {
      (GivenYear(2001): RuleYear) <= (Maximum: RuleYear) shouldBe true
      (Minimum: RuleYear) <= (Maximum: RuleYear) shouldBe true
      (Only: RuleYear) <= (Maximum: RuleYear) shouldBe true
      (Minimum: RuleYear) >= (Minimum: RuleYear) && (Minimum: RuleYear) <= (Minimum: RuleYear) shouldBe true
    }
    it should "make everything equal to Only" in {
      (GivenYear(2001): RuleYear) >= (Only: RuleYear) shouldBe true
      (Minimum: RuleYear) >=(Only: RuleYear) shouldBe true
      (Maximum: RuleYear) >= (Only: RuleYear) shouldBe true
      (Only: RuleYear) <= (Only: RuleYear) && (Only: RuleYear) >= (Only: RuleYear) shouldBe true
    }
}
