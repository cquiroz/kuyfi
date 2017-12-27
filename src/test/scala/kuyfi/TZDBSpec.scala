package kuyfi

import java.time.{DayOfWeek, LocalTime, Month, LocalDateTime}

import kuyfi.TZDB._
import org.scalatest.{FlatSpec, Matchers}

import cats._
import cats.instances._
import cats.implicits._

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

  "Until" should
    "calculate date time" in {
      val until = Until(1998,Some(Month.APRIL),Some(AfterWeekday(DayOfWeek.SUNDAY,1)),Some(AtWallTime(LocalTime.of(3, 0), endOfDay = false)))
      until.toDateTime shouldBe LocalDateTime.of(1998, Month.APRIL, 5, 3, 0)
      until.toDateTime shouldBe LocalDateTime.of(1998, Month.APRIL, 5, 3, 0)
    }

  "On" should
    "calculate day on a year" in {
      val on = AfterWeekday(DayOfWeek.SUNDAY, 1)
      on.dayOnYear(1998, Month.APRIL) shouldBe 5
      val on2 = DayOfTheMonth(5)
      on2.dayOnYear(1998, Month.APRIL) shouldBe 5
    }
}
