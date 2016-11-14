package kuyfi

import java.time.{DayOfWeek, LocalTime, Month}
import java.time.format.TextStyle
import java.util.Locale

import org.scalatest.{FlatSpec, Matchers}
import atto.Atto._
import atto._
import TZDBParser._
import atto.ParseResult.{Done, Fail}

class ParserSpec extends FlatSpec with Matchers {
  "TZDBParser from field" should
    "parse from maximum" in {
      (fromParser parseOnly "maximum") shouldBe Done("", Maximum)
    }
    it should "parse from minimum" in {
      (fromParser parseOnly "minimum") shouldBe Done("", Minimum)
    }
    it should "parse with a date" in {
      (fromParser parseOnly "1974") shouldBe Done("", GivenYear(1974))
    }

  "TZDBParser to field" should
    "parse to maximum" in {
      (toParser parseOnly "maximum") shouldBe Done("", Maximum)
    }
    it should "parse to minimum" in {
      (toParser parseOnly "minimum") shouldBe Done("", Minimum)
    }
    it should "parse to only" in {
      (toParser parseOnly "only") shouldBe Done("", Only)
    }
    it should "parse with a date" in {
      (toParser parseOnly "1974") shouldBe Done("", GivenYear(1974))
    }

  "TZDBParser in field" should
    "parse in June" in {
      (monthParser parseOnly "Jun") shouldBe Done("", Month.JUNE)
    }
    it should "parse in December" in {
      (monthParser parseOnly "Dec") shouldBe Done("", Month.DECEMBER)
    }
    it should "parse non valid" in {
      (monthParser parseOnly "abc") shouldBe Fail("abc", Nil, "unknown month")
    }

  "TZDBParser dayParser" should
    "parse Mon" in {
      (dayParser parseOnly "Mon") shouldBe Done("", DayOfWeek.MONDAY)
    }
    it should "parse in Sun" in {
      (dayParser parseOnly "Sun") shouldBe Done("", DayOfWeek.SUNDAY)
    }
    it should "parse non valid" in {
      (dayParser parseOnly "abc") shouldBe Fail("abc", Nil, "unknown day")
    }

  "TZDBParser AfterWeekday" should
    "sun after the eighth" in {
      (afterWeekdayParser parseOnly "Sun>=8") shouldBe Done("", AfterWeekday(DayOfWeek.SUNDAY, 8))
    }

  "TZDBParser BeforeWeekday" should
    "sun before the 25th" in {
      (beforeWeekdayParser parseOnly "Sun<=25") shouldBe Done("", BeforeWeekday(DayOfWeek.SUNDAY, 25))
    }

  "TZDBParser LastWeekday of" should
    "last sunday" in {
      (lastWeekdayParser parseOnly "lastSun") shouldBe Done("", LastWeekday(DayOfWeek.SUNDAY))
    }
    it should "last monday" in {
      (lastWeekdayParser parseOnly "lastMon") shouldBe Done("", LastWeekday(DayOfWeek.MONDAY))
    }
  "TZDBParser AtTime parser" should
    "parse single number time" in {
      (atParser parseOnly "2") shouldBe Done("", AtWallTime(LocalTime.of(2, 0)))
    }
    it should "parse wall time two number time" in {
      (atParser parseOnly "16") shouldBe Done("", AtWallTime(LocalTime.of(16, 0)))
    }
    it should "parse wall time h:m time" in {
      (atParser parseOnly "2:00") shouldBe Done("", AtWallTime(LocalTime.of(2, 0)))
    }
    it should "parse wall time h:m 24h time" in {
      (atParser parseOnly "15:00") shouldBe Done("", AtWallTime(LocalTime.of(15, 0)))
    }
    it should "parse wall time h:m:s time" in {
      (atParser parseOnly "1:28:14") shouldBe Done("", AtWallTime(LocalTime.of(1, 28, 14)))
    }
    it should "parse explicit wall time two number time" in {
      (atParser parseOnly "16w") shouldBe Done("", AtWallTime(LocalTime.of(16, 0)))
    }
    it should "parse explicit wall time h:m time" in {
      (atParser parseOnly "2:00w") shouldBe Done("", AtWallTime(LocalTime.of(2, 0)))
    }
    it should "parse explicit wall time h:m 24h time" in {
      (atParser parseOnly "15:00w") shouldBe Done("", AtWallTime(LocalTime.of(15, 0)))
    }
    it should "parse explicit wall time h:m:s time" in {
      (atParser parseOnly "1:28:14w") shouldBe Done("", AtWallTime(LocalTime.of(1, 28, 14)))
    }
    it should "parse explicit standard time two number time" in {
      (atParser parseOnly "16s") shouldBe Done("", AtStandardTime(LocalTime.of(16, 0)))
    }
    it should "parse explicit standard time h:m time" in {
      (atParser parseOnly "2:00s") shouldBe Done("", AtStandardTime(LocalTime.of(2, 0)))
    }
    it should "parse explicit standard time h:m 24h time" in {
      (atParser parseOnly "15:00s") shouldBe Done("", AtStandardTime(LocalTime.of(15, 0)))
    }
    it should "parse explicit standard time h:m:s time" in {
      (atParser parseOnly "1:28:14s") shouldBe Done("", AtStandardTime(LocalTime.of(1, 28, 14)))
    }
    it should "parse explicit universal time two number time" in {
      (atParser parseOnly "16u") shouldBe Done("", AtUniversalTime(LocalTime.of(16, 0)))
    }
    it should "parse explicit universal time h:m time" in {
      (atParser parseOnly "2:00u") shouldBe Done("", AtUniversalTime(LocalTime.of(2, 0)))
    }
    it should "parse explicit universal time h:m 24h time" in {
      (atParser parseOnly "15:00u") shouldBe Done("", AtUniversalTime(LocalTime.of(15, 0)))
    }
    it should "parse explicit universal time h:m:s time" in {
      (atParser parseOnly "1:28:14u") shouldBe Done("", AtUniversalTime(LocalTime.of(1, 28, 14)))
    }

  "TZDBParser" should
    "parse Rules" in {
      val rule = "Rule	Algeria	1916	only	-	Jun	14	23:00s	1:00	S"
      (ruleParser parseOnly rule) shouldBe Done("",
        Rule("Algeria", GivenYear(1916), Only, Month.JUNE, DayOfTheMonth(14), AtStandardTime(LocalTime.of(23, 0)), RuleSave(LocalTime.of(1, 0)), RuleLetter('S')))
    }
}
