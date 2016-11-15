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
      val rules = List(
        "Rule	Algeria	1916	only	-	Jun	14	23:00s	1:00	S" ->
          Rule("Algeria", GivenYear(1916), Only, Month.JUNE, DayOfTheMonth(14), AtStandardTime(LocalTime.of(23, 0)), RuleSave(LocalTime.of(1, 0)), RuleLetter("S")),
        "Rule	Egypt	1995	2010	-	Apr	lastFri	 0:00s	1:00	S" ->
          Rule("Egypt", GivenYear(1995), GivenYear(2010), Month.APRIL, LastWeekday(DayOfWeek.FRIDAY), AtStandardTime(LocalTime.of(0, 0)), RuleSave(LocalTime.of(1, 0)), RuleLetter("S")),
        "Rule	Egypt	2007	only	-	Sep	Thu>=1	24:00	0	-" ->
          Rule("Egypt", GivenYear(2007), Only, Month.SEPTEMBER, AfterWeekday(DayOfWeek.THURSDAY, 1), AtWallTime(LocalTime.of(0, 0)), RuleSave(LocalTime.of(0, 0)), RuleLetter("-")),
        "Rule	Ghana	1920	1942	-	Sep	 1	0:00	0:20	GHST" ->
          Rule("Ghana", GivenYear(1920), GivenYear(1942), Month.SEPTEMBER, DayOfTheMonth(1), AtWallTime(LocalTime.of(0, 0)), RuleSave(LocalTime.of(0, 20)), RuleLetter("GHST")),
        "Rule RussiaAsia	1981	1984	-	Apr	1	 0:00	1:00	S" ->
          Rule("RussiaAsia", GivenYear(1981), GivenYear(1984), Month.APRIL, DayOfTheMonth(1), AtWallTime(LocalTime.of(0, 0)), RuleSave(LocalTime.of(1, 0)), RuleLetter("S")),
        "Rule	Lebanon	1993	max	-	Mar	lastSun	0:00	1:00	S" ->
          Rule("Lebanon", GivenYear(1993), Maximum, Month.MARCH, LastWeekday(DayOfWeek.SUNDAY), AtWallTime(LocalTime.of(0, 0)), RuleSave(LocalTime.of(1, 0)), RuleLetter("S")),
        "Rule	Syria	1991	only	-	Apr	 1	0:00	1:00	S" ->
          Rule("Syria", GivenYear(1991), Only, Month.APRIL, DayOfTheMonth(1), AtWallTime(LocalTime.of(0, 0)), RuleSave(LocalTime.of(1, 0)), RuleLetter("S")),
        "Rule	Regina	1945	only	-	Aug	14	23:00u	1:00	P # Peace" ->
          Rule("Regina", GivenYear(1945), Only, Month.AUGUST, DayOfTheMonth(14), AtUniversalTime(LocalTime.of(23, 0)), RuleSave(LocalTime.of(1, 0)), RuleLetter("P"))
      )
      rules.foreach { rule =>
        (ruleParser parseOnly rule._1) shouldBe Done("", rule._2)
      }
    }
    it should "parse Link" in {
      val links = List(
        "Link America/Curacao America/Aruba" ->
          Link("America/Curacao", "America/Aruba"),
        "Link	America/Curacao	America/Kralendijk	# Caribbean Netherlands" ->
          Link("America/Curacao", "America/Kralendijk"),
        "Link America/Port_of_Spain America/Marigot	# St Martin (French part)" ->
          Link("America/Port_of_Spain", "America/Marigot"),
        "Link	America/Los_Angeles	US/Pacific-New	##" ->
          Link("America/Los_Angeles", "US/Pacific-New"),
        "Link	Etc/GMT				Etc/GMT+0" ->
          Link("Etc/GMT", "Etc/GMT+0"),
        "Link Africa/Abidjan Africa/Sao_Tome	# São Tomé and Príncipe" ->
          Link("Africa/Abidjan", "Africa/Sao_Tome")
      )
      links.foreach { link =>
        (linkParser parseOnly link._1) shouldBe Done("", link._2)
      }
    }
}
