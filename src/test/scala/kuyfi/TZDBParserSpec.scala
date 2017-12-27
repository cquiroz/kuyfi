package kuyfi

import java.time.{DayOfWeek, LocalTime, Month}

import org.scalatest.{FlatSpec, Matchers}
import atto._
import Atto._
import TZDB._
import TZDBParser._
import atto.ParseResult.{Done, Fail}

class TZDBParserSpec extends FlatSpec with Matchers {
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

  "TZDBParser on" should
    "fixed day" in {
      (onParser parseOnly "24") shouldBe Done("", DayOfTheMonth(24))
    }
    it should "calculate dayOfMontIndicator for fixed day" in {
      (onParser parseOnly "24").map(_.dayOfMonthIndicator) shouldBe Done("", Some(24))
    }
    it should "calculate dayOfMontIndicator for lastSun" in {
      (onParser parseOnly "lastSun").map(_.dayOfMonthIndicator) shouldBe Done("", None)
    }
    it should "calculate dayOfMontIndicator for Sat>=5" in {
      (onParser parseOnly "Sat>=5").map(_.dayOfMonthIndicator) shouldBe Done("", Some(5))
    }
    it should "calculate dayOfMontIndicator for Sat<=5" in {
      (onParser parseOnly "Sat<=5").map(_.dayOfMonthIndicator) shouldBe Done("", Some(5))
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
    it should "parse explicit gmt time h:m 24h time" in {
      (atParser parseOnly "15:00g") shouldBe Done("", AtUniversalTime(LocalTime.of(15, 0)))
    }
    it should "parse explicit z time h:m 24h time" in {
      (atParser parseOnly "15:00z") shouldBe Done("", AtUniversalTime(LocalTime.of(15, 0)))
    }
    it should "parse explicit universal time h:m:s time" in {
      (atParser parseOnly "1:28:14u") shouldBe Done("", AtUniversalTime(LocalTime.of(1, 28, 14)))
    }
    it should "calculate if at the end of day" in {
      (atParser parseOnly "2:20").map(_.endOfDay) shouldBe Done("", false)
      (atParser parseOnly "2:20s").map(_.endOfDay) shouldBe Done("", false)
      (atParser parseOnly "2:20u").map(_.endOfDay) shouldBe Done("", false)
      (atParser parseOnly "24:00g").map(_.endOfDay) shouldBe Done("", true)
      //(atParser parseOnly "-").map(_.endOfDay) shouldBe Done("", false)
      (atParser parseOnly "3z").map(_.endOfDay) shouldBe Done("", false)
      (atParser parseOnly "0:00").map(_.endOfDay) shouldBe Done("", false)
    }

  "TZDBParser" should
    "parse Rules" in {
      val rules = List(
        "Rule	Algeria	1916	only	-	Jun	14	23:00s	1:00	S" ->
          Rule("Algeria", GivenYear(1916), Only, Month.JUNE, DayOfTheMonth(14), AtStandardTime(LocalTime.of(23, 0)), Save(LocalTime.of(1, 0)), Letter("S")),
        "Rule	Egypt	1995	2010	-	Apr	lastFri	 0:00s	1:00	S" ->
          Rule("Egypt", GivenYear(1995), GivenYear(2010), Month.APRIL, LastWeekday(DayOfWeek.FRIDAY), AtStandardTime(LocalTime.of(0, 0)), Save(LocalTime.of(1, 0)), Letter("S")),
        "Rule	Egypt	2007	only	-	Sep	Thu>=1	24:00	0	-" ->
          Rule("Egypt", GivenYear(2007), Only, Month.SEPTEMBER, AfterWeekday(DayOfWeek.THURSDAY, 1), AtWallTime(LocalTime.of(0, 0), true), Save(LocalTime.of(0, 0)), Letter("-")),
        "Rule	Ghana	1920	1942	-	Sep	 1	0:00	0:20	GHST" ->
          Rule("Ghana", GivenYear(1920), GivenYear(1942), Month.SEPTEMBER, DayOfTheMonth(1), AtWallTime(LocalTime.of(0, 0)), Save(LocalTime.of(0, 20)), Letter("GHST")),
        "Rule RussiaAsia	1981	1984	-	Apr	1	 0:00	1:00	S" ->
          Rule("RussiaAsia", GivenYear(1981), GivenYear(1984), Month.APRIL, DayOfTheMonth(1), AtWallTime(LocalTime.of(0, 0)), Save(LocalTime.of(1, 0)), Letter("S")),
        "Rule	Lebanon	1993	max	-	Mar	lastSun	0:00	1:00	S" ->
          Rule("Lebanon", GivenYear(1993), Maximum, Month.MARCH, LastWeekday(DayOfWeek.SUNDAY), AtWallTime(LocalTime.of(0, 0)), Save(LocalTime.of(1, 0)), Letter("S")),
        "Rule	Syria	1991	only	-	Apr	 1	0:00	1:00	S" ->
          Rule("Syria", GivenYear(1991), Only, Month.APRIL, DayOfTheMonth(1), AtWallTime(LocalTime.of(0, 0)), Save(LocalTime.of(1, 0)), Letter("S")),
        "Rule	Regina	1945	only	-	Aug	14	23:00u	1:00	P # Peace" ->
          Rule("Regina", GivenYear(1945), Only, Month.AUGUST, DayOfTheMonth(14), AtUniversalTime(LocalTime.of(23, 0)), Save(LocalTime.of(1, 0)), Letter("P")),
        "Rule Indianapolis 1941	only	-	Jun	22	2:00	1:00	D" ->
          Rule("Indianapolis", GivenYear(1941), Only, Month.JUNE, DayOfTheMonth(22), AtWallTime(LocalTime.of(2, 0)), Save(LocalTime.of(1, 0)), Letter("D")),
        "Rule	Syria	2007	only	-	Nov	 Fri>=1	0:00	0	-" ->
          Rule("Syria", GivenYear(2007), Only, Month.NOVEMBER, AfterWeekday(DayOfWeek.FRIDAY, 1), AtWallTime(LocalTime.of(0, 0)), Save(LocalTime.of(0, 0)), Letter("-")),
        "Rule	Morocco	2011	only	-	Jul	31	 0	0	-" ->
          Rule("Morocco", GivenYear(2011), Only, Month.JULY, DayOfTheMonth(31), AtWallTime(LocalTime.of(0, 0)), Save(LocalTime.of(0, 0)), Letter("-")),
        "Rule	SystemV	1974	only	-	Jan	6	2:00	1:00	D" ->
          Rule("SystemV", GivenYear(1974), Only, Month.JANUARY, DayOfTheMonth(6), AtWallTime(LocalTime.of(2, 0)), Save(LocalTime.of(1, 0)), Letter("D"))
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
    it should "parse single-line Zone" in {
      val zones = List(
        "Zone	EST		 -5:00	-	EST" ->
          Zone("EST", List(ZoneTransition(GmtOffset(-5, 0, 0), NullRule, "EST", None))),
        "Zone	Africa/Abidjan	-0:16:08 -	LMT	1912" ->
          Zone("Africa/Abidjan", List(ZoneTransition(GmtOffset(0, -16, -8), NullRule, "LMT", Some(Until(1912, None, None, None))))),
        "Zone	Africa/Bissau	-1:02:20 -	LMT	1912 Jan  1" ->
          Zone("Africa/Bissau", List(ZoneTransition(GmtOffset(-1, -2, -20), NullRule, "LMT", Some(Until(1912, Some(Month.JANUARY), Some(DayOfTheMonth(1)), None))))),
        "Zone	Africa/Nairobi	2:27:16	-	LMT	1928 Jul" ->
          Zone("Africa/Nairobi", List(ZoneTransition(GmtOffset(2, 27, 16), NullRule, "LMT", Some(Until(1928, Some(Month.JULY), None, None))))),
        "Zone Antarctica/DumontDUrville 0 -	-00	1947" ->
          Zone("Antarctica/DumontDUrville", List(ZoneTransition(GmtOffset(0, 0, 0), NullRule, "-00", Some(Until(1947, None, None, None))))),
        "Zone Pacific/Honolulu -10:31:26 -     LMT    1900 Jan  1 12:00" ->
          Zone("Pacific/Honolulu", List(ZoneTransition(GmtOffset(-10, -31, -26), NullRule, "LMT", Some(Until(1900, Some(Month.JANUARY), Some(DayOfTheMonth(1)), Some(AtWallTime(LocalTime.of(12, 0)))))))),
        "Zone America/Juneau -8:57:41 -	LMT	1900 Aug 20 12:00" ->
          Zone("America/Juneau", List(ZoneTransition(GmtOffset(-8, -57, -41), NullRule, "LMT", Some(Until(1900, Some(Month.AUGUST), Some(DayOfTheMonth(20)), Some(AtWallTime(LocalTime.of(12, 0))))))))
      )
      zones.foreach { zone =>
        (zoneParser parseOnly zone._1) shouldBe Done("", zone._2)
      }
    }
    it should "parse a multi-line Zone" in {
      val zones = List(
        """Zone Pacific/Honolulu	-10:31:26 -	LMT	1896 Jan 13 12:00
          |			-10:30	-	HST	1933 Apr 30  2:00
          |			-10:30	1:00	HDT	1933 May 21 12:00
          |			-10:30	-	HST	1942 Feb  9  2:00
          |			-10:30	1:00	HDT	1945 Sep 30  2:00
          |			-10:30	-	HST	1947 Jun  8  2:00
          |			-10:00	-	HST""".stripMargin ->
          Zone("Pacific/Honolulu", List(
            ZoneTransition(GmtOffset(-10, -31, -26), NullRule,                        "LMT", Some(Until(1896, Some(Month.JANUARY),   Some(DayOfTheMonth(13)), Some(AtWallTime(LocalTime.of(12, 0)))))),
            ZoneTransition(GmtOffset(-10, -30,   0), NullRule,                        "HST", Some(Until(1933, Some(Month.APRIL),     Some(DayOfTheMonth(30)), Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-10, -30,   0), FixedOffset(GmtOffset(1, 0, 0)), "HDT", Some(Until(1933, Some(Month.MAY),       Some(DayOfTheMonth(21)), Some(AtWallTime(LocalTime.of(12, 0)))))),
            ZoneTransition(GmtOffset(-10, -30,   0), NullRule,                        "HST", Some(Until(1942, Some(Month.FEBRUARY),  Some(DayOfTheMonth(9)),  Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-10, -30,   0), FixedOffset(GmtOffset(1, 0, 0)), "HDT", Some(Until(1945, Some(Month.SEPTEMBER), Some(DayOfTheMonth(30)), Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-10, -30,   0), NullRule,                        "HST", Some(Until(1947, Some(Month.JUNE),      Some(DayOfTheMonth(8)),  Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-10,   0,   0), NullRule,                        "HST", None)
          )),
        """Zone America/Phoenix	-7:28:18 -	LMT	1883 Nov 18 11:31:42
          |			-7:00	US	M%sT	1944 Jan  1  0:01
          |			-7:00	-	MST	1944 Apr  1  0:01
          |			-7:00	US	M%sT	1944 Oct  1  0:01
          |			-7:00	-	MST	1967
          |			-7:00	US	M%sT	1968 Mar 21
          |			-7:00	-	MST""".stripMargin ->
          Zone("America/Phoenix", List(
            ZoneTransition(GmtOffset(-7, -28, -18), NullRule,  "LMT",  Some(Until(1883, Some(Month.NOVEMBER), Some(DayOfTheMonth(18)), Some(AtWallTime(LocalTime.of(11, 31, 42)))))),
            ZoneTransition(GmtOffset(-7,   0,   0), RuleId("US"), "M%sT", Some(Until(1944, Some(Month.JANUARY),  Some(DayOfTheMonth(1)),  Some(AtWallTime(LocalTime.of(0, 1)))))),
            ZoneTransition(GmtOffset(-7,   0,   0), NullRule,  "MST",  Some(Until(1944, Some(Month.APRIL),    Some(DayOfTheMonth(1)),  Some(AtWallTime(LocalTime.of(0, 1)))))),
            ZoneTransition(GmtOffset(-7,   0,   0), RuleId("US"), "M%sT", Some(Until(1944, Some(Month.OCTOBER),  Some(DayOfTheMonth(1)),  Some(AtWallTime(LocalTime.of(0, 1)))))),
            ZoneTransition(GmtOffset(-7,   0,   0), NullRule,  "MST",  Some(Until(1967, None,                 None,                    None))),
            ZoneTransition(GmtOffset(-7,   0,   0), RuleId("US"), "M%sT", Some(Until(1968, Some(Month.MARCH),    Some(DayOfTheMonth(21)), None))),
            ZoneTransition(GmtOffset(-7,   0,   0), NullRule,  "MST",  None)
          )),
        """Zone America/Indiana/Tell_City -5:47:03 - LMT	1883 Nov 18 12:12:57
          |			-6:00	US	C%sT	1946
          |			-6:00 Perry	C%sT	1964 Apr 26  2:00
          |			-5:00	-	EST	1969
          |			-5:00	US	E%sT	1971
          |			-5:00	-	EST	2006 Apr  2  2:00
          |			-6:00	US	C%sT""".stripMargin ->
          Zone("America/Indiana/Tell_City", List(
            ZoneTransition(GmtOffset(-5, -47, -3), NullRule,     "LMT",  Some(Until(1883, Some(Month.NOVEMBER), Some(DayOfTheMonth(18)), Some(AtWallTime(LocalTime.of(12, 12, 57)))))),
            ZoneTransition(GmtOffset(-6,   0,  0), RuleId("US"),    "C%sT", Some(Until(1946, None,                 None,                    None))),
            ZoneTransition(GmtOffset(-6,   0,  0), RuleId("Perry"), "C%sT", Some(Until(1964, Some(Month.APRIL),    Some(DayOfTheMonth(26)), Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-5,   0,  0), NullRule,     "EST",  Some(Until(1969, None,                 None,                    None))),
            ZoneTransition(GmtOffset(-5,   0,  0), RuleId("US"),    "E%sT", Some(Until(1971, None,                 None,                    None))),
            ZoneTransition(GmtOffset(-5,   0,  0), NullRule,     "EST",  Some(Until(2006, Some(Month.APRIL),    Some(DayOfTheMonth(2)),  Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-6,   0,  0), RuleId("US"),    "C%sT", None)
          )),
        """Zone America/Guadeloupe	-4:06:08 -	LMT	1911 Jun  8 # Pointe-à-Pitre
          |			-4:00	 -	AST
          |""".stripMargin ->
          Zone("America/Guadeloupe", List(
            ZoneTransition(GmtOffset(-4, -6, -8), NullRule, "LMT", Some(Until(1911, Some(Month.JUNE), Some(DayOfTheMonth(8)), None))),
            ZoneTransition(GmtOffset(-4,  0,  0), NullRule, "AST", None)
          )),
        """Zone America/Juneau	 15:02:19 -	LMT	1867 Oct 18
          |			 -8:57:41 -	LMT	1900 Aug 20 12:00
          |""".stripMargin ->
          Zone("America/Juneau", List(
            ZoneTransition(GmtOffset(15,  2, 19),   NullRule,  "LMT",   Some(Until(1867, Some(Month.OCTOBER),  Some(DayOfTheMonth(18)), None))),
            ZoneTransition(GmtOffset(-8, -57, -41), NullRule,  "LMT",   Some(Until(1900, Some(Month.AUGUST),   Some(DayOfTheMonth(20)), Some(AtWallTime(LocalTime.of(12, 0))))))
          )),
        """Zone	America/Caracas	-4:27:44 -	LMT	1890
          |			-4:27:40 -	CMT	1912 Feb 12 # Caracas Mean Time?
          |			-4:30	-	VET	1965 Jan  1  0:00 # Venezuela T.
          |""".stripMargin ->
          Zone("America/Caracas", List(
            ZoneTransition(GmtOffset(-4, -27, -44), NullRule,  "LMT",   Some(Until(1890, None, None, None))),
            ZoneTransition(GmtOffset(-4, -27, -40), NullRule,  "CMT",   Some(Until(1912, Some(Month.FEBRUARY), Some(DayOfTheMonth(12)), None))),
            ZoneTransition(GmtOffset(-4, -30,   0), NullRule,  "VET",   Some(Until(1965, Some(Month.JANUARY), Some(DayOfTheMonth(1)), Some(AtWallTime(LocalTime.of(0, 0))))))
          )),
        """Zone	Asia/Kathmandu	5:41:16 -	LMT	1920
          |			5:30	-	IST	1986
          |			5:45	-	NPT	# Nepal Time
          |""".stripMargin ->
          Zone("Asia/Kathmandu", List(
            ZoneTransition(GmtOffset(5, 41, 16), NullRule,  "LMT",   Some(Until(1920, None, None, None))),
            ZoneTransition(GmtOffset(5, 30,  0), NullRule,  "IST",   Some(Until(1986, None, None, None))),
            ZoneTransition(GmtOffset(5, 45,  0), NullRule,  "NPT",   None)
          )),
        """Zone Africa/Casablanca	-0:30:20 -	LMT	1913 Oct 26
          |			 0:00	Morocco	WE%sT	1984 Mar 16
          |			 1:00	-	CET	1986
          |			 0:00	Morocco	WE%sT""".stripMargin ->
          Zone("Africa/Casablanca", List(
            ZoneTransition(GmtOffset(-0, -30, -20), NullRule,          "LMT",   Some(Until(1913, Some(Month.OCTOBER), Some(DayOfTheMonth(26)), None))),
            ZoneTransition(GmtOffset( 0,   0,   0), RuleId("Morocco"), "WE%sT", Some(Until(1984, Some(Month.MARCH), Some(DayOfTheMonth(16)), None))),
            ZoneTransition(GmtOffset( 1,   0,   0), NullRule,          "CET",   Some(Until(1986, None, None, None))),
            ZoneTransition(GmtOffset( 0,   0,   0), RuleId("Morocco"), "WE%sT", None)
          )),
        """Zone America/Swift_Current -7:11:20 -	LMT	1905 Sep
          |			-7:00	Canada	M%sT	1946 Apr lastSun  2:00
          |			-7:00	Regina	M%sT	1950
          |			-7:00	Swift	M%sT	1972 Apr lastSun  2:00
          |			-6:00	-	CST
          |""".stripMargin ->
          Zone("America/Swift_Current", List(
            ZoneTransition(GmtOffset(-7, -11, -20), NullRule,         "LMT",  Some(Until(1905, Some(Month.SEPTEMBER), None, None))),
            ZoneTransition(GmtOffset(-7,   0,   0), RuleId("Canada"), "M%sT", Some(Until(1946, Some(Month.APRIL), Some(LastWeekday(DayOfWeek.SUNDAY)), Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-7,   0,   0), RuleId("Regina"), "M%sT", Some(Until(1950, None, None, None))),
            ZoneTransition(GmtOffset(-7,   0,   0), RuleId("Swift"),  "M%sT", Some(Until(1972, Some(Month.APRIL), Some(LastWeekday(DayOfWeek.SUNDAY)), Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-6,   0,   0), NullRule,         "CST",  None)
          ))
      )
      zones.foreach { zone =>
        (zoneParserNl parseOnly zone._1) shouldBe Done("", zone._2)
      }
      // Tests some special cases
      // The first zone has an unknown rule
      val firstRule = zones.headOption.flatMap { z =>
        z._2.transitions.headOption.map(_.ruleId)
      }
      firstRule shouldBe Some(NullRule)

      // The second zone has an id
      val secondRule = zones.lift(1).flatMap { _._2.transitions.lift(1).map(_.ruleId) }
      secondRule shouldBe Some(RuleId("US"))

      // The third zone has a fixed offset
      val thirdRule = zones.headOption.flatMap { _._2.transitions.lift(2).map(_.ruleId) }
      thirdRule shouldBe Some(FixedOffset(GmtOffset(1, 0, 0)))
      // The third zone has a fixed offset
      val fifthRule = zones.lift(5).flatMap { _._2.transitions.lift(2).map(_.offset) }
      fifthRule shouldBe Some(GmtOffset(-4, -30, 0))
    }
    it should "parse contiguous Zones" in {
      val zones = List(
        """Zone America/Juneau	 15:02:19 -	LMT	1867 Oct 18
          |			 -8:57:41 -	LMT	1900 Aug 20 12:00
          |			 -8:00	-	PST	1942
          |			 -8:00	US	P%sT	1946
          |			 -8:00	-	PST	1969
          |			 -8:00	US	P%sT	1980 Apr 27  2:00
          |			 -9:00	US	Y%sT	1980 Oct 26  2:00
          |			 -8:00	US	P%sT	1983 Oct 30  2:00
          |			 -9:00	US	Y%sT	1983 Nov 30
          |			 -9:00	US	AK%sT
          |Zone America/Sitka	 14:58:47 -	LMT	1867 Oct 18
          |			 -9:01:13 -	LMT	1900 Aug 20 12:00
          |			 -8:00	-	PST	1942
          |			 -8:00	US	P%sT	1946
          |			 -8:00	-	PST	1969
          |			 -8:00	US	P%sT	1983 Oct 30  2:00
          |			 -9:00	US	Y%sT	1983 Nov 30
          |			 -9:00	US	AK%sT
          |""".stripMargin ->
          List(Zone("America/Juneau", List(
              ZoneTransition(GmtOffset(15,   2,  19), NullRule,     "LMT",   Some(Until(1867, Some(Month.OCTOBER),  Some(DayOfTheMonth(18)), None))),
              ZoneTransition(GmtOffset(-8, -57, -41), NullRule,     "LMT",   Some(Until(1900, Some(Month.AUGUST),   Some(DayOfTheMonth(20)), Some(AtWallTime(LocalTime.of(12, 0)))))),
              ZoneTransition(GmtOffset(-8,   0,   0), NullRule,     "PST",   Some(Until(1942, None,                 None,                    None))),
              ZoneTransition(GmtOffset(-8,   0,   0), RuleId("US"), "P%sT",  Some(Until(1946, None,                 None,                    None))),
              ZoneTransition(GmtOffset(-8,   0,   0), NullRule,     "PST",   Some(Until(1969, None,                 None,                    None))),
              ZoneTransition(GmtOffset(-8,   0,   0), RuleId("US"), "P%sT",  Some(Until(1980, Some(Month.APRIL),    Some(DayOfTheMonth(27)), Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset(-9,   0,   0), RuleId("US"), "Y%sT",  Some(Until(1980, Some(Month.OCTOBER),  Some(DayOfTheMonth(26)), Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset(-8,   0,   0), RuleId("US"), "P%sT",  Some(Until(1983, Some(Month.OCTOBER),  Some(DayOfTheMonth(30)), Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset(-9,   0,   0), RuleId("US"), "Y%sT",  Some(Until(1983, Some(Month.NOVEMBER), Some(DayOfTheMonth(30)), None))),
              ZoneTransition(GmtOffset(-9,   0,   0), RuleId("US"), "AK%sT", None)
            )),
            Zone("America/Sitka", List(
              ZoneTransition(GmtOffset(14, 58,  47), NullRule,     "LMT",   Some(Until(1867, Some(Month.OCTOBER),  Some(DayOfTheMonth(18)), None))),
              ZoneTransition(GmtOffset(-9, -1, -13), NullRule,     "LMT",   Some(Until(1900, Some(Month.AUGUST),   Some(DayOfTheMonth(20)), Some(AtWallTime(LocalTime.of(12, 0)))))),
              ZoneTransition(GmtOffset(-8,  0,   0), NullRule,     "PST",   Some(Until(1942, None,                 None,                    None))),
              ZoneTransition(GmtOffset(-8,  0,   0), RuleId("US"), "P%sT",  Some(Until(1946, None,                 None,                    None))),
              ZoneTransition(GmtOffset(-8,  0,   0), NullRule,     "PST",   Some(Until(1969, None,                 None,                    None))),
              ZoneTransition(GmtOffset(-8,  0,   0), RuleId("US"), "P%sT",  Some(Until(1983, Some(Month.OCTOBER),  Some(DayOfTheMonth(30)), Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset(-9,  0,   0), RuleId("US"), "Y%sT",  Some(Until(1983, Some(Month.NOVEMBER), Some(DayOfTheMonth(30)), None))),
              ZoneTransition(GmtOffset(-9,  0,   0), RuleId("US"), "AK%sT", None)
          )))
        )
      zones.foreach { zone =>
        (many(zoneParserNl) parseOnly zone._1) shouldBe Done("", zone._2)
      }
    }
    it should "parse Zones with comments" in {
      val zones = List(
        """Zone	Africa/Tripoli	0:52:44 -	LMT	1920
          |			1:00	Libya	CE%sT	1959
          |			2:00	-	EET	1982
          |			1:00	Libya	CE%sT	1990 May  4
          |# The 1996 and 1997 entries are from Shanks & Pottenger;
          |# the IATA SSIM data entries contain some obvious errors.
          |			2:00	-	EET	1996 Sep 30
          |			1:00	Libya	CE%sT	1997 Oct  4
          |			2:00	-	EET	2012 Nov 10  2:00
          |			1:00	Libya	CE%sT	2013 Oct 25  2:00
          |			2:00	-	EET""".stripMargin ->
          Zone("Africa/Tripoli", List(
              ZoneTransition(GmtOffset( 0, 52, 44), NullRule,        "LMT",   Some(Until(1920, None,                  None,                    None))),
              ZoneTransition(GmtOffset( 1,  0,  0), RuleId("Libya"), "CE%sT", Some(Until(1959, None,                  None,                    None))),
              ZoneTransition(GmtOffset( 2,  0,  0), NullRule,        "EET",   Some(Until(1982, None,                  None,                    None))),
              ZoneTransition(GmtOffset( 1,  0,  0), RuleId("Libya"), "CE%sT", Some(Until(1990, Some(Month.MAY),       Some(DayOfTheMonth(4)),  None))),
              ZoneTransition(GmtOffset( 2,  0,  0), NullRule,        "EET",   Some(Until(1996, Some(Month.SEPTEMBER), Some(DayOfTheMonth(30)), None))),
              ZoneTransition(GmtOffset( 1,  0,  0), RuleId("Libya"), "CE%sT", Some(Until(1997, Some(Month.OCTOBER),   Some(DayOfTheMonth(4)),  None))),
              ZoneTransition(GmtOffset( 2,  0,  0), NullRule,        "EET",   Some(Until(2012, Some(Month.NOVEMBER),  Some(DayOfTheMonth(10)), Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset( 1,  0,  0), RuleId("Libya"), "CE%sT", Some(Until(2013, Some(Month.OCTOBER),   Some(DayOfTheMonth(25)), Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset( 2,  0,  0), NullRule,        "EET",   None)
          )),
        """Zone	Europe/Belfast	-0:23:40 -	LMT	1880 Aug  2
          |			-0:25:21 -	DMT	1916 May 21  2:00
          |						# DMT = Dublin/Dunsink MT
          |			-0:25:21 1:00	IST	1916 Oct  1  2:00s
          |						# IST = Irish Summer Time
          |			 0:00	GB-Eire	%s	1968 Oct 27
          |			 1:00	-	BST	1971 Oct 31  2:00u
          |			 0:00	GB-Eire	%s	1996
          |			 0:00	EU	GMT/BST""".stripMargin ->
          Zone("Europe/Belfast", List(
              ZoneTransition(GmtOffset(0, -23, -40), NullRule,                        "LMT",     Some(Until(1880, Some(Month.AUGUST),  Some(DayOfTheMonth(2)),  None))),
              ZoneTransition(GmtOffset(0, -25, -21), NullRule,                        "DMT",     Some(Until(1916, Some(Month.MAY),     Some(DayOfTheMonth(21)), Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset(0, -25, -21), FixedOffset(GmtOffset(1, 0, 0)), "IST",     Some(Until(1916, Some(Month.OCTOBER), Some(DayOfTheMonth(1)),  Some(AtStandardTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset(0,   0,   0), RuleId("GB-Eire"),               "%s",      Some(Until(1968, Some(Month.OCTOBER), Some(DayOfTheMonth(27)), None))),
              ZoneTransition(GmtOffset(1,   0,   0), NullRule,                        "BST",     Some(Until(1971, Some(Month.OCTOBER), Some(DayOfTheMonth(31)), Some(AtUniversalTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset(0,   0,   0), RuleId("GB-Eire"),               "%s",      Some(Until(1996, None,                None,                    None))),
              ZoneTransition(GmtOffset(0,   0,   0), RuleId("EU"),                    "GMT/BST", None)
          )),
        """Zone	Africa/Windhoek	1:08:24 -	LMT	1892 Feb 8
          |			1:30	-	SWAT	1903 Mar    # SW Africa Time
          |			2:00	-	SAST	1942 Sep 20  2:00
          |			2:00	1:00	SAST	1943 Mar 21  2:00
          |			2:00	-	SAST	1990 Mar 21 # independence
          |			2:00	-	CAT	1994 Apr  3
          |			1:00	Namibia	WA%sT""".stripMargin ->
          Zone("Africa/Windhoek", List(
              ZoneTransition(GmtOffset(1,  8, 24), NullRule,                        "LMT",   Some(Until(1892, Some(Month.FEBRUARY),  Some(DayOfTheMonth(8)),  None))),
              ZoneTransition(GmtOffset(1, 30,  0), NullRule,                        "SWAT",  Some(Until(1903, Some(Month.MARCH), None, None))),
              ZoneTransition(GmtOffset(2,  0,  0), NullRule,                        "SAST",  Some(Until(1942, Some(Month.SEPTEMBER), Some(DayOfTheMonth(20)),  Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset(2,  0,  0), FixedOffset(GmtOffset(1, 0, 0)), "SAST",  Some(Until(1943, Some(Month.MARCH), Some(DayOfTheMonth(21)),  Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset(2,  0,  0), NullRule,                        "SAST",  Some(Until(1990, Some(Month.MARCH), Some(DayOfTheMonth(21)), None))),
              ZoneTransition(GmtOffset(2,  0,  0), NullRule,                        "CAT",   Some(Until(1994, Some(Month.APRIL), Some(DayOfTheMonth(3)), None))),
              ZoneTransition(GmtOffset(1,  0,  0), RuleId("Namibia"),               "WA%sT", None)
          ))
        )
      zones.foreach { zone =>
        (zoneParser parseOnly zone._1) shouldBe Done("", zone._2)
      }
    }
    it should "parse a complete file" in {
      val text = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/systemv"), "UTF-8").mkString
      val r = TZDBParser.parseFile(text)
      r should matchPattern {
        case Done("", _) =>
      }
    }
    it should "parse all relevant files" in {
      TZDBParser.tzdbFiles.foreach { f =>
        val text = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream(s"/$f"), "UTF-8").mkString
        val r = TZDBParser.parseFile(text)
        // Checks that it ingests the whole file
        r should matchPattern {
          case Done("", _) =>
        }
      }
    }
    it should "parse a whole dir" in {
      import better.files._

      val r = file"src/test/resources/"
      val rows = TZDBParser.parseAll(r).unsafeRunSync()
      // Check a few well-known items
      rows.flatMap(_.select[Link]) should contain (Link("America/Port_of_Spain", "America/Anguilla"))
      rows.flatMap(_.select[Rule]) should contain (Rule("Thule", GivenYear(1993), GivenYear(2006), Month.OCTOBER, LastWeekday(DayOfWeek.SUNDAY), AtWallTime(LocalTime.of(2, 0)), Save(LocalTime.of(0, 0)), Letter("S")))
      rows.flatMap(_.select[Zone]) should contain (Zone("Africa/Cairo", List(ZoneTransition(GmtOffset(2, 5, 9), NullRule, "LMT", Some(Until(1900, Some(Month.OCTOBER), None, None))), ZoneTransition(GmtOffset(2, 0, 0), RuleId("Egypt"), "EE%sT", None))))
      rows should not be empty
    }

}
