package kuyfi

import java.time.{DayOfWeek, LocalTime, Month}

import org.scalatest.{FlatSpec, Matchers}
import atto._
import atto.Atto._
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
    it should "parse single-line Zone" in {
      val zones = List(
        "Zone	EST		 -5:00	-	EST" ->
          Zone("EST", List(ZoneTransition(GmtOffset(-5, 0, 0), "-", "EST", None))),
        "Zone	Africa/Abidjan	-0:16:08 -	LMT	1912" ->
          Zone("Africa/Abidjan", List(ZoneTransition(GmtOffset(0, -16, 8), "-", "LMT", Some(Until(1912, None, None, None))))),
        "Zone	Africa/Bissau	-1:02:20 -	LMT	1912 Jan  1" ->
          Zone("Africa/Bissau", List(ZoneTransition(GmtOffset(-1, 2, 20), "-", "LMT", Some(Until(1912, Some(Month.JANUARY), Some(DayOfTheMonth(1)), None))))),
        "Zone	Africa/Nairobi	2:27:16	-	LMT	1928 Jul" ->
          Zone("Africa/Nairobi", List(ZoneTransition(GmtOffset(2, 27, 16), "-", "LMT", Some(Until(1928, Some(Month.JULY), None, None))))),
        "Zone Antarctica/DumontDUrville 0 -	-00	1947" ->
          Zone("Antarctica/DumontDUrville", List(ZoneTransition(GmtOffset(0, 0, 0), "-", "-00", Some(Until(1947, None, None, None))))),
        "Zone Pacific/Honolulu -10:31:26 -     LMT    1900 Jan  1 12:00" ->
          Zone("Pacific/Honolulu", List(ZoneTransition(GmtOffset(-10, 31, 26), "-", "LMT", Some(Until(1900, Some(Month.JANUARY), Some(DayOfTheMonth(1)), Some(AtWallTime(LocalTime.of(12, 0))))))))
      )
      zones.foreach { zone =>
        (zoneParser parseOnly zone._1) shouldBe Done("", zone._2)
      }
    }
    it should "parse multi-line Zone" in {
      val zones = List(
        """Zone Pacific/Honolulu	-10:31:26 -	LMT	1896 Jan 13 12:00
          |			-10:30	-	HST	1933 Apr 30  2:00
          |			-10:30	1:00	HDT	1933 May 21 12:00
          |			-10:30	-	HST	1942 Feb  9  2:00
          |			-10:30	1:00	HDT	1945 Sep 30  2:00
          |			-10:30	-	HST	1947 Jun  8  2:00
          |			-10:00	-	HST""".stripMargin ->
          Zone("Pacific/Honolulu", List(
            ZoneTransition(GmtOffset(-10, 31, 26), "-", "LMT", Some(Until(1896, Some(Month.JANUARY), Some(DayOfTheMonth(13)), Some(AtWallTime(LocalTime.of(12, 0)))))),
            ZoneTransition(GmtOffset(-10, 30, 0), "-", "HST", Some(Until(1933, Some(Month.APRIL), Some(DayOfTheMonth(30)), Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-10, 30, 0), "1:00", "HDT", Some(Until(1933, Some(Month.MAY), Some(DayOfTheMonth(21)), Some(AtWallTime(LocalTime.of(12, 0)))))),
            ZoneTransition(GmtOffset(-10, 30, 0), "-", "HST", Some(Until(1942, Some(Month.FEBRUARY), Some(DayOfTheMonth(9)), Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-10, 30, 0), "1:00", "HDT", Some(Until(1945, Some(Month.SEPTEMBER), Some(DayOfTheMonth(30)), Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-10, 30, 0), "-", "HST", Some(Until(1947, Some(Month.JUNE), Some(DayOfTheMonth(8)), Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-10, 0, 0), "-", "HST", None)
          )),
        """Zone America/Phoenix	-7:28:18 -	LMT	1883 Nov 18 11:31:42
          |			-7:00	US	M%sT	1944 Jan  1  0:01
          |			-7:00	-	MST	1944 Apr  1  0:01
          |			-7:00	US	M%sT	1944 Oct  1  0:01
          |			-7:00	-	MST	1967
          |			-7:00	US	M%sT	1968 Mar 21
          |			-7:00	-	MST""".stripMargin ->
          Zone("America/Phoenix", List(
            ZoneTransition(GmtOffset(-7, 28, 18), "-", "LMT", Some(Until(1883, Some(Month.NOVEMBER), Some(DayOfTheMonth(18)), Some(AtWallTime(LocalTime.of(11, 31, 42)))))),
            ZoneTransition(GmtOffset(-7, 0, 0), "US", "M%sT", Some(Until(1944, Some(Month.JANUARY), Some(DayOfTheMonth(1)), Some(AtWallTime(LocalTime.of(0, 1)))))),
            ZoneTransition(GmtOffset(-7, 0, 0), "-", "MST", Some(Until(1944, Some(Month.APRIL), Some(DayOfTheMonth(1)), Some(AtWallTime(LocalTime.of(0, 1)))))),
            ZoneTransition(GmtOffset(-7, 0, 0), "US", "M%sT", Some(Until(1944, Some(Month.OCTOBER), Some(DayOfTheMonth(1)), Some(AtWallTime(LocalTime.of(0, 1)))))),
            ZoneTransition(GmtOffset(-7, 0, 0), "-", "MST", Some(Until(1967, None, None, None))),
            ZoneTransition(GmtOffset(-7, 0, 0), "US", "M%sT", Some(Until(1968, Some(Month.MARCH), Some(DayOfTheMonth(21)), None))),
            ZoneTransition(GmtOffset(-7, 0, 0), "-", "MST", None)
          )),
        """Zone America/Indiana/Tell_City -5:47:03 - LMT	1883 Nov 18 12:12:57
          |			-6:00	US	C%sT	1946
          |			-6:00 Perry	C%sT	1964 Apr 26  2:00
          |			-5:00	-	EST	1969
          |			-5:00	US	E%sT	1971
          |			-5:00	-	EST	2006 Apr  2  2:00
          |			-6:00	US	C%sT""".stripMargin ->
          Zone("America/Indiana/Tell_City", List(
            ZoneTransition(GmtOffset(-5, 47, 3), "-",    "LMT",  Some(Until(1883, Some(Month.NOVEMBER), Some(DayOfTheMonth(18)), Some(AtWallTime(LocalTime.of(12, 12, 57)))))),
            ZoneTransition(GmtOffset(-6, 0, 0), "US",    "C%sT", Some(Until(1946, None, None, None))),
            ZoneTransition(GmtOffset(-6, 0, 0), "Perry", "C%sT", Some(Until(1964, Some(Month.APRIL), Some(DayOfTheMonth(26)), Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-5, 0, 0), "-",     "EST",  Some(Until(1969, None, None, None))),
            ZoneTransition(GmtOffset(-5, 0, 0), "US",    "E%sT", Some(Until(1971, None, None, None))),
            ZoneTransition(GmtOffset(-5, 0, 0), "-",     "EST",  Some(Until(2006, Some(Month.APRIL), Some(DayOfTheMonth(2)), Some(AtWallTime(LocalTime.of(2, 0)))))),
            ZoneTransition(GmtOffset(-6, 0, 0), "US",    "C%sT", None)
          ))
      )
      zones.foreach { zone =>
        (zoneParser parseOnly zone._1) shouldBe Done("", zone._2)
      }
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
              ZoneTransition(GmtOffset( 15,  2, 19), "-",    "LMT", Some(Until(1867, Some(Month.OCTOBER),   Some(DayOfTheMonth(18)), None))),
              ZoneTransition(GmtOffset( -8, 57, 41), "-",    "LMT", Some(Until(1900, Some(Month.AUGUST),    Some(DayOfTheMonth(20)), Some(AtWallTime(LocalTime.of(12, 0)))))),
              ZoneTransition(GmtOffset( -8,  0,  0), "-",    "PST", Some(Until(1942, None, None, None))),
              ZoneTransition(GmtOffset( -8,  0,  0), "US",   "P%sT", Some(Until(1946, None, None, None))),
              ZoneTransition(GmtOffset( -8,  0,  0), "-",    "PST", Some(Until(1969, None, None, None))),
              ZoneTransition(GmtOffset( -8,  0,  0), "US",   "P%sT", Some(Until(1980, Some(Month.APRIL),    Some(DayOfTheMonth(27)), Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset( -9,  0,  0), "US",   "Y%sT", Some(Until(1980, Some(Month.OCTOBER),    Some(DayOfTheMonth(26)), Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset( -8,  0,  0), "US",   "P%sT", Some(Until(1983, Some(Month.OCTOBER),    Some(DayOfTheMonth(30)), Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset( -9,  0,  0), "US",   "Y%sT", Some(Until(1983, Some(Month.NOVEMBER),    Some(DayOfTheMonth(30)), None))),
              ZoneTransition(GmtOffset( -9,  0,  0), "US",   "AK%sT", None)
            )),
            Zone("America/Sitka", List(
              ZoneTransition(GmtOffset( 14, 58, 47), "-",    "LMT", Some(Until(1867, Some(Month.OCTOBER),   Some(DayOfTheMonth(18)), None))),
              ZoneTransition(GmtOffset( -9,  1, 13), "-",    "LMT", Some(Until(1900, Some(Month.AUGUST),    Some(DayOfTheMonth(20)), Some(AtWallTime(LocalTime.of(12, 0)))))),
              ZoneTransition(GmtOffset( -8,  0,  0), "-",    "PST", Some(Until(1942, None, None, None))),
              ZoneTransition(GmtOffset( -8,  0,  0), "US",   "P%sT", Some(Until(1946, None, None, None))),
              ZoneTransition(GmtOffset( -8,  0,  0), "-",    "PST", Some(Until(1969, None, None, None))),
              ZoneTransition(GmtOffset( -8,  0,  0), "US",   "P%sT", Some(Until(1983, Some(Month.OCTOBER),    Some(DayOfTheMonth(30)), Some(AtWallTime(LocalTime.of(2, 0)))))),
              ZoneTransition(GmtOffset( -9,  0,  0), "US",   "Y%sT", Some(Until(1983, Some(Month.NOVEMBER),    Some(DayOfTheMonth(30)), None))),
              ZoneTransition(GmtOffset( -9,  0,  0), "US",   "AK%sT", None)
          )))
        )
      zones.foreach { zone =>
        (many(zoneParser <~ char('\n')) parseOnly zone._1) shouldBe Done("", zone._2)
      }
    }
    it should "parse a complete file" in {
      val text = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/northamerica"), "UTF-8").mkString
      println(TZDBParser.parseFile(text))
      //println(text)
    }
}
