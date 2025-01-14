package kuyfi

import java.time.{ DayOfWeek, LocalTime, Month }

import atto._
import Atto._
import atto.ParseResult.{ Done, Fail }
import cats.effect.unsafe.implicits._
import java.io.File
import TZDB._
import TZDBParser._

class TZDBParserSuite extends munit.FunSuite {
  test("parse comment") {
    assertEquals(commentParser.parseOnly("# tzdb data for Africa and environs"),
                 Done("", Comment(" tzdb data for Africa and environs"))
    )
  }
  test("parse whitespace-prefixed comment") {
    assertEquals(commentParser.parseOnly("		#STDOFF	2:05:08.9"),
                 Done("", Comment("STDOFF	2:05:08.9"))
    )
  }

  test("parse from maximum") {
    assertEquals(fromParser.parseOnly("maximum"), Done("", Maximum))
  }
  test("parse from minimum") {
    assertEquals(fromParser.parseOnly("minimum"), Done("", Minimum))
  }
  test("parse with a date") {
    assertEquals(fromParser.parseOnly("1974"), Done("", GivenYear(1974)))
  }

  test("parse to maximum") {
    assertEquals(toParser.parseOnly("maximum"), Done("", Maximum))
  }
  test("parse to minimum") {
    assertEquals(toParser.parseOnly("minimum"), Done("", Minimum))
  }
  test("parse to only") {
    assertEquals(toParser.parseOnly("only"), Done("", Only))
  }
  test("parse with a date") {
    assertEquals(toParser.parseOnly("1974"), Done("", GivenYear(1974)))
  }

  test("parse in June") {
    assertEquals(monthParser.parseOnly("Jun"), Done("", Month.JUNE))
  }
  test("parse in December") {
    assertEquals(monthParser.parseOnly("Dec"), Done("", Month.DECEMBER))
  }
  test("parse non valid") {
    assertEquals(monthParser.parseOnly("abc"), Fail("abc", Nil, "unknown month"))
  }

  test("parse Mon") {
    assertEquals(dayParser.parseOnly("Mon"), Done("", DayOfWeek.MONDAY))
  }
  test("parse in Sun") {
    assertEquals(dayParser.parseOnly("Sun"), Done("", DayOfWeek.SUNDAY))
  }
  test("parse non valid") {
    assertEquals(dayParser.parseOnly("abc"), Fail("abc", Nil, "unknown day"))
  }

  test("sun after the eighth") {
    assertEquals(afterWeekdayParser.parseOnly("Sun>=8"),
                 Done("", AfterWeekday(DayOfWeek.SUNDAY, 8))
    )
  }

  test("sun before the 25th") {
    assertEquals(beforeWeekdayParser.parseOnly("Sun<=25"),
                 Done("", BeforeWeekday(DayOfWeek.SUNDAY, 25))
    )
  }

  test("last sunday") {
    assertEquals(lastWeekdayParser.parseOnly("lastSun"), Done("", LastWeekday(DayOfWeek.SUNDAY)))
  }
  test("last monday") {
    assertEquals(lastWeekdayParser.parseOnly("lastMon"), Done("", LastWeekday(DayOfWeek.MONDAY)))
  }

  test("fixed day") {
    assertEquals(onParser.parseOnly("24"), Done("", DayOfTheMonth(24)))
  }
  test("calculate dayOfMontIndicator for fixed day") {
    assertEquals(onParser.parseOnly("24").map(_.dayOfMonthIndicator), Done("", Some(24)))
  }
  test("calculate dayOfMontIndicator for lastSun") {
    assertEquals(onParser.parseOnly("lastSun").map(_.dayOfMonthIndicator), Done("", None))
  }
  test("calculate dayOfMontIndicator for Sat>=5") {
    assertEquals(onParser.parseOnly("Sat>=5").map(_.dayOfMonthIndicator), Done("", Some(5)))
  }
  test("calculate dayOfMontIndicator for Sat<=5") {
    assertEquals(onParser.parseOnly("Sat<=5").map(_.dayOfMonthIndicator), Done("", Some(5)))
  }

  test("parse single number time") {
    assertEquals(atParser.parseOnly("2"), Done("", AtWallTime(LocalTime.of(2, 0))))
  }
  test("parse wall time two number time") {
    assertEquals(atParser.parseOnly("16"), Done("", AtWallTime(LocalTime.of(16, 0))))
  }
  test("parse wall time h:m time") {
    assertEquals(atParser.parseOnly("2:00"), Done("", AtWallTime(LocalTime.of(2, 0))))
  }
  test("parse wall time h:m 24h time") {
    assertEquals(atParser.parseOnly("15:00"), Done("", AtWallTime(LocalTime.of(15, 0))))
  }
  test("parse wall time h:m:s time") {
    assertEquals(atParser.parseOnly("1:28:14"), Done("", AtWallTime(LocalTime.of(1, 28, 14))))
  }
  test("parse explicit wall time two number time") {
    assertEquals(atParser.parseOnly("16w"), Done("", AtWallTime(LocalTime.of(16, 0))))
  }
  test("parse explicit wall time h:m time") {
    assertEquals(atParser.parseOnly("2:00w"), Done("", AtWallTime(LocalTime.of(2, 0))))
  }
  test("parse explicit wall time h:m 24h time") {
    assertEquals(atParser.parseOnly("15:00w"), Done("", AtWallTime(LocalTime.of(15, 0))))
  }
  test("parse explicit wall time h:m:s time") {
    assertEquals(atParser.parseOnly("1:28:14w"), Done("", AtWallTime(LocalTime.of(1, 28, 14))))
  }
  test("parse explicit standard time two number time") {
    assertEquals(atParser.parseOnly("16s"), Done("", AtStandardTime(LocalTime.of(16, 0))))
  }
  test("parse explicit standard time h:m time") {
    assertEquals(atParser.parseOnly("2:00s"), Done("", AtStandardTime(LocalTime.of(2, 0))))
  }
  test("parse explicit standard time h:m 24h time") {
    assertEquals(atParser.parseOnly("15:00s"), Done("", AtStandardTime(LocalTime.of(15, 0))))
  }
  test("parse explicit standard time h:m:s time") {
    assertEquals(atParser.parseOnly("1:28:14s"), Done("", AtStandardTime(LocalTime.of(1, 28, 14))))
  }
  test("parse explicit universal time two number time") {
    assertEquals(atParser.parseOnly("16u"), Done("", AtUniversalTime(LocalTime.of(16, 0))))
  }
  test("parse explicit universal time h:m time") {
    assertEquals(atParser.parseOnly("2:00u"), Done("", AtUniversalTime(LocalTime.of(2, 0))))
  }
  test("parse explicit universal time h:m 24h time") {
    assertEquals(atParser.parseOnly("15:00u"), Done("", AtUniversalTime(LocalTime.of(15, 0))))
  }
  test("parse explicit gmt time h:m 24h time") {
    assertEquals(atParser.parseOnly("15:00g"), Done("", AtUniversalTime(LocalTime.of(15, 0))))
  }
  test("parse explicit z time h:m 24h time") {
    assertEquals(atParser.parseOnly("15:00z"), Done("", AtUniversalTime(LocalTime.of(15, 0))))
  }
  test("parse explicit universal time h:m:s time") {
    assertEquals(atParser.parseOnly("1:28:14u"), Done("", AtUniversalTime(LocalTime.of(1, 28, 14))))
  }
  test("calculate if at the end of day") {
    assertEquals(atParser.parseOnly("2:20").map(_.endOfDay), Done("", false))
    assertEquals(atParser.parseOnly("2:20s").map(_.endOfDay), Done("", false))
    assertEquals(atParser.parseOnly("2:20u").map(_.endOfDay), Done("", false))
    assertEquals(atParser.parseOnly("24:00g").map(_.endOfDay), Done("", true))
    // (atParser parseOnly "-").map(_.endOfDay) , Done("", false)
    assertEquals(atParser.parseOnly("3z").map(_.endOfDay), Done("", false))
    assertEquals(atParser.parseOnly("0:00").map(_.endOfDay), Done("", false))
  }

  test("parse Rules") {
    val rules = List(
      "Rule Japan 1948 1951 - Sep Sat>=8 25:00 0 S"          ->
        Rule(
          "Japan",
          GivenYear(1948),
          GivenYear(1951),
          Month.SEPTEMBER,
          AfterWeekday(DayOfWeek.SATURDAY, 9),
          AtWallTime(LocalTime.of(1, 0), true, 1),
          Save(true, LocalTime.of(0, 0)),
          Letter("S")
        ),
      "Rule	Algeria	1916	only	-	Jun	14	23:00s	1:00	S"        ->
        Rule("Algeria",
             GivenYear(1916),
             Only,
             Month.JUNE,
             DayOfTheMonth(14),
             AtStandardTime(LocalTime.of(23, 0)),
             Save(true, LocalTime.of(1, 0)),
             Letter("S")
        ),
      "Rule	Egypt	1995	2010	-	Apr	lastFri	 0:00s	1:00	S"     ->
        Rule(
          "Egypt",
          GivenYear(1995),
          GivenYear(2010),
          Month.APRIL,
          LastWeekday(DayOfWeek.FRIDAY),
          AtStandardTime(LocalTime.of(0, 0)),
          Save(true, LocalTime.of(1, 0)),
          Letter("S")
        ),
      "Rule	Egypt	2007	only	-	Sep	Thu>=1	24:00	0	-"          ->
        Rule(
          "Egypt",
          GivenYear(2007),
          Only,
          Month.SEPTEMBER,
          AfterWeekday(DayOfWeek.THURSDAY, 1),
          AtWallTime(LocalTime.of(0, 0), true, 0),
          Save(true, LocalTime.of(0, 0)),
          Letter("-")
        ),
      "Rule	Ghana	1920	1942	-	Sep	 1	0:00	0:20	GHST"         ->
        Rule("Ghana",
             GivenYear(1920),
             GivenYear(1942),
             Month.SEPTEMBER,
             DayOfTheMonth(1),
             AtWallTime(LocalTime.of(0, 0)),
             Save(true, LocalTime.of(0, 20)),
             Letter("GHST")
        ),
      "Rule RussiaAsia	1981	1984	-	Apr	1	 0:00	1:00	S"       ->
        Rule("RussiaAsia",
             GivenYear(1981),
             GivenYear(1984),
             Month.APRIL,
             DayOfTheMonth(1),
             AtWallTime(LocalTime.of(0, 0)),
             Save(true, LocalTime.of(1, 0)),
             Letter("S")
        ),
      "Rule	Lebanon	1993	max	-	Mar	lastSun	0:00	1:00	S"      ->
        Rule("Lebanon",
             GivenYear(1993),
             Maximum,
             Month.MARCH,
             LastWeekday(DayOfWeek.SUNDAY),
             AtWallTime(LocalTime.of(0, 0)),
             Save(true, LocalTime.of(1, 0)),
             Letter("S")
        ),
      "Rule	Syria	1991	only	-	Apr	 1	0:00	1:00	S"            ->
        Rule("Syria",
             GivenYear(1991),
             Only,
             Month.APRIL,
             DayOfTheMonth(1),
             AtWallTime(LocalTime.of(0, 0)),
             Save(true, LocalTime.of(1, 0)),
             Letter("S")
        ),
      "Rule	Regina	1945	only	-	Aug	14	23:00u	1:00	P # Peace" ->
        Rule("Regina",
             GivenYear(1945),
             Only,
             Month.AUGUST,
             DayOfTheMonth(14),
             AtUniversalTime(LocalTime.of(23, 0)),
             Save(true, LocalTime.of(1, 0)),
             Letter("P")
        ),
      "Rule Indianapolis 1941	only	-	Jun	22	2:00	1:00	D"     ->
        Rule("Indianapolis",
             GivenYear(1941),
             Only,
             Month.JUNE,
             DayOfTheMonth(22),
             AtWallTime(LocalTime.of(2, 0)),
             Save(true, LocalTime.of(1, 0)),
             Letter("D")
        ),
      "Rule	Syria	2007	only	-	Nov	 Fri>=1	0:00	0	-"          ->
        Rule("Syria",
             GivenYear(2007),
             Only,
             Month.NOVEMBER,
             AfterWeekday(DayOfWeek.FRIDAY, 1),
             AtWallTime(LocalTime.of(0, 0)),
             Save(true, LocalTime.of(0, 0)),
             Letter("-")
        ),
      "Rule	Morocco	2011	only	-	Jul	31	 0	0	-"               ->
        Rule("Morocco",
             GivenYear(2011),
             Only,
             Month.JULY,
             DayOfTheMonth(31),
             AtWallTime(LocalTime.of(0, 0)),
             Save(true, LocalTime.of(0, 0)),
             Letter("-")
        ),
      "Rule	SystemV	1974	only	-	Jan	6	2:00	1:00	D"           ->
        Rule("SystemV",
             GivenYear(1974),
             Only,
             Month.JANUARY,
             DayOfTheMonth(6),
             AtWallTime(LocalTime.of(2, 0)),
             Save(true, LocalTime.of(1, 0)),
             Letter("D")
        ),
      "Rule	Eire	1996	max	-	Oct	lastSun	 1:00u	-1:00	GMT"    ->
        Rule(
          "Eire",
          GivenYear(1996),
          Maximum,
          Month.OCTOBER,
          LastWeekday(DayOfWeek.SUNDAY),
          AtUniversalTime(LocalTime.of(1, 0)),
          Save(false, LocalTime.of(1, 0)),
          Letter("GMT")
        )
    )
    rules.foreach(rule => assertEquals(ruleParser.parseOnly(rule._1), Done("", rule._2)))
  }
  test("parse Link") {
    val links = List(
      "Link America/Curacao America/Aruba"                                   ->
        Link("America/Curacao", "America/Aruba"),
      "Link	America/Curacao	America/Kralendijk	# Caribbean Netherlands"      ->
        Link("America/Curacao", "America/Kralendijk"),
      "Link America/Port_of_Spain America/Marigot	# St Martin (French part)" ->
        Link("America/Port_of_Spain", "America/Marigot"),
      "Link	America/Los_Angeles	US/Pacific-New	##"                           ->
        Link("America/Los_Angeles", "US/Pacific-New"),
      "Link	Etc/GMT				Etc/GMT+0"                                            ->
        Link("Etc/GMT", "Etc/GMT+0"),
      "Link Africa/Abidjan Africa/Sao_Tome	# São Tomé and Príncipe"          ->
        Link("Africa/Abidjan", "Africa/Sao_Tome")
    )
    links.foreach(link => assertEquals(linkParser.parseOnly(link._1), Done("", link._2)))
  }
  test("parse single-line Zone") {
    val zones = List(
      "Zone	EST		 -5:00	-	EST"                                         ->
        Zone("EST", List(ZoneTransition(GmtOffset(-5, 0, 0), NullRule, "EST", None))),
      "Zone	Africa/Abidjan	-0:16:08 -	LMT	1912"                        ->
        Zone("Africa/Abidjan",
             List(
               ZoneTransition(GmtOffset(0, -16, -8),
                              NullRule,
                              "LMT",
                              Some(Until(1912, None, None, None))
               )
             )
        ),
      "Zone	Africa/Bissau	-1:02:20 -	LMT	1912 Jan  1"                  ->
        Zone("Africa/Bissau",
             List(
               ZoneTransition(GmtOffset(-1, -2, -20),
                              NullRule,
                              "LMT",
                              Some(Until(1912, Some(Month.JANUARY), Some(DayOfTheMonth(1)), None))
               )
             )
        ),
      "Zone	Africa/Nairobi	2:27:16	-	LMT	1928 Jul"                     ->
        Zone("Africa/Nairobi",
             List(
               ZoneTransition(GmtOffset(2, 27, 16),
                              NullRule,
                              "LMT",
                              Some(Until(1928, Some(Month.JULY), None, None))
               )
             )
        ),
      "Zone Antarctica/DumontDUrville 0 -	-00	1947"                    ->
        Zone(
          "Antarctica/DumontDUrville",
          List(
            ZoneTransition(GmtOffset(0, 0, 0), NullRule, "-00", Some(Until(1947, None, None, None)))
          )
        ),
      "Zone Pacific/Honolulu -10:31:26 -     LMT    1900 Jan  1 12:00" ->
        Zone(
          "Pacific/Honolulu",
          List(
            ZoneTransition(GmtOffset(-10, -31, -26),
                           NullRule,
                           "LMT",
                           Some(
                             Until(1900,
                                   Some(Month.JANUARY),
                                   Some(DayOfTheMonth(1)),
                                   Some(AtWallTime(LocalTime.of(12, 0)))
                             )
                           )
            )
          )
        ),
      "Zone America/Juneau -8:57:41 -	LMT	1900 Aug 20 12:00"           ->
        Zone(
          "America/Juneau",
          List(
            ZoneTransition(GmtOffset(-8, -57, -41),
                           NullRule,
                           "LMT",
                           Some(
                             Until(1900,
                                   Some(Month.AUGUST),
                                   Some(DayOfTheMonth(20)),
                                   Some(AtWallTime(LocalTime.of(12, 0)))
                             )
                           )
            )
          )
        )
    )
    zones.foreach(zone => assertEquals(zoneParser.parseOnly(zone._1), Done("", zone._2)))
  }
  test("parse a multi-line Zone") {
    val zones     = List(
      """Zone Pacific/Honolulu	-10:31:26 -	LMT	1896 Jan 13 12:00
          |			-10:30	-	HST	1933 Apr 30  2:00
          |			-10:30	1:00	HDT	1933 May 21 12:00
          |			-10:30	-	HST	1942 Feb  9  2:00
          |			-10:30	1:00	HDT	1945 Sep 30  2:00
          |			-10:30	-	HST	1947 Jun  8  2:00
          |			-10:00	-	HST""".stripMargin        ->
        Zone(
          "Pacific/Honolulu",
          List(
            ZoneTransition(GmtOffset(-10, -31, -26),
                           NullRule,
                           "LMT",
                           Some(
                             Until(1896,
                                   Some(Month.JANUARY),
                                   Some(DayOfTheMonth(13)),
                                   Some(AtWallTime(LocalTime.of(12, 0)))
                             )
                           )
            ),
            ZoneTransition(GmtOffset(-10, -30, 0),
                           NullRule,
                           "HST",
                           Some(
                             Until(1933,
                                   Some(Month.APRIL),
                                   Some(DayOfTheMonth(30)),
                                   Some(AtWallTime(LocalTime.of(2, 0)))
                             )
                           )
            ),
            ZoneTransition(
              GmtOffset(-10, -30, 0),
              FixedOffset(GmtOffset(1, 0, 0)),
              "HDT",
              Some(
                Until(1933,
                      Some(Month.MAY),
                      Some(DayOfTheMonth(21)),
                      Some(AtWallTime(LocalTime.of(12, 0)))
                )
              )
            ),
            ZoneTransition(GmtOffset(-10, -30, 0),
                           NullRule,
                           "HST",
                           Some(
                             Until(1942,
                                   Some(Month.FEBRUARY),
                                   Some(DayOfTheMonth(9)),
                                   Some(AtWallTime(LocalTime.of(2, 0)))
                             )
                           )
            ),
            ZoneTransition(
              GmtOffset(-10, -30, 0),
              FixedOffset(GmtOffset(1, 0, 0)),
              "HDT",
              Some(
                Until(1945,
                      Some(Month.SEPTEMBER),
                      Some(DayOfTheMonth(30)),
                      Some(AtWallTime(LocalTime.of(2, 0)))
                )
              )
            ),
            ZoneTransition(GmtOffset(-10, -30, 0),
                           NullRule,
                           "HST",
                           Some(
                             Until(1947,
                                   Some(Month.JUNE),
                                   Some(DayOfTheMonth(8)),
                                   Some(AtWallTime(LocalTime.of(2, 0)))
                             )
                           )
            ),
            ZoneTransition(GmtOffset(-10, 0, 0), NullRule, "HST", None)
          )
        ),
      """Zone America/Phoenix	-7:28:18 -	LMT	1883 Nov 18 11:31:42
          |			-7:00	US	M%sT	1944 Jan  1  0:01
          |			-7:00	-	MST	1944 Apr  1  0:01
          |			-7:00	US	M%sT	1944 Oct  1  0:01
          |			-7:00	-	MST	1967
          |			-7:00	US	M%sT	1968 Mar 21
          |			-7:00	-	MST""".stripMargin         ->
        Zone(
          "America/Phoenix",
          List(
            ZoneTransition(GmtOffset(-7, -28, -18),
                           NullRule,
                           "LMT",
                           Some(
                             Until(1883,
                                   Some(Month.NOVEMBER),
                                   Some(DayOfTheMonth(18)),
                                   Some(AtWallTime(LocalTime.of(11, 31, 42)))
                             )
                           )
            ),
            ZoneTransition(GmtOffset(-7, 0, 0),
                           RuleId("US"),
                           "M%sT",
                           Some(
                             Until(1944,
                                   Some(Month.JANUARY),
                                   Some(DayOfTheMonth(1)),
                                   Some(AtWallTime(LocalTime.of(0, 1)))
                             )
                           )
            ),
            ZoneTransition(GmtOffset(-7, 0, 0),
                           NullRule,
                           "MST",
                           Some(
                             Until(1944,
                                   Some(Month.APRIL),
                                   Some(DayOfTheMonth(1)),
                                   Some(AtWallTime(LocalTime.of(0, 1)))
                             )
                           )
            ),
            ZoneTransition(GmtOffset(-7, 0, 0),
                           RuleId("US"),
                           "M%sT",
                           Some(
                             Until(1944,
                                   Some(Month.OCTOBER),
                                   Some(DayOfTheMonth(1)),
                                   Some(AtWallTime(LocalTime.of(0, 1)))
                             )
                           )
            ),
            ZoneTransition(GmtOffset(-7, 0, 0),
                           NullRule,
                           "MST",
                           Some(Until(1967, None, None, None))
            ),
            ZoneTransition(GmtOffset(-7, 0, 0),
                           RuleId("US"),
                           "M%sT",
                           Some(Until(1968, Some(Month.MARCH), Some(DayOfTheMonth(21)), None))
            ),
            ZoneTransition(GmtOffset(-7, 0, 0), NullRule, "MST", None)
          )
        ),
      """Zone America/Indiana/Tell_City -5:47:03 - LMT	1883 Nov 18 12:12:57
          |			-6:00	US	C%sT	1946
          |			-6:00 Perry	C%sT	1964 Apr 26  2:00
          |			-5:00	-	EST	1969
          |			-5:00	US	E%sT	1971
          |			-5:00	-	EST	2006 Apr  2  2:00
          |			-6:00	US	C%sT""".stripMargin       ->
        Zone(
          "America/Indiana/Tell_City",
          List(
            ZoneTransition(GmtOffset(-5, -47, -3),
                           NullRule,
                           "LMT",
                           Some(
                             Until(1883,
                                   Some(Month.NOVEMBER),
                                   Some(DayOfTheMonth(18)),
                                   Some(AtWallTime(LocalTime.of(12, 12, 57)))
                             )
                           )
            ),
            ZoneTransition(GmtOffset(-6, 0, 0),
                           RuleId("US"),
                           "C%sT",
                           Some(Until(1946, None, None, None))
            ),
            ZoneTransition(GmtOffset(-6, 0, 0),
                           RuleId("Perry"),
                           "C%sT",
                           Some(
                             Until(1964,
                                   Some(Month.APRIL),
                                   Some(DayOfTheMonth(26)),
                                   Some(AtWallTime(LocalTime.of(2, 0)))
                             )
                           )
            ),
            ZoneTransition(GmtOffset(-5, 0, 0),
                           NullRule,
                           "EST",
                           Some(Until(1969, None, None, None))
            ),
            ZoneTransition(GmtOffset(-5, 0, 0),
                           RuleId("US"),
                           "E%sT",
                           Some(Until(1971, None, None, None))
            ),
            ZoneTransition(GmtOffset(-5, 0, 0),
                           NullRule,
                           "EST",
                           Some(
                             Until(2006,
                                   Some(Month.APRIL),
                                   Some(DayOfTheMonth(2)),
                                   Some(AtWallTime(LocalTime.of(2, 0)))
                             )
                           )
            ),
            ZoneTransition(GmtOffset(-6, 0, 0), RuleId("US"), "C%sT", None)
          )
        ),
      """Zone America/Guadeloupe	-4:06:08 -	LMT	1911 Jun  8 # Pointe-à-Pitre
          |			-4:00	 -	AST
          |""".stripMargin                       ->
        Zone(
          "America/Guadeloupe",
          List(
            ZoneTransition(GmtOffset(-4, -6, -8),
                           NullRule,
                           "LMT",
                           Some(Until(1911, Some(Month.JUNE), Some(DayOfTheMonth(8)), None))
            ),
            ZoneTransition(GmtOffset(-4, 0, 0), NullRule, "AST", None)
          )
        ),
      """Zone America/Juneau	 15:02:19 -	LMT	1867 Oct 18
          |			 -8:57:41 -	LMT	1900 Aug 20 12:00
          |""".stripMargin                       ->
        Zone(
          "America/Juneau",
          List(
            ZoneTransition(GmtOffset(15, 2, 19),
                           NullRule,
                           "LMT",
                           Some(Until(1867, Some(Month.OCTOBER), Some(DayOfTheMonth(18)), None))
            ),
            ZoneTransition(GmtOffset(-8, -57, -41),
                           NullRule,
                           "LMT",
                           Some(
                             Until(1900,
                                   Some(Month.AUGUST),
                                   Some(DayOfTheMonth(20)),
                                   Some(AtWallTime(LocalTime.of(12, 0)))
                             )
                           )
            )
          )
        ),
      """Zone	America/Caracas	-4:27:44 -	LMT	1890
          |			-4:27:40 -	CMT	1912 Feb 12 # Caracas Mean Time?
          |			-4:30	-	VET	1965 Jan  1  0:00 # Venezuela T.
          |""".stripMargin                       ->
        Zone(
          "America/Caracas",
          List(
            ZoneTransition(GmtOffset(-4, -27, -44),
                           NullRule,
                           "LMT",
                           Some(Until(1890, None, None, None))
            ),
            ZoneTransition(GmtOffset(-4, -27, -40),
                           NullRule,
                           "CMT",
                           Some(Until(1912, Some(Month.FEBRUARY), Some(DayOfTheMonth(12)), None))
            ),
            ZoneTransition(GmtOffset(-4, -30, 0),
                           NullRule,
                           "VET",
                           Some(
                             Until(1965,
                                   Some(Month.JANUARY),
                                   Some(DayOfTheMonth(1)),
                                   Some(AtWallTime(LocalTime.of(0, 0)))
                             )
                           )
            )
          )
        ),
      """Zone	Asia/Kathmandu	5:41:16 -	LMT	1920
          |			5:30	-	IST	1986
          |			5:45	-	NPT	# Nepal Time
          |""".stripMargin                       ->
        Zone(
          "Asia/Kathmandu",
          List(
            ZoneTransition(GmtOffset(5, 41, 16),
                           NullRule,
                           "LMT",
                           Some(Until(1920, None, None, None))
            ),
            ZoneTransition(GmtOffset(5, 30, 0),
                           NullRule,
                           "IST",
                           Some(Until(1986, None, None, None))
            ),
            ZoneTransition(GmtOffset(5, 45, 0), NullRule, "NPT", None)
          )
        ),
      """Zone Africa/Casablanca	-0:30:20 -	LMT	1913 Oct 26
          |			 0:00	Morocco	WE%sT	1984 Mar 16
          |			 1:00	-	CET	1986
          |			 0:00	Morocco	WE%sT""".stripMargin ->
        Zone(
          "Africa/Casablanca",
          List(
            ZoneTransition(GmtOffset(-0, -30, -20),
                           NullRule,
                           "LMT",
                           Some(Until(1913, Some(Month.OCTOBER), Some(DayOfTheMonth(26)), None))
            ),
            ZoneTransition(GmtOffset(0, 0, 0),
                           RuleId("Morocco"),
                           "WE%sT",
                           Some(Until(1984, Some(Month.MARCH), Some(DayOfTheMonth(16)), None))
            ),
            ZoneTransition(GmtOffset(1, 0, 0),
                           NullRule,
                           "CET",
                           Some(Until(1986, None, None, None))
            ),
            ZoneTransition(GmtOffset(0, 0, 0), RuleId("Morocco"), "WE%sT", None)
          )
        ),
      """Zone	Europe/Prague	0:57:44 -	LMT	1850
          |			0:57:44	-	PMT	1891 Oct    # Prague Mean Time
          |			1:00	C-Eur	CE%sT	1945 May  9
          |			1:00	Czech	CE%sT	1946 Dec  1  3:00
          |# Vanguard section, for zic and other parsers that support negative DST.
          |			1:00	-1:00	GMT	1947 Feb 23  2:00
          |""".stripMargin                       ->
        Zone(
          "Europe/Prague",
          List(
            ZoneTransition(GmtOffset(0, 57, 44),
                           NullRule,
                           "LMT",
                           Some(Until(1850, None, None, None))
            ),
            ZoneTransition(GmtOffset(0, 57, 44),
                           NullRule,
                           "PMT",
                           Some(Until(1891, Some(Month.OCTOBER), None, None))
            ),
            ZoneTransition(GmtOffset(1, 0, 0),
                           RuleId("C-Eur"),
                           "CE%sT",
                           Some(Until(1945, Some(Month.MAY), Some(DayOfTheMonth(9)), None))
            ),
            ZoneTransition(GmtOffset(1, 0, 0),
                           RuleId("Czech"),
                           "CE%sT",
                           Some(
                             Until(1946,
                                   Some(Month.DECEMBER),
                                   Some(DayOfTheMonth(1)),
                                   Some(AtWallTime(LocalTime.of(3, 0)))
                             )
                           )
            ),
            ZoneTransition(
              GmtOffset(1, 0, 0),
              FixedOffset(GmtOffset(-1, 0, 0)),
              "GMT",
              Some(
                Until(1947,
                      Some(Month.FEBRUARY),
                      Some(DayOfTheMonth(23)),
                      Some(AtWallTime(LocalTime.of(2, 0)))
                )
              )
            )
          )
        ),
      """Zone America/Guadeloupe	-4:06:08 -	LMT	1911 Jun  8 # Pointe-à-Pitre
        |			-4:00	 -	AST
        |""".stripMargin                         ->
        Zone(
          "America/Guadeloupe",
          List(
            ZoneTransition(GmtOffset(-4, -6, -8),
                           NullRule,
                           "LMT",
                           Some(Until(1911, Some(Month.JUNE), Some(DayOfTheMonth(8)), None))
            ),
            ZoneTransition(GmtOffset(-4, 0, 0), NullRule, "AST", None)
          )
        )
    )
    zones.foreach(zone => assertEquals(zoneParserNl.parseOnly(zone._1), Done("", zone._2)))
    // Tests some special cases
    // The first zone has an unknown rule
    val firstRule = zones.headOption.flatMap(z => z._2.transitions.headOption.map(_.ruleId))
    assertEquals(firstRule, Some(NullRule))

    // The second zone has an id
    val secondRule = zones.lift(1).flatMap(_._2.transitions.lift(1).map(_.ruleId))
    assertEquals(secondRule, Some(RuleId("US")))

    // The third zone has a fixed offset
    val thirdRule = zones.headOption.flatMap(_._2.transitions.lift(2).map(_.ruleId))
    assertEquals(thirdRule, Some(FixedOffset(GmtOffset(1, 0, 0))))
    // The third zone has a fixed offset
    val fifthRule = zones.lift(5).flatMap(_._2.transitions.lift(2).map(_.offset))
    assertEquals(fifthRule, Some(GmtOffset(-4, -30, 0)))
  }
  test("parse contiguous Zones") {
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
        List(
          Zone(
            "America/Juneau",
            List(
              ZoneTransition(GmtOffset(15, 2, 19),
                             NullRule,
                             "LMT",
                             Some(Until(1867, Some(Month.OCTOBER), Some(DayOfTheMonth(18)), None))
              ),
              ZoneTransition(GmtOffset(-8, -57, -41),
                             NullRule,
                             "LMT",
                             Some(
                               Until(1900,
                                     Some(Month.AUGUST),
                                     Some(DayOfTheMonth(20)),
                                     Some(AtWallTime(LocalTime.of(12, 0)))
                               )
                             )
              ),
              ZoneTransition(GmtOffset(-8, 0, 0),
                             NullRule,
                             "PST",
                             Some(Until(1942, None, None, None))
              ),
              ZoneTransition(GmtOffset(-8, 0, 0),
                             RuleId("US"),
                             "P%sT",
                             Some(Until(1946, None, None, None))
              ),
              ZoneTransition(GmtOffset(-8, 0, 0),
                             NullRule,
                             "PST",
                             Some(Until(1969, None, None, None))
              ),
              ZoneTransition(GmtOffset(-8, 0, 0),
                             RuleId("US"),
                             "P%sT",
                             Some(
                               Until(1980,
                                     Some(Month.APRIL),
                                     Some(DayOfTheMonth(27)),
                                     Some(AtWallTime(LocalTime.of(2, 0)))
                               )
                             )
              ),
              ZoneTransition(GmtOffset(-9, 0, 0),
                             RuleId("US"),
                             "Y%sT",
                             Some(
                               Until(1980,
                                     Some(Month.OCTOBER),
                                     Some(DayOfTheMonth(26)),
                                     Some(AtWallTime(LocalTime.of(2, 0)))
                               )
                             )
              ),
              ZoneTransition(GmtOffset(-8, 0, 0),
                             RuleId("US"),
                             "P%sT",
                             Some(
                               Until(1983,
                                     Some(Month.OCTOBER),
                                     Some(DayOfTheMonth(30)),
                                     Some(AtWallTime(LocalTime.of(2, 0)))
                               )
                             )
              ),
              ZoneTransition(
                GmtOffset(-9, 0, 0),
                RuleId("US"),
                "Y%sT",
                Some(Until(1983, Some(Month.NOVEMBER), Some(DayOfTheMonth(30)), None))
              ),
              ZoneTransition(GmtOffset(-9, 0, 0), RuleId("US"), "AK%sT", None)
            )
          ),
          Zone(
            "America/Sitka",
            List(
              ZoneTransition(GmtOffset(14, 58, 47),
                             NullRule,
                             "LMT",
                             Some(Until(1867, Some(Month.OCTOBER), Some(DayOfTheMonth(18)), None))
              ),
              ZoneTransition(GmtOffset(-9, -1, -13),
                             NullRule,
                             "LMT",
                             Some(
                               Until(1900,
                                     Some(Month.AUGUST),
                                     Some(DayOfTheMonth(20)),
                                     Some(AtWallTime(LocalTime.of(12, 0)))
                               )
                             )
              ),
              ZoneTransition(GmtOffset(-8, 0, 0),
                             NullRule,
                             "PST",
                             Some(Until(1942, None, None, None))
              ),
              ZoneTransition(GmtOffset(-8, 0, 0),
                             RuleId("US"),
                             "P%sT",
                             Some(Until(1946, None, None, None))
              ),
              ZoneTransition(GmtOffset(-8, 0, 0),
                             NullRule,
                             "PST",
                             Some(Until(1969, None, None, None))
              ),
              ZoneTransition(GmtOffset(-8, 0, 0),
                             RuleId("US"),
                             "P%sT",
                             Some(
                               Until(1983,
                                     Some(Month.OCTOBER),
                                     Some(DayOfTheMonth(30)),
                                     Some(AtWallTime(LocalTime.of(2, 0)))
                               )
                             )
              ),
              ZoneTransition(
                GmtOffset(-9, 0, 0),
                RuleId("US"),
                "Y%sT",
                Some(Until(1983, Some(Month.NOVEMBER), Some(DayOfTheMonth(30)), None))
              ),
              ZoneTransition(GmtOffset(-9, 0, 0), RuleId("US"), "AK%sT", None)
            )
          )
        )
    )
    zones.foreach(zone => assertEquals(many(zoneParserNl).parseOnly(zone._1), Done("", zone._2)))
  }
  test("parse contiguous Zones with inconsistent left padding") {
    val zones = List(
      """Zone Pacific/Majuro	 11:24:48 -	LMT	1901
          |			 11:00	-	+11	1914 Oct
          |			  9:00	-	+09	1919 Feb  1
          |			 11:00	-	+11	1937
          |			 10:00	-	+10	1941 Apr  1
          |			  9:00	-	+09	1944 Jan 30
          |			 11:00	-	+11	1969 Oct
          |			 12:00	-	+12
          |Zone Pacific/Kwajalein	 11:09:20 -	LMT	1901
          |			 11:00	-	+11	1937
          |			 10:00	-	+10	1941 Apr  1
          |			  9:00	-	+09	1944 Feb  6
          |			 11:00	-	+11	1969 Oct
          |			-12:00	-	-12	1993 Aug 20 24:00
          |			 12:00	-	+12
          |""".stripMargin ->
        List(
          Zone(
            "Pacific/Majuro",
            List(
              ZoneTransition(GmtOffset(11, 24, 48),
                             NullRule,
                             "LMT",
                             Some(Until(1901, None, None, None))
              ),
              ZoneTransition(GmtOffset(11, 0, 0),
                             NullRule,
                             "+11",
                             Some(Until(1914, Some(Month.OCTOBER), None, None))
              ),
              ZoneTransition(GmtOffset(9, 0, 0),
                             NullRule,
                             "+09",
                             Some(Until(1919, Some(Month.FEBRUARY), Some(DayOfTheMonth(1)), None))
              ),
              ZoneTransition(GmtOffset(11, 0, 0),
                             NullRule,
                             "+11",
                             Some(Until(1937, None, None, None))
              ),
              ZoneTransition(GmtOffset(10, 0, 0),
                             NullRule,
                             "+10",
                             Some(Until(1941, Some(Month.APRIL), Some(DayOfTheMonth(1)), None))
              ),
              ZoneTransition(GmtOffset(9, 0, 0),
                             NullRule,
                             "+09",
                             Some(Until(1944, Some(Month.JANUARY), Some(DayOfTheMonth(30)), None))
              ),
              ZoneTransition(GmtOffset(11, 0, 0),
                             NullRule,
                             "+11",
                             Some(Until(1969, Some(Month.OCTOBER), None, None))
              ),
              ZoneTransition(GmtOffset(12, 0, 0), NullRule, "+12", None)
            )
          ),
          Zone(
            "Pacific/Kwajalein",
            List(
              ZoneTransition(GmtOffset(11, 9, 20),
                             NullRule,
                             "LMT",
                             Some(Until(1901, None, None, None))
              ),
              ZoneTransition(GmtOffset(11, 0, 0),
                             NullRule,
                             "+11",
                             Some(Until(1937, None, None, None))
              ),
              ZoneTransition(GmtOffset(10, 0, 0),
                             NullRule,
                             "+10",
                             Some(Until(1941, Some(Month.APRIL), Some(DayOfTheMonth(1)), None))
              ),
              ZoneTransition(GmtOffset(9, 0, 0),
                             NullRule,
                             "+09",
                             Some(Until(1944, Some(Month.FEBRUARY), Some(DayOfTheMonth(6)), None))
              ),
              ZoneTransition(GmtOffset(11, 0, 0),
                             NullRule,
                             "+11",
                             Some(Until(1969, Some(Month.OCTOBER), None, None))
              ),
              ZoneTransition(GmtOffset(-12, 0, 0),
                             NullRule,
                             "-12",
                             Some(
                               Until(1993,
                                     Some(Month.AUGUST),
                                     Some(DayOfTheMonth(20)),
                                     Some(AtWallTime(LocalTime.of(0, 0), true, 0))
                               )
                             )
              ),
              ZoneTransition(GmtOffset(12, 0, 0), NullRule, "+12", None)
            )
          )
        )
    )
    zones.foreach(zone => assertEquals(many(zoneParserNl).parseOnly(zone._1), Done("", zone._2)))
  }
  test("parse Zones with comments") {
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
          |			2:00	-	EET""".stripMargin         ->
        Zone(
          "Africa/Tripoli",
          List(
            ZoneTransition(GmtOffset(0, 52, 44),
                           NullRule,
                           "LMT",
                           Some(Until(1920, None, None, None))
            ),
            ZoneTransition(GmtOffset(1, 0, 0),
                           RuleId("Libya"),
                           "CE%sT",
                           Some(Until(1959, None, None, None))
            ),
            ZoneTransition(GmtOffset(2, 0, 0),
                           NullRule,
                           "EET",
                           Some(Until(1982, None, None, None))
            ),
            ZoneTransition(GmtOffset(1, 0, 0),
                           RuleId("Libya"),
                           "CE%sT",
                           Some(Until(1990, Some(Month.MAY), Some(DayOfTheMonth(4)), None))
            ),
            ZoneTransition(GmtOffset(2, 0, 0),
                           NullRule,
                           "EET",
                           Some(Until(1996, Some(Month.SEPTEMBER), Some(DayOfTheMonth(30)), None))
            ),
            ZoneTransition(GmtOffset(1, 0, 0),
                           RuleId("Libya"),
                           "CE%sT",
                           Some(Until(1997, Some(Month.OCTOBER), Some(DayOfTheMonth(4)), None))
            ),
            ZoneTransition(GmtOffset(2, 0, 0),
                           NullRule,
                           "EET",
                           Some(
                             Until(2012,
                                   Some(Month.NOVEMBER),
                                   Some(DayOfTheMonth(10)),
                                   Some(AtWallTime(LocalTime.of(2, 0)))
                             )
                           )
            ),
            ZoneTransition(GmtOffset(1, 0, 0),
                           RuleId("Libya"),
                           "CE%sT",
                           Some(
                             Until(2013,
                                   Some(Month.OCTOBER),
                                   Some(DayOfTheMonth(25)),
                                   Some(AtWallTime(LocalTime.of(2, 0)))
                             )
                           )
            ),
            ZoneTransition(GmtOffset(2, 0, 0), NullRule, "EET", None)
          )
        ),
      """Zone	Europe/Belfast	-0:23:40 -	LMT	1880 Aug  2
          |			-0:25:21 -	DMT	1916 May 21  2:00
          |						# DMT = Dublin/Dunsink MT
          |			-0:25:21 1:00	IST	1916 Oct  1  2:00s
          |						# IST = Irish Summer Time
          |			 0:00	GB-Eire	%s	1968 Oct 27
          |			 1:00	-	BST	1971 Oct 31  2:00u
          |			 0:00	GB-Eire	%s	1996
          |			 0:00	EU	GMT/BST""".stripMargin   ->
        Zone(
          "Europe/Belfast",
          List(
            ZoneTransition(GmtOffset(0, -23, -40),
                           NullRule,
                           "LMT",
                           Some(Until(1880, Some(Month.AUGUST), Some(DayOfTheMonth(2)), None))
            ),
            ZoneTransition(GmtOffset(0, -25, -21),
                           NullRule,
                           "DMT",
                           Some(
                             Until(1916,
                                   Some(Month.MAY),
                                   Some(DayOfTheMonth(21)),
                                   Some(AtWallTime(LocalTime.of(2, 0)))
                             )
                           )
            ),
            ZoneTransition(
              GmtOffset(0, -25, -21),
              FixedOffset(GmtOffset(1, 0, 0)),
              "IST",
              Some(
                Until(1916,
                      Some(Month.OCTOBER),
                      Some(DayOfTheMonth(1)),
                      Some(AtStandardTime(LocalTime.of(2, 0)))
                )
              )
            ),
            ZoneTransition(GmtOffset(0, 0, 0),
                           RuleId("GB-Eire"),
                           "%s",
                           Some(Until(1968, Some(Month.OCTOBER), Some(DayOfTheMonth(27)), None))
            ),
            ZoneTransition(GmtOffset(1, 0, 0),
                           NullRule,
                           "BST",
                           Some(
                             Until(1971,
                                   Some(Month.OCTOBER),
                                   Some(DayOfTheMonth(31)),
                                   Some(AtUniversalTime(LocalTime.of(2, 0)))
                             )
                           )
            ),
            ZoneTransition(GmtOffset(0, 0, 0),
                           RuleId("GB-Eire"),
                           "%s",
                           Some(Until(1996, None, None, None))
            ),
            ZoneTransition(GmtOffset(0, 0, 0), RuleId("EU"), "GMT/BST", None)
          )
        ),
      """Zone	Africa/Windhoek	1:08:24 -	LMT	1892 Feb 8
          |			1:30	-	SWAT	1903 Mar    # SW Africa Time
          |			2:00	-	SAST	1942 Sep 20  2:00
          |			2:00	1:00	SAST	1943 Mar 21  2:00
          |			2:00	-	SAST	1990 Mar 21 # independence
          |			2:00	-	CAT	1994 Apr  3
          |			1:00	Namibia	WA%sT""".stripMargin ->
        Zone(
          "Africa/Windhoek",
          List(
            ZoneTransition(GmtOffset(1, 8, 24),
                           NullRule,
                           "LMT",
                           Some(Until(1892, Some(Month.FEBRUARY), Some(DayOfTheMonth(8)), None))
            ),
            ZoneTransition(GmtOffset(1, 30, 0),
                           NullRule,
                           "SWAT",
                           Some(Until(1903, Some(Month.MARCH), None, None))
            ),
            ZoneTransition(GmtOffset(2, 0, 0),
                           NullRule,
                           "SAST",
                           Some(
                             Until(1942,
                                   Some(Month.SEPTEMBER),
                                   Some(DayOfTheMonth(20)),
                                   Some(AtWallTime(LocalTime.of(2, 0)))
                             )
                           )
            ),
            ZoneTransition(
              GmtOffset(2, 0, 0),
              FixedOffset(GmtOffset(1, 0, 0)),
              "SAST",
              Some(
                Until(1943,
                      Some(Month.MARCH),
                      Some(DayOfTheMonth(21)),
                      Some(AtWallTime(LocalTime.of(2, 0)))
                )
              )
            ),
            ZoneTransition(GmtOffset(2, 0, 0),
                           NullRule,
                           "SAST",
                           Some(Until(1990, Some(Month.MARCH), Some(DayOfTheMonth(21)), None))
            ),
            ZoneTransition(GmtOffset(2, 0, 0),
                           NullRule,
                           "CAT",
                           Some(Until(1994, Some(Month.APRIL), Some(DayOfTheMonth(3)), None))
            ),
            ZoneTransition(GmtOffset(1, 0, 0), RuleId("Namibia"), "WA%sT", None)
          )
        )
    )
    zones.foreach(zone => assertEquals(zoneParser.parseOnly(zone._1), Done("", zone._2)))
  }
  test("parse a complete file") {
    val text = scala.io.Source
      .fromInputStream(this.getClass.getResourceAsStream("/systemv"), "UTF-8")
      .mkString
    val r    = TZDBParser.parseFile(text)
    r match {
      case Done("", _) => assert(true)
      case _           => fail("parsing failure")
    }
  }
  test("parse all relevant files") {
    TZDBParser.tzdbFiles.foreach { f =>
      val text =
        scala.io.Source.fromInputStream(this.getClass.getResourceAsStream(s"/$f"), "UTF-8").mkString
      val r    = TZDBParser.parseFile(text)
      // Checks that it ingests the whole file
      r match {
        case Done("", _) => assert(true)
        case _           => fail("parsing failed")
      }
    }
  }
  test("parse a whole dir") {

    val r    = new File("src/test/resources/")
    val rows = TZDBParser.parseAll(r).unsafeRunSync()
    // Check a few well-known items
    assert(rows.flatMap(_.select[Link]).contains(Link("America/Port_of_Spain", "America/Anguilla")))
    assert(
      rows
        .flatMap(_.select[Rule])
        .contains(
          Rule(
            "Thule",
            GivenYear(1993),
            GivenYear(2006),
            Month.OCTOBER,
            LastWeekday(DayOfWeek.SUNDAY),
            AtWallTime(LocalTime.of(2, 0)),
            Save(true, LocalTime.of(0, 0)),
            Letter("S")
          )
        )
    )
    assert(
      rows
        .flatMap(_.select[Zone])
        .contains(
          Zone(
            "Africa/Cairo",
            List(
              ZoneTransition(GmtOffset(2, 5, 9),
                             NullRule,
                             "LMT",
                             Some(Until(1900, Some(Month.OCTOBER), None, None))
              ),
              ZoneTransition(GmtOffset(2, 0, 0), RuleId("Egypt"), "EE%sT", None)
            )
          )
        )
    )
    assert(rows.nonEmpty)
  }

}
