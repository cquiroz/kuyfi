package kuyfi

import cats.syntax.all._
import java.time.{ DayOfWeek, LocalDateTime, LocalTime, Month }
import kuyfi.TZDB._

class TZDBSuite extends munit.FunSuite {
  test("orders given years") {
    assert((GivenYear(2001): RuleYear) <= (GivenYear(2002): RuleYear))
    assert((GivenYear(2004): RuleYear) >= (GivenYear(2002): RuleYear))
    assert((GivenYear(2004): RuleYear) == (GivenYear(2004): RuleYear))
  }
  test("make everything less than Maximum") {
    assert((GivenYear(2001): RuleYear) <= (Maximum: RuleYear))
    assert((Minimum: RuleYear) <= (Maximum: RuleYear))
    assert((Only: RuleYear) <= (Maximum: RuleYear))
    assert((Maximum: RuleYear) >= (Maximum: RuleYear) && (Maximum: RuleYear) <= (Maximum: RuleYear))
  }
  test("make everything more than Maximum") {
    assert((GivenYear(2001): RuleYear) <= (Maximum: RuleYear))
    assert((Minimum: RuleYear) <= (Maximum: RuleYear))
    assert((Only: RuleYear) <= (Maximum: RuleYear))
    assert((Minimum: RuleYear) >= (Minimum: RuleYear) && (Minimum: RuleYear) <= (Minimum: RuleYear))
  }
  test("make everything equal to Only") {
    assert((GivenYear(2001): RuleYear) >= (Only: RuleYear))
    assert((Minimum: RuleYear) >= (Only: RuleYear))
    assert((Maximum: RuleYear) >= (Only: RuleYear))
    assert((Only: RuleYear) <= (Only: RuleYear) && (Only: RuleYear) >= (Only: RuleYear))
  }

  test("calculate date time") {
    val until = Until(1998,
                      Some(Month.APRIL),
                      Some(AfterWeekday(DayOfWeek.SUNDAY, 1)),
                      Some(AtWallTime(LocalTime.of(3, 0), endOfDay = false, 0))
    )
    assertEquals(until.toDateTime, LocalDateTime.of(1998, Month.APRIL, 5, 3, 0))
    assertEquals(until.toDateTime, LocalDateTime.of(1998, Month.APRIL, 5, 3, 0))
  }

  test("calculate day on a year") {
    val on  = AfterWeekday(DayOfWeek.SUNDAY, 1)
    assertEquals(on.dayOnYear(1998, Month.APRIL), 5)
    val on2 = DayOfTheMonth(5)
    assertEquals(on2.dayOnYear(1998, Month.APRIL), 5)
  }
}
