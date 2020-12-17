package actors.queues

import drt.shared.dates.{DateLike, LocalDate, UtcDate}
import org.specs2.mutable.Specification
import services.SDate

class DateRangeSpec extends Specification {
  "Concerning BST dates" >> {
    "Given a start date of 2020-05-01T00:00+1 (2020-04-30T23:00) and an end date the same" >> {
      "When I ask for the UTC date range" >> {
        "I should get 2020-04-30" >> {
          val date = SDate("2020-05-01T00:00:00+01:00")
          val range: Seq[DateLike] = DateRange.utcDateRange(date, date)

          range === Seq(UtcDate(2020, 4, 30))
        }
      }
    }

    "Given a start date of 2020-05-01T00:00+1 (2020-04-30T23:00) and an end date the same" >> {
      "When I ask for the Local date range" >> {
        "I should get 2020-05-01" >> {
          val date = SDate("2020-05-01T00:00:00+01:00")
          val range: Seq[DateLike] = DateRange.localDateRange(date, date)

          range === Seq(LocalDate(2020, 5, 1))
        }
      }
    }
  }

  "Concerning UTC dates" >> {
    "Given a start date of 2020-01-01T00:00 and an end date the same" >> {
      "When I ask for the local date range" >> {
        "I should get just 2020-01-01" >> {
          val date = SDate("2020-01-01T00:00:00")
          val range: Seq[DateLike] = DateRange.utcDateRange(date, date)

          range === Seq(UtcDate(2020, 1, 1))
        }
      }
    }

    "Given a start date of 2020-05-01T00:00 and an end date the same" >> {
      "When I ask for the local date range" >> {
        "I should get just 2020-01-01" >> {
          val date = SDate("2020-01-01T00:00")
          val range: Seq[DateLike] = DateRange.localDateRange(date, date)

          range === Seq(LocalDate(2020, 1, 1))
        }
      }
    }
  }
}