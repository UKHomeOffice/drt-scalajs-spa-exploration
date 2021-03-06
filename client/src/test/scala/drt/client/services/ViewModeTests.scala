package drt.client.services

import drt.client.services.JSDateConversions.SDate
import drt.shared.CrunchApi.MillisSinceEpoch
import utest.{TestSuite, _}

object ViewModeTests extends TestSuite {
  val nowMillis: MillisSinceEpoch = SDate.now().millisSinceEpoch

  override def tests: Tests = Tests {
    "ViewMode" - {
      "Given a ViewLive" - {
        val viewLive = ViewLive
        "When I ask if it's historic" - {
          val isHistoric = viewLive.isHistoric(SDate.now())
          "Then I should get false" - {
            assert(!isHistoric)
          }
        }
      }

      "Given a ViewPointInTime" - {
        val viewLive = ViewPointInTime(SDate(0L))
        "When I ask if it's historic" - {
          val isHistoric = viewLive.isHistoric(SDate.now())
          "Then I should get true" - {
            assert(isHistoric)
          }
        }
      }

      "Given a ViewDay with a now of 2020-06-01T00:00 BST, and date of the same" - {
        val now = SDate("2020-06-01T00:00")
        val viewDay = ViewDay(now)
        "When I ask if it's historic" - {
          "Then I should get false" - {
            assert(!viewDay.isHistoric(now))
          }
        }
      }

      "Given a ViewDay with a now of 2020-06-01T00:00 BST, and date of midnight that morning" - {
        val now = SDate("2020-06-01T00:00")
        val viewDay = ViewDay(SDate("2020-06-01T00:00"))
        "When I ask if it's historic" - {
          "Then I should get false" - {
            assert(!viewDay.isHistoric(now))
          }
        }
      }

      "Given a ViewDay with a now of 2020-06-01T00:00 BST, and date of one minutes before midnight that morning" - {
        val now = SDate("2020-06-01T00:00")
        val viewDay = ViewDay(SDate("2020-06-01T00:00").addMinutes(-1))
        "When I ask if it's historic" - {
          "Then I should get false" - {
            assert(viewDay.isHistoric(now))
          }
        }
      }
    }

    "Given the same two live view mode instances " +
      "When asking if they're different " +
      "Then the answer should be false" - {
      val currentViewMode = ViewLive

      val isDifferent = currentViewMode.isDifferentTo(currentViewMode)
      val expected = false

      assert(isDifferent == expected)
    }

    "Given the same two day view mode instances " +
      "When asking if they're different " +
      "Then the answer should be false" - {
      val currentViewMode = ViewDay(SDate.now())

      val isDifferent = currentViewMode.isDifferentTo(currentViewMode)
      val expected = false

      assert(isDifferent == expected)
    }

    "Given the same two snapshot view mode instances " +
      "When asking if they're different " +
      "Then the answer should be false" - {
      val currentViewMode = ViewPointInTime(SDate.now())

      val isDifferent = currentViewMode.isDifferentTo(currentViewMode)
      val expected = false

      assert(isDifferent == expected)
    }

    "Given two distinct day view mode instances of the same type " +
      "When asking if they're different " +
      "Then the answer should be yes" - {
      val now = SDate.now()
      val currentViewMode = ViewDay(now)
      val newSameViewMode = ViewDay(now)

      val isDifferent = currentViewMode.isDifferentTo(newSameViewMode)
      val expected = true

      assert(isDifferent == expected)
    }

    "Given a live view mode and a day view mode " +
      "When asking if they're different " +
      "Then the answer should be yes" - {
      val liveViewMode = ViewLive
      val dayViewMode = ViewDay(SDate.now())

      val isDifferent = liveViewMode.isDifferentTo(dayViewMode)
      val expected = true

      assert(isDifferent == expected)
    }

    "Given a live view mode and a snapshot view mode " +
      "When asking if they're different " +
      "Then the answer should be yes" - {
      val liveViewMode = ViewLive
      val snapshotViewMode = ViewPointInTime(SDate.now())

      val isDifferent = liveViewMode.isDifferentTo(snapshotViewMode)
      val expected = true

      assert(isDifferent == expected)
    }

    "Given a day view mode and a snapshot view mode " +
      "When asking if they're different " +
      "Then the answer should be yes" - {
      val dayViewMode = ViewDay(SDate.now())
      val snapshotViewMode = ViewPointInTime(SDate.now())

      val isDifferent = dayViewMode.isDifferentTo(snapshotViewMode)
      val expected = true

      assert(isDifferent == expected)
    }
  }
}
