package hydrozoa.multisig.persistence

import java.time.{Duration, Instant}
import org.scalatest.funsuite.AnyFunSuite

/** [[ArchiveWatermarks]]: what an attached archiver has durably copied, as it last reported. */
class ArchiveWatermarksTest extends AnyFunSuite:

    private val t0 = Instant.parse("2026-09-17T00:00:00Z")
    private val block = Cf.Block
    private val request0 = Cf.Request(hydrozoa.multisig.consensus.peer.HeadPeerNumber(0))

    test("a first report is held in full") {
        val w = ArchiveWatermarks.empty()
        val report = w.record(Map(block -> 10L, request0 -> 99L), t0)

        assert(report.advanced == Map(block -> 10L, request0 -> 99L)): Unit
        assert(report.regressed.isEmpty): Unit
        assert(w.watermark(block).contains(10L)): Unit
        assert(w.watermark(request0).contains(99L))
    }

    test("a higher watermark advances it") {
        val w = ArchiveWatermarks.empty()
        w.record(Map(block -> 10L), t0): Unit
        val report = w.record(Map(block -> 20L), t0.plusSeconds(5))

        assert(report.advanced == Map(block -> 20L)): Unit
        assert(w.watermark(block).contains(20L))
    }

    /** The node cannot un-delete what it removed on the strength of the earlier report, so the
      * higher figure stands — and the regression is reported rather than swallowed, because it
      * usually means the archive was rebuilt and now holds less than the node assumed.
      */
    test("a lower watermark is reported and ignored") {
        val w = ArchiveWatermarks.empty()
        w.record(Map(block -> 500L), t0): Unit
        val report = w.record(Map(block -> 100L), t0.plusSeconds(5))

        assert(report.regressed == Map(block -> 100L)): Unit
        assert(report.advanced.isEmpty): Unit
        assert(w.watermark(block).contains(500L), "a regression lowered the held watermark")
    }

    test("families move independently") {
        val w = ArchiveWatermarks.empty()
        w.record(Map(block -> 10L, request0 -> 10L), t0): Unit
        val report = w.record(Map(block -> 5L, request0 -> 20L), t0.plusSeconds(5))

        assert(report.regressed == Map(block -> 5L)): Unit
        assert(report.advanced == Map(request0 -> 20L)): Unit
        assert(w.watermark(block).contains(10L)): Unit
        assert(w.watermark(request0).contains(20L))
    }

    test("a family left out of a report keeps its watermark") {
        val w = ArchiveWatermarks.empty()
        w.record(Map(block -> 10L, request0 -> 10L), t0): Unit
        w.record(Map(block -> 11L), t0.plusSeconds(5)): Unit

        assert(w.watermark(request0).contains(10L))
    }

    test("nothing is held before the first report") {
        val w = ArchiveWatermarks.empty()
        assert(w.watermarks.isEmpty): Unit
        assert(w.lastReportAt.isEmpty)
    }

    /** Before the first report the archiver is not fresh — same answer as after a silence, and for
      * retention the same consequence: an archiver was declared and is not accounted for, so
      * nothing may be deleted on its behalf.
      */
    test("an archiver that has never reported is not fresh") {
        val w = ArchiveWatermarks.empty()
        assert(!w.isFresh(Duration.ofMinutes(15), t0))
    }

    test("freshness is judged from the last report, not the last advance") {
        val w = ArchiveWatermarks.empty()
        w.record(Map(block -> 10L), t0): Unit
        // A repeat carries no new data but still proves the archiver is running.
        w.record(Map(block -> 10L), t0.plusSeconds(600)): Unit

        assert(w.isFresh(Duration.ofMinutes(15), t0.plusSeconds(1200))): Unit
        assert(!w.isFresh(Duration.ofMinutes(15), t0.plusSeconds(2000)))
    }

    test("freshness expires exactly at the window, not before") {
        val w = ArchiveWatermarks.empty()
        w.record(Map(block -> 1L), t0): Unit
        val window = Duration.ofMinutes(15)

        assert(
          w.isFresh(window, t0.plus(window)),
          "the boundary itself should still be fresh"
        ): Unit
        assert(!w.isFresh(window, t0.plus(window).plusMillis(1)))
    }

end ArchiveWatermarksTest
