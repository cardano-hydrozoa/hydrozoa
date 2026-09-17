package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import java.time.Instant
import org.scalatest.funsuite.AnyFunSuite

/** [[AckRetention]]: whether a confirmed ack may be deleted yet. */
class AckRetentionTest extends AnyFunSuite:

    private val t0 = Instant.parse("2026-09-17T00:00:00Z")
    private val softAck0 = Cf.SoftAck(HeadPeerNumber(0))
    private val hardAck0 = Cf.HardAck(PeerId.Head(HeadPeerNumber(0)))

    /** No archiver: confirmation alone makes an ack redundant, so nothing holds it. */
    test("unconstrained retention prunes anything") {
        assert(AckRetention.unconstrained.mayPrune(softAck0, 0)): Unit
        assert(AckRetention.unconstrained.mayPrune(softAck0, Long.MaxValue)): Unit
        assert(AckRetention.unconstrained.mayPrune(hardAck0, 12345))
    }

    test("no declared archiver yields unconstrained retention") {
        assert(AckRetention.forArchiver(None).mayPrune(softAck0, 99))
    }

    /** An archiver that has never reported holds everything: it was declared and is not yet
      * accounted for, which is the same conservative reading retention takes everywhere else.
      */
    test("an archiver that has reported nothing prunes nothing") {
        val retention = AckRetention.forArchiver(Some(ArchiveWatermarks.empty()))
        assert(!retention.mayPrune(softAck0, 0)): Unit
        assert(!retention.mayPrune(hardAck0, 0))
    }

    test("an ack at or below the watermark may be pruned; one above may not") {
        val watermarks = ArchiveWatermarks.empty()
        watermarks.record(Map(softAck0 -> 10L), t0): Unit
        val retention = AckRetention.forArchiver(Some(watermarks))

        assert(retention.mayPrune(softAck0, 9)): Unit
        assert(retention.mayPrune(softAck0, 10), "the watermark itself is archived"): Unit
        assert(!retention.mayPrune(softAck0, 11))
    }

    /** The decision is per family, so one lane running ahead never licenses pruning another. This
      * is what lets soft-ack indices (block numbers) and hard-ack indices (an independent per-peer
      * cursor) be compared without converting between the two numbering spaces.
      */
    test("a watermark on one family does not license pruning another") {
        val watermarks = ArchiveWatermarks.empty()
        watermarks.record(Map(softAck0 -> 500L), t0): Unit
        val retention = AckRetention.forArchiver(Some(watermarks))

        assert(retention.mayPrune(softAck0, 500)): Unit
        assert(!retention.mayPrune(hardAck0, 1), "a hard-ack was pruned on a soft-ack's watermark")
        assert(!retention.mayPrune(Cf.SoftAck(HeadPeerNumber(1)), 1))
    }

    /** The holder is live, so retention follows it without being rebuilt — which is what makes a
      * watermark report a prune trigger rather than something only the next restart notices.
      */
    test("retention tracks the holder as reports arrive") {
        val watermarks = ArchiveWatermarks.empty()
        val retention = AckRetention.forArchiver(Some(watermarks))

        assert(!retention.mayPrune(hardAck0, 7)): Unit
        watermarks.record(Map(hardAck0 -> 7L), t0): Unit
        assert(retention.mayPrune(hardAck0, 7))
    }

end AckRetentionTest
