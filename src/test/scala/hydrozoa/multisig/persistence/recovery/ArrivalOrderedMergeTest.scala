package hydrozoa.multisig.persistence.recovery

import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.{ArrivalStamp, FamilyKey, FamilyValue}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Property tests for [[ArrivalOrderedMerge]] — the §5.4 stamp-ordered interleaving of family
  * tails.
  */
class ArrivalOrderedMergeTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    private val genStamp: Gen[ArrivalStamp] =
        for
            gen <- Gen.choose(1, 4)
            mono <- Gen.choose(0L, 1_000_000L)
        yield ArrivalStamp(gen, mono)

    private val genKey: Gen[FamilyKey] = Gen.oneOf(
      Gen.choose(0, Int.MaxValue).map(n => FamilyKey.Block(BlockNumber(n))),
      Gen.choose(0, Int.MaxValue).map(n => FamilyKey.Stack(StackNumber(n))),
      for p <- Gen.choose(0, 255); n <- Gen.choose(0L, (1L << 40) - 1)
      yield FamilyKey.Request(HeadPeerNumber(p), RequestNumber(n)),
      for p <- Gen.choose(0, 255); n <- Gen.choose(0, Int.MaxValue)
      yield FamilyKey.SoftAck(HeadPeerNumber(p), SoftAckNumber(n)),
      for p <- Gen.choose(0, 255); n <- Gen.choose(0, Int.MaxValue)
      yield FamilyKey.HardAck(PeerId.Head(HeadPeerNumber(p)), HardAckNumber(n))
    )

    private val genEntry: Gen[RawFamilyEntry] =
        for
            key <- genKey
            stamp <- genStamp
            payload <- Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(_.toArray)
        yield RawFamilyEntry(key, stamp, FamilyValue.frame(stamp, payload))

    /** A stable projection for multiset / order comparison (Array has no structural equality). */
    private def project(e: RawFamilyEntry): (Int, Long, String, Vector[Byte]) =
        (e.stamp.generation, e.stamp.monotonicNanos, e.key.cf.name, e.key.encode.toVector)

    test("merge output is non-decreasing by (generation, monotonicNanos)") {
        forAll(Gen.listOf(genEntry)) { entries =>
            val merged = ArrivalOrderedMerge.mergeAll(entries)
            val stamps = merged.map(e => (e.stamp.generation, e.stamp.monotonicNanos))
            assert(stamps == stamps.sorted)
        }
    }

    test("merge is a permutation of its input (no entry dropped or invented)") {
        forAll(Gen.listOf(genEntry)) { entries =>
            val merged = ArrivalOrderedMerge.mergeAll(entries)
            assert(merged.map(project).sorted == entries.map(project).sorted)
        }
    }

    test("merge of per-family groups equals merge of the flattened bag") {
        forAll(Gen.listOf(Gen.listOf(genEntry))) { perLane =>
            assert(
              ArrivalOrderedMerge.merge(perLane).map(project)
                  == ArrivalOrderedMerge.mergeAll(perLane.flatten).map(project)
            )
        }
    }

    test("generation is the high-order key — a later process sorts after an earlier one") {
        // The whole point of `generation` (§5.4): a higher-generation entry sorts AFTER a
        // lower-generation one even when its monotonicNanos is smaller (each process's monotonic
        // clock resets). The non-decreasing property alone would pass if the code sorted by
        // monotonicNanos only, so pin the cross-generation semantics directly.
        val earlierProcess = RawFamilyEntry(
          FamilyKey.Block(BlockNumber(0)),
          ArrivalStamp(1, 100L),
          FamilyValue.frame(ArrivalStamp(1, 100L), Array.emptyByteArray)
        )
        val laterProcess = RawFamilyEntry(
          FamilyKey.Block(BlockNumber(1)),
          ArrivalStamp(2, 1L),
          FamilyValue.frame(ArrivalStamp(2, 1L), Array.emptyByteArray)
        )
        val merged = ArrivalOrderedMerge.mergeAll(List(laterProcess, earlierProcess))
        assert(merged.map(_.stamp) == List(ArrivalStamp(1, 100L), ArrivalStamp(2, 1L)))
    }

    test("merge is deterministic — same input, same output order") {
        forAll(Gen.listOf(genEntry)) { entries =>
            assert(
              ArrivalOrderedMerge.mergeAll(entries).map(project)
                  == ArrivalOrderedMerge.mergeAll(entries).map(project)
            )
        }
    }

    /** Build an entry with an explicit stamp so two entries can be forced to collide on it. */
    private def entryAt(key: FamilyKey, stamp: ArrivalStamp): RawFamilyEntry =
        RawFamilyEntry(key, stamp, FamilyValue.frame(stamp, Array.emptyByteArray))

    test("tiebreak: identical stamp in different CFs orders by cf.ordinal") {
        val stamp = ArrivalStamp(1, 42L)
        // Stack.ordinal > Block.ordinal in the Cf enum, so Block must come first regardless of input
        // order — the (gen, mono, cf.name) tuple, not insertion order, decides.
        val block = entryAt(FamilyKey.Block(BlockNumber(0)), stamp)
        val stack = entryAt(FamilyKey.Stack(StackNumber(0)), stamp)
        assert(
          block.key.cf.name < stack.key.cf.name,
          "precondition: Block sorts before Stack"
        )
        assert(
          ArrivalOrderedMerge.mergeAll(List(stack, block)).map(_.key) == List(block.key, stack.key)
        )
    }

    test("tiebreak: identical stamp, same CF orders by unsigned key bytes") {
        val stamp = ArrivalStamp(1, 42L)
        val peer = HeadPeerNumber(0)
        // Same Request CF, same peer, ascending RequestNumber — big-endian keys sort numerically.
        val lo = entryAt(FamilyKey.Request(peer, RequestNumber(1)), stamp)
        val hi = entryAt(FamilyKey.Request(peer, RequestNumber(2)), stamp)
        assert(ArrivalOrderedMerge.mergeAll(List(hi, lo)).map(_.key) == List(lo.key, hi.key))
    }

    test("tiebreak: unsigned byte order — high peer byte sorts after low (not signed)") {
        val stamp = ArrivalStamp(1, 42L)
        // Peer 200 (0xC8) is negative as a signed byte; unsigned lex must still sort it after peer 1.
        val low = entryAt(FamilyKey.SoftAck(HeadPeerNumber(1), SoftAckNumber(0)), stamp)
        val high = entryAt(FamilyKey.SoftAck(HeadPeerNumber(200), SoftAckNumber(0)), stamp)
        assert(ArrivalOrderedMerge.mergeAll(List(high, low)).map(_.key) == List(low.key, high.key))
    }
