package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.TransactionHash
import scalus.uplc.builtin.ByteString

/** Sanity tests for [[StoreKey]] — every key knows its CF, encodes to the expected width, and
  * (where comparable) sorts the way the design assumes (big-endian numeric for spine-indexed CFs).
  */
class StoreKeyTest extends AnyFunSuite:

    /** A 32-byte hash for the effect-index key tests. */
    private val sampleHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromArray(Array.tabulate(32)(_.toByte)))

    test("every StoreKey type maps to its expected Cf") {
        val cases: List[(StoreKey, Cf)] = List(
          JournalKey.Block(BlockNumber(0)) -> Cf.Block,
          JournalKey.Stack(StackNumber(0)) -> Cf.Stack,
          JournalKey.SoftAck(HeadPeerNumber(0), SoftAckNumber(0)) -> Cf.SoftAck(HeadPeerNumber(0)),
          JournalKey.HardAck(PeerId.Head(HeadPeerNumber(0)), HardAckNumber(0)) ->
              Cf.HardAck(PeerId.Head(HeadPeerNumber(0))),
          StoreKey.BlockResult(BlockNumber(0)) -> Cf.BlockResult,
          StoreKey.SoftConfirmation(BlockNumber(0)) -> Cf.SoftConfirmation,
          StoreKey.HardConfirmation(StackNumber(0)) -> Cf.HardConfirmation,
          StoreKey.DepositMap -> Cf.DepositMap,
          StoreKey.Treasury -> Cf.Treasury,
          StoreKey.EvacuationMap(BlockNumber(0)) -> Cf.EvacuationMap,
          StoreKey.RequestHighWater(BlockNumber(0)) -> Cf.RequestHighWater,
          StoreKey.L2CommandNumber(BlockNumber(0)) -> Cf.L2CommandNumber,
          StoreKey.RequestBlockIndex(RequestId(HeadPeerNumber(0), RequestNumber(0))) ->
              Cf.RequestBlockIndex,
          StoreKey.BlockStackIndex(BlockNumber(0)) -> Cf.BlockStackIndex,
          StoreKey.EffectStackIndex(sampleHash) -> Cf.EffectStackIndex,
          StoreKey.Meta("schema-version") -> Cf.Meta
        )
        cases.foreach { case (k, expected) =>
            assert(k.cf == expected, s"$k mapped to ${k.cf}, expected $expected")
        }
    }

    test("spine-indexed metadata keys encode to 4 big-endian bytes") {
        val keys: List[StoreKey] = List(
          StoreKey.BlockResult(BlockNumber(7)),
          StoreKey.SoftConfirmation(BlockNumber(99)),
          StoreKey.HardConfirmation(StackNumber(123)),
          StoreKey.EvacuationMap(BlockNumber(42)),
          StoreKey.RequestHighWater(BlockNumber(5)),
          StoreKey.L2CommandNumber(BlockNumber(11)),
          StoreKey.BlockStackIndex(BlockNumber(3))
        )
        keys.foreach { k =>
            assert(k.encode.length == 4, s"$k encoded to ${k.encode.length} bytes, expected 4")
        }
    }

    test("EffectStackIndex keys encode as the 32 raw l1TxId bytes") {
        val k = StoreKey.EffectStackIndex(sampleHash)
        assert(java.util.Arrays.equals(k.encode, Array.tabulate(32)(_.toByte)))
    }

    test("RequestBlockIndex keys encode as the packed-i64 RequestId, author-prefix-ordered") {
        def key(peer: Int, num: Long) =
            StoreKey.RequestBlockIndex(RequestId(HeadPeerNumber(peer), RequestNumber(num))).encode
        val _ = assert(key(1, 7).length == 8, s"encoded to ${key(1, 7).length} bytes, expected 8")
        // Same author: lex order == request-number order (the low bits of the i64).
        val _ = assert(java.util.Arrays.compareUnsigned(key(1, 1), key(1, 2)) < 0)
        // Different authors: the author sits in the high bits, so each author's rows stay
        // contiguous. (1L << 40) - 1 is RequestNumber's maximum — the 40-bit low half.
        assert(java.util.Arrays.compareUnsigned(key(0, (1L << 40) - 1), key(1, 0)) < 0)
    }

    test("singleton snapshot keys all encode to the same empty key") {
        val keys: List[StoreKey] = List(StoreKey.DepositMap, StoreKey.Treasury)
        keys.foreach { k =>
            assert(k.encode.isEmpty, s"$k encoded to ${k.encode.length} bytes, expected 0")
        }
    }

    test("Meta key encodes its name as UTF-8") {
        val k = StoreKey.Meta("schema-version")
        assert(java.util.Arrays.equals(k.encode, "schema-version".getBytes("UTF-8")))
    }

    test("BlockResult / SoftConfirmation keys sort lex == numeric (big-endian invariant)") {
        val a = StoreKey.BlockResult(BlockNumber(1)).encode
        val b = StoreKey.BlockResult(BlockNumber(2)).encode
        val c = StoreKey.SoftConfirmation(BlockNumber(0)).encode
        val d = StoreKey.SoftConfirmation(BlockNumber(Int.MaxValue)).encode
        val _ = assert(java.util.Arrays.compareUnsigned(a, b) < 0)
        assert(java.util.Arrays.compareUnsigned(c, d) < 0)
    }

    test("JournalKey is a StoreKey — accepted by APIs typed over StoreKey") {
        // Mirror what WriteBatch / Persistence do — accept a JournalKey wherever a StoreKey is expected.
        def takesAnyStoreKey(k: StoreKey): Cf = k.cf
        val _ = assert(takesAnyStoreKey(JournalKey.Block(BlockNumber(42))) == Cf.Block)
        assert(takesAnyStoreKey(StoreKey.DepositMap) == Cf.DepositMap)
    }
