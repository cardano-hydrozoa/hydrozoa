package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.ack.{HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalatest.funsuite.AnyFunSuite

/** Sanity tests for [[StoreKey]] — every key knows its CF, encodes to the expected width, and
  * (where comparable) sorts the way the design assumes (big-endian numeric for spine-indexed CFs).
  */
class StoreKeyTest extends AnyFunSuite:

    test("every StoreKey type maps to its expected Cf") {
        val cases: List[(StoreKey, Cf)] = List(
          LaneKey.Block(BlockNumber(0)) -> Cf.Block,
          LaneKey.Stack(StackNumber(0)) -> Cf.Stack,
          LaneKey.SoftAck(HeadPeerNumber(0), SoftAckNumber(0)) -> Cf.SoftAck,
          LaneKey.HardAck(HeadPeerNumber(0), HardAckNumber(0)) -> Cf.HardAck,
          StoreKey.BlockResult(BlockNumber(0)) -> Cf.BlockResult,
          StoreKey.SoftConfirmation(BlockNumber(0)) -> Cf.SoftConfirmation,
          StoreKey.HardConfirmation(StackNumber(0)) -> Cf.HardConfirmation,
          StoreKey.DepositMap -> Cf.DepositMap,
          StoreKey.Treasury -> Cf.Treasury,
          StoreKey.EvacuationMap(BlockNumber(0)) -> Cf.EvacuationMap,
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
          StoreKey.EvacuationMap(BlockNumber(42))
        )
        keys.foreach { k =>
            assert(k.encode.length == 4, s"$k encoded to ${k.encode.length} bytes, expected 4")
        }
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

    test("LaneKey is a StoreKey — accepted by APIs typed over StoreKey") {
        // Mirror what WriteBatch / Persistence do — accept a LaneKey wherever a StoreKey is expected.
        def takesAnyStoreKey(k: StoreKey): Cf = k.cf
        val _ = assert(takesAnyStoreKey(LaneKey.Block(BlockNumber(42))) == Cf.Block)
        assert(takesAnyStoreKey(StoreKey.DepositMap) == Cf.DepositMap)
    }
