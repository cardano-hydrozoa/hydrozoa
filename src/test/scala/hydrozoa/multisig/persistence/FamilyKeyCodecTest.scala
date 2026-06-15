package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.ack.{HardAckNumber, HubHardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Property-based round-trip tests for [[FamilyKey]] encode / decode.
  *
  * Verifies the §7.1 invariant: bytes written can be parsed back to the same `FamilyKey` for every
  * family case across the full legal index range.
  */
class FamilyKeyCodecTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    private val peer: Gen[HeadPeerNumber] =
        Gen.choose(0, (1 << 8) - 1).map(HeadPeerNumber(_))

    private val coilPeer: Gen[CoilPeerNumber] =
        Gen.choose(0, (1 << 8) - 1).map(CoilPeerNumber(_))

    private val blockNum: Gen[BlockNumber] = Gen.choose(0, Int.MaxValue).map(BlockNumber(_))
    private val stackNum: Gen[StackNumber] = Gen.choose(0, Int.MaxValue).map(StackNumber(_))
    private val softNum: Gen[SoftAckNumber] = Gen.choose(0, Int.MaxValue).map(SoftAckNumber(_))
    private val hardNum: Gen[HardAckNumber] = Gen.choose(0, Int.MaxValue).map(HardAckNumber(_))
    private val hubHardNum: Gen[HubHardAckNumber] =
        Gen.choose(0, Int.MaxValue).map(HubHardAckNumber(_))
    private val reqNum: Gen[RequestNumber] =
        Gen.choose(0L, (1L << 40) - 1L).map(RequestNumber(_))

    private val laneKey: Gen[FamilyKey] = Gen.oneOf(
      blockNum.map(FamilyKey.Block(_)),
      stackNum.map(FamilyKey.Stack(_)),
      for p <- peer; n <- reqNum yield FamilyKey.Request(p, n),
      for p <- peer; n <- softNum yield FamilyKey.SoftAck(p, n),
      for p <- peer; n <- hardNum yield FamilyKey.HardAck(p, n),
      for c <- coilPeer; n <- hardNum yield FamilyKey.CoilHardAck(c, n),
      for p <- peer; n <- hubHardNum yield FamilyKey.HubHardAck(p, n)
    )

    test("encode then decode is identity for every FamilyKey case") {
        forAll(laneKey) { key =>
            val bytes = key.encode
            val back = FamilyKey.decode(key.familyId.cf, bytes)
            assert(back == key, s"round-trip failed for $key")
        }
    }

    test("encoded width matches §7.1 spec for each family") {
        forAll(laneKey) { key =>
            val expected = key match
                case FamilyKey.Block(_)          => 4
                case FamilyKey.Stack(_)          => 4
                case FamilyKey.Request(_, _)     => 8
                case FamilyKey.SoftAck(_, _)     => 4
                case FamilyKey.HardAck(_, _)     => 4
                case FamilyKey.CoilHardAck(_, _) => 4
                case FamilyKey.HubHardAck(_, _)  => 4
            assert(key.encode.length == expected, s"width mismatch for $key")
        }
    }

    test("lexicographic byte order matches numeric index order within a spine") {
        // Big-endian fixed-width is what makes range scans correct.
        forAll(Gen.choose(0, Int.MaxValue - 1)) { (n: Int) =>
            val a = FamilyKey.Block(BlockNumber(n)).encode
            val b = FamilyKey.Block(BlockNumber(n + 1)).encode
            assert(compareBytes(a, b) < 0, s"$n bytes should be < ${n + 1} bytes")
        }
    }

    test("FamilyKey.familyId picks the right Cf") {
        forAll(laneKey) { key =>
            val cf = key.familyId.cf
            val expected = key match
                case _: FamilyKey.Block       => Cf.Block
                case _: FamilyKey.Stack       => Cf.Stack
                case k: FamilyKey.Request     => Cf.Request(k.peer)
                case k: FamilyKey.SoftAck     => Cf.SoftAck(k.peer)
                case k: FamilyKey.HardAck     => Cf.HardAck(k.peer)
                case k: FamilyKey.CoilHardAck => Cf.CoilHardAck(k.coil)
                case k: FamilyKey.HubHardAck  => Cf.HubHardAck(k.hub)
            assert(cf == expected)
        }
    }

    private def compareBytes(a: Array[Byte], b: Array[Byte]): Int =
        val limit = math.min(a.length, b.length)
        var i = 0
        while i < limit do
            val cmp = (a(i) & 0xff) - (b(i) & 0xff)
            if cmp != 0 then return cmp
            i += 1
        a.length - b.length
