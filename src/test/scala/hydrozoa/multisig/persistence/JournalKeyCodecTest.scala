package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.ack.{HardAckNumber, HubHardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Property-based round-trip tests for [[JournalKey]] encode / decode.
  *
  * Verifies the §7.1 invariant: bytes written can be parsed back to the same `JournalKey` for every
  * journal case across the full legal index range.
  */
class JournalKeyCodecTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    private val peer: Gen[HeadPeerNumber] =
        Gen.choose(0, (1 << 8) - 1).map(HeadPeerNumber(_))

    private val coilPeer: Gen[CoilPeerNumber] =
        Gen.choose(0, (1 << 8) - 1).map(CoilPeerNumber(_))

    /** A hard-ack author — head or coil — exercising both wire-tag bits of the unified journal. */
    private val peerId: Gen[PeerId] =
        Gen.oneOf(peer.map(PeerId.Head(_)), coilPeer.map(PeerId.Coil(_)))

    private val blockNum: Gen[BlockNumber] = Gen.choose(0, Int.MaxValue).map(BlockNumber(_))
    private val stackNum: Gen[StackNumber] = Gen.choose(0, Int.MaxValue).map(StackNumber(_))
    private val softNum: Gen[SoftAckNumber] = Gen.choose(0, Int.MaxValue).map(SoftAckNumber(_))
    private val hardNum: Gen[HardAckNumber] = Gen.choose(0, Int.MaxValue).map(HardAckNumber(_))
    private val hubHardNum: Gen[HubHardAckNumber] =
        Gen.choose(0, Int.MaxValue).map(HubHardAckNumber(_))
    private val reqNum: Gen[RequestNumber] =
        Gen.choose(0L, (1L << 40) - 1L).map(RequestNumber(_))

    private val laneKey: Gen[JournalKey] = Gen.oneOf(
      blockNum.map(JournalKey.Block(_)),
      stackNum.map(JournalKey.Stack(_)),
      for p <- peer; n <- reqNum yield JournalKey.Request(p, n),
      for p <- peer; n <- softNum yield JournalKey.SoftAck(p, n),
      for p <- peerId; n <- hardNum yield JournalKey.HardAck(p, n),
      for p <- peer; n <- hubHardNum yield JournalKey.HubHardAck(p, n)
    )

    test("encode then decode is identity for every JournalKey case") {
        forAll(laneKey) { key =>
            val bytes = key.encode
            val back = JournalKey.decode(key.journalId.cf, bytes)
            assert(back == key, s"round-trip failed for $key")
        }
    }

    test("encoded width matches §7.1 spec for each journal") {
        forAll(laneKey) { key =>
            val expected = key match
                case JournalKey.Block(_)         => 4
                case JournalKey.Stack(_)         => 4
                case JournalKey.Request(_, _)    => 8
                case JournalKey.SoftAck(_, _)    => 4
                case JournalKey.HardAck(_, _)    => 4
                case JournalKey.HubHardAck(_, _) => 4
            assert(key.encode.length == expected, s"width mismatch for $key")
        }
    }

    test("lexicographic byte order matches numeric index order within a spine") {
        // Big-endian fixed-width is what makes range scans correct.
        forAll(Gen.choose(0, Int.MaxValue - 1)) { (n: Int) =>
            val a = JournalKey.Block(BlockNumber(n)).encode
            val b = JournalKey.Block(BlockNumber(n + 1)).encode
            assert(compareBytes(a, b) < 0, s"$n bytes should be < ${n + 1} bytes")
        }
    }

    test("JournalKey.journalId picks the right Cf") {
        forAll(laneKey) { key =>
            val cf = key.journalId.cf
            val expected = key match
                case _: JournalKey.Block      => Cf.Block
                case _: JournalKey.Stack      => Cf.Stack
                case k: JournalKey.Request    => Cf.Request(k.peer)
                case k: JournalKey.SoftAck    => Cf.SoftAck(k.peer)
                case k: JournalKey.HardAck    => Cf.HardAck(k.peer)
                case k: JournalKey.HubHardAck => Cf.HubHardAck(k.hub)
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
