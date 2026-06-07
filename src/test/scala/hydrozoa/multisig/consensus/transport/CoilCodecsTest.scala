package hydrozoa.multisig.consensus.transport

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, HardAckWithId, HubHardAckNumber, SoftAck, SoftAckId, SoftAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.{OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.BatchNumber
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalatest.funsuite.AnyFunSuite

/** Round-trip tests for the hub→coil wire codecs ([[CoilFrame]] carrying `Population` /
  * `OwnHardAck` batches). Number-only messages are checked by equality; messages carrying opaque
  * IArray-backed signatures are checked for JSON stability (encode → parse → re-encode), mirroring
  * [[CodecsTest]].
  */
class CoilCodecsTest extends AnyFunSuite {

    given CardanoNetwork.Section = CardanoNetwork.Preprod

    private def sig(bs: Int*): TxSignature = TxSignature(IArray.from(bs.map(_.toByte)))

    private val h0 = HeadPeerNumber(0)

    private def coilHardAck(num: Int): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Coil(CoilPeerNumber(0)), HardAckNumber(num)),
          stackNum = StackNumber(num),
          payload = HardAck.Round1Payload.Initial(fallbackSig = sig(1, 2, 3))
        )

    private def roundTrip(frame: CoilFrame): CoilFrame =
        CoilFrame.parse(CoilFrame.encode(frame)) match {
            case Right(decoded) => decoded
            case Left(err)      => fail(s"CoilFrame.parse failed: $err")
        }

    private def assertJsonStable(frame: CoilFrame): Unit = {
        val text = CoilFrame.encode(frame)
        CoilFrame.parse(text) match {
            case Right(decoded) =>
                assert(
                  CoilFrame.encode(decoded) == text,
                  s"re-encode differs:\n  first: $text\n  again: ${CoilFrame.encode(decoded)}"
                )
            case Left(err) => fail(s"CoilFrame.parse failed: $err")
        }
    }

    test("CoilFrame.Hello round-trips") {
        val frame = CoilFrame.Hello(coilNum = 3)
        assert(roundTrip(frame) == frame)
    }

    test("CoilFrame.Msg(OwnHardAck.Get) round-trips") {
        val frame = CoilFrame.Msg(OwnHardAck.Get(BatchNumber(7), HardAckNumber(4)))
        assert(roundTrip(frame) == frame)
    }

    test("CoilFrame.Msg(OwnHardAck.New, empty) round-trips") {
        val frame = CoilFrame.Msg(OwnHardAck.New(BatchNumber(1), None))
        assert(roundTrip(frame) == frame)
    }

    test("CoilFrame.Msg(OwnHardAck.New with a HardAck) round-trips") {
        assertJsonStable(CoilFrame.Msg(OwnHardAck.New(BatchNumber(2), Some(coilHardAck(0)))))
    }

    test("CoilFrame.Msg(Population.Get) round-trips") {
        val get = Population.Get(
          batchNum = BatchNumber(5),
          block = BlockNumber(1),
          stack = StackNumber(1),
          requests = Map(h0 -> RequestNumber.zero),
          softAcks = Map(h0 -> SoftAckNumber.zero.increment),
          headHardAcks = Map(h0 -> HardAckNumber.zero),
          coilHardAcks = Map(h0 -> HubHardAckNumber.zero)
        )
        assert(roundTrip(CoilFrame.Msg(get)) == CoilFrame.Msg(get))
    }

    test("CoilFrame.Msg(Population.New, empty maps) round-trips") {
        val nw = Population.New(
          batchNum = BatchNumber(6),
          block = None,
          stack = None,
          requests = Map.empty,
          softAcks = Map.empty,
          headHardAcks = Map.empty,
          coilHardAcks = Map.empty
        )
        assert(roundTrip(CoilFrame.Msg(nw)) == CoilFrame.Msg(nw))
    }

    test("CoilFrame.Msg(Population.New with soft/head/coil acks) round-trips") {
        val softAck = SoftAck(
          ackId = SoftAckId(h0, SoftAckNumber(5)),
          blockNum = BlockNumber(11),
          headerSignature =
              BlockHeader.Minor.HeaderSignature(IArray[Byte](1.toByte, 2.toByte, 3.toByte)),
          finalizationRequested = true,
        )
        val headHardAck = HardAck(
          ackId = HardAckId(PeerId.Head(h0), HardAckNumber(9)),
          stackNum = StackNumber(6),
          payload = HardAck.Round2Payload.Regular(firstUnlockSig = sig(20, 21))
        )
        val coilHubAck = HardAckWithId(
          hubPeer = h0,
          seqNum = HubHardAckNumber(3),
          ack = coilHardAck(1)
        )
        val nw = Population.New(
          batchNum = BatchNumber(8),
          block = None,
          stack = None,
          requests = Map.empty,
          softAcks = Map(h0 -> Some(softAck)),
          headHardAcks = Map(h0 -> Some(headHardAck)),
          coilHardAcks = Map(h0 -> Some(coilHubAck))
        )
        assertJsonStable(CoilFrame.Msg(nw))
    }
}
