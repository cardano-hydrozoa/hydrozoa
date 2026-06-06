package hydrozoa.multisig.consensus.transport

import cats.data.NonEmptyList
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, HardAckWithId, HubHardAckNumber, SoftAck, SoftAckId, SoftAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.Mesh
import hydrozoa.multisig.consensus.liaison.{BatchNumber, LiaisonProtocol}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalatest.funsuite.AnyFunSuite

/** Round-trip tests for the wire codecs used by [[PeerWsTransport]] — the head ↔ head mesh batch
  * messages ([[Mesh.Get]] / [[Mesh.New]]) only, since the hub↔coil links are in-process and never
  * hit the wire.
  *
  * What we want to catch: a payload that encodes successfully but decodes to a different value
  * (opaque-int parsed wrong, list collapsed, etc.). All decode failures are surfaced; equality is
  * checked against the original instance.
  */
class CodecsTest extends AnyFunSuite {

    given CardanoNetwork.Section = CardanoNetwork.Preprod

    // A representative initial-cursor Get (single-head schedule: first brief items are 1).
    private val testMeshGet: Mesh.Get = Mesh.Get(
      batchNum = BatchNumber.zero,
      block = BlockNumber(1),
      stack = StackNumber(1),
      request = RequestNumber.zero,
      softAck = SoftAckNumber.zero.increment,
      headHardAck = HardAckNumber.zero,
      hubHardAck = HubHardAckNumber.zero
    )

    private def emptyNew(batchNum: BatchNumber): Mesh.New = Mesh.New(
      batchNum = batchNum,
      block = None,
      stack = None,
      requests = Nil,
      softAck = None,
      headHardAck = None,
      hubHardAck = None
    )

    private def roundTrip(frame: Frame): Frame = {
        val text = Frame.encode(frame)
        Frame.parse(text) match {
            case Right(decoded) => decoded
            case Left(err)      => fail(s"Frame.parse failed: $err\nText: $text")
        }
    }

    test("Hello frame round-trips") {
        val frame = Frame.Hello(peerNum = 7)
        assert(roundTrip(frame) == frame)
    }

    test("Frame.Msg(Mesh.Get initial cursors) round-trips") {
        val frame = Frame.Msg(testMeshGet)
        assert(roundTrip(frame) == frame)
    }

    test("Frame.Msg(Mesh.Get with non-zero fields) round-trips") {
        val get = Mesh.Get(
          batchNum = BatchNumber(42),
          block = BlockNumber(99),
          stack = StackNumber(4),
          request = RequestNumber(7),
          softAck = SoftAckNumber(13),
          headHardAck = HardAckNumber(8),
          hubHardAck = HubHardAckNumber(5)
        )
        val frame = Frame.Msg(get)
        roundTrip(frame) match {
            case Frame.Msg(decoded: Mesh.Get) =>
                assert(decoded.batchNum == get.batchNum)
                assert(decoded.block == get.block)
                assert(decoded.stack == get.stack)
                assert(decoded.request == get.request)
                assert(decoded.softAck == get.softAck)
                assert(decoded.headHardAck == get.headHardAck)
                assert(decoded.hubHardAck == get.hubHardAck)
            case other => fail(s"Expected Msg(Mesh.Get), got: $other")
        }
    }

    test("Frame.Msg(empty Mesh.New) round-trips") {
        val nmb = emptyNew(BatchNumber(1))
        val frame = Frame.Msg(nmb)
        roundTrip(frame) match {
            case Frame.Msg(decoded: Mesh.New) =>
                assert(decoded.batchNum == nmb.batchNum)
                assert(decoded.softAck.isEmpty)
                assert(decoded.block.isEmpty)
                assert(decoded.stack.isEmpty)
                assert(decoded.headHardAck.isEmpty)
                assert(decoded.hubHardAck.isEmpty)
                assert(decoded.requests.isEmpty)
            case other => fail(s"Expected Msg(Mesh.New), got: $other")
        }
    }

    test("Frame.Msg(Mesh.New with SoftAck) round-trips") {
        val ack = SoftAck(
          ackId = SoftAckId(HeadPeerNumber(2), SoftAckNumber(5)),
          blockNum = BlockNumber(11),
          headerSignature = BlockHeader.Minor.HeaderSignature(
            IArray[Byte](1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte)
          ),
          finalizationRequested = true,
        )
        val nmb = emptyNew(BatchNumber(3)).copy(softAck = Some(ack))
        val frame = Frame.Msg(nmb)
        roundTrip(frame) match {
            case Frame.Msg(decoded: Mesh.New) =>
                assert(decoded.batchNum == nmb.batchNum)
                decoded.softAck match {
                    case Some(decodedAck: SoftAck) =>
                        assert(decodedAck.ackId == ack.ackId)
                        assert(decodedAck.blockNum == ack.blockNum)
                        assert(
                          (decodedAck.headerSignature: IArray[Byte]).toList ==
                              (ack.headerSignature: IArray[Byte]).toList
                        )
                        assert(decodedAck.finalizationRequested == ack.finalizationRequested)
                    case other => fail(s"Expected Some(SoftAck), got: $other")
                }
            case other => fail(s"Expected Msg(Mesh.New), got: $other")
        }
    }

    // HardAck payloads carry opaque `TxSignature` (IArray-backed) so structural `==` is
    // reference-sensitive; assert JSON stability instead — encode, parse, re-encode, compare.
    // This catches "encodes but decodes to a different value" without IArray equality pitfalls.
    private def assertJsonStable(frame: Frame): Unit = {
        val text = Frame.encode(frame)
        Frame.parse(text) match {
            case Right(decoded) =>
                assert(
                  Frame.encode(decoded) == text,
                  s"re-encode differs:\n  first: $text\n  again: ${Frame.encode(decoded)}"
                )
            case Left(err) => fail(s"Frame.parse failed: $err\nText: $text")
        }
    }

    private def sig(bs: Int*): TxSignature = TxSignature(IArray.from(bs.map(_.toByte)))

    private def headHardAck(payload: HardAck.Payload): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Head(HeadPeerNumber(2)), HardAckNumber(9)),
          stackNum = StackNumber(6),
          payload = payload
        )

    private def hardAckFrame(payload: HardAck.Payload): Frame =
        Frame.Msg(emptyNew(BatchNumber(5)).copy(headHardAck = Some(headHardAck(payload))))

    test("Frame.Msg(Mesh.New with HardAck Round1Regular: OnlyPartial) round-trips") {
        assertJsonStable(
          hardAckFrame(
            HardAck.Round1Payload.Regular.OnlyPartial(
              partial = HardAck.Round1Payload.PartitionSigs.FinalPartial(rollouts = Nil)
            )
          )
        )
    }

    test("Frame.Msg(Mesh.New with HardAck Round1Regular: PartialThenCompletes) round-trips") {
        assertJsonStable(
          hardAckFrame(
            HardAck.Round1Payload.Regular.PartialThenCompletes(
              partial = HardAck.Round1Payload.PartitionSigs.MajorPartial(
                fallback = sig(4, 5),
                rollouts = List(sig(6, 7, 8)),
                refunds = List(sig(9), sig(10, 11)),
                sec = Some(
                  BlockHeader.Minor.HeaderSignature(IArray[Byte](12.toByte, 13.toByte))
                )
              ),
              completes = NonEmptyList.of(
                HardAck.Round1Payload.PartitionSigs.FinalComplete(
                  finalization = sig(44),
                  rollouts = List(sig(45), sig(46))
                )
              )
            )
          )
        )
    }

    test("Frame.Msg(Mesh.New with HardAck Round1Regular: MinorThenPartial) round-trips") {
        assertJsonStable(
          hardAckFrame(
            HardAck.Round1Payload.Regular.MinorThenPartial(
              minor = HardAck.Round1Payload.PartitionSigs.Minor(
                sec = BlockHeader.Minor.HeaderSignature(IArray[Byte](14.toByte)),
                refunds = List(sig(15, 16))
              ),
              partial = HardAck.Round1Payload.PartitionSigs.MajorPartial(
                fallback = sig(50),
                rollouts = Nil,
                refunds = List(sig(51)),
                sec = None
              )
            )
          )
        )
    }

    test(
      "Frame.Msg(Mesh.New with HardAck Round1Regular: " +
          "MinorThenPartialThenCompletes) round-trips"
    ) {
        assertJsonStable(
          hardAckFrame(
            HardAck.Round1Payload.Regular.MinorThenPartialThenCompletes(
              minor = HardAck.Round1Payload.PartitionSigs.Minor(
                sec = BlockHeader.Minor.HeaderSignature(IArray[Byte](60.toByte)),
                refunds = Nil
              ),
              partial = HardAck.Round1Payload.PartitionSigs.MajorPartial(
                fallback = sig(61),
                rollouts = Nil,
                refunds = Nil,
                sec = Some(BlockHeader.Minor.HeaderSignature(IArray[Byte](62.toByte)))
              ),
              completes = NonEmptyList.of(
                HardAck.Round1Payload.PartitionSigs.FinalComplete(
                  finalization = sig(63),
                  rollouts = List(sig(64))
                )
              )
            )
          )
        )
    }

    test("Frame.Msg(Mesh.New with HardAck Round2Regular) round-trips") {
        assertJsonStable(hardAckFrame(HardAck.Round2Payload.Regular(firstUnlockSig = sig(20, 21))))
    }

    test("Frame.Msg(Mesh.New with HardAck Round1Initial) round-trips") {
        assertJsonStable(hardAckFrame(HardAck.Round1Payload.Initial(fallbackSig = sig(30))))
    }

    test("Frame.Msg(Mesh.New with HardAck Sole) round-trips") {
        assertJsonStable(
          hardAckFrame(
            HardAck.SolePayload(
              sec = BlockHeader.Minor.HeaderSignature(IArray[Byte](42.toByte)),
              refunds = List(sig(40, 41))
            )
          )
        )
    }

    test("Frame.Msg(Mesh.New with a re-sequenced coil HardAckWithId) round-trips") {
        val hubAck = HardAckWithId(
          hubPeer = HeadPeerNumber(0),
          seqNum = HubHardAckNumber(3),
          ack = HardAck(
            ackId = HardAckId(PeerId.Coil(CoilPeerNumber(1)), HardAckNumber(2)),
            stackNum = StackNumber(4),
            payload = HardAck.Round1Payload.Initial(fallbackSig = sig(5, 6))
          )
        )
        assertJsonStable(Frame.Msg(emptyNew(BatchNumber(7)).copy(hubHardAck = Some(hubAck))))
    }

    test("Frame.fromWire accepts Mesh.Get and Mesh.New, rejects others") {
        val get = testMeshGet
        val nmb = emptyNew(BatchNumber.zero)

        assert(Frame.fromWire(get).contains(get))
        assert(Frame.fromWire(nmb).contains(nmb))
        assert(Frame.fromWire(LiaisonProtocol.PreStart).isEmpty)
    }
}
