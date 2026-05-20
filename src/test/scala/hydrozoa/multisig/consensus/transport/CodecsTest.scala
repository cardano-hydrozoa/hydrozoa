package hydrozoa.multisig.consensus.transport

import cats.data.NonEmptyList
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.PeerLiaison
import hydrozoa.multisig.consensus.PeerLiaison.Request.{GetMsgBatch, NewMsgBatch}
import hydrozoa.multisig.consensus.ack.{AckId, HardAck, HardAckId, HardAckNumber, SoftAck}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import org.scalatest.funsuite.AnyFunSuite

/** Round-trip tests for the wire codecs used by [[PeerWsTransport]].
  *
  * What we want to catch: a payload that encodes successfully but decodes to a different value
  * (opaque-int parsed wrong, list collapsed, etc.). All decode failures are surfaced; equality is
  * checked against the original instance.
  */
class CodecsTest extends AnyFunSuite {

    given CardanoNetwork.Section = CardanoNetwork.Preprod

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

    test("Frame.Msg(GetMsgBatch.initial) round-trips") {
        val frame = Frame.Msg(GetMsgBatch.initial)
        assert(roundTrip(frame) == frame)
    }

    test("Frame.Msg(GetMsgBatch with non-zero fields) round-trips") {
        val gmb = GetMsgBatch(
          batchNum = PeerLiaison.Batch.Number(42),
          ackNum = hydrozoa.multisig.consensus.ack.AckNumber(13),
          blockNum = BlockNumber(99),
          stackBriefNum = StackNumber(4),
          hardAckNum = HardAckNumber(8),
          requestNum = RequestNumber(7),
        )
        val frame = Frame.Msg(gmb)
        roundTrip(frame) match {
            case Frame.Msg(decoded: GetMsgBatch) =>
                assert(decoded.batchNum == gmb.batchNum)
                assert(decoded.ackNum == gmb.ackNum)
                assert(decoded.blockNum == gmb.blockNum)
                assert(decoded.stackBriefNum == gmb.stackBriefNum)
                assert(decoded.hardAckNum == gmb.hardAckNum)
                assert(decoded.requestNum == gmb.requestNum)
            case other => fail(s"Expected Msg(GetMsgBatch), got: $other")
        }
    }

    test("Frame.Msg(empty NewMsgBatch) round-trips") {
        val nmb = NewMsgBatch(
          batchNum = PeerLiaison.Batch.Number(1),
          ack = None,
          blockBrief = None,
          stackBrief = None,
          hardAck = None,
          requests = Nil,
        )
        val frame = Frame.Msg(nmb)
        roundTrip(frame) match {
            case Frame.Msg(decoded: NewMsgBatch) =>
                assert(decoded.batchNum == nmb.batchNum)
                assert(decoded.ack.isEmpty)
                assert(decoded.blockBrief.isEmpty)
                assert(decoded.stackBrief.isEmpty)
                assert(decoded.hardAck.isEmpty)
                assert(decoded.requests.isEmpty)
            case other => fail(s"Expected Msg(NewMsgBatch), got: $other")
        }
    }

    test("Frame.Msg(NewMsgBatch with SoftAck) round-trips") {
        val ack = SoftAck(
          ackId = AckId(HeadPeerNumber(2), hydrozoa.multisig.consensus.ack.AckNumber(5)),
          blockNum = BlockNumber(11),
          headerSignature = BlockHeader.Minor.HeaderSignature(
            IArray[Byte](1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte)
          ),
          finalizationRequested = true,
        )
        val nmb = NewMsgBatch(
          batchNum = PeerLiaison.Batch.Number(3),
          ack = Some(ack),
          blockBrief = None,
          stackBrief = None,
          hardAck = None,
          requests = Nil,
        )
        val frame = Frame.Msg(nmb)
        roundTrip(frame) match {
            case Frame.Msg(decoded: NewMsgBatch) =>
                assert(decoded.batchNum == nmb.batchNum)
                decoded.ack match {
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
            case other => fail(s"Expected Msg(NewMsgBatch), got: $other")
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

    private def hardAckFrame(payload: HardAck.Payload): Frame =
        Frame.Msg(
          NewMsgBatch(
            batchNum = PeerLiaison.Batch.Number(5),
            ack = None,
            blockBrief = None,
            stackBrief = None,
            hardAck = Some(
              HardAck(
                ackId = HardAckId(HeadPeerNumber(2), HardAckNumber(9)),
                stackNum = StackNumber(6),
                payload = payload
              )
            ),
            requests = Nil,
          )
        )

    test("Frame.Msg(NewMsgBatch with HardAck Round1Regular: OnlyPartial) round-trips") {
        assertJsonStable(
          hardAckFrame(
            HardAck.Round1Payload.Regular.OnlyPartial(
              partial = HardAck.Round1Payload.PartitionSigs.FinalPartial(rollouts = Nil)
            )
          )
        )
    }

    test("Frame.Msg(NewMsgBatch with HardAck Round1Regular: PartialThenCompletes) round-trips") {
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

    test("Frame.Msg(NewMsgBatch with HardAck Round1Regular: MinorThenPartial) round-trips") {
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
      "Frame.Msg(NewMsgBatch with HardAck Round1Regular: " +
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

    test("Frame.Msg(NewMsgBatch with HardAck Round2Regular) round-trips") {
        assertJsonStable(hardAckFrame(HardAck.Round2Payload.Regular(firstUnlockSig = sig(20, 21))))
    }

    test("Frame.Msg(NewMsgBatch with HardAck Round1Initial) round-trips") {
        assertJsonStable(hardAckFrame(HardAck.Round1Payload.Initial(fallbackSig = sig(30))))
    }

    test("Frame.Msg(NewMsgBatch with HardAck Sole) round-trips") {
        assertJsonStable(
          hardAckFrame(
            HardAck.SolePayload(
              sec = BlockHeader.Minor.HeaderSignature(IArray[Byte](42.toByte)),
              refunds = List(sig(40, 41))
            )
          )
        )
    }

    test("Frame.fromWire accepts GetMsgBatch and NewMsgBatch, rejects others") {
        val gmb = GetMsgBatch.initial
        val nmb = NewMsgBatch(PeerLiaison.Batch.Number(0), None, None, None, None, Nil)

        assert(Frame.fromWire(gmb).contains(gmb))
        assert(Frame.fromWire(nmb).contains(nmb))
        assert(Frame.fromWire(PeerLiaison.PreStart).isEmpty)
    }
}
