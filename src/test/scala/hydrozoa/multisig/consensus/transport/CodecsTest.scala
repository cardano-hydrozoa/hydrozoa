package hydrozoa.multisig.consensus.transport

import cats.data.NonEmptyList
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, HardAckWithId, HubHardAckNumber, SoftAck, SoftAckId, SoftAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.Mesh
import hydrozoa.multisig.consensus.liaison.{BatchNumber, LiaisonProtocol}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
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

    private def roundTrip(frame: HeadFrame): HeadFrame = {
        val text = HeadFrame.encode(frame)
        HeadFrame.parse(text) match {
            case Right(decoded) => decoded
            case Left(err)      => fail(s"HeadFrame.parse failed: $err\nText: $text")
        }
    }

    test("Hello frame round-trips") {
        val frame = HeadFrame.Hello(peerNum = 7)
        assert(roundTrip(frame) == frame)
    }

    test("HeadFrame.Msg(Mesh.Get initial cursors) round-trips") {
        val frame = HeadFrame.Msg(testMeshGet)
        assert(roundTrip(frame) == frame)
    }

    test("HeadFrame.Msg(Mesh.Get with non-zero fields) round-trips") {
        val get = Mesh.Get(
          batchNum = BatchNumber(42),
          block = BlockNumber(99),
          stack = StackNumber(4),
          request = RequestNumber(7),
          softAck = SoftAckNumber(13),
          headHardAck = HardAckNumber(8),
          hubHardAck = HubHardAckNumber(5)
        )
        val frame = HeadFrame.Msg(get)
        roundTrip(frame) match {
            case HeadFrame.Msg(decoded: Mesh.Get) =>
                val _ = assert(decoded.batchNum == get.batchNum)
                val _ = assert(decoded.block == get.block)
                val _ = assert(decoded.stack == get.stack)
                val _ = assert(decoded.request == get.request)
                val _ = assert(decoded.softAck == get.softAck)
                val _ = assert(decoded.headHardAck == get.headHardAck)
                assert(decoded.hubHardAck == get.hubHardAck)
            case other => fail(s"Expected Msg(Mesh.Get), got: $other")
        }
    }

    test("HeadFrame.Msg(empty Mesh.New) round-trips") {
        val nmb = emptyNew(BatchNumber(1))
        val frame = HeadFrame.Msg(nmb)
        roundTrip(frame) match {
            case HeadFrame.Msg(decoded: Mesh.New) =>
                val _ = assert(decoded.batchNum == nmb.batchNum)
                val _ = assert(decoded.softAck.isEmpty)
                val _ = assert(decoded.block.isEmpty)
                val _ = assert(decoded.stack.isEmpty)
                val _ = assert(decoded.headHardAck.isEmpty)
                val _ = assert(decoded.hubHardAck.isEmpty)
                assert(decoded.requests.isEmpty)
            case other => fail(s"Expected Msg(Mesh.New), got: $other")
        }
    }

    test("HeadFrame.Msg(Mesh.New with SoftAck) round-trips") {
        val ack = SoftAck(
          ackId = SoftAckId(HeadPeerNumber(2), SoftAckNumber(5)),
          blockNum = BlockNumber(11),
          headerSignature = BlockHeader.Minor.HeaderSignature(
            IArray[Byte](1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte)
          ),
          finalizationRequested = true,
        )
        val nmb = emptyNew(BatchNumber(3)).copy(softAck = Some(ack))
        val frame = HeadFrame.Msg(nmb)
        roundTrip(frame) match {
            case HeadFrame.Msg(decoded: Mesh.New) =>
                val _ = assert(decoded.batchNum == nmb.batchNum)
                decoded.softAck match {
                    case Some(decodedAck: SoftAck) =>
                        val _ = assert(decodedAck.ackId == ack.ackId)
                        val _ = assert(decodedAck.blockNum == ack.blockNum)
                        val _ = assert(
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
    private def assertJsonStable(frame: HeadFrame): Unit = {
        val text = HeadFrame.encode(frame)
        HeadFrame.parse(text) match {
            case Right(decoded) =>
                val _ = assert(
                  HeadFrame.encode(decoded) == text,
                  s"re-encode differs:\n  first: $text\n  again: ${HeadFrame.encode(decoded)}"
                )
            case Left(err) => fail(s"HeadFrame.parse failed: $err\nText: $text")
        }
    }

    private def sig(bs: Int*): TxSignature = TxSignature(IArray.from(bs.map(_.toByte)))

    private def headHardAck(payload: HardAck.Payload): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Head(HeadPeerNumber(2)), HardAckNumber(9)),
          stackNum = StackNumber(6),
          payload = payload
        )

    private def hardAckFrame(payload: HardAck.Payload): HeadFrame =
        HeadFrame.Msg(emptyNew(BatchNumber(5)).copy(headHardAck = Some(headHardAck(payload))))

    test("HeadFrame.Msg(Mesh.New with HardAck Round1Regular: OnlyPartial) round-trips") {
        assertJsonStable(
          hardAckFrame(
            HardAck.Round1Payload.Regular.OnlyPartial(
              partial = HardAck.Round1Payload.PartitionSigs.FinalPartial(rollouts = Nil)
            )
          )
        )
    }

    test("HeadFrame.Msg(Mesh.New with HardAck Round1Regular: PartialThenCompletes) round-trips") {
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

    test("HeadFrame.Msg(Mesh.New with HardAck Round1Regular: MinorThenPartial) round-trips") {
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
      "HeadFrame.Msg(Mesh.New with HardAck Round1Regular: " +
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

    test("HeadFrame.Msg(Mesh.New with HardAck Round2Regular) round-trips") {
        assertJsonStable(hardAckFrame(HardAck.Round2Payload.Regular(firstUnlockSig = sig(20, 21))))
    }

    test("HeadFrame.Msg(Mesh.New with HardAck Round1Initial) round-trips") {
        assertJsonStable(hardAckFrame(HardAck.Round1Payload.Initial(fallbackSig = sig(30))))
    }

    test("HeadFrame.Msg(Mesh.New with HardAck Sole) round-trips") {
        assertJsonStable(
          hardAckFrame(
            HardAck.SolePayload(
              sec = BlockHeader.Minor.HeaderSignature(IArray[Byte](42.toByte)),
              refunds = List(sig(40, 41))
            )
          )
        )
    }

    test("HeadFrame.Msg(Mesh.New with a re-sequenced coil HardAckWithId) round-trips") {
        val hubAck = HardAckWithId(
          hubPeer = HeadPeerNumber(0),
          seqNum = HubHardAckNumber(3),
          ack = HardAck(
            ackId = HardAckId(PeerId.Coil(CoilPeerNumber(1)), HardAckNumber(2)),
            stackNum = StackNumber(4),
            payload = HardAck.Round1Payload.Initial(fallbackSig = sig(5, 6))
          )
        )
        assertJsonStable(HeadFrame.Msg(emptyNew(BatchNumber(7)).copy(hubHardAck = Some(hubAck))))
    }

    test("HeadFrame.fromWire accepts Mesh.Get and Mesh.New, rejects others") {
        val get = testMeshGet
        val nmb = emptyNew(BatchNumber.zero)

        val _ = assert(HeadFrame.fromWire(get).contains(get))
        val _ = assert(HeadFrame.fromWire(nmb).contains(nmb))
        assert(HeadFrame.fromWire(LiaisonProtocol.PreStart).isEmpty)
    }

    test("RequestId has one canonical JSON shape in both transport and ledger scopes (GUM-131)") {
        import io.circe.Json
        import io.circe.syntax.*
        val rid = RequestId(HeadPeerNumber(2), RequestNumber(7))
        val canonical = Json.obj("headPeerNumber" -> 2.asJson, "requestNumber" -> 7.asJson)

        // The transport scope (where NewMsgBatch.requests is derived) must resolve the same
        // RequestId codec as the ledger scope (where blockBrief events are derived) — there is no
        // transport-local codec anymore, so both fall through to the type's companion.
        val transportShape = {
            import hydrozoa.multisig.consensus.transport.Codecs.given
            rid.asJson
        }
        val ledgerShape = {
            import hydrozoa.multisig.ledger.event.RequestId.given
            rid.asJson
        }
        val _ = assert(transportShape == canonical, s"transport: $transportShape")
        val _ = assert(ledgerShape == canonical, s"ledger: $ledgerShape")
        assert(transportShape == ledgerShape)
    }
}
