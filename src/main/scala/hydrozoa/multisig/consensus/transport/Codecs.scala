package hydrozoa.multisig.consensus.transport

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.multisig.consensus.PeerLiaison.Request.{GetMsgBatch, NewMsgBatch}
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.consensus.ack.{AckId, AckNumber, HardAck, HardAckId, HardAckNumber, SoftAck}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{PeerLiaison, UserRequest, UserRequestBody, UserRequestHeader, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.effects.{PartitionIndex, WithinPartitionIndex}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import scalus.crypto.ed25519.VerificationKey
import scodec.bits.ByteVector

/** JSON codecs for the wire-eligible subset of [[PeerLiaison.Request]].
  *
  * Only [[GetMsgBatch]] and [[NewMsgBatch]] are sent over the wire; all other request variants
  * (RemoteBroadcast, BlockConfirmed, PreStart) are local-only.
  *
  * Codecs are bundled here rather than spread across types because (a) several inner types are
  * opaque and have no public codecs, and (b) the existing codecs in
  * [[hydrozoa.multisig.server.JsonCodecs]] are tailored for the user-facing API (COSE-validated
  * UserRequest decoding) and don't round-trip cleanly between peers.
  */
object Codecs {

    // ---- Opaque-type primitives ----

    given Codec[AckNumber] =
        io.circe.Codec.from(
          Decoder.decodeInt.map(AckNumber.apply),
          Encoder.encodeInt.contramap((a: AckNumber) => a.convert)
        )

    given Codec[PeerLiaison.Batch.Number] =
        io.circe.Codec.from(
          Decoder.decodeInt.map(PeerLiaison.Batch.Number.apply),
          Encoder.encodeInt.contramap((n: PeerLiaison.Batch.Number) => n: Int)
        )

    given Codec[AckId] =
        io.circe.Codec.from(
          Decoder.instance(c =>
              for {
                  pn <- c.downField("peerNum").as[HeadPeerNumber]
                  an <- c.downField("ackNum").as[AckNumber]
              } yield AckId(pn, an)
          ),
          Encoder.instance((id: AckId) =>
              Json.obj("peerNum" -> id.peerNum.asJson, "ackNum" -> id.ackNum.asJson)
          )
        )

    // TODO: unify RequestId JSON shape with the codec in
    //   `hydrozoa.multisig.ledger.event.RequestId` (which uses `headPeerNumber` /
    //   `requestNumber`). Both codecs are in scope at different derivation sites, so the same
    //   RequestId is serialized two different ways within a single NewMsgBatch:
    //     - NewMsgBatch.requests[i].requestId         → `{ "peerNum", "requestNum" }`  (here)
    //     - NewMsgBatch.blockBrief.body.events[i][0]  → `{ "headPeerNumber", "requestNumber" }`
    //   It currently round-trips correctly because each subtree uses a matching encoder/decoder
    //   pair, but it's confusing and brittle. Pick one shape and align both call sites.
    given Codec[RequestId] =
        io.circe.Codec.from(
          Decoder.instance(c =>
              for {
                  pn <- c.downField("peerNum").as[HeadPeerNumber]
                  rn <- c.downField("requestNum").as[RequestNumber]
              } yield RequestId(pn, rn)
          ),
          Encoder.instance((id: RequestId) =>
              Json.obj("peerNum" -> id.peerNum.asJson, "requestNum" -> id.requestNum.asJson)
          )
        )

    private given Codec[BlockHeader.HeaderSignature] =
        io.circe.Codec.from(
          Decoder.decodeString.emap(s =>
              ByteVector
                  .fromHex(s)
                  .toRight(s"Invalid hex for HeaderSignature: $s")
                  .map(bv => BlockHeader.Minor.HeaderSignature(IArray.from(bv.toArray)))
          ),
          Encoder.encodeString.contramap((sig: BlockHeader.HeaderSignature) =>
              ByteVector(IArray.genericWrapArray(sig).toArray).toHex
          )
        )

    // ---- SoftAck ----

    given Codec[SoftAck] = deriveCodec[SoftAck]

    // ---- StackNumber / HardAckNumber / HardAckId / HardAck ----
    //
    // StackBrief has its own derived Codec; we just need primitive opaque-type codecs +
    // a placeholder for HardAck. Real HardAck wire codec lands when slow-side ack collection
    // is wired (M6 full); the auto-confirm stub never serializes a HardAck.

    given Codec[StackNumber] =
        io.circe.Codec.from(
          Decoder.decodeInt.map(StackNumber.apply),
          Encoder.encodeInt.contramap((s: StackNumber) => s.convert)
        )

    given Codec[HardAckNumber] =
        io.circe.Codec.from(
          Decoder.decodeInt.map(HardAckNumber.apply),
          Encoder.encodeInt.contramap((h: HardAckNumber) => h.convert)
        )

    given Codec[HardAckId] =
        io.circe.Codec.from(
          Decoder.instance(c =>
              for {
                  pn <- c.downField("peerNum").as[HeadPeerNumber]
                  hn <- c.downField("hardAckNum").as[HardAckNumber]
              } yield HardAckId(pn, hn)
          ),
          Encoder.instance((id: HardAckId) =>
              Json.obj("peerNum" -> id.peerNum.asJson, "hardAckNum" -> id.hardAckNum.asJson)
          )
        )

    private given Codec[TxSignature] =
        io.circe.Codec.from(
          Decoder.decodeString.emap(s =>
              ByteVector
                  .fromHex(s)
                  .toRight(s"Invalid hex for TxSignature: $s")
                  .map(bv => TxSignature(IArray.from(bv.toArray)))
          ),
          Encoder.encodeString.contramap((sig: TxSignature) =>
              ByteVector(IArray.genericWrapArray(sig.untagged).toArray).toHex
          )
        )

    /** Maps in the hard-ack payload are keyed by opaque [[PartitionIndex]] / tuple keys, which have
      * no `KeyEncoder`. Serialize them as a JSON array of `[key, value]` pairs (circe's automatic
      * tuple instances), independent of key shape.
      */
    private def entriesCodec[K: Encoder: Decoder, V: Encoder: Decoder]: Codec[Map[K, V]] =
        io.circe.Codec.from(
          Decoder.decodeList[(K, V)].map(_.toMap),
          Encoder.encodeList[(K, V)].contramap(_.toList)
        )

    private given partitionSigMapCodec: Codec[Map[PartitionIndex, TxSignature]] = entriesCodec
    private given withinPartitionSigMapCodec
        : Codec[Map[(PartitionIndex, WithinPartitionIndex), TxSignature]] = entriesCodec

    /** Real `Codec[HardAck]` (M6). Discriminated by a `kind` tag. Regular / Sole / Round1Initial
      * round-trip fully; `Round2Initial` (init-tx sig + per-peer `VKeyWitness` list) is the only
      * variant left unsupported on the wire — the initial-stack boot path (StackComposer
      * `Bootstrap`) is not wired yet, so a Round2Initial never reaches transport. It fails loudly
      * if one ever does, rather than silently mis-round-tripping.
      */
    private given Codec[HardAck.Payload] = {
        import HardAck.{Round1Payload, Round2Payload, SolePayload}
        val enc = Encoder.instance[HardAck.Payload] {
            case p: Round1Payload.Regular =>
                Json.obj(
                  "kind" -> "round1Regular".asJson,
                  "settlements" -> p.settlements.asJson,
                  "fallbacks" -> p.fallbacks.asJson,
                  "rollouts" -> p.rollouts.asJson,
                  "refunds" -> p.refunds.asJson,
                  "evacCommit" -> p.evacCommit.asJson,
                  "finalization" -> p.finalization.asJson
                )
            case p: Round1Payload.Initial =>
                Json.obj("kind" -> "round1Initial".asJson, "fallbackSig" -> p.fallbackSig.asJson)
            case p: Round2Payload.Regular =>
                Json.obj(
                  "kind" -> "round2Regular".asJson,
                  "firstUnlockSig" -> p.firstUnlockSig.asJson
                )
            case _: Round2Payload.Initial =>
                throw new IllegalStateException(
                  "HardAck.Round2Payload.Initial is not wire-supported " +
                      "(initial-stack boot path not wired)"
                )
            case p: SolePayload =>
                Json.obj(
                  "kind" -> "sole".asJson,
                  "refunds" -> p.refunds.asJson,
                  "evacCommit" -> p.evacCommit.asJson
                )
        }
        val dec = Decoder.instance[HardAck.Payload] { c =>
            c.downField("kind").as[String].flatMap {
                case "round1Regular" =>
                    for {
                        settlements <- c
                            .downField("settlements")
                            .as[Map[PartitionIndex, TxSignature]]
                        fallbacks <- c
                            .downField("fallbacks")
                            .as[Map[PartitionIndex, TxSignature]]
                        rollouts <- c
                            .downField("rollouts")
                            .as[Map[(PartitionIndex, WithinPartitionIndex), TxSignature]]
                        refunds <- c
                            .downField("refunds")
                            .as[Map[(PartitionIndex, WithinPartitionIndex), TxSignature]]
                        evacCommit <- c
                            .downField("evacCommit")
                            .as[Option[(BlockNumber, BlockHeader.HeaderSignature)]]
                        finalization <- c.downField("finalization").as[Option[TxSignature]]
                    } yield Round1Payload.Regular(
                      settlements,
                      fallbacks,
                      rollouts,
                      refunds,
                      evacCommit,
                      finalization
                    )
                case "round1Initial" =>
                    c.downField("fallbackSig")
                        .as[TxSignature]
                        .map(Round1Payload.Initial.apply)
                case "round2Regular" =>
                    c.downField("firstUnlockSig")
                        .as[TxSignature]
                        .map(Round2Payload.Regular.apply)
                case "round2Initial" =>
                    Left(
                      DecodingFailure(
                        "HardAck.Round2Payload.Initial is not wire-supported " +
                            "(initial-stack boot path not wired)",
                        c.history
                      )
                    )
                case "sole" =>
                    for {
                        refunds <- c
                            .downField("refunds")
                            .as[Map[(PartitionIndex, WithinPartitionIndex), TxSignature]]
                        evacCommit <- c
                            .downField("evacCommit")
                            .as[(BlockNumber, BlockHeader.HeaderSignature)]
                    } yield SolePayload(refunds, evacCommit)
                case other =>
                    Left(DecodingFailure(s"Unknown HardAck.Payload kind: $other", c.history))
            }
        }
        io.circe.Codec.from(dec, enc)
    }

    given Codec[HardAck] =
        io.circe.Codec.from(
          Decoder.instance(c =>
              for {
                  ackId <- c.downField("ackId").as[HardAckId]
                  stackNum <- c.downField("stackNum").as[StackNumber]
                  payload <- c.downField("payload").as[HardAck.Payload]
              } yield HardAck(ackId, stackNum, payload)
          ),
          Encoder.instance((a: HardAck) =>
              Json.obj(
                "ackId" -> a.ackId.asJson,
                "stackNum" -> a.stackNum.asJson,
                "payload" -> a.payload.asJson
              )
          )
        )

    // ---- UserRequestWithId ----
    //
    // Plain field-by-field encoding of UserRequest (header/body/userVk). The COSE
    // signature/validation flow in [[hydrozoa.multisig.server.JsonCodecs]] is intended for
    // the user-facing API, not peer-to-peer. Trust-by-network for v1.

    private given Codec[UserRequestBody.DepositRequestBody] =
        deriveCodec[UserRequestBody.DepositRequestBody]
    private given Codec[UserRequestBody.TransactionRequestBody] =
        deriveCodec[UserRequestBody.TransactionRequestBody]

    given (using CardanoNetwork.Section): Codec[UserRequestHeader] = {
        import hydrozoa.multisig.server.JsonCodecs.given
        io.circe.Codec.from(summon[Decoder[UserRequestHeader]], summon[Encoder[UserRequestHeader]])
    }

    given (using CardanoNetwork.Section): Codec[UserRequest.DepositRequest] = {
        val enc: Encoder[UserRequest.DepositRequest] = Encoder.instance(r =>
            Json.obj(
              "header" -> r.header.asJson,
              "body" -> r.body.asJson,
              "userVk" -> r.userVk.asJson
            )
        )
        val dec: Decoder[UserRequest.DepositRequest] = Decoder.instance(c =>
            for {
                h <- c.downField("header").as[UserRequestHeader]
                b <- c.downField("body").as[UserRequestBody.DepositRequestBody]
                vk <- c.downField("userVk").as[VerificationKey]
            } yield UserRequest.DepositRequest(h, b, vk)
        )
        io.circe.Codec.from(dec, enc)
    }

    given (using CardanoNetwork.Section): Codec[UserRequest.TransactionRequest] = {
        val enc: Encoder[UserRequest.TransactionRequest] = Encoder.instance(r =>
            Json.obj(
              "header" -> r.header.asJson,
              "body" -> r.body.asJson,
              "userVk" -> r.userVk.asJson
            )
        )
        val dec: Decoder[UserRequest.TransactionRequest] = Decoder.instance(c =>
            for {
                h <- c.downField("header").as[UserRequestHeader]
                b <- c.downField("body").as[UserRequestBody.TransactionRequestBody]
                vk <- c.downField("userVk").as[VerificationKey]
            } yield UserRequest.TransactionRequest(h, b, vk)
        )
        io.circe.Codec.from(dec, enc)
    }

    given (using CardanoNetwork.Section): Codec[UserRequestWithId] = {
        val enc: Encoder[UserRequestWithId] = Encoder.instance {
            case UserRequestWithId.DepositRequest(rid, r) =>
                Json.obj(
                  "kind" -> "Deposit".asJson,
                  "requestId" -> rid.asJson,
                  "request" -> r.asJson
                )
            case UserRequestWithId.TransactionRequest(rid, r) =>
                Json.obj(
                  "kind" -> "Transaction".asJson,
                  "requestId" -> rid.asJson,
                  "request" -> r.asJson
                )
        }
        val dec: Decoder[UserRequestWithId] = Decoder.instance(c =>
            for {
                kind <- c.downField("kind").as[String]
                rid <- c.downField("requestId").as[RequestId]
                out <- kind match {
                    case "Deposit" =>
                        c.downField("request")
                            .as[UserRequest.DepositRequest]
                            .map(r => UserRequestWithId.DepositRequest(rid, r))
                    case "Transaction" =>
                        c.downField("request")
                            .as[UserRequest.TransactionRequest]
                            .map(r => UserRequestWithId.TransactionRequest(rid, r))
                    case other =>
                        Left(
                          DecodingFailure(s"Unknown UserRequestWithId kind: $other", c.history)
                        )
                }
            } yield out
        )
        io.circe.Codec.from(dec, enc)
    }

    // ---- BlockBrief.Next ----
    //
    // BlockBrief already has a derived Codec[BlockBrief]; restrict to the wire-eligible
    // subset (Minor | Major | Final) on decode.

    given (using CardanoNetwork.Section): Codec[BlockBrief.Next] = {
        val enc: Encoder[BlockBrief.Next] = Encoder.instance {
            case x: BlockBrief.Minor => Json.obj("kind" -> "Minor".asJson, "v" -> x.asJson)
            case x: BlockBrief.Major => Json.obj("kind" -> "Major".asJson, "v" -> x.asJson)
            case x: BlockBrief.Final => Json.obj("kind" -> "Final".asJson, "v" -> x.asJson)
        }
        val dec: Decoder[BlockBrief.Next] = Decoder.instance(c =>
            c.downField("kind").as[String].flatMap {
                case "Minor" =>
                    c.downField("v").as[BlockBrief.Minor]: Decoder.Result[BlockBrief.Next]
                case "Major" => c.downField("v").as[BlockBrief.Major]
                case "Final" => c.downField("v").as[BlockBrief.Final]
                case other =>
                    Left(DecodingFailure(s"Unknown BlockBrief.Next kind: $other", c.history))
            }
        )
        io.circe.Codec.from(dec, enc)
    }

    // ---- GetMsgBatch / NewMsgBatch ----

    given Codec[GetMsgBatch] = deriveCodec[GetMsgBatch]

    given (using CardanoNetwork.Section): Codec[NewMsgBatch] = deriveCodec[NewMsgBatch]
}
