package hydrozoa.multisig.consensus.transport

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.multisig.consensus.PeerLiaison.Request.{GetMsgBatch, NewMsgBatch}
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.consensus.ack.{AckBlock, AckId, AckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{PeerLiaison, UserRequest, UserRequestBody, UserRequestHeader, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.l1.tx.TxSignature
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

    private given Codec[TxSignature] =
        io.circe.Codec.from(
          Decoder.decodeString.emap(s =>
              ByteVector
                  .fromHex(s)
                  .toRight(s"Invalid hex for TxSignature: $s")
                  .map(bv => TxSignature(IArray.from(bv.toArray)))
          ),
          Encoder.encodeString.contramap((sig: TxSignature) =>
              ByteVector(IArray.genericWrapArray(sig).toArray).toHex
          )
        )

    private given Codec[BlockHeader.Minor.HeaderSignature] =
        io.circe.Codec.from(
          Decoder.decodeString.emap(s =>
              ByteVector
                  .fromHex(s)
                  .toRight(s"Invalid hex for HeaderSignature: $s")
                  .map(bv => BlockHeader.Minor.HeaderSignature(IArray.from(bv.toArray)))
          ),
          Encoder.encodeString.contramap((sig: BlockHeader.Minor.HeaderSignature) =>
              ByteVector(IArray.genericWrapArray(sig).toArray).toHex
          )
        )

    // ---- AckBlock ADT ----

    private given Codec[AckBlock.Minor] = deriveCodec[AckBlock.Minor]
    private given Codec[AckBlock.Major1] = deriveCodec[AckBlock.Major1]
    private given Codec[AckBlock.Major2] = deriveCodec[AckBlock.Major2]
    private given Codec[AckBlock.Final1] = deriveCodec[AckBlock.Final1]
    private given Codec[AckBlock.Final2] = deriveCodec[AckBlock.Final2]

    given Codec[AckBlock] = {
        val enc: Encoder[AckBlock] = Encoder.instance {
            case x: AckBlock.Minor  => Json.obj("kind" -> "Minor".asJson, "v" -> x.asJson)
            case x: AckBlock.Major1 => Json.obj("kind" -> "Major1".asJson, "v" -> x.asJson)
            case x: AckBlock.Major2 => Json.obj("kind" -> "Major2".asJson, "v" -> x.asJson)
            case x: AckBlock.Final1 => Json.obj("kind" -> "Final1".asJson, "v" -> x.asJson)
            case x: AckBlock.Final2 => Json.obj("kind" -> "Final2".asJson, "v" -> x.asJson)
        }
        val dec: Decoder[AckBlock] = Decoder.instance(c =>
            c.downField("kind").as[String].flatMap {
                case "Minor"  => c.downField("v").as[AckBlock.Minor]
                case "Major1" => c.downField("v").as[AckBlock.Major1]
                case "Major2" => c.downField("v").as[AckBlock.Major2]
                case "Final1" => c.downField("v").as[AckBlock.Final1]
                case "Final2" => c.downField("v").as[AckBlock.Final2]
                case other    => Left(DecodingFailure(s"Unknown AckBlock kind: $other", c.history))
            }
        )
        io.circe.Codec.from(dec, enc)
    }

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
