package hydrozoa.multisig.consensus.transport

import cats.data.NonEmptyList
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.multisig.consensus.PeerLiaison.Request.{GetMsgBatch, NewMsgBatch}
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, HubCoilAck, RelayedAck, SoftAck, SoftAckId, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{PeerLiaison, UserRequest, UserRequestBody, UserRequestHeader, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockHeader, BlockNumber}
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

    given Codec[SoftAckNumber] =
        io.circe.Codec.from(
          Decoder.decodeInt.map(SoftAckNumber.apply),
          Encoder.encodeInt.contramap((a: SoftAckNumber) => a.convert)
        )

    given Codec[PeerLiaison.Batch.Number] =
        io.circe.Codec.from(
          Decoder.decodeInt.map(PeerLiaison.Batch.Number.apply),
          Encoder.encodeInt.contramap((n: PeerLiaison.Batch.Number) => n: Int)
        )

    given Codec[SoftAckId] =
        io.circe.Codec.from(
          Decoder.instance(c =>
              for {
                  pn <- c.downField("peerNum").as[HeadPeerNumber]
                  an <- c.downField("ackNum").as[SoftAckNumber]
              } yield SoftAckId(pn, an)
          ),
          Encoder.instance((id: SoftAckId) =>
              Json.obj("peerNum" -> id.peerNum.asJson, "ackNum" -> id.ackNum.asJson)
          )
        )

    // TODO(GUM-131): unify RequestId JSON shape with the codec in
    //   `hydrozoa.multisig.ledger.event.RequestId` (which uses `headPeerNumber` /
    //   `requestNumber`). Both codecs are in scope at different derivation sites, so the same
    //   RequestId is serialized two different ways within a single NewMsgBatch:
    //     - NewMsgBatch.requests[i].requestId         → `{ "peerNum", "requestNum" }`  (here)
    //     - NewMsgBatch.blockBrief.body.events[i][0]  → `{ "headPeerNumber", "requestNumber" }`
    //   It currently round-trips correctly because each subtree uses a matching encoder/decoder
    //   pair, but it's confusing and brittle. Pick one shape and align both call sites.
    //   https://linear.app/gummiworm-labs/issue/GUM-131
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
    // StackBrief has its own derived Codec; here we add the opaque-type primitives and the
    // real discriminated HardAck wire codec — slow-side ack collection is live, so hard-acks
    // traverse the wire between peers.

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
                  pid <- c.downField("peerId").as[PeerId]
                  hn <- c.downField("hardAckNum").as[HardAckNumber]
              } yield HardAckId(pid, hn)
          ),
          Encoder.instance((id: HardAckId) =>
              Json.obj("peerId" -> id.peerId.asJson, "hardAckNum" -> id.hardAckNum.asJson)
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

    /** Per-partition round-1 slot — discriminated by `kind`. Five variants because the slot shape
      * varies by both partition kind (Major/Final/Minor — mirrors
      * [[hydrozoa.multisig.ledger.stack.PartitionEffects]]) AND by whether the partition is the
      * round-1 unlock partition for its stack (Major/MajorUnlock and Final/FinalUnlock; Minor is
      * never the unlock).
      */
    private given Codec[HardAck.Round1Payload.PartitionSigs] = {
        import HardAck.Round1Payload.PartitionSigs
        val enc = Encoder.instance[PartitionSigs] {
            case p: PartitionSigs.MajorComplete =>
                Json.obj(
                  "kind" -> "majorComplete".asJson,
                  "settlement" -> p.settlement.asJson,
                  "fallback" -> p.fallback.asJson,
                  "rollouts" -> p.rollouts.asJson,
                  "refunds" -> p.refunds.asJson,
                  "sec" -> p.sec.asJson
                )
            case p: PartitionSigs.MajorPartial =>
                Json.obj(
                  "kind" -> "majorPartial".asJson,
                  "fallback" -> p.fallback.asJson,
                  "rollouts" -> p.rollouts.asJson,
                  "refunds" -> p.refunds.asJson,
                  "sec" -> p.sec.asJson
                )
            case p: PartitionSigs.FinalComplete =>
                Json.obj(
                  "kind" -> "finalComplete".asJson,
                  "finalization" -> p.finalization.asJson,
                  "rollouts" -> p.rollouts.asJson
                )
            case p: PartitionSigs.FinalPartial =>
                Json.obj(
                  "kind" -> "finalPartial".asJson,
                  "rollouts" -> p.rollouts.asJson
                )
            case p: PartitionSigs.Minor =>
                Json.obj(
                  "kind" -> "minor".asJson,
                  "sec" -> p.sec.asJson,
                  "refunds" -> p.refunds.asJson
                )
        }
        val dec = Decoder.instance[PartitionSigs] { c =>
            c.downField("kind").as[String].flatMap {
                case "majorComplete" =>
                    for {
                        settlement <- c.downField("settlement").as[TxSignature]
                        fallback <- c.downField("fallback").as[TxSignature]
                        rollouts <- c.downField("rollouts").as[List[TxSignature]]
                        refunds <- c.downField("refunds").as[List[TxSignature]]
                        sec <- c.downField("sec").as[Option[BlockHeader.HeaderSignature]]
                    } yield PartitionSigs.MajorComplete(
                      settlement,
                      fallback,
                      rollouts,
                      refunds,
                      sec
                    )
                case "majorPartial" =>
                    for {
                        fallback <- c.downField("fallback").as[TxSignature]
                        rollouts <- c.downField("rollouts").as[List[TxSignature]]
                        refunds <- c.downField("refunds").as[List[TxSignature]]
                        sec <- c.downField("sec").as[Option[BlockHeader.HeaderSignature]]
                    } yield PartitionSigs.MajorPartial(fallback, rollouts, refunds, sec)
                case "finalComplete" =>
                    for {
                        finalization <- c.downField("finalization").as[TxSignature]
                        rollouts <- c.downField("rollouts").as[List[TxSignature]]
                    } yield PartitionSigs.FinalComplete(finalization, rollouts)
                case "finalPartial" =>
                    for {
                        rollouts <- c.downField("rollouts").as[List[TxSignature]]
                    } yield PartitionSigs.FinalPartial(rollouts)
                case "minor" =>
                    for {
                        sec <- c.downField("sec").as[BlockHeader.HeaderSignature]
                        refunds <- c.downField("refunds").as[List[TxSignature]]
                    } yield PartitionSigs.Minor(sec, refunds)
                case other =>
                    Left(
                      DecodingFailure(
                        s"Unknown HardAck.Round1Payload.PartitionSigs kind: $other",
                        c.history
                      )
                    )
            }
        }
        io.circe.Codec.from(dec, enc)
    }

    // ---- PartitionSigs subtype codecs (Partial / Complete / Minor) ----
    //
    // The PartitionSigs codec above already round-trips any concrete variant; these subtype codecs are
    // narrowing adapters used by `Round1Payload.Regular`'s field types (Partial / Complete /
    // Minor). At decode time they re-tag a successfully-decoded PartitionSigs as the expected subtype
    // and fail if the wire variant doesn't belong to that subtype family.

    private given Codec[HardAck.Round1Payload.PartitionSigs.Partial] = io.circe.Codec.from(
      summon[Decoder[HardAck.Round1Payload.PartitionSigs]].emap {
          case p: HardAck.Round1Payload.PartitionSigs.Partial => Right(p)
          case other =>
              Left(s"Expected PartitionSigs.Partial, got ${other.getClass.getSimpleName}")
      },
      summon[Encoder[HardAck.Round1Payload.PartitionSigs]].contramap(identity)
    )

    private given Codec[HardAck.Round1Payload.PartitionSigs.Complete] = io.circe.Codec.from(
      summon[Decoder[HardAck.Round1Payload.PartitionSigs]].emap {
          case c: HardAck.Round1Payload.PartitionSigs.Complete => Right(c)
          case other =>
              Left(s"Expected PartitionSigs.Complete, got ${other.getClass.getSimpleName}")
      },
      summon[Encoder[HardAck.Round1Payload.PartitionSigs]].contramap(identity)
    )

    private given Codec[HardAck.Round1Payload.PartitionSigs.Minor] = io.circe.Codec.from(
      summon[Decoder[HardAck.Round1Payload.PartitionSigs]].emap {
          case m: HardAck.Round1Payload.PartitionSigs.Minor => Right(m)
          case other =>
              Left(s"Expected PartitionSigs.Minor, got ${other.getClass.getSimpleName}")
      },
      summon[Encoder[HardAck.Round1Payload.PartitionSigs]].contramap(identity)
    )

    private given Codec[HardAck.Round1Payload.PartitionSigs.MajorPartial] = io.circe.Codec.from(
      summon[Decoder[HardAck.Round1Payload.PartitionSigs]].emap {
          case mp: HardAck.Round1Payload.PartitionSigs.MajorPartial => Right(mp)
          case other =>
              Left(s"Expected PartitionSigs.MajorPartial, got ${other.getClass.getSimpleName}")
      },
      summon[Encoder[HardAck.Round1Payload.PartitionSigs]].contramap(identity)
    )

    /** Round-1 regular payload — 4-way discriminated by `subKind` (one variant per partition-list
      * layout; see [[HardAck.Round1Payload.Regular]]).
      */
    private given Codec[HardAck.Round1Payload.Regular] = {
        import HardAck.Round1Payload.Regular as R
        val enc = Encoder.instance[R] {
            case r: R.OnlyPartial =>
                Json.obj(
                  "subKind" -> "onlyPartial".asJson,
                  "partial" -> r.partial.asJson
                )
            case r: R.PartialThenCompletes =>
                Json.obj(
                  "subKind" -> "partialThenCompletes".asJson,
                  "partial" -> r.partial.asJson,
                  "completes" -> r.completes.asJson
                )
            case r: R.MinorThenPartial =>
                Json.obj(
                  "subKind" -> "minorThenPartial".asJson,
                  "minor" -> r.minor.asJson,
                  "partial" -> r.partial.asJson
                )
            case r: R.MinorThenPartialThenCompletes =>
                Json.obj(
                  "subKind" -> "minorThenPartialThenCompletes".asJson,
                  "minor" -> r.minor.asJson,
                  "partial" -> r.partial.asJson,
                  "completes" -> r.completes.asJson
                )
        }
        val dec = Decoder.instance[R] { c =>
            c.downField("subKind").as[String].flatMap {
                case "onlyPartial" =>
                    for {
                        partial <- c
                            .downField("partial")
                            .as[HardAck.Round1Payload.PartitionSigs.Partial]
                    } yield R.OnlyPartial(partial)
                case "partialThenCompletes" =>
                    for {
                        partial <- c
                            .downField("partial")
                            .as[HardAck.Round1Payload.PartitionSigs.MajorPartial]
                        completes <- c
                            .downField("completes")
                            .as[NonEmptyList[HardAck.Round1Payload.PartitionSigs.Complete]]
                    } yield R.PartialThenCompletes(partial, completes)
                case "minorThenPartial" =>
                    for {
                        minor <- c.downField("minor").as[HardAck.Round1Payload.PartitionSigs.Minor]
                        partial <- c
                            .downField("partial")
                            .as[HardAck.Round1Payload.PartitionSigs.Partial]
                    } yield R.MinorThenPartial(minor, partial)
                case "minorThenPartialThenCompletes" =>
                    for {
                        minor <- c.downField("minor").as[HardAck.Round1Payload.PartitionSigs.Minor]
                        partial <- c
                            .downField("partial")
                            .as[HardAck.Round1Payload.PartitionSigs.MajorPartial]
                        completes <- c
                            .downField("completes")
                            .as[NonEmptyList[HardAck.Round1Payload.PartitionSigs.Complete]]
                    } yield R.MinorThenPartialThenCompletes(minor, partial, completes)
                case other =>
                    Left(
                      DecodingFailure(
                        s"Unknown HardAck.Round1Payload.Regular subKind: $other",
                        c.history
                      )
                    )
            }
        }
        io.circe.Codec.from(dec, enc)
    }

    /** Real `Codec[HardAck]`. Discriminated by a `kind` tag. All variants — Regular / Sole /
      * Round1Initial / Round2Initial — round-trip fully.
      */
    private given Codec[HardAck.Payload] = {
        import HardAck.{Round1Payload, Round2Payload, SolePayload}
        val enc = Encoder.instance[HardAck.Payload] {
            case p: Round1Payload.Regular =>
                Json.obj(
                  "kind" -> "round1Regular".asJson,
                  "regular" -> p.asJson
                )
            case p: Round1Payload.Initial =>
                Json.obj("kind" -> "round1Initial".asJson, "fallbackSig" -> p.fallbackSig.asJson)
            case p: Round2Payload.Regular =>
                Json.obj(
                  "kind" -> "round2Regular".asJson,
                  "firstUnlockSig" -> p.firstUnlockSig.asJson
                )
            case p: Round2Payload.Initial =>
                Json.obj(
                  "kind" -> "round2Initial".asJson,
                  "initTxSig" -> p.initTxSig.asJson,
                  "individualSig" -> p.individualSig.asJson
                )
            case p: SolePayload =>
                Json.obj(
                  "kind" -> "sole".asJson,
                  "sec" -> p.sec.asJson,
                  "refunds" -> p.refunds.asJson
                )
        }
        val dec = Decoder.instance[HardAck.Payload] { c =>
            c.downField("kind").as[String].flatMap {
                case "round1Regular" =>
                    c.downField("regular").as[Round1Payload.Regular]
                case "round1Initial" =>
                    c.downField("fallbackSig")
                        .as[TxSignature]
                        .map(Round1Payload.Initial.apply)
                case "round2Regular" =>
                    c.downField("firstUnlockSig")
                        .as[TxSignature]
                        .map(Round2Payload.Regular.apply)
                case "round2Initial" =>
                    for {
                        initTxSig <- c.downField("initTxSig").as[TxSignature]
                        individualSig <- c.downField("individualSig").as[Option[TxSignature]]
                    } yield Round2Payload.Initial(initTxSig, individualSig)
                case "sole" =>
                    for {
                        sec <- c.downField("sec").as[BlockHeader.HeaderSignature]
                        refunds <- c.downField("refunds").as[List[TxSignature]]
                    } yield SolePayload(sec, refunds)
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

    // ---- HubCoilAck ----

    private given Codec[HubCoilAck] = deriveCodec[HubCoilAck]

    // ---- RelayedAck (sealed Soft | Hard) ----

    private given Codec[RelayedAck.Soft] = deriveCodec[RelayedAck.Soft]
    private given Codec[RelayedAck.Hard] = deriveCodec[RelayedAck.Hard]
    private given Codec[RelayedAck] = deriveCodec[RelayedAck]

    // ---- GetMsgBatch / NewMsgBatch ----

    given Codec[GetMsgBatch] = deriveCodec[GetMsgBatch]

    given (using CardanoNetwork.Section): Codec[NewMsgBatch] = deriveCodec[NewMsgBatch]
}
