package hydrozoa.multisig.server

import hydrozoa.lib.cardano.cip116
import hydrozoa.multisig.consensus.EventSequencer.{DepositRequest as EventSeqDepositRequest, L2TxRequest}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.{LedgerEventId, LedgerEventNumber}
import hydrozoa.multisig.server.ApiResponse.{Error, RequestAccepted}
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.cardano.ledger.*
import scalus.crypto.ed25519.{Signature, VerificationKey}
import scalus.uplc.builtin.ByteString
import scodec.bits.ByteVector

/** JSON encoders and decoders for API types */
object JsonCodecs {

    import cip116.JsonCodecs.CIP0116.Conway.{byteArrayEncoder, byteArrayDecoder, coinEncoder, coinDecoder, valueEncoder, valueDecoder}

    // Helper for accessing ApiRequest inner types
    val apiRequest = new ApiRequest()

    // Scalus VerificationKey codec (32 bytes as hex)
    given Encoder[VerificationKey] =
        Encoder.encodeString.contramap(vk => ByteVector(vk.bytes.toArray).toHex)

    given Decoder[VerificationKey] =
        Decoder.decodeString.emap { hexStr =>
            ByteVector
                .fromHex(hexStr)
                .toRight(s"Invalid hex string for verification key: $hexStr")
                .flatMap { bv =>
                    if bv.size == 32 then
                        Right(
                          VerificationKey.unsafeFromByteString(ByteString.fromArray(bv.toArray))
                        )
                    else Left(s"Verification key must be 32 bytes, got ${bv.size}")
                }
        }

    // Scalus Signature codec (64 bytes as hex)
    given Encoder[Signature] =
        Encoder.encodeString.contramap(sig => ByteVector(sig.bytes.toArray).toHex)

    given Decoder[Signature] =
        Decoder.decodeString.emap { hexStr =>
            ByteVector
                .fromHex(hexStr)
                .toRight(s"Invalid hex string for signature: $hexStr")
                .flatMap { bv =>
                    if bv.size == 64 then
                        Right(Signature.unsafeFromByteString(ByteString.fromArray(bv.toArray)))
                    else Left(s"Signature must be 64 bytes, got ${bv.size}")
                }
        }

    // Hash32 codec (32 bytes as hex) - Hash32 is Hash[Blake2b_256, Any]
    given Encoder[Hash32] =
        Encoder.encodeString.contramap(hash => ByteVector(hash.bytes.toArray).toHex)

    given Decoder[Hash32] =
        Decoder.decodeString.emap { hexStr =>
            ByteVector
                .fromHex(hexStr)
                .toRight(s"Invalid hex string for hash: $hexStr")
                .flatMap { bv =>
                    if bv.size == 32 then
                        import scalus.cardano.ledger.{Blake2b_256, Hash}
                        Right(Hash[Blake2b_256, Any](ByteString.fromArray(bv.toArray)))
                    else Left(s"Hash32 must be 32 bytes, got ${bv.size}")
                }
        }

    // AssetName codec (as plain value, not key)
    given assetNameValueEncoder: Encoder[AssetName] =
        Encoder.encodeString.contramap(assetName => assetName.bytes.toHex)

    given assetNameValueDecoder: Decoder[AssetName] =
        Decoder.decodeString.map(s => AssetName.fromHex(s))

    // UserRequestBody codec (enum) - encoded as plain object without type discriminator
    given Encoder[apiRequest.UserRequestBody] = {
        case apiRequest.UserRequestBody.DepositRequestBody(l1Payload, l2Payload) =>
            Json.obj(
              "l1Payload" -> byteArrayEncoder(l1Payload),
              "l2Payload" -> byteArrayEncoder(l2Payload)
            )
        case apiRequest.UserRequestBody.TransactionRequestBody(l2Payload) =>
            Json.obj(
              "l2Payload" -> byteArrayEncoder(l2Payload)
            )
    }

    // Decoder for UserRequestBody - attempts both variants
    given Decoder[apiRequest.UserRequestBody] = c =>
        // Try deposit first (has both l1Payload and l2Payload)
        (for {
            l1Payload <- c.downField("l1Payload").as[Array[Byte]](using byteArrayDecoder)
            l2Payload <- c.downField("l2Payload").as[Array[Byte]](using byteArrayDecoder)
        } yield apiRequest.UserRequestBody.DepositRequestBody(l1Payload, l2Payload))
            .orElse(
              // Try transaction (only l2Payload)
              for {
                  l2Payload <- c.downField("l2Payload").as[Array[Byte]](using byteArrayDecoder)
              } yield apiRequest.UserRequestBody.TransactionRequestBody(l2Payload)
            )

    // UserRequestHeader codec
    given Encoder[apiRequest.UserRequestHeader] =
        (header: apiRequest.UserRequestHeader) =>
            Json.obj(
              "headId" -> assetNameValueEncoder(header.headId),
              "validityStart" -> Json.fromBigInt(header.validityStart),
              "validityEnd" -> Json.fromBigInt(header.validityEnd),
              "bodyHash" -> summon[Encoder[Hash32]].apply(header.bodyHash)
            )

    given Decoder[apiRequest.UserRequestHeader] = c =>
        for {
            headId <- c.downField("headId").as[AssetName](using assetNameValueDecoder)
            validityStart <- c.downField("validityStart").as[BigInt]
            validityEnd <- c.downField("validityEnd").as[BigInt]
            bodyHash <- c.downField("bodyHash").as[Hash32]
        } yield apiRequest.UserRequestHeader(headId, validityStart, validityEnd, bodyHash)

    // UserRequest codec (generic) - uses "deposit" or "transaction" as field name
    given [Body <: apiRequest.UserRequestBody: Encoder: Decoder]
        : Encoder[apiRequest.UserRequest[Body]] with {
        def apply(req: apiRequest.UserRequest[Body]): Json = {
            val bodyFieldName = req.body match {
                case _: apiRequest.UserRequestBody.DepositRequestBody     => "deposit"
                case _: apiRequest.UserRequestBody.TransactionRequestBody => "transaction"
            }
            Json.obj(
              "header" -> summon[Encoder[apiRequest.UserRequestHeader]].apply(req.header),
              bodyFieldName -> summon[Encoder[Body]].apply(req.body),
              "userVk" -> summon[Encoder[VerificationKey]].apply(req.userVk),
              "signature" -> summon[Encoder[Signature]].apply(req.signature)
            )
        }
    }

    given [Body <: apiRequest.UserRequestBody: Encoder: Decoder]
        : Decoder[apiRequest.UserRequest[Body]] with {
        def apply(c: io.circe.HCursor): Decoder.Result[apiRequest.UserRequest[Body]] =
            for {
                header <- c.downField("header").as[apiRequest.UserRequestHeader]
                // Try both "deposit" and "transaction" fields
                body <- c
                    .downField("deposit")
                    .as[Body]
                    .orElse(c.downField("transaction").as[Body])
                userVk <- c.downField("userVk").as[VerificationKey]
                signature <- c.downField("signature").as[Signature]
            } yield apiRequest.UserRequest(header, body, userVk, signature)
    }

    // Specific body type encoders/decoders
    given Encoder[apiRequest.UserRequestBody.DepositRequestBody] =
        summon[Encoder[apiRequest.UserRequestBody]].contramap(identity)

    given Decoder[apiRequest.UserRequestBody.DepositRequestBody] =
        summon[Decoder[apiRequest.UserRequestBody]].emap {
            case body: apiRequest.UserRequestBody.DepositRequestBody => Right(body)
            case _ => Left("Expected DepositRequestBody")
        }

    given Encoder[apiRequest.UserRequestBody.TransactionRequestBody] =
        summon[Encoder[apiRequest.UserRequestBody]].contramap(identity)

    given Decoder[apiRequest.UserRequestBody.TransactionRequestBody] =
        summon[Decoder[apiRequest.UserRequestBody]].emap {
            case body: apiRequest.UserRequestBody.TransactionRequestBody => Right(body)
            case _ => Left("Expected TransactionRequestBody")
        }

    // Type aliases for specific request types
    type DepositRequestBody = apiRequest.UserRequestBody.DepositRequestBody
    type TransactionRequestBody = apiRequest.UserRequestBody.TransactionRequestBody
    type DepositRequest = apiRequest.DepositRequest
    type TransactionRequest = apiRequest.TransactionRequest

    // LedgerEventNumber codec
    given ledgerEventNumberEncoder: Encoder[LedgerEventNumber] =
        Encoder.encodeInt.contramap(_.convert)

    given ledgerEventNumberDecoder: Decoder[LedgerEventNumber] =
        Decoder.decodeInt.map(LedgerEventNumber.apply)

    // HeadPeerNumber codec
    given headPeerNumberEncoder: Encoder[HeadPeerNumber] =
        Encoder.encodeInt.contramap(_.convert)

    given headPeerNumberDecoder: Decoder[HeadPeerNumber] =
        Decoder.decodeInt.map(HeadPeerNumber.apply)

    // LedgerEventId codec
    given ledgerEventIdEncoder: Encoder[LedgerEventId] = (eventId: LedgerEventId) =>
        io.circe.Json.obj(
          "peerNum" -> eventId.peerNum.asJson,
          "requestNum" -> eventId.eventNum.asJson
        )

    given ledgerEventIdDecoder: Decoder[LedgerEventId] = c =>
        for {
            peerNum <- c.downField("peerNum").as[HeadPeerNumber]
            requestNum <- c.downField("requestNum").as[LedgerEventNumber]
        } yield LedgerEventId(peerNum, requestNum)

    // EventSequencer request types (old)
    given l2TxRequestDecoder: Decoder[L2TxRequest] = deriveDecoder[L2TxRequest]

    given l2TxRequestEncoder: Encoder[L2TxRequest] = deriveEncoder[L2TxRequest]

    given eventSeqDepositRequestDecoder: Decoder[EventSeqDepositRequest] =
        deriveDecoder[EventSeqDepositRequest]

    given eventSeqDepositRequestEncoder: Encoder[EventSeqDepositRequest] =
        deriveEncoder[EventSeqDepositRequest]

    // Response types
    given requestAcceptedEncoder: Encoder[RequestAccepted] = deriveEncoder[RequestAccepted]

    given requestAcceptedDecoder: Decoder[RequestAccepted] = deriveDecoder[RequestAccepted]

    given errorEncoder: Encoder[Error] = deriveEncoder[Error]

    given errorDecoder: Decoder[Error] = deriveDecoder[Error]
}
