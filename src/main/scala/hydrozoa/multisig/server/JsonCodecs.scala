package hydrozoa.multisig.server

import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.lib.cardano.cip116
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.server.ApiResponse.{Error, HeadInfo, RequestAccepted}
import hydrozoa.multisig.server.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import io.circe.generic.semiauto.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.crypto.ed25519.{Signature, VerificationKey}
import scalus.uplc.builtin.ByteString

/** JSON encoders and decoders for API types */
object JsonCodecs {

    import cip116.JsonCodecs.CIP0116.Conway.given
    import hydrozoa.config.head.initialization.InitializationParameters.HeadId.{given Encoder[HeadId], given Decoder[HeadId]}

    //// Make TransactionInput codecs available as given instances
    // given Encoder[TransactionInput] = transactionInputEncoder
    // given Decoder[TransactionInput] = transactionInputDecoder

    // UserRequestBody codec (enum) - encoded as plain object without type discriminator
    given Encoder[UserRequestBody] = {
        case UserRequestBody.DepositRequestBody(l1Payload, l2Payload) =>
            Json.obj(
              "l1Payload" -> byteStringEncoder(l1Payload),
              "l2Payload" -> byteStringEncoder(l2Payload)
            )
        case UserRequestBody.TransactionRequestBody(l2Payload) =>
            Json.obj(
              "l2Payload" -> byteStringEncoder(l2Payload)
            )
    }

    // Decoder for UserRequestBody - attempts both variants
    given Decoder[UserRequestBody] = c =>
        // Try deposit first (has both l1Payload and l2Payload)
        (for {
            l1Payload <- c.downField("l1Payload").as[ByteString](using byteStringDecoder)
            l2Payload <- c.downField("l2Payload").as[ByteString](using byteStringDecoder)
        } yield UserRequestBody.DepositRequestBody(l1Payload, l2Payload))
            .orElse(
              // Try transaction (only l2Payload)
              for {
                  l2Payload <- c.downField("l2Payload").as[ByteString](using byteStringDecoder)
              } yield UserRequestBody.TransactionRequestBody(l2Payload)
            )

    // UserRequestHeader codec
    given Encoder[UserRequestHeader] =
        (header: UserRequestHeader) =>
            Json.obj(
              "headId" -> InitializationParameters.HeadId.given_Encoder_HeadId(header.headId),
              "validityStart" -> Json.fromBigInt(header.validityStart),
              "validityEnd" -> Json.fromBigInt(header.validityEnd),
              "bodyHash" -> summon[Encoder[Hash32]].apply(header.bodyHash)
            )

    given Decoder[UserRequestHeader] = c =>
        for {
            headId <- c
                .downField("headId")
                .as[HeadId](using InitializationParameters.HeadId.given_Decoder_HeadId)
            validityStart <- c.downField("validityStart").as[BigInt]
            validityEnd <- c.downField("validityEnd").as[BigInt]
            bodyHash <- c.downField("bodyHash").as[Hash32]
        } yield UserRequestHeader(headId, validityStart, validityEnd, bodyHash)

    // UserRequest codec (generic) - uses "deposit" or "transaction" as field name
    given Encoder[UserRequest] with {
        def apply(req: UserRequest): Json = {
            val bodyFieldName = req.body match {
                case _: UserRequestBody.DepositRequestBody     => "deposit"
                case _: UserRequestBody.TransactionRequestBody => "transaction"
            }
            Json.obj(
              "header" -> summon[Encoder[UserRequestHeader]].apply(req.header),
              bodyFieldName -> (if bodyFieldName == "deposit"
                                then
                                    summon[Encoder[DepositRequestBody]]
                                        .apply(req.body.asInstanceOf[DepositRequestBody])
                                else
                                    summon[Encoder[TransactionRequestBody]]
                                        .apply(req.body.asInstanceOf[TransactionRequestBody])),
              "userVk" -> summon[Encoder[VerificationKey]].apply(req.userVk),
              "signature" -> summon[Encoder[Signature]].apply(req.signature)
            )
        }
    }

    given Decoder[UserRequest] with {
        def apply(c: io.circe.HCursor): Decoder.Result[UserRequest] =
            for {
                header <- c.downField("header").as[UserRequestHeader]
                // Try both "deposit" and "transaction" fields
                body <- c
                    .downField("deposit")
                    .as[DepositRequestBody]
                    .orElse(c.downField("transaction").as[TransactionRequestBody])
                userVk <- c.downField("userVk").as[VerificationKey]
                signature <- c.downField("signature").as[Signature]
                // QUESTION: What exactly are these "ops"?
                userRequest <- body match {
                    case d: DepositRequestBody =>
                        UserRequest
                            .DepositRequest(header, d, userVk, signature)
                            .left
                            .map(e => DecodingFailure(e.getMessage, ops = List.empty))
                    case t: TransactionRequestBody =>
                        UserRequest
                            .TransactionRequest(header, t, userVk, signature)
                            .left
                            .map(e => DecodingFailure(e.getMessage, ops = List.empty))
                }

            } yield userRequest
    }

    // Specific body type encoders/decoders
    given Encoder[UserRequestBody.DepositRequestBody] =
        summon[Encoder[UserRequestBody]].contramap(identity)

    given Decoder[UserRequestBody.DepositRequestBody] =
        summon[Decoder[UserRequestBody]].emap {
            case body: UserRequestBody.DepositRequestBody => Right(body)
            case _                                        => Left("Expected DepositRequest")
        }

    given Encoder[UserRequestBody.TransactionRequestBody] =
        summon[Encoder[UserRequestBody]].contramap(identity)

    given Decoder[UserRequestBody.TransactionRequestBody] =
        summon[Decoder[UserRequestBody]].emap {
            case body: UserRequestBody.TransactionRequestBody => Right(body)
            case _                                            => Left("Expected TransactionRequest")
        }

    // RequestNumber codec
    given requestNumberEncoder: Encoder[RequestNumber] =
        Encoder.encodeInt.contramap(_.convert)

    given requestNumberDecoder: Decoder[RequestNumber] =
        Decoder.decodeInt.map(RequestNumber.apply)

    // HeadPeerNumber codec
    given headPeerNumberEncoder: Encoder[HeadPeerNumber] =
        Encoder.encodeInt.contramap(_.convert)

    given headPeerNumberDecoder: Decoder[HeadPeerNumber] =
        Decoder.decodeInt.map(HeadPeerNumber.apply)

    // RequestId codec
    given requestIdEncoder: Encoder[RequestId] = (requestId: RequestId) =>
        io.circe.Json.fromInt(requestId.asI64)

    given requestIdDecoder: Decoder[RequestId] =
        Decoder.decodeInt.map(RequestId.fromI64)

    // Response types
    given requestAcceptedEncoder: Encoder[RequestAccepted] = deriveEncoder[RequestAccepted]

//    given requestAcceptedDecoder: Decoder[RequestAccepted] = deriveDecoder[RequestAccepted]

    given errorEncoder: Encoder[Error] = deriveEncoder[Error]

    given errorDecoder: Decoder[Error] = deriveDecoder[Error]

    given headInfoEncoder: Encoder[HeadInfo] = deriveEncoder[HeadInfo]

    given headInfoDecoder: Decoder[HeadInfo] = deriveDecoder[HeadInfo]
}
