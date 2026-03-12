package hydrozoa.multisig.server

import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.lib.cardano.cip116
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.server.ApiResponse.{Error, HeadInfo, RequestAccepted}
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.crypto.ed25519.{Signature, VerificationKey}

/** JSON encoders and decoders for API types */
object JsonCodecs {

    import cip116.JsonCodecs.CIP0116.Conway.given
    import hydrozoa.config.head.initialization.InitializationParameters.HeadId.{given Encoder[HeadId], given Decoder[HeadId]}

    //// Make TransactionInput codecs available as given instances
    // given Encoder[TransactionInput] = transactionInputEncoder
    // given Decoder[TransactionInput] = transactionInputDecoder

    // Helper for accessing ApiRequest inner types
    val apiRequest = new ApiRequest()

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
        io.circe.Json.obj(
          "peerNum" -> requestId.peerNum.asJson,
          "requestNum" -> requestId.requestNum.asJson
        )

    given requestIdDecoder: Decoder[RequestId] = c =>
        for {
            peerNum <- c.downField("peerNum").as[HeadPeerNumber]
            requestNum <- c.downField("requestNum").as[RequestNumber]
        } yield RequestId(peerNum, requestNum)

    // Response types
    given requestAcceptedEncoder: Encoder[RequestAccepted] = deriveEncoder[RequestAccepted]

    given requestAcceptedDecoder: Decoder[RequestAccepted] = deriveDecoder[RequestAccepted]

    given errorEncoder: Encoder[Error] = deriveEncoder[Error]

    given errorDecoder: Decoder[Error] = deriveDecoder[Error]

    given headInfoEncoder: Encoder[HeadInfo] = deriveEncoder[HeadInfo]

    given headInfoDecoder: Decoder[HeadInfo] = deriveDecoder[HeadInfo]
}
