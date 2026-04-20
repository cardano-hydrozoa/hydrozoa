package hydrozoa.multisig.server

import com.bloxbean.cardano.client.cip.cip30.{CIP30DataSigner, DataSignature}
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.lib.cardano.cip116
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{RequestValidityEndTimeRaw, RequestValidityStartTimeRaw, UserRequest, UserRequestBody, UserRequestHeader}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.server.ApiResponse.{Error, HeadInfo, RequestAccepted}
import io.circe.generic.semiauto.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import scala.util.Try
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.ByteString

/** JSON encoders and decoders for API types */
object JsonCodecs {

    import cip116.JsonCodecs.CIP0116.Conway.given
    import hydrozoa.config.head.initialization.InitializationParameters.HeadId.{given Decoder[HeadId], given Encoder[HeadId]}

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
              "validityStart" -> Json.fromLong(header.validityStart.toLong),
              "validityEnd" -> Json.fromLong(header.validityEnd.toLong),
              "bodyHash" -> summon[Encoder[Hash32]].apply(header.bodyHash)
            )

    given Decoder[UserRequestHeader] = c =>
        for {
            headId <- c
                .downField("headId")
                .as[HeadId](using InitializationParameters.HeadId.given_Decoder_HeadId)
            validityStart <- c.downField("validityStart").as[Long]
            validityEnd <- c.downField("validityEnd").as[Long]
            bodyHash <- c.downField("bodyHash").as[Hash32]
        } yield UserRequestHeader(
          headId,
          RequestValidityStartTimeRaw(validityStart),
          RequestValidityEndTimeRaw(validityEnd),
          bodyHash
        )

    // UserRequest cannot be encoded - we can only decode it, since the signatures are not needed
    // once the parsing is done.

    // 2Peter: is this an argument in favor of having separate types for encoding/decoding specifically?
    // UserRequestRaw that can be rounded tripped, with no checks, just well-formedness.
    // Then there is one way parseUserRequest :: UserRequestRaw -> Either[_, UserRequest]?

    //// UserRequest codec (generic) - uses "deposit" or "transaction" as field name
    // given Encoder[UserRequest] with {
    //  ...
    // }

    object UserRequestDecoder extends Decoder[UserRequest] {

        object Error {
            trait ValidationError extends Throwable {
                override def getMessage: String = toString
            }

            /** The [[UserRequestHeader.body]] does not match the [[blake2b_256]] hash of the
              * [[UserRequestBody]]
              */
            case object BodyHashMismatch extends ValidationError {
                override def toString: String = "Body hash mismatch"
            }

            /** The COSE signature of the header does not match
              */
            case object SignatureMismatch extends ValidationError {
                override def toString: String = "Signature mismatch"
            }

            case object VerificationKeyParsingFailure extends ValidationError {
                override def toString: String = "Verification key parsing failure"
            }

            case object WrongPayload extends ValidationError {
                override def toString: String = "Signed payload should match the request header"
            }
        }

        /** Validates COSE signature.
          *
          * @param bodyHash
          * @param coseKeyCborHex
          * @param coseSignatureCborHex
          * @return
          *   the verification key and signed payload from the COSEKey/COSESignature if valid, an
          *   error otherwise
          */
        def validateCoseSignature(
            coseKeyCborHex: String,
            coseSignatureCborHex: String
        ): Either[Error.ValidationError, (VerificationKey, ByteString)] = {
            val bbDataSignature = DataSignature(coseSignatureCborHex, coseKeyCborHex)
            for {
                // Verify the signature
                _ <- Either.cond(
                  CIP30DataSigner.INSTANCE.verify(bbDataSignature),
                  (),
                  Error.SignatureMismatch
                )
                // Extract the public key from COSE key parameter -2 (x-coordinate for OKP/Ed25519 keys)
                vKey <- Try(
                  VerificationKey.unsafeFromArray(bbDataSignature.coseKey().otherHeaderAsBytes(-2))
                ).toEither.left.map(_ => Error.VerificationKeyParsingFailure)
                payload = ByteString.fromArray(bbDataSignature.coseSign1().payload())
            } yield (vKey, payload)
        }

        def apply(c: io.circe.HCursor): Decoder.Result[UserRequest] =
            for {
                // QUESTION: What exactly are these "ops" in DecodingFailure cons?
                header <- c.downField("header").as[UserRequestHeader]
                // Try both "deposit" and "transaction" fields
                body <- c
                    .downField("deposit")
                    .as[DepositRequestBody]
                    .orElse(c.downField("transaction").as[TransactionRequestBody])
                // Check body hash
                _ <- Either.cond(
                  body.hash == header.bodyHash,
                  (),
                  DecodingFailure(Error.BodyHashMismatch.getMessage, ops = List.empty)
                )
                // Validate the COSE signature
                coseKeyCborHex <- c.downField("coseKey").as[String]
                cosedSignatureCborHex <- c.downField("coseSignature").as[String]
                ret <- validateCoseSignature(
                  coseKeyCborHex,
                  cosedSignatureCborHex
                ).left
                    .map(e => DecodingFailure(e.getMessage, ops = List.empty))
                (vKey, payload) = ret
                // Check that payload is actually the serialized header from the request
                payloadHeader <- io.circe.parser
                    .decode[UserRequestHeader](
                      new String(payload.bytes, java.nio.charset.StandardCharsets.UTF_8)
                    )
                    .left
                    .map(e =>
                        DecodingFailure(
                          s"Failed to parse payload as UserRequestHeader: ${e.getMessage}",
                          ops = List.empty
                        )
                    )
                _ <- Either.cond(
                  payloadHeader == header,
                  (),
                  DecodingFailure(Error.WrongPayload.getMessage, ops = List.empty)
                )
                // Construct the result
                userRequest = body match {
                    case d: DepositRequestBody =>
                        UserRequest
                            .DepositRequest(header, d, vKey)
                    case t: TransactionRequestBody =>
                        UserRequest
                            .TransactionRequest(header, t, vKey)
                }

            } yield userRequest
    }

    given Decoder[UserRequest] = UserRequestDecoder

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
        Encoder.encodeLong.contramap(_.convert)

    given requestNumberDecoder: Decoder[RequestNumber] =
        Decoder.decodeLong.map(RequestNumber.apply)

    // HeadPeerNumber codec
    given headPeerNumberEncoder: Encoder[HeadPeerNumber] =
        Encoder.encodeInt.contramap(_.convert)

    given headPeerNumberDecoder: Decoder[HeadPeerNumber] =
        Decoder.decodeInt.map(HeadPeerNumber.apply)

    // RequestId codec
    given requestIdEncoder: Encoder[RequestId] = (requestId: RequestId) =>
        io.circe.Json.fromLong(requestId.asI64)

    given requestIdDecoder: Decoder[RequestId] =
        Decoder.decodeLong.map(RequestId.fromI64)

    // Response types
    given requestAcceptedEncoder: Encoder[RequestAccepted] = deriveEncoder[RequestAccepted]

//    given requestAcceptedDecoder: Decoder[RequestAccepted] = deriveDecoder[RequestAccepted]

    given errorEncoder: Encoder[Error] = deriveEncoder[Error]

    given errorDecoder: Decoder[Error] = deriveDecoder[Error]

    // CardanoNativeToken codec - serialized as "{policyIdHex}.{assetNameHex}"
    given cardanoNativeTokenEncoder: Encoder[ApiResponse.CardanoNativeToken] =
        (token: ApiResponse.CardanoNativeToken) => {
            val policyIdHex = token.policyId.toHex
            val assetNameHex = token.tokenName.bytes.toHex
            Json.fromString(s"$policyIdHex.$assetNameHex")
        }

    given cardanoNativeTokenDecoder: Decoder[ApiResponse.CardanoNativeToken] =
        Decoder.decodeString.emap { str =>
            str.split('.') match {
                case Array(policyIdHex, assetNameHex) =>
                    scala.util
                        .Try {
                            val policyIdBytes = ByteString.fromHex(policyIdHex)
                            val assetNameBytes = ByteString.fromHex(assetNameHex)
                            val policyId = ScriptHash.fromByteString(policyIdBytes)
                            val assetName = AssetName(assetNameBytes)
                            ApiResponse.CardanoNativeToken(policyId, assetName)
                        }
                        .toEither
                        .left
                        .map(e => s"Failed to decode CardanoNativeToken: ${e.getMessage}")
                case _ =>
                    Left(
                      s"Invalid CardanoNativeToken format, expected '{policyIdHex}.{assetNameHex}', got: $str"
                    )
            }
        }

    given headInfoEncoder: Encoder[HeadInfo] = deriveEncoder[HeadInfo]

    given headInfoDecoder: Decoder[HeadInfo] = deriveDecoder[HeadInfo]
}
