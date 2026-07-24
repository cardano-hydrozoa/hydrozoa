package hydrozoa.multisig.server

import hydrozoa.lib.cardano.cip116
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{UserRequest, UserRequestBody}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.l2.{L2TxKind, L2TxSummary}
import hydrozoa.multisig.server.ApiResponse.RequestAccepted
import io.bullet.borer.Cbor
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.uplc.builtin.ByteString

/** JSON encoders and decoders for API types */
object JsonCodecs {

    import cip116.JsonCodecs.CIP0116.Conway.given

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

    // UserRequest cannot be encoded - we can only decode it, since the signatures are not needed
    // once the parsing is done.

    // 2Peter: is this an argument in favor of having separate types for encoding/decoding specifically?
    // UserRequestRaw that can be rounded tripped, with no checks, just well-formedness.
    // Then there is one way parseUserRequest :: UserRequestRaw -> Either[_, UserRequest]?

    //// UserRequest codec (generic) - uses "deposit" or "transaction" as field name
    // given Encoder[UserRequest] with {
    //  ...
    // }

    case class UserRequestDecoder() extends Decoder[UserRequest] {

        // The request is its body under a "deposit" or "transaction" field. Authentication is not
        // done here: the L2 payload is a native, self-authenticating tx, and the ledger's stateless
        // screening verifies its signatures before a RequestId is assigned.
        def apply(c: io.circe.HCursor): Decoder.Result[UserRequest] =
            c.downField("deposit")
                .as[DepositRequestBody]
                .map(UserRequest.DepositRequest(_))
                .orElse(
                  c.downField("transaction")
                      .as[TransactionRequestBody]
                      .map(UserRequest.TransactionRequest(_))
                )
    }

    given Decoder[UserRequest] = UserRequestDecoder()

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

    // Response types
    given requestAcceptedEncoder: Encoder[RequestAccepted] =
        (ra: RequestAccepted) => {
            import RequestId.i64.given
            Json.obj("requestId" -> ra.requestId.asJson)
        }

    given requestAcceptedDecoder: Decoder[RequestAccepted] = c => {
        import RequestId.i64.given
        c.downField("requestId").as[RequestId].map(RequestAccepted.apply)
    }

    // ---- L2 query endpoints (GET /l2/cardano-eutxo/utxos, /l2/cardano-eutxo/transactions) ----
    // CIP-0116-structured utxo encoding. `transactionInputEncoder` and `valueEncoder` come from the
    // CIP-0116 givens imported above; only the output object is assembled here, since CIP-0116
    // defines no `TransactionOutput` encoder.

    /** One L2 output as `{ address, value, datum }`: bech32 address, CIP-0116 value, and a tagged
      * datum — `{ "inline": <cbor-hex> }`, `{ "hash": <hex> }`, or `null` when absent — so a
      * consumer can tell an inline datum from a datum-hash reference (CIP-0116 keeps them
      * distinct).
      */
    private def encodeL2Output(output: TransactionOutput): Json =
        val addressJson = output.address match
            case shelley: ShelleyAddress =>
                Json.fromString(shelley.toBech32.getOrElse(shelley.toHex))
            case other => Json.fromString(other.toString)
        def datumHashJson(hash: DataHash): Json = Json.obj("hash" -> Json.fromString(hash.toHex))
        val datumJson = output match
            case TransactionOutput.Shelley(_, _, datumHash) =>
                datumHash.fold(Json.Null)(datumHashJson)
            case TransactionOutput.Babbage(_, _, datumOption, _) =>
                datumOption match
                    case Some(DatumOption.Inline(data)) =>
                        Json.obj(
                          "inline" -> Json.fromString(
                            ByteString.fromArray(Cbor.encode(data).toByteArray).toHex
                          )
                        )
                    case Some(DatumOption.Hash(hash)) => datumHashJson(hash)
                    case None                         => Json.Null
        Json.obj(
          "address" -> addressJson,
          "value" -> output.value.asJson,
          "datum" -> datumJson
        )

    given l2UtxoEncoder: Encoder[Utxo] =
        (utxo: Utxo) =>
            Json.obj(
              "input" -> utxo.input.asJson,
              "output" -> encodeL2Output(utxo.output)
            )

    given l2TxKindEncoder: Encoder[L2TxKind] =
        Encoder.encodeString.contramap {
            case L2TxKind.Transaction       => "transaction"
            case L2TxKind.DepositRegistered => "depositRegistered"
            case L2TxKind.DepositAbsorbed   => "depositAbsorbed"
            case L2TxKind.DepositRejected   => "depositRejected"
        }

    given l2TxSummaryEncoder: Encoder[L2TxSummary] =
        (summary: L2TxSummary) =>
            Json.obj(
              "requestId" -> summary.requestId.asJson,
              "blockNumber" -> summary.blockNumber.asJson,
              "kind" -> summary.kind.asJson
            )
}
