package hydrozoa.multisig.ledger.remote

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l2.given
import hydrozoa.multisig.ledger.l2.{Destination, L2LedgerCommand}
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger.{Request, Response}
import io.bullet.borer.Cbor
import io.circe.generic.semiauto
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import scala.util.Try
import scalus.cardano.ledger.{Coin, KeepRaw, TransactionInput, TransactionOutput, Value}
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.ByteString
import scodec.bits.ByteVector

/** JSON codecs for RemoteL2Ledger WebSocket protocol */
case class RemoteL2LedgerCodecs(config: CardanoNetwork.Section) {

    // Reuse codecs from the HTTP server, excluding types we override for sugar-rush-ledger compatibility
    // We exclude certain codecs here to provide sugar-rush-ledger compatible format
    import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{coinEncoder as _, coinDecoder as _, valueEncoder as _, valueDecoder as _, given}
    import hydrozoa.multisig.server.JsonCodecs.{requestIdEncoder, requestIdDecoder}

    // Coin as raw number (sugar-rush-ledger expects u64, not string)
    given Encoder[Coin] = Encoder.encodeLong.contramap(_.value)
    given Decoder[Coin] = Decoder.decodeLong.map(Coin.apply)

    // Value codec for sugar-rush-ledger format: {"assets": [{"asset": {"tag": "Ada"}, "value": N}]}
    // The old CIP-116 compatible codec is commented out below - we will need it in the future
    given Encoder[Value] = (v: Value) => {
        val adaEntry = io.circe.Json.obj(
          "asset" -> io.circe.Json.obj("tag" -> io.circe.Json.fromString("Ada")),
          "value" -> io.circe.Json.fromLong(v.coin.value)
        )
        // For now, only ADA is supported
        io.circe.Json.obj("assets" -> io.circe.Json.arr(adaEntry))
    }

    // @Peter: this is just missing
    given Decoder[Value] = Decoder.instance { c =>
        // Minimal: just extract ADA from the assets array
        Right(Value(Coin(0))) // TODO: implement if needed
    }

    // QuantizedInstant codec (simplified - loses SlotConfig context)
    // TODO: Include SlotConfig in serialization for proper reconstruction
    implicit val quantizedInstantEncoder: Encoder[QuantizedInstant] =
        Encoder.encodeLong.contramap(_.instant.toEpochMilli)

    implicit val quantizedInstantDecoder: Decoder[QuantizedInstant] =
        Decoder.decodeLong.map(_ =>
            throw new NotImplementedError("QuantizedInstant decoding requires SlotConfig context")
        )

    // BlockNumber codec
    implicit val blockNumberEncoder: Encoder[BlockNumber] =
        Encoder.encodeInt.contramap(_.convert)

    implicit val blockNumberDecoder: Decoder[BlockNumber] =
        Decoder.decodeInt.map(BlockNumber.apply)

    implicit val proxyBlockConfirmationEncoder: Encoder[L2LedgerCommand.ProxyBlockConfirmation] =
        deriveEncoder
    implicit val proxyBlockConfirmationDecoder: Decoder[L2LedgerCommand.ProxyBlockConfirmation] =
        deriveDecoder

    implicit val proxyRequestErrorEncoder: Encoder[L2LedgerCommand.ProxyRequestError] =
        deriveEncoder
    implicit val proxyRequestErrorDecoder: Decoder[L2LedgerCommand.ProxyRequestError] =
        deriveDecoder

    implicit val destinationEncoder: Encoder[Destination] =
        (dest: Destination) => {
            val addressBech32 = dest.address match {
                case s: scalus.cardano.address.ShelleyAddress => s.toBech32.get
                case other                                    => other.toString
            }
            io.circe.Json.obj(
              "address" -> io.circe.Json.fromString(addressBech32),
              "datum" -> io.circe.Json.Null
            )
        }
    implicit val destinationDecoder: Decoder[Destination] =
        Decoder.decodeString.emap(hexStr =>
            for {
                bytes <- ByteVector
                    .fromHex(hexStr)
                    .map(_.toArray)
                    .toRight(s"Invalid hex string: $hexStr")
                dest <- Try(Cbor.decode(bytes).to[Destination].value).toEither.left.map(e =>
                    s"Could not cbor-decode the bytes: ${e}"
                )
            } yield dest
        )
    // TODO: can be removed if we just rename the userVk field?
    implicit val depositRegistrationEncoder: Encoder[L2LedgerCommand.RegisterDeposit] =
        (r: L2LedgerCommand.RegisterDeposit) =>
            io.circe.Json.obj(
              "requestId" -> r.requestId.asJson,
              "userVk" -> summon[Encoder[VerificationKey]].apply(r.userVKey),
              "blockNumber" -> r.blockNumber.asJson,
              "blockCreationStartTime" -> r.blockCreationStartTime.asJson,
              "depositId" -> r.depositId.asJson,
              "depositFee" -> r.depositFee.asJson,
              "depositL2Value" -> r.depositL2Value.asJson,
              "refundDestination" -> r.refundDestination.asJson,
              "l2Payload" -> summon[Encoder[ByteString]].apply(r.l2Payload)
            )
    implicit val depositRegistrationDecoder: Decoder[L2LedgerCommand.RegisterDeposit] =
        deriveDecoder

    implicit val depositDecisionsEncoder: Encoder[L2LedgerCommand.ApplyDepositDecisions] =
        deriveEncoder
    implicit val depositDecisionsDecoder: Decoder[L2LedgerCommand.ApplyDepositDecisions] =
        deriveDecoder

    // TODO: can be removed if we just rename the userVk field?
    implicit val applyTransactionEncoder: Encoder[L2LedgerCommand.ApplyTransaction] =
        (r: L2LedgerCommand.ApplyTransaction) =>
            io.circe.Json.obj(
              "requestId" -> r.requestId.asJson,
              "userVk" -> summon[Encoder[VerificationKey]].apply(r.userVKey),
              "blockNumber" -> r.blockNumber.asJson,
              "blockCreationStartTime" -> r.blockCreationStartTime.asJson,
              "l2Payload" -> summon[Encoder[ByteString]].apply(r.l2Payload)
            )

    implicit val applyTransactionDecoder: Decoder[L2LedgerCommand.ApplyTransaction] =
        deriveDecoder

    // Request codecs
    implicit val requestEncoder: Encoder[Request] = {
        case Request.RegisterDeposit(event) =>
            io.circe.Json.obj("RegisterDeposit" -> event.asJson)
        case Request.ApplyDepositDecisions(event) =>
            io.circe.Json.obj("ApplyDepositDecisions" -> event.asJson)
        case Request.ApplyTransaction(event) =>
            io.circe.Json.obj("ApplyTransaction" -> event.asJson)
        case Request.ProxyBlockConfirmation(event) =>
            io.circe.Json.obj("ProxyBlockConfirmation" -> event.asJson)
        case Request.ProxyRequestError(event) =>
            io.circe.Json.obj("ProxyRequestError" -> event.asJson)
    }

    implicit val requestDecoder: Decoder[Request] = c =>
        c.keys
            .flatMap(_.headOption)
            .toRight(
              io.circe.DecodingFailure("Request must have exactly one field", c.history)
            )
            .flatMap {
                case "RegisterDepositRequest" =>
                    c.downField("RegisterDepositRequest")
                        .as[L2LedgerCommand.RegisterDeposit]
                        .map(Request.RegisterDeposit.apply)
                case "ApplyDepositDecisions" =>
                    c.downField("ApplyDepositDecisions")
                        .as[L2LedgerCommand.ApplyDepositDecisions]
                        .map(Request.ApplyDepositDecisions.apply)
                case "ApplyTransaction" =>
                    c.downField("ApplyTransaction")
                        .as[L2LedgerCommand.ApplyTransaction]
                        .map(Request.ApplyTransaction.apply)
                case "ProxyBlockConfirmation" =>
                    c.downField("ProxyBlockConfirmation")
                        .as[L2LedgerCommand.ProxyBlockConfirmation]
                        .map(Request.ProxyBlockConfirmation.apply)
                case "ProxyRequestError" =>
                    c.downField("ProxyRequestError")
                        .as[L2LedgerCommand.ProxyRequestError]
                        .map(Request.ProxyRequestError.apply)
                case other =>
                    Left(io.circe.DecodingFailure(s"Unknown request type: $other", c.history))
            }

    // Response codecs
    implicit val responseSuccessEncoder: Encoder[Response.Success] = deriveEncoder
    implicit val responseSuccessDecoder: Decoder[Response.Success] = deriveDecoder

    implicit val responseFailureEncoder: Encoder[Response.Failure] = deriveEncoder
    implicit val responseFailureDecoder: Decoder[Response.Failure] = deriveDecoder

    implicit val responseEncoder: Encoder[Response] = {
        case s: Response.Success => s.asJson
        case e: Response.Failure => e.asJson
    }

    implicit val responseDecoder: Decoder[Response] = Decoder.instance { c =>
        c.keys
            .flatMap(_.headOption)
            .toRight(
              io.circe.DecodingFailure("Response must have exactly one field", c.history)
            )
            .flatMap {
                case "Success" =>
                    c.downField("Success").as[Response.Success]
                case "Failure" =>
                    c.downField("Failure").as[Response.Failure]
                case other =>
                    Left(io.circe.DecodingFailure(s"Unknown response type: $other", c.history))
            }
    }

    // EvacuationKey codec
    import hydrozoa.multisig.ledger.joint.EvacuationKey

    given Encoder[EvacuationKey] = Encoder.instance { ek =>
        byteStringEncoder(ek.byteString)
    }

    given Decoder[EvacuationKey] = Decoder.instance { c =>
        byteStringDecoder(c).flatMap { bytes =>
            EvacuationKey(bytes) match {
                case Some(key) => Right(key)
                case None      => Left(io.circe.DecodingFailure("Invalid EvacuationKey", c.history))
            }
        }
    }

    given Encoder[KeepRaw[TransactionOutput]] = Encoder.instance { kr =>
        io.circe.Json.fromString(ByteString.fromArray(kr.raw).toHex)
    }

    given Decoder[KeepRaw[TransactionOutput]] = Decoder.instance { c =>
        c.as[String].flatMap { hexStr =>
            ByteString.fromHex(hexStr) match {
                case bs =>
                    // Decode CBOR bytes to TransactionOutput
                    Try(Cbor.decode(bs.bytes).to[TransactionOutput].value).toEither match {
                        case Right(txOut) => Right(KeepRaw(txOut))
                        case Left(e) =>
                            Left(
                              io.circe.DecodingFailure(
                                s"Failed to decode TransactionOutput from CBOR: ${e.getMessage}",
                                c.history
                              )
                            )
                    }
            }
        }
    }

    given Encoder[EvacuationDiff] = Encoder.instance {
        case EvacuationDiff.Update(key, value) =>
            io.circe.Json.obj(
              "tag" -> io.circe.Json.fromString("Update"),
              "key" -> key.asJson,
              "value" -> value.asJson
            )
        case EvacuationDiff.Delete(key) =>
            io.circe.Json.obj(
              "tag" -> io.circe.Json.fromString("Delete"),
              "key" -> key.asJson
            )
    }

    given evacuationDiffDecoder: Decoder[EvacuationDiff] = Decoder.instance { c =>
        c.downField("tag").as[String].flatMap {
            case "Update" =>
                for {
                    key <- c.downField("key").as[EvacuationKey]
                    value <- c.downField("value").as[Payout.Obligation]
                } yield EvacuationDiff.Update(key, value)
            case "Delete" =>
                c.downField("key").as[EvacuationKey].map(EvacuationDiff.Delete.apply)
            case other =>
                Left(io.circe.DecodingFailure(s"Unknown EvacuationDiff tag: $other", c.history))
        }
    }

    // Payout.Obligation codec
    // Encode directly as TransactionOutput (without "utxo" wrapper) for API compatibility
    given Encoder[Payout.Obligation] = Encoder.instance { po =>
        po.utxo.asJson
    }
    given payoutObligationDecoder: Decoder[Payout.Obligation] = Decoder.instance { c =>
        for {
            unvalidated <- c.as[KeepRaw[TransactionOutput]]
            value <- Payout
                .Obligation(unvalidated, config)
                .left
                .map(e => io.circe.DecodingFailure(e.toString, c.history))
        } yield value
    }

    // Unit codec
    implicit val unitEncoder: Encoder[Unit] = _ => io.circe.Json.obj()
    implicit val unitDecoder: Decoder[Unit] = _ => Right(())
}
