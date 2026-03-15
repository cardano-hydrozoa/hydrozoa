package hydrozoa.multisig.ledger.remote

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
object RemoteL2LedgerCodecs {

    // Reuse codecs from the HTTP server
    import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
    import hydrozoa.multisig.server.JsonCodecs.{requestIdEncoder, requestIdDecoder}

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

    // L2LedgerCommand codecs
    implicit val initializeEncoder: Encoder[L2LedgerCommand.Initialize] = deriveEncoder
    implicit val initializeDecoder: Decoder[L2LedgerCommand.Initialize] = deriveDecoder

    implicit val applyTransactionEncoder: Encoder[L2LedgerCommand.ApplyTransaction] = deriveEncoder
    implicit val applyTransactionDecoder: Decoder[L2LedgerCommand.ApplyTransaction] = deriveDecoder

    implicit val proxyBlockConfirmationEncoder: Encoder[L2LedgerCommand.ProxyBlockConfirmation] =
        deriveEncoder
    implicit val proxyBlockConfirmationDecoder: Decoder[L2LedgerCommand.ProxyBlockConfirmation] =
        deriveDecoder

    implicit val proxyRequestErrorEncoder: Encoder[L2LedgerCommand.ProxyRequestError] =
        deriveEncoder
    implicit val proxyRequestErrorDecoder: Decoder[L2LedgerCommand.ProxyRequestError] =
        deriveDecoder

    /** Destination as a cbor-encoded hex-string */
    implicit val destinationEncoder: Encoder[Destination] =
        Encoder.encodeString.contramap(destination => destination.toHex)
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

    implicit val depositRegistrationEncoder: Encoder[L2LedgerCommand.RegisterDeposit] =
        deriveEncoder
    implicit val depositRegistrationDecoder: Decoder[L2LedgerCommand.RegisterDeposit] =
        deriveDecoder

    implicit val depositDecisionsEncoder: Encoder[L2LedgerCommand.ApplyDepositDecisions] =
        deriveEncoder
    implicit val depositDecisionsDecoder: Decoder[L2LedgerCommand.ApplyDepositDecisions] =
        deriveDecoder

    // Request codecs
    implicit val requestEncoder: Encoder[Request] = {
        case Request.Initialize(event) =>
            io.circe.Json.obj("Initialize" -> event.asJson)
        case Request.RegisterDeposit(event) =>
            io.circe.Json.obj("RegisterDepositRequest" -> event.asJson)
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
                case "Initialize" =>
                    c.downField("Initialize")
                        .as[L2LedgerCommand.Initialize]
                        .map(Request.Initialize.apply)
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

    implicit val responseErrorEncoder: Encoder[Response.Error] = deriveEncoder
    implicit val responseErrorDecoder: Decoder[Response.Error] = deriveDecoder

    implicit val responseEncoder: Encoder[Response] = {
        case s: Response.Success => s.asJson
        case e: Response.Error   => e.asJson
    }

    implicit val responseDecoder: Decoder[Response] =
        responseSuccessDecoder.map(s => s: Response).or(responseErrorDecoder.map(e => e: Response))

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

    // KeepRaw[TransactionOutput] codec
    given Encoder[KeepRaw[TransactionOutput]] = Encoder.instance { kr =>
        io.circe.Json.obj("raw" -> byteStringEncoder(ByteString.fromArray(kr.raw)))
    }

    given Decoder[KeepRaw[TransactionOutput]] = Decoder.instance { c =>
        // TODO: Implement proper KeepRaw[TransactionOutput] decoding
        // This requires CBOR decoding of TransactionOutput from raw bytes
        Left(
          io.circe.DecodingFailure(
            "KeepRaw[TransactionOutput] decoding not implemented - requires CBOR deserialization",
            c.history
          )
        )
    }

    // EvacuationDiff codec
    given Encoder[EvacuationDiff] = Encoder.instance {
        case EvacuationDiff.Update(key, value) =>
            io.circe.Json.obj(
              "type" -> io.circe.Json.fromString("Update"),
              "key" -> key.asJson,
              "value" -> value.asJson
            )
        case EvacuationDiff.Delete(key) =>
            io.circe.Json.obj(
              "type" -> io.circe.Json.fromString("Delete"),
              "key" -> key.asJson
            )
    }

    given Decoder[EvacuationDiff] = Decoder.instance { c =>
        c.downField("type").as[String].flatMap {
            case "Update" =>
                for {
                    key <- c.downField("key").as[EvacuationKey]
                    value <- c.downField("value").as[KeepRaw[TransactionOutput]]
                } yield EvacuationDiff.Update(key, value)
            case "Delete" =>
                c.downField("key").as[EvacuationKey].map(EvacuationDiff.Delete.apply)
            case other =>
                Left(io.circe.DecodingFailure(s"Unknown EvacuationDiff type: $other", c.history))
        }
    }

    // Payout.Obligation codec
    given Encoder[Payout.Obligation] = Encoder.instance { po =>
        io.circe.Json.obj("utxo" -> po.utxo.asJson)
    }

    given Decoder[Payout.Obligation] = Decoder.instance { c =>
        c.downField("utxo").as[KeepRaw[TransactionOutput]].map(Payout.Obligation.apply)
    }

    // Unit codec
    implicit val unitEncoder: Encoder[Unit] = _ => io.circe.Json.obj()
    implicit val unitDecoder: Decoder[Unit] = _ => Right(())
}
