package hydrozoa.multisig.ledger.remote

import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l2.L2LedgerCommand
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger.{Request, Response}
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import scalus.cardano.ledger.{Coin, KeepRaw, TransactionInput, TransactionOutput, Value}

/** JSON codecs for RemoteL2Ledger WebSocket protocol */
object RemoteL2LedgerCodecs {

    // Reuse codecs from the HTTP server
    import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
    import hydrozoa.multisig.server.JsonCodecs.{ledgerEventIdEncoder, ledgerEventIdDecoder}

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

    // L2LedgerEvent codecs
    implicit val l2EventEncoder: Encoder[L2LedgerCommand.ApplyTransactionRequest] = deriveEncoder
    implicit val l2EventDecoder: Decoder[L2LedgerCommand.ApplyTransactionRequest] = deriveDecoder

    implicit val depositRegistrationEncoder: Encoder[L2LedgerCommand.RegisterDepositRequest] =
        deriveEncoder
    implicit val depositRegistrationDecoder: Decoder[L2LedgerCommand.RegisterDepositRequest] =
        deriveDecoder

    implicit val depositDecisionsEncoder: Encoder[L2LedgerCommand.ApplyDepositDecisions] =
        deriveEncoder
    implicit val depositDecisionsDecoder: Decoder[L2LedgerCommand.ApplyDepositDecisions] =
        deriveDecoder

    // Request codecs
    implicit val requestEncoder: Encoder[Request] = {
        case Request.DepositRegistration(event) =>
            io.circe.Json.obj(
              "type" -> "DepositRegistration".asJson,
              "event" -> event.asJson
            )
        case Request.DepositDecisions(event) =>
            io.circe.Json.obj(
              "type" -> "DepositDecisions".asJson,
              "event" -> event.asJson
            )
        case Request.L2Event(event) =>
            io.circe.Json.obj(
              "type" -> "L2Event".asJson,
              "event" -> event.asJson
            )
    }

    implicit val requestDecoder: Decoder[Request] = c =>
        c.downField("type").as[String].flatMap {
            case "DepositRegistration" =>
                c.downField("event")
                    .as[L2LedgerCommand.RegisterDepositRequest]
                    .map(Request.DepositRegistration.apply)
            case "DepositDecisions" =>
                c.downField("event")
                    .as[L2LedgerCommand.ApplyDepositDecisions]
                    .map(Request.DepositDecisions.apply)
            case "L2Event" =>
                c.downField("event").as[L2LedgerCommand.ApplyTransactionRequest].map(Request.L2Event.apply)
            case other =>
                Left(io.circe.DecodingFailure(s"Unknown request type: $other", c.history))
        }

    implicit val requestEnvelopeEncoder: Encoder[Request.Envelope] = deriveEncoder
    implicit val requestEnvelopeDecoder: Decoder[Request.Envelope] = deriveDecoder

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

    implicit val responseEnvelopeEncoder: Encoder[Response.Envelope] = deriveEncoder
    implicit val responseEnvelopeDecoder: Decoder[Response.Envelope] = deriveDecoder

    // EvacuationKey codec
    import hydrozoa.multisig.ledger.joint.EvacuationKey

    given Encoder[EvacuationKey] = Encoder.instance { ek =>
        byteArrayEncoder(ek.bytes)
    }

    given Decoder[EvacuationKey] = Decoder.instance { c =>
        byteArrayDecoder(c).flatMap { bytes =>
            EvacuationKey(bytes) match {
                case Some(key) => Right(key)
                case None      => Left(io.circe.DecodingFailure("Invalid EvacuationKey", c.history))
            }
        }
    }

    // KeepRaw[TransactionOutput] codec
    given Encoder[KeepRaw[TransactionOutput]] = Encoder.instance { kr =>
        io.circe.Json.obj("raw" -> byteArrayEncoder(kr.raw))
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
