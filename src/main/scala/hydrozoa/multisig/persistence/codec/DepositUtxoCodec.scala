package hydrozoa.multisig.persistence.codec

import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{DepositAbsorptionEndTime, DepositAbsorptionStartTime, RequestValidityEndTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{byteStringDecoder, byteStringEncoder, coinDecoder, coinEncoder, shelleyAddressDecoder, shelleyAddressEncoder, transactionInputDecoder, transactionInputEncoder, valueDecoder, valueEncoder}
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import io.bullet.borer.Cbor
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scala.util.Try
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Coin, TransactionInput, Value}
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.{ByteString, Data}

/** Persistence-layer JSON codec for [[DepositUtxo]] and its nested [[DepositUtxo.Datum]].
  *
  * Most fields ride existing CIP-116 / time codecs. The `Datum` field is a `derives FromData,
  * ToData` Plutus type; we persist it as CBOR-hex of the on-chain `Data` form — the standard Plutus
  * round-trip pattern used elsewhere in the project.
  */
object DepositUtxoCodec:

    given datumEncoder: Encoder[DepositUtxo.Datum] = Encoder.instance { d =>
        // ToData[DepositUtxo.Datum] is derived on the type; produce the Plutus Data,
        // CBOR-encode it, and hex-encode the bytes.
        val data = toData(d)
        val cbor = Cbor.encode(data).toByteArray
        Json.fromString(ByteString.fromArray(cbor).toHex)
    }

    given datumDecoder: Decoder[DepositUtxo.Datum] = Decoder.decodeString.emap { hexStr =>
        Try {
            val bytes = ByteString.fromHex(hexStr).bytes
            val data = Cbor.decode(bytes).to[Data].value
            fromData[DepositUtxo.Datum](data)
        }.toEither.left.map(e => s"DepositUtxo.Datum decode failed: ${e.getMessage}")
    }

    given depositUtxoEncoder(using CardanoNetwork.Section): Encoder[DepositUtxo] =
        Encoder.instance { d =>
            Json.obj(
              "utxoId" -> d.utxoId.asJson,
              "address" -> d.address.asJson,
              "datum" -> d.datum.asJson,
              "value" -> d.value.asJson,
              "l2Payload" -> d.l2Payload.asJson,
              "depositFee" -> d.depositFee.asJson,
              "requestValidityEndTime" -> d.requestValidityEndTime.asJson,
              "absorptionStartTime" -> d.absorptionStartTime.asJson,
              "absorptionEndTime" -> d.absorptionEndTime.asJson
            )
        }

    given depositUtxoDecoder(using CardanoNetwork.Section): Decoder[DepositUtxo] =
        Decoder.instance { c =>
            for
                utxoId <- c.downField("utxoId").as[TransactionInput]
                address <- c.downField("address").as[ShelleyAddress]
                datum <- c.downField("datum").as[DepositUtxo.Datum]
                value <- c.downField("value").as[Value]
                l2Payload <- c.downField("l2Payload").as[ByteString]
                depositFee <- c.downField("depositFee").as[Coin]
                rvet <- c.downField("requestValidityEndTime").as[RequestValidityEndTime]
                ast <- c.downField("absorptionStartTime").as[DepositAbsorptionStartTime]
                aet <- c.downField("absorptionEndTime").as[DepositAbsorptionEndTime]
            yield DepositUtxo(
              utxoId = utxoId,
              address = address,
              datum = datum,
              value = value,
              l2Payload = l2Payload,
              depositFee = depositFee,
              requestValidityEndTime = rvet,
              absorptionStartTime = ast,
              absorptionEndTime = aet
            )
        }
