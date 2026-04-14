package hydrozoa.lib.cardano.scalus.codecs.json

import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import io.bullet.borer.Cbor
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, Json, KeyDecoder, KeyEncoder, parser}
import scala.util.Try
import scalus.cardano.address.Network
import scalus.cardano.ledger.{CardanoInfo, ProtocolParams, SlotConfig, Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxo}
import scalus.crypto.ed25519.SigningKey
import scalus.uplc.builtin.ByteString

/** Codecs for scalus types that differ from CIP-0116.
  */
object Codecs {

    given protocolParamsDecoder: Decoder[ProtocolParams] = Decoder.decodeString.emap(rawString =>
        Try(ProtocolParams.fromBlockfrostJson(rawString)).toEither.left.map(e =>
            "ProtocolParams decoding failed. NOTE: we wrap the scalus blockfrost codec," +
                s"which uses the upickle JSON library instead of circe. The message from upickle is: $e"
        )
    )

    given protocolParamsEncoder: Encoder[ProtocolParams] with {
        override def apply(pp: ProtocolParams): Json = {
            val scalusSerialization =
                upickle.write(pp)(using ProtocolParams.blockfrostParamsReadWriter)
            val Right(json) = parser.parse(scalusSerialization): @unchecked
            json
        }
    }

    given cardanoInfoEncoder: Encoder[CardanoInfo] = deriveEncoder[CardanoInfo]

    given cardanoInfoDecoder: Decoder[CardanoInfo] = deriveDecoder[CardanoInfo]

    given networkEncoder: Encoder[Network] = deriveEncoder[Network]

    given networkDecoder: Decoder[Network] = deriveDecoder[Network]

    given slotConfigEncoder: Encoder[SlotConfig] = deriveEncoder[SlotConfig]

    given slotConfigDecoder: Decoder[SlotConfig] = deriveDecoder[SlotConfig]

    given utxoEncoder: Encoder[Utxo] = deriveEncoder[Utxo]

    given utxoDecoder: Decoder[Utxo] = deriveDecoder[Utxo]

    val dummySigningKey: SigningKey =
        SigningKey.fromByteString(ByteString.fromHex("00" * 32)) match {
            case Right(sk) => sk
            case Left(e) =>
                throw RuntimeException(
                  s"exception thrown when constructing dummy signing key $e"
                )
        }

    given transactionDecoder: Decoder[Transaction] = Decoder.decodeString.emap(hex =>
        val bytes = ByteString.fromHex(hex).bytes
        Try(Transaction.fromCbor(bytes)).toEither.left.map(e =>
            "CBOR decoding of transaction failed. Error Message:" +
                s" $e.getMessage"
        )
    )

    // FIXME (maybe?): combine with `given Encoder[KeepRaw[TransactionOutput]]` in RemoteL2LedgerCodecs(?)
    given transactionOutputEncoder: Encoder[TransactionOutput] with {

        def apply(txOut: TransactionOutput): Json = {
            val cbor = Cbor.encode(txOut).toByteArray
            Json.fromString(ByteString.fromArray(cbor).toHex)
        }
    }

    given transactionOutputDecoder: Decoder[TransactionOutput] = Decoder.instance { c =>
        for {
            hex <- c.as[String]
            bytes <- Try(ByteString.fromHex(hex).bytes).toEither.left.map(e =>
                io.circe.DecodingFailure(
                  s"Hex decoding of the transaction output failed. Message: ${e.getMessage}",
                  c.history
                )
            )
            txOut <- Try(Cbor.decode(bytes).to[TransactionOutput].value).toEither.left.map(e =>
                io.circe.DecodingFailure(
                  s"CBOR decoding of the transaction output failed. Message: ${e.getMessage}",
                  c.history
                )
            )
        } yield txOut

    }

    given transactionInputKeyEncoder: KeyEncoder[TransactionInput] =
        KeyEncoder.encodeKeyString.contramap(ti =>
            ti.transactionId.toHex ++ "#" ++ ti.index.toString
        )

    given transactionInputKeyDecoder: KeyDecoder[TransactionInput] with {
        override def apply(s: String): Option[TransactionInput] =
            s.split("#").toList match {
                case txIdStr :: idxStr :: Nil =>
                    for {
                        txId <- Try(TransactionHash.fromHex(txIdStr)).toOption
                        int <- KeyDecoder
                            .decodeKeyInt(idxStr)
                        idx <- if int >= 0 then Some(int) else None
                    } yield TransactionInput(txId, idx)
                case _ => None
            }
    }

}
