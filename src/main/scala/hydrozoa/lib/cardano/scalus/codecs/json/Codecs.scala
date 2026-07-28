package hydrozoa.lib.cardano.scalus.codecs.json

import io.bullet.borer.Cbor
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, DecodingFailure, Encoder, Json, KeyDecoder, KeyEncoder, parser}
import scala.util.Try
import scalus.cardano.address.Network
import scalus.cardano.ledger.{CardanoInfo, CostModels, DRepVotingThresholds, ExUnitPrices, ExUnits, KeepRaw, NonNegativeInterval, PoolVotingThresholds, ProtocolParams, ProtocolVersion, SlotConfig, Transaction, TransactionHash, TransactionInput, TransactionOutput, UnitInterval, Utxo}
import scalus.crypto.ed25519.SigningKey
import scalus.uplc.builtin.ByteString

/** Codecs for scalus types that differ from CIP-0116.
  */
object Codecs {

    /** Symmetric, human-observable JSON codec for [[ProtocolParams]] and its nested field types.
      *
      * scalus provides only upickle codecs for `ProtocolParams`, and neither round-trips exactly:
      * `blockfrostParamsReadWriter` is write/read asymmetric (its `cost_models` writer emits arrays
      * while the reader expects objects), and `cardanoCliParamsReadWriter` routes the
      * `UnitInterval`/`NonNegativeInterval` fields through `Double`, which is lossy (a mainnet
      * `priceSteps` of `0.0000721` decodes back as `0.000072`). We instead derive circe codecs
      * field-by-field from the record structure and serialize the two fraction types as exact
      * `{ "numerator": <Long>, "denominator": <Long> }` objects, so a `Custom` head-config's
      * `CardanoInfo` stays readable and survives a serialize/deserialize cycle unchanged.
      *
      * The leaf codecs are defined before the derived aggregates that summon them.
      */

    /** Shared `{ "numerator": <Long>, "denominator": <Long> }` shape for the two scalus fraction
      * types. The decoder wraps construction in `Try` because both `require` a positive
      * denominator, which keeps it total — a malformed fraction yields a `Left`, never a thrown
      * exception.
      */
    private def fractionEncoder[A](numerator: A => Long, denominator: A => Long): Encoder[A] =
        Encoder.instance(fraction =>
            Json.obj(
              "numerator" -> Json.fromLong(numerator(fraction)),
              "denominator" -> Json.fromLong(denominator(fraction))
            )
        )

    private def fractionDecoder[A](name: String)(build: (Long, Long) => A): Decoder[A] =
        Decoder.instance(c =>
            for {
                numerator <- c.downField("numerator").as[Long]
                denominator <- c.downField("denominator").as[Long]
                fraction <- Try(build(numerator, denominator)).toEither.left.map(e =>
                    DecodingFailure(s"Invalid $name: ${e.getMessage}", c.history)
                )
            } yield fraction
        )

    given nonNegativeIntervalEncoder: Encoder[NonNegativeInterval] =
        fractionEncoder[NonNegativeInterval](_.numerator, _.denominator)
    given nonNegativeIntervalDecoder: Decoder[NonNegativeInterval] =
        fractionDecoder("NonNegativeInterval")((numerator, denominator) =>
            NonNegativeInterval(numerator, denominator)
        )

    given unitIntervalEncoder: Encoder[UnitInterval] =
        fractionEncoder[UnitInterval](_.numerator, _.denominator)
    given unitIntervalDecoder: Decoder[UnitInterval] =
        fractionDecoder("UnitInterval")((numerator, denominator) =>
            UnitInterval(numerator, denominator)
        )

    /** `cost_models` serialize as a JSON object keyed by language id (`"0"`, `"1"`, `"2"`), each
      * mapped to its cost list. Keys are emitted in id order for a deterministic encoding.
      */
    given costModelsEncoder: Encoder[CostModels] = Encoder.instance(costModels =>
        Json.obj(
          costModels.models.toSeq.sortBy(_._1).map { case (languageId, costs) =>
              languageId.toString -> Json.arr(costs.map(Json.fromLong)*)
          }*
        )
    )

    given costModelsDecoder: Decoder[CostModels] = Decoder.instance(c =>
        for {
            raw <- c.as[Map[String, List[Long]]]
            models <- Try(
              raw.map { case (languageId, costs) => languageId.toInt -> costs.toIndexedSeq }
            ).toEither.left.map(e =>
                DecodingFailure(s"Invalid CostModels: ${e.getMessage}", c.history)
            )
        } yield CostModels(models)
    )

    given protocolVersionEncoder: Encoder[ProtocolVersion] = deriveEncoder[ProtocolVersion]
    given protocolVersionDecoder: Decoder[ProtocolVersion] = deriveDecoder[ProtocolVersion]

    given exUnitsEncoder: Encoder[ExUnits] = deriveEncoder[ExUnits]
    given exUnitsDecoder: Decoder[ExUnits] = deriveDecoder[ExUnits]

    given exUnitPricesEncoder: Encoder[ExUnitPrices] = deriveEncoder[ExUnitPrices]
    given exUnitPricesDecoder: Decoder[ExUnitPrices] = deriveDecoder[ExUnitPrices]

    given drepVotingThresholdsEncoder: Encoder[DRepVotingThresholds] =
        deriveEncoder[DRepVotingThresholds]
    given drepVotingThresholdsDecoder: Decoder[DRepVotingThresholds] =
        deriveDecoder[DRepVotingThresholds]

    given poolVotingThresholdsEncoder: Encoder[PoolVotingThresholds] =
        deriveEncoder[PoolVotingThresholds]
    given poolVotingThresholdsDecoder: Decoder[PoolVotingThresholds] =
        deriveDecoder[PoolVotingThresholds]

    given protocolParamsEncoder: Encoder[ProtocolParams] = deriveEncoder[ProtocolParams]
    given protocolParamsDecoder: Decoder[ProtocolParams] = deriveDecoder[ProtocolParams]

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

    given transactionEncoder: Encoder[Transaction] =
        Encoder.encodeString.contramap(tx => ByteString.fromArray(tx.toCbor).toHex)

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

    /** [[KeepRaw]][[[TransactionOutput]]] CBOR-hex codec — the raw bytes the Scalus reader
      * preserves on read are written back verbatim.
      *
      * Hoisted from `RemoteL2LedgerCodecs` (where it was first introduced) so persistence, L2-RPC,
      * and anything else that needs the wire-identical raw form can share one implementation.
      */
    given keepRawTransactionOutputEncoder: Encoder[KeepRaw[TransactionOutput]] =
        Encoder.instance(kr => Json.fromString(ByteString.fromArray(kr.raw).toHex))

    given keepRawTransactionOutputDecoder: Decoder[KeepRaw[TransactionOutput]] =
        Decoder.instance { c =>
            c.as[String].flatMap { hexStr =>
                val bs = ByteString.fromHex(hexStr)
                Try(Cbor.decode(bs.bytes).to[TransactionOutput].value).toEither.left
                    .map(e =>
                        io.circe.DecodingFailure(
                          s"Failed to decode TransactionOutput from CBOR: ${e.getMessage}",
                          c.history
                        )
                    )
                    .map(KeepRaw.apply)
            }
        }

    /** NOTE: This encoder is NOT CIP-0116 compliant.
      */
    given transactionInputAlternateEncoder: Encoder[TransactionInput] =
        Encoder.encodeString.contramap(ti => ti.transactionId.toHex ++ "#" ++ ti.index.toString)

    /** NOTE: This decoder is NOT CIP-0116 compliant.
      */
    given transactionInputAlternateDecoder: Decoder[TransactionInput] = Decoder.instance(c =>
        def helper[A](msg: String): DecodingFailure =
            io.circe.DecodingFailure(msg, c.history)

        for {
            s <- c.as[String]
            ti <- s.split("#").toList match {
                case txIdStr :: idxStr :: Nil =>
                    for {
                        txId <- Try(TransactionHash.fromHex(txIdStr)).toEither.left.map(throwable =>
                            helper(throwable.getMessage)
                        )
                        int <- parser.decode[Int](idxStr).left.map(e => helper(e.getMessage))
                        idx <-
                            if int >= 0 then Right(int)
                            else Left(helper("TransactionInput index is negative"))
                    } yield TransactionInput(txId, idx)
                case _ =>
                    Left(
                      helper(
                        "invalid format for transaction input. " +
                            "Expected the transaction hash, followed by '#', followed by the index, as a JSON string."
                      )
                    )
            }
        } yield ti
    )

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
