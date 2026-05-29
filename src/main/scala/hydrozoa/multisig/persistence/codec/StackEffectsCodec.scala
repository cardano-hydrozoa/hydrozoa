package hydrozoa.multisig.persistence.codec

import cats.data.NonEmptyList
import cats.syntax.functor.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, InitializationTx}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.codec.FallbackTxCodec.{fallbackTxDecoder, fallbackTxEncoder}
import hydrozoa.multisig.persistence.codec.InitializationTxCodec.{initializationTxDecoder, initializationTxEncoder}
import hydrozoa.multisig.persistence.codec.PartitionEffectsCodec.{partitionEffectsDecoder, partitionEffectsEncoder}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}

/** Persistence-layer JSON codec for [[StackEffects.HardConfirmed]] — the value type stored at
  * `StoreKey.HardConfirmation`. Sealed trait with `Initial` (stack 0 — `InitializationTx` +
  * `FallbackTx`) and `Regular` (stack 1+ — NEL of multisigned partitions) variants.
  *
  * Tag-discriminated `{"kind": "Initial"|"Regular"}` per the agreed convention.
  */
object StackEffectsCodec:

    private type SecMS = StandaloneEvacuationCommitment.MultiSigned

    private given partitionsListEncoder(using
        CardanoNetwork.Section
    ): Encoder[NonEmptyList[PartitionEffects[SecMS]]] =
        Encoder.encodeList[PartitionEffects[SecMS]].contramap(_.toList)

    private given partitionsListDecoder(using
        CardanoNetwork.Section
    ): Decoder[NonEmptyList[PartitionEffects[SecMS]]] =
        Decoder
            .decodeList[PartitionEffects[SecMS]]
            .emap(list =>
                NonEmptyList
                    .fromList(list)
                    .toRight(
                      "expected non-empty list of partitions for StackEffects.HardConfirmed.Regular"
                    )
            )

    given initialEncoder(using
        CardanoNetwork.Section
    ): Encoder[StackEffects.HardConfirmed.Initial] =
        Encoder.instance { i =>
            Json.obj(
              "initializationTx" -> i.initializationTx.asJson,
              "fallbackTx" -> i.fallbackTx.asJson
            )
        }

    given initialDecoder(using
        CardanoNetwork.Section
    ): Decoder[StackEffects.HardConfirmed.Initial] =
        Decoder.instance { c =>
            for
                initTx <- c.downField("initializationTx").as[InitializationTx]
                fallback <- c.downField("fallbackTx").as[FallbackTx]
            yield StackEffects.HardConfirmed.Initial(
              initializationTx = initTx,
              fallbackTx = fallback
            )
        }

    given regularEncoder(using
        CardanoNetwork.Section
    ): Encoder[StackEffects.HardConfirmed.Regular] =
        Encoder.instance { r =>
            Json.obj("partitions" -> r.partitions.asJson)
        }

    given regularDecoder(using
        CardanoNetwork.Section
    ): Decoder[StackEffects.HardConfirmed.Regular] =
        Decoder.instance { c =>
            for parts <- c.downField("partitions").as[NonEmptyList[PartitionEffects[SecMS]]]
            yield StackEffects.HardConfirmed.Regular(partitions = parts)
        }

    given hardConfirmedEncoder(using CardanoNetwork.Section): Encoder[StackEffects.HardConfirmed] =
        Encoder.instance {
            case i: StackEffects.HardConfirmed.Initial =>
                addKind(initialEncoder.apply(i), "Initial")
            case r: StackEffects.HardConfirmed.Regular =>
                addKind(regularEncoder.apply(r), "Regular")
        }

    given hardConfirmedDecoder(using CardanoNetwork.Section): Decoder[StackEffects.HardConfirmed] =
        Decoder.instance { c =>
            c.downField("kind").as[String].flatMap {
                case "Initial" => c.as[StackEffects.HardConfirmed.Initial].widen
                case "Regular" => c.as[StackEffects.HardConfirmed.Regular].widen
                case other =>
                    Left(
                      io.circe.DecodingFailure(
                        s"unknown StackEffects.HardConfirmed kind: $other",
                        c.history
                      )
                    )
            }
        }

    private def addKind(json: Json, kind: String): Json =
        json.deepMerge(Json.obj("kind" -> Json.fromString(kind)))
