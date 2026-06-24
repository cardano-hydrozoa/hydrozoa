package hydrozoa.multisig.persistence.codec

import cats.data.NonEmptyList
import cats.syntax.functor.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, InitializationTx, RefundTx, RolloutTx, SettlementTx}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackBrief, StackEffects, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.codec.FallbackTxCodec.{fallbackTxDecoder, fallbackTxEncoder}
import hydrozoa.multisig.persistence.codec.InitializationTxCodec.{initializationTxDecoder, initializationTxEncoder}
import hydrozoa.multisig.persistence.codec.PartitionEffectsCodec.{finalDecoder, finalEncoder}
import hydrozoa.multisig.persistence.codec.RefundTxCodec.{refundTxDecoder, refundTxEncoder}
import hydrozoa.multisig.persistence.codec.RolloutTxCodec.{rolloutTxDecoder, rolloutTxEncoder}
import hydrozoa.multisig.persistence.codec.SecCodec.{standaloneEvacCommitmentDecoder, standaloneEvacCommitmentEncoder}
import hydrozoa.multisig.persistence.codec.SettlementTxCodec.{settlementTxDecoder, settlementTxEncoder}
import io.circe.syntax.*
import io.circe.{Codec, Decoder, Encoder, Json}

/** Persistence-layer JSON codec for [[Stack.Unsigned]] — the value stored at
  * [[hydrozoa.multisig.persistence.StoreKey.UnsignedStack]]. StackComposer persists it before
  * handing the stack to `SlowConsensusActor`, so SCA can re-form its in-flight cell on recovery: a
  * `HardAck` signs the *effects*, which a [[StackBrief]] (range only) does not carry.
  *
  * Mirrors [[StackEffectsCodec]] / [[PartitionEffectsCodec]] but over the **bare**
  * [[StandaloneEvacuationCommitment]] (no aggregated signatures yet), reusing the SEC-agnostic
  * `Final` partition codec and the same tag-discriminated `{"kind": …}` convention.
  */
object UnsignedStackCodec:

    private type Sec = StandaloneEvacuationCommitment
    private type Effects = PartitionEffects[Sec]

    // ---- bare-SEC partitions (Major / Minor; Final is SEC-agnostic and reused) ----

    private given majorEncoder(using CardanoNetwork.Section): Encoder[PartitionEffects.Major[Sec]] =
        Encoder.instance { m =>
            Json.obj(
              "settlement" -> m.settlement.asJson,
              "fallback" -> m.fallback.asJson,
              "rollouts" -> m.rollouts.asJson,
              "refunds" -> m.refunds.asJson,
              "sec" -> m.sec.asJson
            )
        }

    private given majorDecoder(using CardanoNetwork.Section): Decoder[PartitionEffects.Major[Sec]] =
        Decoder.instance { c =>
            for
                settlement <- c.downField("settlement").as[SettlementTx]
                fallback <- c.downField("fallback").as[FallbackTx]
                rollouts <- c.downField("rollouts").as[List[RolloutTx]]
                refunds <- c.downField("refunds").as[List[RefundTx]]
                sec <- c.downField("sec").as[Option[Sec]]
            yield PartitionEffects.Major[Sec](settlement, fallback, rollouts, refunds, sec)
        }

    private given minorEncoder(using CardanoNetwork.Section): Encoder[PartitionEffects.Minor[Sec]] =
        Encoder.instance { m =>
            Json.obj("sec" -> m.sec.asJson, "refunds" -> m.refunds.asJson)
        }

    private given minorDecoder(using CardanoNetwork.Section): Decoder[PartitionEffects.Minor[Sec]] =
        Decoder.instance { c =>
            for
                sec <- c.downField("sec").as[Sec]
                refunds <- c.downField("refunds").as[List[RefundTx]]
            yield PartitionEffects.Minor[Sec](sec, refunds)
        }

    private given partitionEffectsEncoder(using CardanoNetwork.Section): Encoder[Effects] =
        Encoder.instance {
            case m: PartitionEffects.Major[Sec]  => addKind(majorEncoder.apply(m), "Major")
            case f: PartitionEffects.Final       => addKind(finalEncoder.apply(f), "Final")
            case mi: PartitionEffects.Minor[Sec] => addKind(minorEncoder.apply(mi), "Minor")
        }

    private given partitionEffectsDecoder(using CardanoNetwork.Section): Decoder[Effects] =
        Decoder.instance { c =>
            c.downField("kind").as[String].flatMap {
                case "Major" => c.as[PartitionEffects.Major[Sec]].widen
                case "Final" => c.as[PartitionEffects.Final].widen
                case "Minor" => c.as[PartitionEffects.Minor[Sec]].widen
                case other =>
                    Left(
                      io.circe.DecodingFailure(s"unknown PartitionEffects kind: $other", c.history)
                    )
            }
        }

    private given partitionsEncoder(using CardanoNetwork.Section): Encoder[NonEmptyList[Effects]] =
        Encoder.encodeList[Effects].contramap(_.toList)

    private given partitionsDecoder(using CardanoNetwork.Section): Decoder[NonEmptyList[Effects]] =
        Decoder
            .decodeList[Effects]
            .emap(list =>
                NonEmptyList
                    .fromList(list)
                    .toRight("expected non-empty partitions for StackEffects.Unsigned.Regular")
            )

    // ---- StackEffects.Unsigned (Initial / Regular) ----

    private given unsignedEffectsEncoder(using
        CardanoNetwork.Section
    ): Encoder[StackEffects.Unsigned] =
        Encoder.instance {
            case i: StackEffects.Unsigned.Initial =>
                addKind(
                  Json.obj(
                    "initializationTx" -> i.initializationTx.asJson,
                    "fallbackTx" -> i.fallbackTx.asJson
                  ),
                  "Initial"
                )
            case r: StackEffects.Unsigned.Regular =>
                addKind(Json.obj("partitions" -> r.partitions.asJson), "Regular")
        }

    private given unsignedEffectsDecoder(using
        CardanoNetwork.Section
    ): Decoder[StackEffects.Unsigned] =
        Decoder.instance { c =>
            c.downField("kind").as[String].flatMap {
                case "Initial" =>
                    for
                        initTx <- c.downField("initializationTx").as[InitializationTx]
                        fallback <- c.downField("fallbackTx").as[FallbackTx]
                    yield StackEffects.Unsigned.Initial(initTx, fallback)
                case "Regular" =>
                    c.downField("partitions")
                        .as[NonEmptyList[Effects]]
                        .map(StackEffects.Unsigned.Regular(_))
                case other =>
                    Left(
                      io.circe
                          .DecodingFailure(s"unknown StackEffects.Unsigned kind: $other", c.history)
                    )
            }
        }

    // ---- Stack.Unsigned (brief + effects) ----

    given unsignedStackCodec(using CardanoNetwork.Section): Codec[Stack.Unsigned] =
        Codec.from(
          Decoder.instance { c =>
              for
                  brief <- c.downField("brief").as[StackBrief]
                  effects <- c.downField("effects").as[StackEffects.Unsigned]
              yield Stack.Unsigned(brief, effects)
          },
          Encoder.instance { u =>
              Json.obj("brief" -> u.brief.asJson, "effects" -> u.effects.asJson)
          }
        )

    private def addKind(json: Json, kind: String): Json =
        json.deepMerge(Json.obj("kind" -> Json.fromString(kind)))
