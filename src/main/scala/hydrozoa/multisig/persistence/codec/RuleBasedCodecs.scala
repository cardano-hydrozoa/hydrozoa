package hydrozoa.multisig.persistence.codec

import cats.syntax.functor.*
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{byteStringDecoder, byteStringEncoder, transactionInputDecoder, transactionInputEncoder, valueDecoder, valueEncoder}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.cardano.ledger.{TransactionInput, Value}
import scalus.uplc.builtin.ByteString

/** Persistence-layer JSON codecs for the rule-based-regime types that appear inside
  * [[hydrozoa.multisig.ledger.l1.tx.FallbackTx]] and (later) the rule-based regime's own
  * persistence read-set.
  *
  * `RuleBasedTreasuryDatum` is a Scala `enum` with `Unresolved` / `Resolved` variants — encoded
  * tag-discriminated per the agreed convention. Sub-fields ride through CIP-116 (ByteString, Value)
  * and Circe built-ins (BigInt, List). `MembershipProof` and the `VerificationKey` used here are
  * both type-aliased to `scalus.uplc.builtin.ByteString` (see `TreasuryState.scala`), so they pick
  * up the CIP-116 byteString codec automatically.
  */
object RuleBasedCodecs:

    // --- RuleBasedTreasuryDatum ---

    private given unresolvedEncoder: Encoder[RuleBasedTreasuryDatum.Unresolved] =
        Encoder.instance { u =>
            Json.obj(
              "headMp" -> (u.headMp: ByteString).asJson,
              "deadlineVoting" -> u.deadlineVoting.asJson,
              "versionMajor" -> u.versionMajor.asJson
            )
        }

    private given unresolvedDecoder: Decoder[RuleBasedTreasuryDatum.Unresolved] =
        Decoder.instance { c =>
            for
                mp <- c.downField("headMp").as[ByteString]
                dv <- c.downField("deadlineVoting").as[BigInt]
                vm <- c.downField("versionMajor").as[BigInt]
            yield RuleBasedTreasuryDatum.Unresolved(mp, dv, vm)
        }

    private given resolvedEncoder: Encoder[RuleBasedTreasuryDatum.Resolved] = Encoder.instance {
        r =>
            Json.obj(
              "headMp" -> (r.headMp: ByteString).asJson,
              "evacuationActive" -> (r.evacuationActive: ByteString).asJson,
              "version" -> Json.arr(r.version._1.asJson, r.version._2.asJson)
            )
    }

    private given resolvedDecoder: Decoder[RuleBasedTreasuryDatum.Resolved] = Decoder.instance {
        c =>
            for
                mp <- c.downField("headMp").as[ByteString]
                ea <- c.downField("evacuationActive").as[ByteString]
                v <- c.downField("version").as[(BigInt, BigInt)]
            yield RuleBasedTreasuryDatum.Resolved(mp, ea, v)
    }

    given ruleBasedTreasuryDatumEncoder: Encoder[RuleBasedTreasuryDatum] = Encoder.instance {
        case u: RuleBasedTreasuryDatum.Unresolved =>
            addKind(unresolvedEncoder(u), "Unresolved")
        case r: RuleBasedTreasuryDatum.Resolved =>
            addKind(resolvedEncoder(r), "Resolved")
    }

    given ruleBasedTreasuryDatumDecoder: Decoder[RuleBasedTreasuryDatum] = Decoder.instance { c =>
        c.downField("kind").as[String].flatMap {
            case "Unresolved" => c.as[RuleBasedTreasuryDatum.Unresolved].widen
            case "Resolved"   => c.as[RuleBasedTreasuryDatum.Resolved].widen
            case other =>
                Left(
                  io.circe.DecodingFailure(
                    s"unknown RuleBasedTreasuryDatum kind: $other",
                    c.history
                  )
                )
        }
    }

    // --- RuleBasedTreasuryOutput ---

    given ruleBasedTreasuryOutputEncoder: Encoder[RuleBasedTreasuryOutput] = Encoder.instance { o =>
        Json.obj(
          "datum" -> o.datum.asJson,
          "value" -> o.value.asJson
        )
    }

    given ruleBasedTreasuryOutputDecoder: Decoder[RuleBasedTreasuryOutput] = Decoder.instance { c =>
        for
            datum <- c.downField("datum").as[RuleBasedTreasuryDatum]
            value <- c.downField("value").as[Value]
        yield RuleBasedTreasuryOutput(datum, value)
    }

    // --- RuleBasedTreasuryUtxo ---

    given ruleBasedTreasuryUtxoEncoder: Encoder[RuleBasedTreasuryUtxo] = Encoder.instance { u =>
        Json.obj(
          "utxoId" -> u.utxoId.asJson,
          "treasuryOutput" -> u.treasuryOutput.asJson
        )
    }

    given ruleBasedTreasuryUtxoDecoder: Decoder[RuleBasedTreasuryUtxo] = Decoder.instance { c =>
        for
            id <- c.downField("utxoId").as[TransactionInput]
            out <- c.downField("treasuryOutput").as[RuleBasedTreasuryOutput]
        yield RuleBasedTreasuryUtxo(utxoId = id, treasuryOutput = out)
    }

    private def addKind(json: Json, kind: String): Json =
        json.deepMerge(Json.obj("kind" -> Json.fromString(kind)))
