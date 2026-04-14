package hydrozoa.config

import cats.syntax.all.*
import hydrozoa.config
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.given
import hydrozoa.rulebased.ledger.l1.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import io.circe.*
import io.circe.generic.semiauto.*
import scalus.cardano.ledger.{TransactionInput, Utxo}
import scalus.cardano.txbuilder.TransactionBuilderStep.ReferenceOutput

final case class ScriptReferenceUtxos(
    override val rulebasedTreasuryScriptUtxo: ScriptReferenceUtxos.TreasuryScriptUtxo,
    override val disputeResolutionScriptUtxo: ScriptReferenceUtxos.DisputeScriptUtxo
) extends ScriptReferenceUtxos.Section {
    override val scriptReferenceUtxos: ScriptReferenceUtxos = this
    def toList: List[Utxo] =
        List(rulebasedTreasuryScriptUtxo.utxo, disputeResolutionScriptUtxo.utxo)
}

object ScriptReferenceUtxos {
    case class Unresolved(
        override val rulebasedTreasuryScriptInput: TransactionInput,
        override val disputeResolutionScriptInput: TransactionInput
    ) extends Unresolved.Section {
        override val scriptReferenceUtxosUnresolved: Unresolved = this
    }

    object Unresolved {
        trait Section {
            def scriptReferenceUtxosUnresolved: Unresolved
            def rulebasedTreasuryScriptInput: TransactionInput
            def disputeResolutionScriptInput: TransactionInput
        }
    }

    trait Section extends Unresolved.Section {
        def scriptReferenceUtxos: ScriptReferenceUtxos

        def rulebasedTreasuryScriptUtxo: ScriptReferenceUtxos.TreasuryScriptUtxo
        def disputeResolutionScriptUtxo: ScriptReferenceUtxos.DisputeScriptUtxo

        final def referenceTreasury: ReferenceOutput = ReferenceOutput(
          rulebasedTreasuryScriptUtxo.utxo
        )
        final def referenceDispute: ReferenceOutput = ReferenceOutput(
          disputeResolutionScriptUtxo.utxo
        )

        override transparent inline def scriptReferenceUtxosUnresolved: Unresolved =
            Unresolved(
              rulebasedTreasuryScriptInput,
              disputeResolutionScriptInput
            )

        override transparent inline def rulebasedTreasuryScriptInput: TransactionInput =
            scriptReferenceUtxos.rulebasedTreasuryScriptUtxo.utxo.input

        override transparent inline def disputeResolutionScriptInput: TransactionInput =
            scriptReferenceUtxos.disputeResolutionScriptUtxo.utxo.input
    }

    // TODO: Expand errors, add toString
    enum Error extends Throwable:
        case InvalidTreasuryScriptUtxo
        case InvalidDisputeScriptUxo

    case class TreasuryScriptUtxo private (utxo: Utxo)

    object TreasuryScriptUtxo {
        // TODO: Once we have a version script setup, we need to adjust this apply method
        def apply(
            network: CardanoNetwork.Section,
            utxo: Utxo
        ): Either[ScriptReferenceUtxos.Error, TreasuryScriptUtxo] =
            for {
                actualNetwork <- utxo.output.address.getNetwork
                    .toRight(ScriptReferenceUtxos.Error.InvalidTreasuryScriptUtxo)
                _ <- Either.cond(
                  actualNetwork == network.network,
                  (),
                  ScriptReferenceUtxos.Error.InvalidTreasuryScriptUtxo
                )

                scriptRef <- utxo.output.scriptRef.toRight(
                  ScriptReferenceUtxos.Error.InvalidTreasuryScriptUtxo
                )
                _ <- Either.cond(
                  scriptRef.script == RuleBasedTreasuryScript.compiledPlutusV3Script,
                  (),
                  ScriptReferenceUtxos.Error.InvalidTreasuryScriptUtxo
                )
            } yield TreasuryScriptUtxo(utxo)
    }

    case class DisputeScriptUtxo private (utxo: Utxo)

    object DisputeScriptUtxo {
        // TODO: Once we have a version script setup, we need to adjust this apply method
        def apply(
            network: CardanoNetwork.Section,
            utxo: Utxo
        ): Either[ScriptReferenceUtxos.Error, DisputeScriptUtxo] =
            for {
                actualNetwork <- utxo.output.address.getNetwork
                    .toRight(ScriptReferenceUtxos.Error.InvalidDisputeScriptUxo)
                _ <- Either.cond(
                  actualNetwork == network.network,
                  (),
                  ScriptReferenceUtxos.Error.InvalidDisputeScriptUxo
                )

                scriptRef <- utxo.output.scriptRef.toRight(
                  ScriptReferenceUtxos.Error.InvalidDisputeScriptUxo
                )
                _ <- Either.cond(
                  scriptRef.script == DisputeResolutionScript.compiledPlutusV3Script,
                  (()),
                  ScriptReferenceUtxos.Error.InvalidDisputeScriptUxo
                )
            } yield DisputeScriptUtxo(utxo)
    }

    given Encoder[ScriptReferenceUtxos] = deriveEncoder[ScriptReferenceUtxos]

    given scriptReferenceUtxos(using
        network: CardanoNetwork.Section
    ): Decoder[ScriptReferenceUtxos] = deriveDecoder[ScriptReferenceUtxos]

    // TODO This must be updated when we start getting script versions/hashses from the config
    given Encoder[TreasuryScriptUtxo] = utxoEncoder.contramap(_.utxo)

    given treasuryReferenceScriptUtxoDecoder(using
        network: CardanoNetwork.Section
    ): Decoder[TreasuryScriptUtxo] =
        utxoDecoder.emap(utxo =>
            TreasuryScriptUtxo(network, utxo).left.map(e =>
                "Failed to construct rule-based treasury reference script utxo." +
                    s"Failure: $e"
            )
        )

    given Encoder[DisputeScriptUtxo] = utxoEncoder.contramap(_.utxo)

    given disputeSrriptUtxoDecoder(using
        network: CardanoNetwork.Section
    ): Decoder[DisputeScriptUtxo] =
        utxoDecoder.emap(utxo =>
            DisputeScriptUtxo(network, utxo).left.map(e =>
                "Failed to construct dispute script utxo." +
                    s"Failure: $e"
            )
        )
}
