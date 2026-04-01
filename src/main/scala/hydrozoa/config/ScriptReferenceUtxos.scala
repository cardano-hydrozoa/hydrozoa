package hydrozoa.config

import cats.syntax.all.*
import hydrozoa.config
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.rulebased.ledger.l1.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import scalus.cardano.ledger.Utxo
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
    trait Section {
        def scriptReferenceUtxos: ScriptReferenceUtxos

        def rulebasedTreasuryScriptUtxo: ScriptReferenceUtxos.TreasuryScriptUtxo
        def disputeResolutionScriptUtxo: ScriptReferenceUtxos.DisputeScriptUtxo

        final def referenceTreasury: ReferenceOutput = ReferenceOutput(
          rulebasedTreasuryScriptUtxo.utxo
        )
        final def referenceDispute: ReferenceOutput = ReferenceOutput(
          disputeResolutionScriptUtxo.utxo
        )
    }

    // TODO: Expand errors, add toString
    enum Error extends Throwable:
        case InvalidTreasuryScriptUtxo
        case InvalidDisputeScriptUxo

    case class TreasuryScriptUtxo private (utxo: Utxo)

    object TreasuryScriptUtxo {
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
}
