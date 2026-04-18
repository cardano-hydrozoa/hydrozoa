package hydrozoa.config

import cats.syntax.all.*
import hydrozoa.config
import hydrozoa.config.head.network.CardanoNetwork
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

        def rulebasedTreasuryScriptUtxo: ScriptReferenceUtxos.TreasuryScriptUtxo =
            scriptReferenceUtxos.rulebasedTreasuryScriptUtxo
        def disputeResolutionScriptUtxo: ScriptReferenceUtxos.DisputeScriptUtxo =
            scriptReferenceUtxos.disputeResolutionScriptUtxo

        final def referenceTreasury: ReferenceOutput = ReferenceOutput(
          rulebasedTreasuryScriptUtxo.utxo
        )
        final def referenceDispute: ReferenceOutput = ReferenceOutput(
          disputeResolutionScriptUtxo.utxo
        )
    }

    enum Error extends Throwable:
        case InvalidTreasuryScriptUtxo
        case InvalidDisputeScriptUtxo

        override def toString: String = this match
            case InvalidTreasuryScriptUtxo => "InvalidTreasuryScriptUtxo"
            case InvalidDisputeScriptUtxo  => "InvalidDisputeScriptUtxo"

        override def getMessage: String = this match
            case InvalidTreasuryScriptUtxo =>
                "The provided UTXO is not a valid treasury script reference UTXO"
            case InvalidDisputeScriptUtxo =>
                "The provided UTXO is not a valid dispute resolution script reference UTXO"

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

                actualHash = scriptRef.script.scriptHash
                _ <- Either.cond(
                  actualHash == hydrozoa.config.HydrozoaBlueprint.treasuryScriptHash,
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
                    .toRight(ScriptReferenceUtxos.Error.InvalidDisputeScriptUtxo)
                _ <- Either.cond(
                  actualNetwork == network.network,
                  (),
                  ScriptReferenceUtxos.Error.InvalidDisputeScriptUtxo
                )

                scriptRef <- utxo.output.scriptRef.toRight(
                  ScriptReferenceUtxos.Error.InvalidDisputeScriptUtxo
                )

                actualHash = scriptRef.script.scriptHash
                _ <- Either.cond(
                  actualHash == hydrozoa.config.HydrozoaBlueprint.disputeScriptHash,
                  (),
                  ScriptReferenceUtxos.Error.InvalidDisputeScriptUtxo
                )
            } yield DisputeScriptUtxo(utxo)
    }
}
