package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.*
import hydrozoa.lib.tx.ScriptSource.{NativeScriptAttached, NativeScriptValue}
import hydrozoa.lib.tx.TransactionBuilderStep.{ModifyAuxiliaryData, Send, Spend}
import hydrozoa.lib.tx.{
    NativeScriptWitness,
    SomeBuildError,
    TransactionBuilder,
    TransactionBuilderStep
}
import hydrozoa.multisig.ledger.DappLedger
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scala.language.implicitConversions
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler

final case class FinalizationTx(
    private val treasurySpent: TreasuryUtxo,
    override val tx: Transaction
) extends Tx

object FinalizationTx {
    case class Recipe(
        majorVersion: Int,
        utxosWithdrawn: Set[(TransactionInput, TransactionOutput)],
        headNativeScript: HeadMultisigScript,
        treasuryUtxo: TreasuryUtxo,
        changeAddress: ShelleyAddress,
        headTokenName: AssetName,
        network: Network,
        protocolParams: ProtocolParams,
        evaluator: PlutusScriptEvaluator,
        validators: Seq[Validator]
    )

    def build(recipe: Recipe): Either[SomeBuildError, FinalizationTx] = {
        val beaconTokenBurn: TransactionBuilderStep.Mint =
            TransactionBuilderStep.Mint(
              recipe.headNativeScript.policyId,
              recipe.headTokenName,
              -1L,
              NativeScriptWitness(
                NativeScriptValue(recipe.headNativeScript.script),
                recipe.headNativeScript.requiredSigners
              )
            )

        val createWithdrawnUtxos: Seq[Send] =
            recipe.utxosWithdrawn.toSeq.map(utxo => Send(utxo._2))

        val spendTreasury: Spend = Spend(
          utxo = recipe.treasuryUtxo.asUtxo,
          witness = NativeScriptWitness(NativeScriptAttached, Set.empty)
        )

        val addChangeOutput: Send = Send(
          Babbage(
            recipe.headNativeScript.mkAddress(recipe.network),
            Value.zero,
            None,
            None
          )
        )

        val addMetaData = ModifyAuxiliaryData(_ =>
            Some(
              MD(
                MD.L1TxTypes.Finalization,
                recipe.headNativeScript.mkAddress(recipe.network)
              )
            )
        )

        val steps: Seq[TransactionBuilderStep] = createWithdrawnUtxos
            .appended(beaconTokenBurn)
            .appended(spendTreasury)
            .appended(addChangeOutput)
            .appended(addMetaData)

        for {
            unbalanced <- TransactionBuilder
                .build(recipe.network, steps)
            finalized <- unbalanced
                .finalizeContext(
                  protocolParams = recipe.protocolParams,
                  diffHandler = new ChangeOutputDiffHandler(
                    recipe.protocolParams,
                    1
                  ).changeOutputDiffHandler,
                  evaluator = recipe.evaluator,
                  validators = recipe.validators
                )
        } yield FinalizationTx(treasurySpent = recipe.treasuryUtxo, tx = finalized.transaction)
    }

}
