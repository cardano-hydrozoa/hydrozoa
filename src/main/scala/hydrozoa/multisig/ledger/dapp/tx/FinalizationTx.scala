package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.*
import hydrozoa.lib.tx.BuildError.{BalancingError, StepError, ValidationError}
import hydrozoa.lib.tx.ScriptSource.{NativeScriptAttached, NativeScriptValue}
import hydrozoa.lib.tx.TransactionBuilderStep.{ModifyAuxiliaryData, Send, Spend}
import hydrozoa.lib.tx.{BuildError, ExpectedSigner, NativeScriptWitness, TransactionBuilder, TransactionBuilderStep, TransactionUnspentOutput, TxBuildError}
import hydrozoa.multisig.ledger.DappLedger
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scala.language.implicitConversions
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.txbuilder.{BuilderContext, LowLevelTxBuilder, TxBalancingError}

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
        context: BuilderContext
    )
    
    def build(recipe: Recipe): Either[BuildError, FinalizationTx] = {
        val beaconTokenBurn: TransactionBuilderStep.Mint =
            TransactionBuilderStep.Mint(
              recipe.headNativeScript.policyId,
              recipe.headTokenName,
              -1L,
              NativeScriptWitness(
                NativeScriptValue(recipe.headNativeScript.script),
                recipe.headNativeScript.requiredSigners.toSortedSet.toSet.map(ExpectedSigner(_))
              )
            )

        val createWithdrawnUtxos: Seq[Send] =
            recipe.utxosWithdrawn.toSeq.map(utxo => Send(utxo._2))

        val spendTreasury: Spend = Spend(
          utxo = TransactionUnspentOutput(recipe.treasuryUtxo.toUtxo),
          witness = NativeScriptWitness(NativeScriptAttached, Set.empty)
        )

        val addChangeOutput: Send = Send(
          Babbage(
            recipe.headNativeScript.address(recipe.context.network),
            Value.zero,
            None,
            None
          )
        )

        val addMetaData = ModifyAuxiliaryData(_ =>
            Some(
              MD(
                MD.L1TxTypes.Finalization,
                recipe.headNativeScript.address(recipe.context.network)
              )
            )
        )

        val steps : Seq[TransactionBuilderStep] = createWithdrawnUtxos
            .appended(beaconTokenBurn)
            .appended(spendTreasury)
            .appended(addChangeOutput)
            .appended(addMetaData)
        
        for {
            unbalanced <- TransactionBuilder.build(recipe.context.network, steps).left.map(StepError(_))
            finalized <- unbalanced.finalizeContext(
                protocolParams = recipe.context.protocolParams,
                diffHandler = new ChangeOutputDiffHandler(recipe.context.protocolParams, 1).changeOutputDiffHandler,
                evaluator = recipe.context.evaluator,
                validators = recipe.context.validators
                ).left.map({
                case balanceError: TxBalancingError => BalancingError(balanceError)
                case validationError: TransactionException =>
                    ValidationError(validationError)
            })
        } yield FinalizationTx(treasurySpent = recipe.treasuryUtxo, tx = finalized.transaction)
    }

    
}
