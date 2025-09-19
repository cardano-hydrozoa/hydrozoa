package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.*
import hydrozoa.multisig.ledger.DappLedger
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.txbuilder.{
    BuilderContext,
    LowLevelTxBuilder,
    SelectInputs,
    TxBalancingError
}

import scala.collection.immutable.SortedMap
import scala.language.implicitConversions

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

    def build(recipe: Recipe): Either[TxBalancingError, FinalizationTx] = {
        val beaconTokenBurn =
            MultiAsset(
              SortedMap.from(
                Seq(
                  (
                    recipe.headNativeScript.policyId,
                    SortedMap.from(Seq((recipe.headTokenName, -1.toLong)))
                  )
                )
              )
            )

        val valueWithdrawn: Value =
            recipe.utxosWithdrawn.foldLeft(Value.zero)((s, w) => s + w._2.value)

        lazy val builder = {
            val b1 = recipe.context.buildNewTx
                .attachNativeScript(recipe.headNativeScript.script, index = 0)
                // change output
                .payTo(
                  address = recipe.headNativeScript.address(recipe.context.network),
                  value = Value.zero
                )
                .selectInputs(selectInputs =
                    SelectInputs.particular(Set(recipe.treasuryUtxo.utxo._1))
                )
                .addMint(beaconTokenBurn)
                .addOutputs(recipe.utxosWithdrawn.toSeq.map(_._2))
                .setAuxData(
                  MD(
                    MD.L1TxTypes.Finalization,
                    recipe.headNativeScript.address(recipe.context.network)
                  )
                )
                .addDummyVKeys(recipe.headNativeScript.numSigners)

            LowLevelTxBuilder
                .balanceFeeAndChange(
                  initial = b1.tx,
                  changeOutputIdx = 0,
                  protocolParams = recipe.context.protocolParams,
                  resolvedUtxo = recipe.context.utxo,
                  evaluator = recipe.context.evaluator
                )
                .map(tx => removeDummyVKeys(recipe.headNativeScript.numSigners, tx))
        }

        builder.map(tx => FinalizationTx(treasurySpent = recipe.treasuryUtxo, tx = tx))
    }

}
