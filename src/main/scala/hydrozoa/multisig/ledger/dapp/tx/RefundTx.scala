package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.*
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import scala.language.implicitConversions
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.txbuilder.{
    BuilderContext,
    LowLevelTxBuilder,
    SelectInputs,
    TxBalancingError
}

sealed trait RefundTx {
    def depositSpent: DepositUtxo
}

object RefundTx {
    final case class Immediate(
        override val depositSpent: DepositUtxo,
        override val tx: Transaction
    ) extends Tx,
          RefundTx

    final case class PostDated(
        override val depositSpent: DepositUtxo,
        override val tx: Transaction
    ) extends Tx,
          RefundTx

    object Immediate {
        def build(): Immediate = ???
    }

    object PostDated {
        case class Recipe(
            depositTx: DepositTx,
            txIx: Int,
            context: BuilderContext,
            headScript: HeadMultisigScript,
            validityStartSlot: Slot
        )

        def build(recipe: Recipe): Either[TxBalancingError, PostDated] = {
            val deposit = recipe.depositTx.depositProduced
            // NB: Fee is paid from deposit itself
            val depositDatum = deposit._3
            val refundOutput: TransactionOutput =
                TransactionOutput(
                  address =
                      depositDatum.refundAddress.toScalusLedger(network = recipe.context.network),
                  value = Value(deposit._4),
                  datumOption = depositDatum.refundDatum.asScala.map(Inline(_))
                )

            val builder = {
                val b1 = recipe.context.buildNewTx
                    .attachNativeScript(script = recipe.headScript.script, index = 0)
                    .selectInputs(
                      SelectInputs.particular(
                        Set(TransactionInput(recipe.depositTx.tx.id, recipe.txIx))
                      )
                    )
                    .addOutputs(Seq(refundOutput))
                    .validFrom(recipe.validityStartSlot)
                    .setAuxData(
                      MD(
                        MD.L1TxTypes.RefundPostDated,
                        recipe.headScript.address(recipe.context.network)
                      )
                    )

                LowLevelTxBuilder
                    .balanceFeeAndChange(
                      initial = addDummyVKeys(recipe.headScript.numSigners, b1.tx),
                      changeOutputIdx = 0,
                      protocolParams = recipe.context.protocolParams,
                      resolvedUtxo = recipe.context.utxo,
                      evaluator = recipe.context.evaluator
                    )
                    .map(tx => removeDummyVKeys(recipe.headScript.numSigners, tx))
            }

            builder.map(tx => PostDated(depositSpent = recipe.depositTx.depositProduced, tx = tx))
        }

    }
}
