package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.BuildErrorOr
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import scala.language.implicitConversions
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.SomeBuildError.*

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

    object Builder {
        trait PartialResult[T <: RefundTx]
            extends Tx.Builder.HasCtx,
              State.Fields.HasInputRequired {
            def complete(depositSpent: DepositUtxo): BuildErrorOr[T] = ???
        }

        object State {
            object Fields {
                sealed trait HasInputRequired {
                    def inputValueNeeded: Value
                }
            }
        }
    }

    trait Builder[T <: RefundTx] extends Tx.Builder {
        def refundInstructions: DepositUtxo.Refund.Instructions

        final def partialResult: BuildErrorOr[Builder.PartialResult[T]] = ???
    }

    object PostDated {
        case class Recipe(
            depositTx: DepositTx,
            validityStartSlot: Slot
        )

//        def build(recipe: Recipe): Either[SomeBuildError, PostDated] = {
//            /////////////////////////////////////////////////////////////////////
//            // Data extraction
//
//            val deposit = recipe.depositTx.depositProduced
//            // NB: Fee is paid from deposit itself
//            val depositDatum = deposit.l1OutputDatum
//            val refundOutput: TransactionOutput =
//                TransactionOutput(
//                  address = depositDatum.refundInstructions.address
//                      .toScalusLedger(network = recipe.network),
//                  value = Value(deposit.l1OutputValue),
//                  datumOption = depositDatum.refundInstructions.datum.asScala.map(Inline(_))
//                )
//
//            /////////////////////////////////////////////////////////////////////
//            // Step definitions
//
//            val spendDeposit = Spend(
//              utxo = recipe.depositTx.depositProduced.toUtxo,
//              witness = NativeScriptWitness(
//                scriptSource = NativeScriptValue(recipe.headScript.script),
//                additionalSigners = recipe.headScript.requiredSigners
//              )
//            )
//
//            val createRefund: Send = Send(refundOutput)
//
//            val setValidity = ValidityStartSlot(recipe.validityStartSlot.slot)
//
//            val modifyAuxiliaryData = ModifyAuxiliaryData(_ =>
//                Some(
//                  MD(
//                    RefundPostDated(
//                      recipe.headScript.mkAddress(recipe.network)
//                    )
//                  )
//                )
//            )
//
//            val steps = List(spendDeposit, createRefund, setValidity, modifyAuxiliaryData)
//
//            //////////////////////////////////////////////////////////////////////
//            // Build and finalize
//
//            for {
//                unbalanced <- TransactionBuilder
//                    .build(recipe.network, steps)
//                finalized <- unbalanced
//                    .finalizeContext(
//                      recipe.protocolParams,
//                      diffHandler = new ChangeOutputDiffHandler(
//                        recipe.protocolParams,
//                        0
//                      ).changeOutputDiffHandler,
//                      evaluator = recipe.evaluator,
//                      validators = recipe.validators
//                    )
//            } yield PostDated(
//              refundContingency = ???,
//              depositSpent = recipe.depositTx.depositProduced,
//              tx = finalized.transaction
//            )
//        }

    }
}
