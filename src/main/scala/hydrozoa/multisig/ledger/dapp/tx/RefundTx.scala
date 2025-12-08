package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuildErrorOr, explainConst}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.prebalancedLovelaceDiffHandler
import scala.annotation.tailrec
import scala.language.implicitConversions
import scalus.builtin.ByteString
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.rules.TransactionSizeValidator
import scalus.cardano.ledger.utils.TxBalance
import scalus.cardano.txbuilder.SomeBuildError.*
import scalus.cardano.txbuilder.TransactionBuilderStep.{ModifyAuxiliaryData, ReferenceOutput, Send, Spend, ValidityStartSlot}
import scalus.cardano.txbuilder.TxBalancingError.CantBalance
import scalus.cardano.txbuilder.{SomeBuildError, TransactionBuilder}
import scalus.ledger.api.v3.PosixTime

sealed trait RefundTx {
    def depositSpent: DepositUtxo
    def refundInstructions: DepositUtxo.Refund.Instructions
}

// TODO: There's a lot of noise here due to the Immediate/PostDated distinction, whereas the only difference between
//   them is that RefundTx.Immediate doesn't use the Refund.Instructions.startTime as its validity start time.
object RefundTx {
    final case class Immediate(
        override val depositSpent: DepositUtxo,
        override val refundInstructions: DepositUtxo.Refund.Instructions,
        override val tx: Transaction
    ) extends Tx,
          RefundTx

    final case class PostDated(
        override val depositSpent: DepositUtxo,
        override val refundInstructions: DepositUtxo.Refund.Instructions,
        override val tx: Transaction
    ) extends Tx,
          RefundTx

    object Builder {
        final case class Immediate(
            override val config: Tx.Builder.Config,
            override val refundInstructions: DepositUtxo.Refund.Instructions,
            override val refundValue: Value
        ) extends Builder[RefundTx.Immediate] {
            override val mValidityStartSlot: None.type = None
            override val stepRefundMetadata = ModifyAuxiliaryData(_ =>
                Some(MD(MD.RefundImmediate(headAddress = config.headAddress)))
            )

            override def postProcess(
                ctx: TransactionBuilder.Context,
                depositUtxo: DepositUtxo,
                refundInstructions: DepositUtxo.Refund.Instructions
            ): RefundTx.Immediate =
                RefundTx.Immediate(depositUtxo, refundInstructions, ctx.transaction)
        }

        final case class PostDated(
            override val config: Tx.Builder.Config,
            override val refundInstructions: DepositUtxo.Refund.Instructions,
            override val refundValue: Value
        ) extends Builder[RefundTx.PostDated] {
            override val mValidityStartSlot: Some[PosixTime] = Some(refundInstructions.startTime)
            override val stepRefundMetadata = ModifyAuxiliaryData(_ =>
                Some(MD(MD.RefundPostDated(headAddress = config.headAddress)))
            )

            override def postProcess(
                ctx: TransactionBuilder.Context,
                depositUtxo: DepositUtxo,
                refundInstructions: DepositUtxo.Refund.Instructions
            ): RefundTx.PostDated =
                RefundTx.PostDated(depositUtxo, refundInstructions, ctx.transaction)
        }

        final case class PartialResult[T <: RefundTx](
            builder: Builder[T],
            override val ctx: TransactionBuilder.Context,
            override val inputValueNeeded: Value
        ) extends Tx.Builder.HasCtx,
              State.Fields.HasInputRequired {
            def complete(depositSpent: DepositUtxo): BuildErrorOr[T] = for {
                addedDepositSpent <- TransactionBuilder
                    .modify(ctx, List(Spend(depositSpent.toUtxo)))
                    .explainConst("adding real spend deposit failed.")
            } yield builder.postProcess(addedDepositSpent, depositSpent, builder.refundInstructions)
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
        import BuilderOps.*

        def refundInstructions: DepositUtxo.Refund.Instructions
        def refundValue: Value

        def mValidityStartSlot: Option[PosixTime]
        def stepRefundMetadata: ModifyAuxiliaryData

        def postProcess(
            ctx: TransactionBuilder.Context,
            depositUtxo: DepositUtxo,
            refundInstructions: DepositUtxo.Refund.Instructions
        ): T

        final def partialResult: BuildErrorOr[Builder.PartialResult[T]] = {
            val stepReferenceHNS = ReferenceOutput(config.headNativeScriptReferenceInput)

            val refundOutput: TransactionOutput = TransactionOutput.Babbage(
              address = config.headAddress,
              value = refundValue,
              datumOption = refundInstructions.datum.asScala.map(Inline(_)),
              scriptRef = None
            )

            val sendRefund = Send(refundOutput)

            val setValidity = ValidityStartSlot(
              config.env.slotConfig.timeToSlot(refundInstructions.startTime.toLong)
            )

            val steps = List(stepRefundMetadata, stepReferenceHNS, setValidity, sendRefund)

            for {
                ctx <- TransactionBuilder
                    .build(config.env.network, steps)
                    .explainConst("adding base refund steps failed")

                valueNeeded = Placeholder.inputValueNeeded(ctx)

                valueNeededWithFee <- trialFinishLoop(ctx, valueNeeded)
            } yield Builder.PartialResult[T](this, ctx, valueNeededWithFee)
        }

        // TODO: This is very similar to the trialFinishLoop in RolloutTx. Factor out a common lib function?
        @tailrec
        private def trialFinishLoop(
            ctx: TransactionBuilder.Context,
            trialValue: Value
        ): BuildErrorOr[Value] = {
            val spendDeposit = Utxo(
              BuilderOps.Placeholder.utxoId,
              TransactionOutput.Babbage(
                address = config.headAddress,
                value = trialValue,
                datumOption =
                    None, // Datum is not really empty, but that doesn't affect balancing here.
                scriptRef = None
              )
            )

            val trialResult = for {
                addedSpendDeposit <- TransactionBuilder.modify(ctx, List(Spend(spendDeposit)))
                res <- addedSpendDeposit.finalizeContext(
                  config.env.protocolParams,
                  prebalancedLovelaceDiffHandler,
                  config.evaluator,
                  List(TransactionSizeValidator)
                )
            } yield res

            trialResult match {
                case Left(
                      SomeBuildError.ValidationError(
                        e: InvalidTransactionSizeException,
                        errorCtx
                      )
                    ) =>
                    Left(SomeBuildError.ValidationError(e, errorCtx))
                        .explainConst("trial to add placeholder spend deposit failed")
                case Left(SomeBuildError.BalancingError(CantBalance(diff), _errorCtx)) =>
                    trialFinishLoop(ctx, trialValue - Value(Coin(diff)))
                case Right(_) => Right(trialValue)
                case e =>
                    throw new RuntimeException(
                      "should be impossible; " +
                          s"loop only has two possible Lefts, but got $e"
                    )
            }
        }
    }

    private object BuilderOps {
        object Placeholder {
            // A UtxoID with the largest possible size in Flat encoding
            // - Transaction ID should be 32 bytes of 1111 1111 because Flat uses the least number of bytes.
            // - The index can be zero because Flat will still use a full byte
            // https://hackage.haskell.org/package/flat-0.6/docs/Flat-Class.html
            val utxoId = TransactionInput(
              transactionId = TransactionHash.fromByteString(
                ByteString.fromArray(Array.fill(32)(Byte.MinValue))
              ),
              index = 0
            )

            def inputValueNeeded(ctx: TransactionBuilder.Context): Value =
                TxBalance.produced(ctx.transaction)
        }
    }

    object PostDated {
        case class Recipe(
            depositTx: DepositTx,
            validityStartSlot: Slot
        )
    }
}
