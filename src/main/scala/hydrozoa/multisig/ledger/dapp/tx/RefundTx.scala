package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toEpochQuantizedInstant, toQuantizedInstant}
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuildErrorOr, explainConst}
import hydrozoa.multisig.ledger.dapp.tx.TxTiming.*
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.{Utxo as _, prebalancedLovelaceDiffHandler, *}
import monocle.{Focus, Lens}
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Failure, Success}
import scalus.builtin.ByteString
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.rules.TransactionSizeValidator
import scalus.cardano.ledger.utils.TxBalance
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.ScriptSource.NativeScriptAttached
import scalus.cardano.txbuilder.SomeBuildError.*
import scalus.cardano.txbuilder.TransactionBuilderStep.{ModifyAuxiliaryData, ReferenceOutput, Send, Spend, ValidityStartSlot}

sealed trait RefundTx {
    def tx: Transaction
    def mStartTime: Option[QuantizedInstant] = this match {
        case self: RefundTx.PostDated => Some(self.startTime)
        case _                        => None
    }
}

object RefundTx {

    // TODO: shall we keep it for now?
    final case class Immediate(override val tx: Transaction) extends RefundTx, Tx[Immediate] {
        override val txLens: Lens[Immediate, Transaction] = Focus[Immediate](_.tx)
    }

    final case class PostDated(override val tx: Transaction, startTime: QuantizedInstant)
        extends RefundTx,
          Tx[PostDated] {
        override val txLens: Lens[PostDated, Transaction] = Focus[PostDated](_.tx)
    }

    object Builder {
        final case class Immediate(
            override val config: Tx.Builder.Config,
            override val refundInstructions: DepositUtxo.Refund.Instructions,
            override val refundValue: Value
        ) extends Builder[RefundTx.Immediate] {
            override val mValidityStart: None.type = None
            override val stepRefundMetadata =
                ModifyAuxiliaryData(_ => Some(MD(MD.Refund(headAddress = config.headAddress))))

            override def mkPartialResult(
                ctx: TransactionBuilder.Context,
                valueNeeded: Value
            ): PartialResult.Immediate =
                PartialResult.Immediate(ctx, valueNeeded, refundInstructions)
        }

        final case class PostDated(
            override val config: Tx.Builder.Config,
            override val refundInstructions: DepositUtxo.Refund.Instructions,
            override val refundValue: Value,
        ) extends Builder[RefundTx.PostDated] {
            override val mValidityStart: Some[QuantizedInstant] =
                Some(refundInstructions.startTime.toEpochQuantizedInstant(config.env.slotConfig))
            override val stepRefundMetadata =
                ModifyAuxiliaryData(_ => Some(MD(MD.Refund(headAddress = config.headAddress))))

            override def mkPartialResult(
                ctx: TransactionBuilder.Context,
                valueNeeded: Value
            ): PartialResult.PostDated =
                PartialResult.PostDated(ctx, valueNeeded, refundInstructions, config.env.slotConfig)
        }

        sealed trait PartialResult[T <: RefundTx]
            extends Tx.Builder.HasCtx,
              State.Fields.HasInputRequired {
            def refundInstructions: DepositUtxo.Refund.Instructions
            def postProcess(ctx: TransactionBuilder.Context): T

            final def complete(
                depositSpent: DepositUtxo,
                config: Tx.Builder.Config
            ): BuildErrorOr[T] = for {
                addedDepositSpent <- TransactionBuilder
                    .modify(
                      ctx,
                      List(
                        Spend(
                          depositSpent.toUtxo,
                          NativeScriptWitness(
                            NativeScriptAttached,
                            config.headNativeScript.requiredSigners
                          )
                        )
                      )
                    )
                    .explainConst("adding real spend deposit failed.")
                finalized <- addedDepositSpent
                    .finalizeContext(
                      config.env.protocolParams,
                      prebalancedLovelaceDiffHandler,
                      config.evaluator,
                      config.validators
                    )
                    .explainConst("finalizing partial result completion failed")
            } yield postProcess(finalized)
        }

        object PartialResult {
            final case class Immediate(
                override val ctx: TransactionBuilder.Context,
                override val inputValueNeeded: Value,
                override val refundInstructions: DepositUtxo.Refund.Instructions
            ) extends PartialResult[RefundTx.Immediate] {
                override def postProcess(ctx: TransactionBuilder.Context): RefundTx.Immediate =
                    RefundTx.Immediate(ctx.transaction)
            }

            final case class PostDated(
                override val ctx: TransactionBuilder.Context,
                override val inputValueNeeded: Value,
                override val refundInstructions: DepositUtxo.Refund.Instructions,
                slotConfig: SlotConfig,
            ) extends PartialResult[RefundTx.PostDated] {
                override def postProcess(ctx: TransactionBuilder.Context): RefundTx.PostDated =
                    RefundTx.PostDated(
                      ctx.transaction,
                      refundInstructions.startTime.toEpochQuantizedInstant(slotConfig)
                    )
            }
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

        def mValidityStart: Option[QuantizedInstant]
        def stepRefundMetadata: ModifyAuxiliaryData

        def mkPartialResult(
            ctx: TransactionBuilder.Context,
            valueNeededWithFee: Value
        ): Builder.PartialResult[T]

        final def partialResult: BuildErrorOr[Builder.PartialResult[T]] = {
            val stepReferenceHNS = ReferenceOutput(config.multisigRegimeUtxo.asUtxo)

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

                valueNeeded = Placeholder.inputValueNeeded(ctx, config.env.protocolParams)

                valueNeededWithFee <- trialFinishLoop(ctx, valueNeeded)
            } yield mkPartialResult(ctx, valueNeededWithFee)
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
                addedSpendDeposit <- TransactionBuilder.modify(
                  ctx,
                  List(
                    Spend(
                      spendDeposit,
                      NativeScriptWitness(
                        NativeScriptAttached,
                        config.headNativeScript.requiredSigners
                      )
                    )
                  )
                )
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
                case Left(
                      SomeBuildError.BalancingError(
                        TxBalancingError.Failed(WrappedCoin(Coin(diff))),
                        _errorCtx
                      )
                    ) =>
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

            def inputValueNeeded(ctx: TransactionBuilder.Context, params: ProtocolParams): Value =
                TxBalance.produced(ctx.transaction, params)
        }
    }

    sealed trait ParseError extends Throwable

    case class MetadataParseError(wrapped: MD.ParseError) extends ParseError
    case class TxCborDeserializationFailed(e: Throwable) extends ParseError

    def parse(
        txBytes: Tx.Serialized,
        env: CardanoInfo
    ): Either[ParseError, RefundTx] = {
        given ProtocolVersion = env.protocolParams.protocolVersion
        given OriginalCborByteArray = OriginalCborByteArray(txBytes)

        io.bullet.borer.Cbor.decode(txBytes).to[Transaction].valueTry match {
            case Success(tx) =>
                for {
                    _ <- MD.parse(tx) match {
                        case Right(md: Metadata.Refund) => Right(md)
                        case Right(md) =>
                            Left(MetadataParseError(MD.UnexpectedTxType(md, "Refund")))
                        case Left(e) => Left(MetadataParseError(e))
                    }

                    refundTx: RefundTx = tx.body.value.validityStartSlot match {
                        case None => RefundTx.Immediate(tx)
                        case Some(startSlot) =>
                            RefundTx.PostDated(
                              tx,
                              Slot(startSlot).toQuantizedInstant(env.slotConfig)
                            )
                    }
                } yield refundTx
            case Failure(e) => Left(TxCborDeserializationFailed(e))
        }

    }
}
