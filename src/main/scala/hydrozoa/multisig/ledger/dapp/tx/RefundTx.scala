package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.config.head.initialization.InitialBlock
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toEpochQuantizedInstant, toQuantizedInstant}
import hydrozoa.lib.cardano.scalus.txbuilder.DiffHandler.{WrappedCoin, prebalancedLovelaceDiffHandler}
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuilderResultSimple, explainConst}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import monocle.{Focus, Lens}
import scala.annotation.tailrec
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
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{ModifyAuxiliaryData, ReferenceOutput, Send, Spend, ValidityStartSlot}

sealed trait RefundTx {
    def tx: Transaction
    def mStartTime: Option[QuantizedInstant] = this match {
        case self: RefundTx.PostDated => Some(self.startTime)
        case _                        => None
    }
}

object RefundTx {
    export RefundTxOps.{Build, Parse, PartialResult}

    // TODO: shall we keep it for now?
    final case class Immediate(override val tx: Transaction) extends RefundTx, Tx[Immediate] {
        override val txLens: Lens[Immediate, Transaction] = Focus[Immediate](_.tx)
        override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
    }

    final case class PostDated(override val tx: Transaction, startTime: QuantizedInstant)
        extends RefundTx,
          Tx[PostDated] {
        override val txLens: Lens[PostDated, Transaction] = Focus[PostDated](_.tx)
        override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
    }
}

private object RefundTxOps {
    type Config = CardanoNetwork.Section & HeadPeers.Section & InitialBlock.Section

    object Build {
        final case class Immediate(override val config: Config)(
            override val refundInstructions: DepositUtxo.Refund.Instructions,
            override val refundValue: Value
        ) extends Build[RefundTx.Immediate] {
            override val mValidityStart: None.type = None
            override val stepRefundMetadata =
                ModifyAuxiliaryData(_ =>
                    Some(MD(MD.Refund(headAddress = config.headMultisigAddress)))
                )

            override def mkPartialResult(
                ctx: TransactionBuilder.Context,
                valueNeeded: Value
            ): PartialResult.Immediate =
                PartialResult.Immediate(ctx, valueNeeded, refundInstructions)
        }

        final case class PostDated(override val config: Config)(
            override val refundInstructions: DepositUtxo.Refund.Instructions,
            override val refundValue: Value,
        ) extends Build[RefundTx.PostDated] {
            override val mValidityStart: Some[QuantizedInstant] =
                Some(
                  refundInstructions.startTime.toEpochQuantizedInstant(
                    config.slotConfig
                  )
                )
            override val stepRefundMetadata =
                ModifyAuxiliaryData(_ =>
                    Some(MD(MD.Refund(headAddress = config.headMultisigAddress)))
                )

            override def mkPartialResult(
                ctx: TransactionBuilder.Context,
                valueNeeded: Value
            ): PartialResult.PostDated =
                PartialResult.PostDated(
                  ctx,
                  valueNeeded,
                  refundInstructions,
                  config.slotConfig
                )
        }
    }

    trait Build[T <: RefundTx] {
        def config: Config
        import BuilderOps.*

        def refundInstructions: DepositUtxo.Refund.Instructions
        def refundValue: Value

        def mValidityStart: Option[QuantizedInstant]
        def stepRefundMetadata: ModifyAuxiliaryData

        def mkPartialResult(
            ctx: TransactionBuilder.Context,
            valueNeededWithFee: Value
        ): PartialResult[T]

        final def partialResult: BuilderResultSimple[PartialResult[T]] = {
            val stepReferenceHNS = ReferenceOutput(config.multisigRegimeUtxo.asUtxo)
            // FIXME: We are not allowed to assumed the existence of the multisigRegimeUtxo here.
            //   We must attach the multisig script inline to the transaction.

            val refundOutput: TransactionOutput = TransactionOutput.Babbage(
              address = config.headMultisigAddress,
              value = refundValue,
              datumOption = refundInstructions.datum.asScala.map(Inline(_)),
              scriptRef = None
            )

            val sendRefund = Send(refundOutput)

            val setValidity = ValidityStartSlot(
              config.slotConfig.timeToSlot(refundInstructions.startTime.toLong)
            )

            val steps = List(stepRefundMetadata, stepReferenceHNS, setValidity, sendRefund)

            for {
                ctx <- TransactionBuilder
                    .build(config.network, steps)
                    .explainConst("adding base refund steps failed")

                valueNeeded = Placeholder.inputValueNeeded(ctx, config.cardanoProtocolParams)

                valueNeededWithFee <- trialFinishLoop(ctx, valueNeeded)
            } yield mkPartialResult(ctx, valueNeededWithFee)
        }

        @tailrec
        private def trialFinishLoop(
            ctx: TransactionBuilder.Context,
            trialValue: Value
        ): BuilderResultSimple[Value] = {
            val spendDeposit = Utxo(
              BuilderOps.Placeholder.utxoId,
              TransactionOutput.Babbage(
                address = config.headMultisigAddress,
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
                        config.headMultisigScript.requiredSigners
                      )
                    )
                  )
                )
                res <- addedSpendDeposit.finalizeContext(
                  config.cardanoProtocolParams,
                  prebalancedLovelaceDiffHandler,
                  config.plutusScriptEvaluatorForTxBuild,
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

    sealed trait PartialResult[T <: RefundTx] extends Tx.Builder.HasCtx {
        def refundInstructions: DepositUtxo.Refund.Instructions
        def inputValueNeeded: Value

        def postProcess(ctx: TransactionBuilder.Context): T

        final def complete(
            depositSpent: DepositUtxo,
            config: Config
        ): BuilderResultSimple[T] = for {
            addedDepositSpent <- TransactionBuilder
                .modify(
                  ctx,
                  List(
                    Spend(
                      depositSpent.toUtxo,
                      NativeScriptWitness(
                        NativeScriptAttached,
                        config.headMultisigScript.requiredSigners
                      )
                    )
                  )
                )
                .explainConst("adding real spend deposit failed.")
            finalized <- addedDepositSpent
                .finalizeContext(
                  config.cardanoProtocolParams,
                  prebalancedLovelaceDiffHandler,
                  config.plutusScriptEvaluatorForTxBuild,
                  Tx.Validators.nonSigningNonValidityChecksValidators
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

    object Parse {
        type ParseErrorOr[A] = Either[Error, A]

        enum Error extends Throwable {
            case MetadataParseError(wrapped: MD.ParseError)
            case TxCborDeserializationFailed(e: Throwable)
        }
    }

    final case class Parse(config: Config)(
        txBytes: Tx.Serialized,
    ) {
        import Parse.*
        import Parse.Error.*

        def result: ParseErrorOr[RefundTx] = {
            given ProtocolVersion = config.cardanoProtocolVersion

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
                                  Slot(startSlot).toQuantizedInstant(config.slotConfig)
                                )
                        }
                    } yield refundTx
                case Failure(e) => Left(TxCborDeserializationFailed(e))
            }

        }

    }
}
