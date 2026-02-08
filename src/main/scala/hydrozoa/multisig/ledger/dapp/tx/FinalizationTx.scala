package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toQuantizedInstant}
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.Finalization
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuildErrorOr, explainConst}
import hydrozoa.multisig.ledger.dapp.txseq.RolloutTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, MultisigTreasuryUtxo, RolloutUtxo}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import monocle.{Focus, Lens}
import scala.collection.immutable.Vector
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Coin, Sized, Slot, Transaction, TransactionInput, TransactionOutput as TxOutput, Utxo, Value}
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*

// TODO: why don't we have direct payouts here?
sealed trait FinalizationTx
    extends Tx[FinalizationTx],
      BlockVersion.Major.Produced,
      MultisigTreasuryUtxo.Spent,
      RolloutUtxo.MbProduced,
      HasResolvedUtxos,
      HasValidityEnd {
    def tx: Transaction
    def txLens: Lens[FinalizationTx, Transaction]
}

object FinalizationTx {
    export FinalizationTxOps.Build

    sealed trait WithPayouts extends FinalizationTx

    sealed trait NoRollouts extends FinalizationTx

    case class NoPayouts(
        override val validityEnd: QuantizedInstant,
        override val tx: Transaction,
        override val majorVersionProduced: BlockVersion.Major,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val resolvedUtxos: ResolvedUtxos,
        override val txLens: Lens[FinalizationTx, Transaction] =
            Focus[NoPayouts](_.tx).asInstanceOf[Lens[FinalizationTx, Transaction]]
    ) extends NoRollouts

    case class WithOnlyDirectPayouts(
        override val validityEnd: QuantizedInstant,
        override val tx: Transaction,
        override val majorVersionProduced: BlockVersion.Major,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val resolvedUtxos: ResolvedUtxos,
        override val txLens: Lens[FinalizationTx, Transaction] =
            Focus[WithOnlyDirectPayouts](_.tx).asInstanceOf[Lens[FinalizationTx, Transaction]]
    ) extends WithPayouts,
          NoRollouts

    case class WithRollouts(
        override val validityEnd: QuantizedInstant,
        override val tx: Transaction,
        override val majorVersionProduced: BlockVersion.Major,
        override val treasurySpent: MultisigTreasuryUtxo,
        override val rolloutProduced: RolloutUtxo,
        override val resolvedUtxos: ResolvedUtxos,
        override val txLens: Lens[FinalizationTx, Transaction] =
            Focus[WithRollouts](_.tx).asInstanceOf[Lens[FinalizationTx, Transaction]]
    ) extends WithPayouts,
          RolloutUtxo.Produced

}

private object FinalizationTxOps {
    sealed trait Result[T <: FinalizationTx] extends Tx.AugmentedResult[T]

    object Result {
        type NoRollouts = Result[FinalizationTx.NoRollouts]

        sealed trait WithPayouts extends Result[FinalizationTx.WithPayouts]

        case class NoPayouts(
            override val transaction: FinalizationTx.NoPayouts
        ) extends Result[FinalizationTx.NoPayouts]

        case class WithOnlyDirectPayouts(
            override val transaction: FinalizationTx.WithOnlyDirectPayouts,
        ) extends WithPayouts

        case class WithRollouts(
            override val transaction: FinalizationTx.WithRollouts,
            rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult,
        ) extends WithPayouts
    }

    type ResultFor[T <: FinalizationTx] <: Result[?] = T match {
        case FinalizationTx.NoPayouts             => Result.NoPayouts
        case FinalizationTx.WithPayouts           => Result.WithPayouts
        case FinalizationTx.WithOnlyDirectPayouts => Result.WithOnlyDirectPayouts
        case FinalizationTx.WithRollouts          => Result.WithRollouts
    }

    object Build {
        type Config = CardanoNetwork.Section & FallbackContingency.Section & HeadPeers.Section &
            InitialBlock.Section & InitializationParameters.Section

        case class NoPayouts(
            override val config: Config,
            override val kzgCommitment: KzgCommitment,
            override val majorVersionProduced: BlockVersion.Major,
            override val treasuryToSpend: MultisigTreasuryUtxo,
            override val depositsToSpend: Vector[DepositUtxo],
            override val validityEnd: QuantizedInstant,
        ) extends Build[FinalizationTx.NoPayouts](
              mbRolloutTxSeqPartial = None
            ) {
            override def complete(state: State): BuildErrorOr[ResultFor[FinalizationTx.NoPayouts]] =
                Right(CompleteNoPayouts(state))
        }

        case class WithPayouts(
            override val config: Config,
            override val kzgCommitment: KzgCommitment,
            override val majorVersionProduced: BlockVersion.Major,
            override val treasuryToSpend: MultisigTreasuryUtxo,
            override val depositsToSpend: Vector[DepositUtxo],
            override val validityEnd: QuantizedInstant,
            rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult
        ) extends Build[FinalizationTx.WithPayouts](
              mbRolloutTxSeqPartial = Some(rolloutTxSeqPartial)
            ) {
            override def complete(
                state: State
            ): BuildErrorOr[ResultFor[FinalizationTx.WithPayouts]] =
                CompleteWithPayouts(state, rolloutTxSeqPartial)
        }
    }

    sealed trait Build[T <: FinalizationTx](
        mbRolloutTxSeqPartial: Option[RolloutTxSeq.Builder.PartialResult]
    ) {
        import Build.*

        def config: Config
        def kzgCommitment: KzgCommitment
        def majorVersionProduced: BlockVersion.Major
        def treasuryToSpend: MultisigTreasuryUtxo
        def depositsToSpend: Vector[DepositUtxo]
        def validityEnd: QuantizedInstant

        def complete(state: State): BuildErrorOr[ResultFor[T]]

        final def result: BuildErrorOr[ResultFor[T]] = for {
            pessimistic <- BasePessimistic()

            finished <- TxBuilder
                .finalizeContext(pessimistic.ctx)
                .explainConst("finishing finalization tx failed")

            completed <- complete(pessimistic.copy(ctx = finished))
        } yield completed

        final case class State(override val ctx: TransactionBuilder.Context)
            extends Tx.Builder.HasCtx

        private object BasePessimistic {
            def apply(): BuildErrorOr[State] = for {
                _ <- Either
                    .cond(checkTreasuryToSpendAddress, (), ???)
                    .explainConst("treasury to spend has incorrect head address")
                _ <- Either
                    .cond(checkEquityAdaOnly, (), ???)
                    .explainConst("L2 liabilities don't cover all L1 non-ADA assets")
                ctx <- TransactionBuilder
                    .build(config.network, definiteSteps)
                    .explainConst("definite steps failed")
                addedPessimisticRollout <- BasePessimistic
                    .mbApplySendRollout(ctx)
                    .explainConst("sending the rollout tx failed in base pessimistic")
                _ <- TxBuilder
                    .finalizeContext(addedPessimisticRollout)
                    .explainConst("finishing base pessimistic failed")
            } yield State(ctx = ctx)

            /////////////////////////////////////////////////////////
            // Checks

            // TODO: Ensure this holds by construction
            private def checkTreasuryToSpendAddress: Boolean =
                treasuryToSpend.address == config.headMultisigAddress

            private def checkEquityAdaOnly: Boolean = remainingEquityValue.assets.isEmpty

            /////////////////////////////////////////////////////////
            // Base steps
            private val modifyAuxiliaryData = ModifyAuxiliaryData(_ =>
                Some(MD(Finalization(headAddress = config.headMultisigAddress)))
            )

            private val validityEndSlot = ValidityEndSlot(validityEnd.toSlot.slot)

            private val baseSteps = List(modifyAuxiliaryData, validityEndSlot)

            /////////////////////////////////////////////////////////
            // Burn steps
            private val burnTreasuryToken = Mint(
              config.headMultisigScript.script.scriptHash,
              config.headTokenNames.treasuryTokenName,
              -1,
              config.headMultisigScript.witnessValue
            )

            private val burnMultisigRegimeToken = Mint(
              config.headMultisigScript.script.scriptHash,
              config.headTokenNames.multisigRegimeTokenName,
              -1,
              config.headMultisigScript.witnessAttached
            )

            private val burnSteps = List(burnTreasuryToken, burnMultisigRegimeToken)

            /////////////////////////////////////////////////////////
            // Spend steps
            private val spendTreasury =
                Spend(treasuryToSpend.asUtxo, config.headMultisigScript.witnessAttached)

            private val spendMultisigRegime =
                Spend(config.multisigRegimeUtxo.asUtxo, config.headMultisigScript.witnessAttached)

            private val spendSteps = List(spendTreasury, spendMultisigRegime)

            /////////////////////////////////////////////////////////
            // Send rollout (maybe)
            private def mkRolloutOutput(value: Value): TxOutput.Babbage = TxOutput.Babbage(
              address = config.headMultisigAddress,
              value = value,
              datumOption = None,
              scriptRef = None
            )

            private val mbRolloutValue: Option[Value] =
                mbRolloutTxSeqPartial.map(_.firstOrOnly.inputValueNeeded)

            private val mbRolloutOutput: Option[TxOutput.Babbage] =
                mbRolloutValue.map(mkRolloutOutput)

            /** We apply this step if the first rollout tx doesn't get merged into the finalization
              * tx.
              */
            def mbApplySendRollout(
                ctx: TransactionBuilder.Context
            ): Either[SomeBuildError, TransactionBuilder.Context] =
                mbRolloutOutput.fold(Right(ctx))((output: TxOutput.Babbage) =>
                    TransactionBuilder.modify(ctx, List(Send(output)))
                )

            /////////////////////////////////////////////////////////
            // Send peer payouts
            private def mkPeerPayout(addr: ShelleyAddress, lovelace: Coin): Send = Send(
              TxOutput.Babbage(
                address = addr,
                value = Value(lovelace),
                datumOption = None,
                scriptRef = None,
              )
            )

            private val remainingEquityValue: Value =
                mbRolloutValue.fold(treasuryToSpend.value)(treasuryToSpend.value - _)

            private val remainingEquityLovelace: Coin = remainingEquityValue.coin

            private val headPeerAddresses = config.headPeerAddresses

            private val equityPayouts = config
                .distributeEquity(remainingEquityLovelace)
                .toSortedMap
                .withDefault(_ => Coin.zero)

            private val contingencyPayouts =
                config.distributeFallbackContingencyInFinalization.toSortedMap
                    .withDefault(_ => Coin.zero)

            private val sendPeerPayouts = headPeerAddresses.toSortedMap
                .transform((pNum, addr) =>
                    mkPeerPayout(addr, contingencyPayouts(pNum) + equityPayouts(pNum))
                )
                .values

            /////////////////////////////////////////////////////////
            // Definite steps
            private val definiteSteps: List[TransactionBuilderStep] =
                baseSteps ++ spendSteps ++ burnSteps ++ sendPeerPayouts
        }

        private[tx] object CompleteNoPayouts {
            def apply(state: State): Result.NoPayouts = {
                val finalizationTx: FinalizationTx.NoPayouts = FinalizationTx.NoPayouts(
                  validityEnd = Slot(state.ctx.transaction.body.value.ttl.get)
                      .toQuantizedInstant(config.cardanoInfo.slotConfig),
                  tx = state.ctx.transaction,
                  majorVersionProduced = majorVersionProduced,
                  treasurySpent = treasuryToSpend,
                  resolvedUtxos = state.ctx.resolvedUtxos
                )
                Result.NoPayouts(transaction = finalizationTx)
            }
        }

        private[tx] object CompleteWithPayouts {

            /** When building a finalization transaction with payouts, try to merge the first
              * rollout, and then apply post-processing to assemble the result. Assumes that:
              *
              *   - The spent treasury utxo is the first input (unchecked).
              *   - The first N outputs are peer payouts (unchecked).
              *   - The next output after that is the rollout utxo, if produced (asserted).
              *
              * @throws AssertionError
              *   when the asserted assumptions are broken.
              */
            @throws[AssertionError]
            def apply(
                state: State,
                rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult,
            ): BuildErrorOr[Result.WithPayouts] = for {
                mergeTrial <- TryMerge(state, rolloutTxSeqPartial)
            } yield {
                import TryMerge.Result.*

                val (finished, mergeResult) = mergeTrial

                val tx = finished.ctx.transaction

                def withOnlyDirectPayouts: Result.WithOnlyDirectPayouts =
                    Result.WithOnlyDirectPayouts(
                      transaction = FinalizationTx.WithOnlyDirectPayouts(
                        majorVersionProduced = majorVersionProduced,
                        treasurySpent = treasuryToSpend,
                        tx = tx,
                        // this is safe since we always set ttl
                        validityEnd = Slot(tx.body.value.ttl.get)
                            .toQuantizedInstant(config.cardanoInfo.slotConfig),
                        resolvedUtxos = finished.ctx.resolvedUtxos
                      )
                    )

                def withRollouts(
                    rollouts: RolloutTxSeq.Builder.PartialResult,
                ): Result.WithRollouts =
                    Result.WithRollouts(
                      transaction = FinalizationTx.WithRollouts(
                        majorVersionProduced = majorVersionProduced,
                        treasurySpent = treasuryToSpend,
                        rolloutProduced = unsafeGetRolloutProduced(finished.ctx),
                        tx = tx,
                        // this is safe since we always set ttl
                        validityEnd = Slot(tx.body.value.ttl.get)
                            .toQuantizedInstant(config.cardanoInfo.slotConfig),
                        resolvedUtxos = finished.ctx.resolvedUtxos
                      ),
                      rolloutTxSeqPartial = rollouts,
                    )

                mergeResult match {
                    case NotMerged => withRollouts(rolloutTxSeqPartial)
                    case Merged(mbFirstSkipped) =>
                        mbFirstSkipped match {
                            case None => withOnlyDirectPayouts
                            case Some(firstSkipped) =>
                                withRollouts(firstSkipped.partialResult)
                        }
                }
            }

            /** Given the transaction context of a [[Builder.WithPayouts]] that has finished
              * building, apply post-processing to get the [[RolloutUtxo]] produced by the
              * [[FinalizationTx.WithRollouts]], if it was produced. Assumes that the rollout
              * produced immediately follows the N peer payouts.
              *
              * @param ctx
              *   The transaction context of a finished builder state.
              * @throws AssertionError
              *   when the assumption is broken.
              * @return
              */
            private def unsafeGetRolloutProduced(
                ctx: TransactionBuilder.Context
            ): RolloutUtxo = {
                val tx = ctx.transaction
                val outputs = tx.body.value.outputs

                assert(outputs.nonEmpty)
                val outputsTail = outputs.tail

                assert(outputsTail.nonEmpty)
                val rolloutOutput = outputsTail.head.value

                RolloutUtxo(
                  Utxo(
                    TransactionInput(transactionId = tx.id, index = config.nHeadPeers),
                    rolloutOutput
                  )
                )
            }

            object TryMerge {
                enum Result {
                    case NotMerged
                    case Merged(
                        mbRolloutTxSeqPartialSkipped: Option[
                          RolloutTxSeq.Builder.PartialResult.SkipFirst
                        ]
                    )
                }

                def apply(
                    state: State,
                    rolloutTxSeqPartial: RolloutTxSeq.Builder.PartialResult
                ): BuildErrorOr[(State, TryMerge.Result)] =
                    import TryMerge.Result.*
                    import state.*

                    val firstRolloutTxPartial = rolloutTxSeqPartial.firstOrOnly

                    val rolloutTx: Transaction = firstRolloutTxPartial.ctx.transaction

                    def sendOutput(x: Sized[TxOutput]): Send = Send(x.value)

                    val optimisticSteps: List[Send] =
                        rolloutTx.body.value.outputs.map(sendOutput).toList

                    val optimisticTrial: BuildErrorOr[TransactionBuilder.Context] = for {
                        newCtx <- TransactionBuilder
                            .modify(ctx, optimisticSteps)
                            .explainConst("adding optimistic steps failed")
                        finished <- TxBuilder
                            .finalizeContext(newCtx)
                            .explainConst("finishing optimistic trial failed")
                    } yield finished

                    lazy val pessimisticBackup: BuildErrorOr[TransactionBuilder.Context] =
                        for {
                            newCtx <- BasePessimistic
                                .mbApplySendRollout(ctx)
                                .explainConst("pessmistic backup in merging failed")
                            finished <- TxBuilder
                                .finalizeContext(newCtx)
                                .explainConst(
                                  "finishing pessimistic backup failed"
                                )
                        } yield finished

                    // Keep the optimistic transaction (which merged the finalization tx with the first rollout tx)
                    // if it worked out. Otherwise, use the pessimistic transaction.
                    for {
                        newCtx <- optimisticTrial.orElse(pessimisticBackup)

                        finishedState = State(ctx = newCtx)

                        mergeResult =
                            if optimisticTrial.isLeft then NotMerged
                            else Merged(rolloutTxSeqPartial.skipFirst)

                    } yield (finishedState, mergeResult)
            }
        }

        private object PostProcess {

            /** Given the transaction context of a [[Builder]] that has finished building, apply
              * post-processing to get the [[MultisigTreasuryUtxo]] produced by the
              * [[FinalizationTx]]. Assumes that the treasury output is present in the transaction
              * and is the first output.
              *
              * @param ctx
              *   The transaction context of a finished builder state.
              * @throws AssertionError
              *   when the assumption is broken.
              * @return
              */
            @throws[AssertionError]
            def getTreasuryProduced(
                majorVersion: BlockVersion.Major,
                treasurySpent: MultisigTreasuryUtxo,
                ctx: State
            ): MultisigTreasuryUtxo = {
                val tx = ctx.ctx.transaction
                val outputs = tx.body.value.outputs

                assert(outputs.nonEmpty)
                // TODO: Throw other assertion errors in `.fromUtxo` and instead of `.get`?
                MultisigTreasuryUtxo
                    .fromUtxo(Utxo(TransactionInput(tx.id, 0), outputs.head.value))
                    .get
            }
        }

        private object TxBuilder {
            private val diffHandler = Change.changeOutputDiffHandler(
              _,
              _,
              protocolParams = config.cardanoProtocolParams,
              changeOutputIdx = 0
            )

            def finalizeContext(
                ctx: TransactionBuilder.Context
            ): Either[SomeBuildError, TransactionBuilder.Context] =
                ctx.finalizeContext(
                  config.cardanoProtocolParams,
                  diffHandler = diffHandler,
                  evaluator = config.plutusScriptEvaluatorForTxBuild,
                  validators = Tx.Validators.nonSigningValidators
                )
        }

    }

}
