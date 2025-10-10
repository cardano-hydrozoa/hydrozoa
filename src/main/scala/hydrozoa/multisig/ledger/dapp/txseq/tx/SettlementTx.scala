package hydrozoa.multisig.ledger.dapp.txseq.tx

import hydrozoa.lib.tx.BuildError.{BalancingError, StepError, ValidationError}
import hydrozoa.lib.tx.TransactionBuilder
import hydrozoa.lib.tx.TransactionBuilderStep.{Mint, *}
import hydrozoa.lib.tx.{
    BuildError,
    TransactionBuilder,
    TransactionBuilderStep,
    TransactionUnspentOutput
}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.{RolloutTx, SettlementTx, Metadata as MD}
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo.mkMultisigTreasuryDatum
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.TransactionMetadatum.Int
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.txbuilder.{TxBalancingError, BuilderContext as ScalusBuilderContext}
import scalus.cardano.ledger.{Mint as SMint, *}

import scala.annotation.tailrec

object SettlementTx {
    object Builder {
        final case class Result(
            settlementTx: SettlementTx,
            override val absorbedDeposits: List[DepositUtxo],
            override val remainingDeposits: List[DepositUtxo]
        ) extends HasDepositsPartition

        trait HasDepositsPartition {
            def absorbedDeposits: List[DepositUtxo]
            def remainingDeposits: List[DepositUtxo]
        }

        trait HasTxBuilderContext {
            def txBuilderContext: TransactionBuilder.Context
        }

        trait HasSendRollout {
            def sendRollout: Option[Send]
        }

        final case class BasePessimistic(
            override val txBuilderContext: TransactionBuilder.Context,
            override val sendRollout: Option[Send]
        ) extends HasTxBuilderContext,
              HasSendRollout

        final case class AddedDeposits(
            override val absorbedDeposits: List[DepositUtxo],
            override val remainingDeposits: List[DepositUtxo],
            override val txBuilderContext: TransactionBuilder.Context,
            override val sendRollout: Option[Send]
        ) extends HasTxBuilderContext,
              HasSendRollout,
              HasDepositsPartition

        final case class AddedPayouts(
            remainingPayouts: List[Babbage],
            override val txBuilderContext: TransactionBuilder.Context,
            override val sendRollout: Option[Send]
        ) extends HasTxBuilderContext,
              HasSendRollout
    }

    final case class Builder(
        majorVersion: Int,
        deposits: List[DepositUtxo],
        payouts: List[Babbage],
        treasuryUtxo: TreasuryUtxo,
        rolloutTokenName: AssetName,
        headNativeScript: HeadMultisigScript,
        // The reference script for the HNS should live inside the multisig regime witness UTxO
        headNativeScriptReferenceInput: TransactionUnspentOutput,
        context: ScalusBuilderContext
    ) {
        import Builder.*
        import BuilderUtils.*

        object PartialResult {
            def needsRolloutFees: Either[BuildError, PartialResult.NeedsRolloutFees] = {
                for {
                    pessimistic <- basePessimistic
                    addedDeposits <- addDeposits(pessimistic)
                    addedPayouts <- addPayouts(addedDeposits)
                    partialResult = mkNeedsRolloutFees(addedDeposits, addedPayouts)
                } yield partialResult
            }
            
            final case class NeedsRolloutFees(
                override val txBuilderContext: TransactionBuilder.Context,
                override val sendRollout: Option[Send],
                override val absorbedDeposits: List[DepositUtxo],
                override val remainingDeposits: List[DepositUtxo]
            ) extends HasTxBuilderContext,
                  HasSendRollout,
                  HasDepositsPartition {
                def complete(rolloutFees: Coin): Either[BuildError, Result] =
                    addRolloutFees(this, rolloutFees)
            }
        }

        def basePessimistic: Either[BuildError, BasePessimistic] =
            ???

        def addDeposits(state: BasePessimistic): Either[BuildError, AddedDeposits] =
            ???

        def tryAddDeposit(
            txBuilderContext: TransactionBuilder.Context,
            mockSendTreasury: Send,
            mockSendRollout: Send,
            deposit: DepositUtxo
        ): Either[BuildError, TransactionBuilder.Context] = ???

        def addPayouts(state: AddedDeposits): Either[BuildError, AddedPayouts] =
            ???

        def tryAddPayout(
            txBuilderContext: TransactionBuilder.Context,
            mockSendTreasury: Send,
            mockSendRollout: Send,
            payout: Babbage
        ): TransactionBuilder.Context = ???

        def addRolloutFees(
            state: PartialResult.NeedsRolloutFees,
            rolloutFees: Coin
        ): Either[BuildError, Result] =
            ???

        object BuilderUtils {
            def skipAddingDeposits(state: BasePessimistic): AddedDeposits = {
                import state.*
                AddedDeposits(
                    absorbedDeposits = List.empty,
                    remainingDeposits = deposits,
                    txBuilderContext = txBuilderContext,
                    sendRollout = sendRollout
                )
            }

            def skipAddingPayouts(state: AddedDeposits): AddedPayouts = {
                import state.*
                AddedPayouts(
                    remainingPayouts = payouts,
                    txBuilderContext = txBuilderContext,
                    sendRollout = sendRollout
                )
            }

            def mkNeedsRolloutFees(
                addedDeposits: AddedDeposits,
                addedPayouts: AddedPayouts
            ): PartialResult.NeedsRolloutFees =
                PartialResult.NeedsRolloutFees(
                    txBuilderContext = addedPayouts.txBuilderContext,
                    sendRollout = addedPayouts.sendRollout,
                    absorbedDeposits = addedDeposits.absorbedDeposits,
                    remainingDeposits = addedDeposits.remainingDeposits
                )

            def finalize(
                state: HasTxBuilderContext & HasSendRollout
            ): Either[BuildError, TransactionBuilder.Context] = {
                import state.*
                for {
                    // Add the rollout output step to tx-builder context
                    addedRolloutOutput <- TransactionBuilder
                        .modify(txBuilderContext, sendRollout.toList)
                        .left
                        .map(StepError(_))

                    // Try to build, balance, and validate the resulting transaction
                    finalized <- addedRolloutOutput
                        .finalizeContext(
                            protocolParams = context.protocolParams,
                            diffHandler = ChangeOutputDiffHandler(
                                protocolParams = context.protocolParams,
                                changeOutputIdx = 0
                            ).changeOutputDiffHandler,
                            evaluator = context.evaluator,
                            validators = context.validators
                        )
                        .left
                        .map({
                            case balanceError: TxBalancingError =>
                                BalancingError(balanceError)
                            case validationError: TransactionException =>
                                ValidationError(validationError)
                        })
                } yield finalized
            }
        }
    }
}
