package hydrozoa.rulebased.ledger.dapp.tx

import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.tx.Tx
import hydrozoa.multisig.ledger.virtual.commitment.Membership
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.{TreasuryRedeemer, WithdrawRedeemer}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.{ResolvedDatum, RuleBasedTreasuryDatum}
import hydrozoa.rulebased.ledger.dapp.tx.Withdrawal.Builder.calculateResidualTreasury
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import scala.annotation.tailrec
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionException.{ExUnitsExceedMaxException, InvalidTransactionSizeException}
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.ScriptSource.PlutusScriptValue
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.ledger.api.v3.{TxId, TxOutRef}
import scalus.prelude.List as SList

object Withdrawal {
    case class Config(cardanoInfo: CardanoInfo, changeAddress: ShelleyAddress)

    case class Tx(
        treasuryUtxoSpent: RuleBasedTreasuryUtxo,
        treasuryUtxoProduced: RuleBasedTreasuryUtxo,
        withdrawalOutputs: List[OutputL2],
        tx: Transaction
    )

    object Builder {
        def extractTreasuryDatum(
            treasuryUtxo: RuleBasedTreasuryUtxo
        ): Either[Builder.Error, ResolvedDatum] = {

            treasuryUtxo.datum match {
                case RuleBasedTreasuryDatum.Resolved(resolved) => Right(resolved)
                case RuleBasedTreasuryDatum.Unresolved(_)      => Left(Error.TreasuryNotResolved)
            }
        }

        def calculateResidualTreasury(
            treasuryUtxo: RuleBasedTreasuryUtxo,
            withdrawals: UtxoSetL2
        ): Either[Builder.Error, Value] = {

            val totalWithdrawalValue = withdrawals.foldLeft(Value.zero)(_ + _._2.value)
            val treasuryValue = treasuryUtxo.value
            val residueValue = treasuryValue - totalWithdrawalValue

            if residueValue.isPositive
            then Right(residueValue)
            else Left(Error.InsufficientTreasuryFunds(residueValue))
        }

        case class Recipe(
            treasuryUtxo: RuleBasedTreasuryUtxo,
            withdrawalsSubset: UtxoSetL2,
            activeSet: UtxoSetL2,
            feeUtxos: UtxoSetL1
        ) {
            def halve: Recipe =
                val newSubset = withdrawalsSubset.drop(withdrawalsSubset.size / 2)
                Recipe(
                  treasuryUtxo,
                  UtxoSet[L2](newSubset),
                  activeSet,
                  feeUtxos
                )
        }

        enum Error:
            case InvalidTreasuryDatum(msg: String)
            case TreasuryNotResolved
            case InsufficientTreasuryFunds(negativeDiff: Value)
            case NoWithdrawals
            case NotASubset(withdrawalsSubset: UtxoSetL2, activeSet: UtxoSetL2)

    }
}

case class Withdrawal(config: Withdrawal.Config) {

    /** Attempts to build a withdrawal transaction. If the transaction build fails because of tx
      * size or ex unit restrictions, it will halve the size of the withdrawal subset and try again.
      */
    // TODO: There's a bunch of things that could be hoisted out of the loop
    @tailrec
    final def build(recipe: Withdrawal.Builder.Recipe): Either[
      Membership.MembershipCheckError | Withdrawal.Builder.Error | SomeBuildError,
      Withdrawal.Tx
    ] =
        (for {
            // TODO these lines can be moved into a smart constructor for Recipe
            _ <-
                if recipe.withdrawalsSubset.nonEmpty then Right(())
                else Left(Withdrawal.Builder.Error.NoWithdrawals)
            _ <-
                if recipe.withdrawalsSubset.asScalus.keySet
                        .subsetOf(recipe.activeSet.asScalus.keySet)
                then Right(())
                else
                    Left(
                      Withdrawal.Builder.Error.NotASubset(
                        withdrawalsSubset = recipe.withdrawalsSubset,
                        activeSet = recipe.activeSet
                      )
                    )

            // TODO: This can be hoisted outside the loop
            treasuryDatum <- Withdrawal.Builder.extractTreasuryDatum(recipe.treasuryUtxo)

            membershipProof <- Membership.mkMembershipProofValidated(
              recipe.activeSet.asScalus,
              recipe.withdrawalsSubset.asScalus
            )

            proofBS = ByteString.fromArray(IArray.genericWrapArray(membershipProof).toArray)

            // From this point we should choose and stick to a particular order of withdrawals, so
            // to order of outputs in the tx (starting from index 1) and the order of utxo ids in
            // the redeemer should be the same.
            withdrawalsList = recipe.withdrawalsSubset.asScalus.toList

            withdrawRedeemer = TreasuryRedeemer.Withdraw(
              WithdrawRedeemer(
                SList.from(
                  withdrawalsList
                      .map((utxoId, _) => TxOutRef(TxId(utxoId.transactionId), utxoId.index))
                ),
                proofBS
              )
            )
            newTreasuryDatum =
                RuleBasedTreasuryDatum.Resolved(treasuryDatum.copy(utxosActive = proofBS))

            // withdrawal outputs
            withdrawalOutputs = withdrawalsList.map(_._2)

            residualValue <- calculateResidualTreasury(
              recipe.treasuryUtxo,
              recipe.withdrawalsSubset
            )

            context <- TransactionBuilder
                .build(
                  config.cardanoInfo.network,
                  List(
                    // Spend the treasury utxo with withdrawal proof
                    Spend(
                      recipe.treasuryUtxo.asUtxo,
                      ThreeArgumentPlutusScriptWitness(
                        PlutusScriptValue(RuleBasedTreasuryScript.compiledPlutusV3Script),
                        withdrawRedeemer.toData,
                        DatumInlined,
                        Set.empty
                      )
                    ),
                    // Create the empty change utxo
                    Send(
                      Babbage(
                        address = config.changeAddress,
                        value = Value.zero,
                        datumOption = None,
                        scriptRef = None
                      )
                    ),
                    // Send the remaining treasury back
                    Send(
                      Babbage(
                        address = recipe.treasuryUtxo.address,
                        value = residualValue,
                        datumOption = Some(Inline(newTreasuryDatum.toData)),
                        scriptRef = None
                      )
                    )
                  )
                      ++
                          // Outputs for withdrawals
                          withdrawalOutputs.map(Send(_))
                          // Spend the fee utxos
                          ++ recipe.feeUtxos.toList.map((ti, to) =>
                              Spend(scalus.cardano.ledger.Utxo(ti, to), PubKeyWitness)
                          )
                )

            finalized <- context
                .finalizeContext(
                  protocolParams = config.cardanoInfo.protocolParams,
                  diffHandler =
                      Change.changeOutputDiffHandler(_, _, config.cardanoInfo.protocolParams, 0),
                  evaluator = PlutusScriptEvaluator(config.cardanoInfo, EvaluateAndComputeCost),
                  validators = Tx.Validators.nonSigningValidators
                )

            newTreasuryUtxo = RuleBasedTreasuryUtxo(
              utxoId = TransactionInput(
                finalized.transaction.id,
                0
              ), // Treasury output index
              address = recipe.treasuryUtxo.address,
              datum = recipe.treasuryUtxo.datum,
              value = residualValue
            )

        } yield Withdrawal.Tx(
          treasuryUtxoSpent = recipe.treasuryUtxo,
          treasuryUtxoProduced = newTreasuryUtxo,
          withdrawalOutputs = withdrawalOutputs.map(Output[L2]),
          tx = finalized.transaction
        )) match {
            case Right(w) => Right(w)
            case Left(SomeBuildError.ValidationError(e: InvalidTransactionSizeException, ctx)) =>
                build(recipe.halve)
            case Left(SomeBuildError.ValidationError(e: ExUnitsExceedMaxException, ctx)) =>
                build(recipe.halve)
            case Left(e) => Left(e)
        }

}
