package hydrozoa.rulebased.ledger.dapp.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.{TreasuryRedeemer, WithdrawRedeemer}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.{ResolvedDatum, RuleBasedTreasuryDatum}
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.ScriptSource.PlutusScriptValue
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend, ValidityEndSlot}
import scalus.ledger.api.v3.{TxId, TxOutRef}
import scalus.prelude.List as SList

final case class WithdrawTx(
    treasuryUtxoSpent: RuleBasedTreasuryUtxo,
    treasuryUtxoProduced: RuleBasedTreasuryUtxo,
    withdrawalOutputs: List[OutputL2],
    tx: Transaction
)

object WithdrawTx {

    case class Recipe(
        treasuryUtxo: RuleBasedTreasuryUtxo,
        // NB: The order doesn't matter in the recipe, since either all withdrawals should make it to the tx.
        withdrawals: UtxoSetL2,
        membershipProof: IArray[Byte],
        validityEndSlot: Long,
        network: Network,
        protocolParams: ProtocolParams,
        evaluator: PlutusScriptEvaluator,
        validators: Seq[Validator]
    )

    enum WithdrawalTxError:
        case InvalidTreasuryDatum(msg: String)
        case TreasuryNotResolved
        case InsufficientTreasuryFunds(negativeDiff: Value)
        case NoWithdrawals

    def build(recipe: Recipe): Either[SomeBuildError | WithdrawalTxError, WithdrawTx] = {
        import WithdrawalTxError.*

        for {
            _ <- if recipe.withdrawals.nonEmpty then Right(()) else Left(NoWithdrawals)
            treasuryDatum <- extractTreasuryDatum(recipe.treasuryUtxo)
            residualValue <- calculateResidualTreasury(recipe.treasuryUtxo, recipe.withdrawals)
            result <- buildWithdrawTx(recipe, treasuryDatum, residualValue)
        } yield result
    }

    private def extractTreasuryDatum(
        treasuryUtxo: RuleBasedTreasuryUtxo
    ): Either[WithdrawalTxError, ResolvedDatum] = {
        import WithdrawalTxError.*

        treasuryUtxo.datum match {
            case RuleBasedTreasuryDatum.Resolved(resolved) => Right(resolved)
            case RuleBasedTreasuryDatum.Unresolved(_)      => Left(TreasuryNotResolved)
        }
    }

    private def calculateResidualTreasury(
        treasuryUtxo: RuleBasedTreasuryUtxo,
        withdrawals: UtxoSetL2
    ): Either[WithdrawalTxError, Value] = {
        import WithdrawalTxError.*

        val totalWithdrawalValue = withdrawals.foldLeft(Value.zero)(_ + _._2.value)
        val treasuryValue = treasuryUtxo.value
        val residueValue = treasuryValue - totalWithdrawalValue

        if residueValue.isPositive
        then Right(residueValue)
        else Left(InsufficientTreasuryFunds(residueValue))
    }

    private def buildWithdrawTx(
        recipe: Recipe,
        treasuryDatum: ResolvedDatum,
        residualValue: Value
    ): Either[SomeBuildError, WithdrawTx] = {
        import recipe.*

        val proofBS = ByteString.fromArray(IArray.genericWrapArray(membershipProof).toArray)

        // From this point we should choose and stick to a particular order of withdrawals, so
        // to order of outputs in the tx (starting from index 1) and the order of utxo ids in
        // the redeemer should be the same.
        val withdrawalsList = withdrawals.untagged.toList

        val withdrawRedeemer = TreasuryRedeemer.Withdraw(
          WithdrawRedeemer(
            SList.from(
              withdrawalsList
                  .map((utxoId, _) => TxOutRef(TxId(utxoId.transactionId), utxoId.index))
            ),
            proofBS
          )
        )
        val newTreasuryDatum =
            RuleBasedTreasuryDatum.Resolved(treasuryDatum.copy(utxosActive = proofBS))

        // withdrawal outputs
        val withdrawalOutputs = withdrawalsList.map(_._2)

        for {
            context <- TransactionBuilder
                .build(
                  network,
                  List(
                    // Spend the treasury utxo with withdrawal proof
                    Spend(
                      treasuryUtxo.asUtxo,
                      ThreeArgumentPlutusScriptWitness(
                        PlutusScriptValue(RuleBasedTreasuryScript.compiledPlutusV3Script),
                        withdrawRedeemer.toData,
                        DatumInlined,
                        Set.empty
                      )
                    ),
                    // Send the remaining treasury back
                    Send(
                      Babbage(
                        address = treasuryUtxo.address,
                        value = residualValue,
                        datumOption = Some(Inline(newTreasuryDatum.toData)),
                        scriptRef = None
                      )
                    ),
                    ValidityEndSlot(validityEndSlot)
                  )
                      ++
                          // Outputs for withdrawals
                          withdrawalOutputs.map(Send(_))
                )

            finalized <- context
                .finalizeContext(
                  protocolParams = protocolParams,
                  diffHandler = new ChangeOutputDiffHandler(
                    protocolParams,
                    0
                  ).changeOutputDiffHandler,
                  evaluator = evaluator,
                  validators = validators
                )

            newTreasuryUtxo = RuleBasedTreasuryUtxo(
              treasuryTokenName = treasuryUtxo.treasuryTokenName,
              utxoId = TransactionInput(
                finalized.transaction.id,
                0
              ), // Treasury output index
              address = treasuryUtxo.address,
              datum = treasuryUtxo.datum,
              value = residualValue
            )

        } yield WithdrawTx(
          treasuryUtxoSpent = treasuryUtxo,
          treasuryUtxoProduced = newTreasuryUtxo,
          withdrawalOutputs = withdrawalOutputs.toList,
          tx = finalized.transaction
        )
    }
}
