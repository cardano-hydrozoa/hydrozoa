package hydrozoa.rulebased.ledger.dapp.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.lib.tx.Datum.DatumInlined
import hydrozoa.lib.tx.ScriptSource.PlutusScriptValue
import hydrozoa.lib.tx.TransactionBuilderStep.{Send, Spend, ValidityEndSlot}
import hydrozoa.lib.tx.{
    SomeBuildError,
    ThreeArgumentPlutusScriptWitness,
    TransactionBuilder,
    TransactionUnspentOutput
}
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.{
    TreasuryRedeemer,
    WithdrawRedeemer
}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.{ResolvedDatum, RuleBasedTreasuryDatum}
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.*
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
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

        val proofBS = ByteString.fromArray(IArray.genericWrapArray(recipe.membershipProof).toArray)
        val withdrawRedeemer = TreasuryRedeemer.Withdraw(
          WithdrawRedeemer(
            SList.from(
              withdrawals.keys
                  .map(_.untagged)
                  .map(utxoId => TxOutRef(TxId(utxoId.transactionId), utxoId.index))
            ),
            proofBS
          )
        )
        val newTreasuryDatum =
            RuleBasedTreasuryDatum.Resolved(treasuryDatum.copy(utxosActive = proofBS))

        // withdrawal outputs
        val withdrawalOutputs = recipe.withdrawals.values

        for {
            context <- TransactionBuilder
                .build(
                  recipe.network,
                  List(
                    // Spend the treasury utxo with withdrawal proof
                    Spend(
                      TransactionUnspentOutput(treasuryUtxo.toUtxo),
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
                        address = treasuryUtxo.addr,
                        value = residualValue,
                        datumOption = Some(Inline(newTreasuryDatum.toData)),
                        scriptRef = None
                      )
                    ),
                    ValidityEndSlot(recipe.validityEndSlot)
                  )
                      ++
                          // Outputs for withdrawals
                          withdrawalOutputs.map(Send(_))
                )

            finalized <- context
                .finalizeContext(
                  protocolParams = recipe.protocolParams,
                  diffHandler = new ChangeOutputDiffHandler(
                    recipe.protocolParams,
                    0
                  ).changeOutputDiffHandler,
                  evaluator = recipe.evaluator,
                  validators = recipe.validators
                )

            newTreasuryUtxo = RuleBasedTreasuryUtxo(
              beaconTokenName = recipe.treasuryUtxo.beaconTokenName,
              txId = TransactionInput(
                finalized.transaction.id,
                0
              ), // Treasury output index
              addr = recipe.treasuryUtxo.addr,
              datum = recipe.treasuryUtxo.datum,
              value = residualValue
            )

        } yield WithdrawTx(
          treasuryUtxoSpent = recipe.treasuryUtxo,
          treasuryUtxoProduced = newTreasuryUtxo,
          withdrawalOutputs = withdrawalOutputs.toList,
          tx = finalized.transaction
        )
    }
}
