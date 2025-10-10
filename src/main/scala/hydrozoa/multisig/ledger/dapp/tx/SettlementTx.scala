package hydrozoa.multisig.ledger.dapp.tx

import cats.implicits.*
import hydrozoa.lib.tx.*
import hydrozoa.lib.tx.ScriptSource.NativeScriptValue
import hydrozoa.lib.tx.TransactionBuilderStep.{ModifyAuxiliaryData, Send, Spend}
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo.mkMultisigTreasuryDatum
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, RolloutUtxo, TreasuryUtxo}
import scala.collection
import scala.language.{implicitConversions, reflectiveCalls}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler

final case class SettlementTx(
    treasurySpent: TreasuryUtxo,
    treasuryProduced: TreasuryUtxo,
    depositsSpent: List[DepositUtxo],
    rolloutProduced: Option[RolloutUtxo],
    override val tx: Transaction
) extends Tx

object SettlementTx {
    case class Recipe(
        majorVersion: Int,
        deposits: List[DepositUtxo],
        utxosWithdrawn: Map[TransactionInput, TransactionOutput],
        treasuryUtxo: TreasuryUtxo,
        headNativeScript: HeadMultisigScript,
        network: Network,
        protocolParams: ProtocolParams,
        evaluator: PlutusScriptEvaluator,
        validators: Seq[Validator]
    )

    def build(recipe: Recipe): Either[BuildError, SettlementTx] = {
        //////////////////////////////////////////////////////
        // Data extraction

        val headAddress = recipe.headNativeScript.address(recipe.network)

        val utxos =
            recipe.deposits
                .map(_.toUtxo)
                .toBuffer

        val withdrawnValue: Value =
            recipe.utxosWithdrawn.values.map(_.value).foldLeft(Value.zero)((acc, v) => acc + v)

        //////////////
        // Datum
        // TODO: Pass the hash of the protocol parameters in the datum
        val treasuryDatum =
            mkMultisigTreasuryDatum(recipe.majorVersion, ByteString.empty)

        // The new treasury value should be the sum of all inputs minus withdrawals minus fee
        // -- the inputs will be the deposits and the old treasury utxo
        // FIXME: factor out this calculation
        // TODO: this might not work as expected, since it will produce
        //  lists like that: ada,token,...,ada,...
        val inputsValue: Value = {
            utxos.foldLeft(Value.zero)((b, utxo) => b + utxo._2.value)
        }

        val treasuryValue: Value = inputsValue - withdrawnValue

        /////////////////////////////////////////////////////////////
        // Step definition

        val spendRecipeAndDeposits: Seq[Spend] = utxos.toSeq.map(utxo =>
            Spend(
              TransactionUnspentOutput(
                utxo._1,
                utxo._2
              ),
              witness = NativeScriptWitness(
                NativeScriptValue(recipe.headNativeScript.script),
                recipe.headNativeScript.requiredSigners.toSortedSet.unsorted
                    .map(ExpectedSigner(_))
              )
            )
        )

        val createTreasuryOutput =
            Send(
              Babbage(
                address = headAddress,
                value = treasuryValue,
                datumOption = Some(Inline(treasuryDatum.toData)),
                scriptRef = None
              )
            )

        val modifyAuxiliaryData =
            ModifyAuxiliaryData(_ => Some(MD(MD.L1TxTypes.Settlement, headAddress)))

        // N.B.: Withdrawals may be empty
        val createWithdrawals: Seq[Send] = recipe.utxosWithdrawn.toSeq.map(utxo => Send(utxo._2))

        val steps = spendRecipeAndDeposits
            .appended(createTreasuryOutput)
            .appended(modifyAuxiliaryData)
            .appendedAll(createWithdrawals)

        //////////////////////////////////////////////////////////////////////////////
        // Build and finalize
        for {
            unbalanced <- TransactionBuilder
                .build(recipe.network, steps)
                .left
                .map(BuildError.StepError(_))
            finalized <- unbalanced
                .finalizeContext(
                  recipe.protocolParams,
                  diffHandler = new ChangeOutputDiffHandler(
                    recipe.protocolParams,
                    0
                  ).changeOutputDiffHandler,
                  evaluator = recipe.evaluator,
                  validators = recipe.validators
                )

            /////////////////////////////////////////////////////////////////////////
            // Post-process result
        } yield (
          SettlementTx(
            treasurySpent = recipe.treasuryUtxo,
            treasuryProduced = recipe.treasuryUtxo.copy(
              txId = TransactionInput(transactionId = finalized.transaction.id, index = 0),
              value = treasuryValue,
              datum = treasuryDatum
            ),
            depositsSpent = recipe.deposits,
            rolloutProduced =
                if recipe.utxosWithdrawn.isEmpty then None
                else
                    Some(
                      RolloutUtxo(
                        (
                          TransactionInput(finalized.transaction.id, 1),
                          finalized.transaction.body.value.outputs(1).value
                        )
                      )
                    )
            ,
            tx = finalized.transaction
          )
        )

    }

}
