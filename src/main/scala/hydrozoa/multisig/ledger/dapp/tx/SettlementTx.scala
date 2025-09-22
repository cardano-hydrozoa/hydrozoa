package hydrozoa.multisig.ledger.dapp.tx

import cats.implicits.*
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo.mkMultisigTreasuryDatum
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, RolloutUtxo, TreasuryUtxo}
import hydrozoa.{addDummyVKeys, removeDummyVKeys, setAuxData}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.txbuilder.*

import scala.collection
import scala.language.{implicitConversions, reflectiveCalls}

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
        context: BuilderContext
    )

    def build(recipe: Recipe): Either[TxBalancingError, SettlementTx] = {

        val headAddress = recipe.headNativeScript.address(recipe.context.network)

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

        lazy val builder = {
            val b1 = recipe.context.buildNewTx
                .attachNativeScript(script = recipe.headNativeScript.script, index = 0)
                // Treasury output
                .payToScript(
                  address = headAddress,
                  value = treasuryValue,
                  datum = treasuryDatum.toData
                )
                // Deposits and treasury
                .selectInputs(SelectInputs.particular(utxos.map(_._1).toSet))
                .setAuxData(MD(MD.L1TxTypes.Settlement, headAddress))
                .addDummyVKeys(recipe.headNativeScript.numSigners)
            val b2 =
                if recipe.utxosWithdrawn.isEmpty then b1
                else
                    // FIXEME: make this into proper payouts/rollouts utxo
                    b1.payTo(address = headAddress, value = withdrawnValue)

            LowLevelTxBuilder
                .balanceFeeAndChange(
                  initial = b2.tx,
                  changeOutputIdx = 0,
                  protocolParams = recipe.context.protocolParams,
                  resolvedUtxo = recipe.context.utxo,
                  evaluator = recipe.context.evaluator
                )
                .map(tx => removeDummyVKeys(recipe.headNativeScript.numSigners, tx))
        }

        builder.map(tx =>
            SettlementTx(
              treasurySpent = recipe.treasuryUtxo,
              treasuryProduced = recipe.treasuryUtxo.copy(
                txId = TransactionInput(transactionId = tx.id, index = 0),
                value = treasuryValue,
                datum = treasuryDatum
              ),
              depositsSpent = recipe.deposits,
              rolloutProduced =
                  if recipe.utxosWithdrawn.isEmpty then None
                  else
                      Some(
                        RolloutUtxo(
                          (TransactionInput(tx.id, 1), tx.body.value.outputs(1).value)
                        )
                      )
              ,
              tx = tx
            )
        )

    }

}
