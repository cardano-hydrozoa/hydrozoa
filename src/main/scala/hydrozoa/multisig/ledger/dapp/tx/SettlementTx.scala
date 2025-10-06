package hydrozoa.multisig.ledger.dapp.tx

import cats.implicits.*
import hydrozoa.lib.tx.ScriptWitness.ScriptValue
import hydrozoa.lib.tx.TransactionBuilderStep.{ModifyAuxData, Pay, SpendOutput}
import hydrozoa.lib.tx.{OutputWitness, TransactionBuilder, TransactionUnspentOutput, TxBuildError}
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo.mkMultisigTreasuryDatum
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, RolloutUtxo, TreasuryUtxo}
import hydrozoa.{addDummyVKeys, removeDummyVKeys}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
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

    enum BuildError:
        case SomeBuilderError(e: TxBuildError)
        case OtherScalusBalancingError(e: TxBalancingError)
        case OtherScalusTransactionException(e: TransactionException)

    def build(recipe: Recipe): Either[BuildError, SettlementTx] = {

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

        val steps =
            // Spend Treasury and Deposits
            utxos.toSeq.map(utxo =>
                SpendOutput(
                  TransactionUnspentOutput(
                    utxo._1,
                    utxo._2
                  ),
                  witness = Some(
                    OutputWitness.NativeScriptOutput(
                      ScriptValue(
                        recipe.headNativeScript.script,
                        recipe.headNativeScript.requiredSigners.toSortedSet
                      )
                    )
                  )
                )
            )
                ++ Seq(
                  // Treasury Output
                  Pay(
                    Babbage(
                      address = headAddress,
                      value = treasuryValue,
                      datumOption = Some(Inline(treasuryDatum.toData)),
                      scriptRef = None
                    )
                  ),
                  ModifyAuxData(_ => Some(MD(MD.L1TxTypes.Settlement, headAddress)))
                ) ++ {
                    if recipe.utxosWithdrawn.isEmpty then Seq.empty
                    else Seq(Pay(Babbage(address = headAddress, value = withdrawnValue)))
                }

        for {
            unbalanced <- TransactionBuilder
                .build(Mainnet, steps)
                .left
                .map(BuildError.SomeBuilderError(_))
            balanced <- LowLevelTxBuilder
                .balanceFeeAndChange(
                  initial = addDummyVKeys(unbalanced.expectedSigners.size, unbalanced.transaction),
                  changeOutputIdx = 0,
                  protocolParams = recipe.context.protocolParams,
                  resolvedUtxo = recipe.context.utxo,
                  evaluator = recipe.context.evaluator
                )
                .map(tx => removeDummyVKeys(recipe.headNativeScript.numSigners, tx))
                .left
                .map(BuildError.OtherScalusBalancingError(_))
            validated <- recipe.context
                .validate(balanced)
                .left
                .map(BuildError.OtherScalusTransactionException(_))

        } yield (
          SettlementTx(
            treasurySpent = recipe.treasuryUtxo,
            treasuryProduced = recipe.treasuryUtxo.copy(
              txId = TransactionInput(transactionId = validated.id, index = 0),
              value = treasuryValue,
              datum = treasuryDatum
            ),
            depositsSpent = recipe.deposits,
            rolloutProduced =
                if recipe.utxosWithdrawn.isEmpty then None
                else
                    Some(
                      RolloutUtxo(
                        (TransactionInput(validated.id, 1), validated.body.value.outputs(1).value)
                      )
                    )
            ,
            tx = validated
          )
        )

    }

}
