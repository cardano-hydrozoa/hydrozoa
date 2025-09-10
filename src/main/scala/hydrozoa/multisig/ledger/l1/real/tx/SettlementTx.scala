package hydrozoa.multisig.ledger.l1.real.tx

import cats.implicits.*
import hydrozoa.emptyTxBody
import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx
import hydrozoa.multisig.ledger.l1.real.script.multisig.HeadMultisigScript.HeadMultisigScript
import hydrozoa.multisig.ledger.l1.real.utxo.TreasuryUtxo.mkMultisigTreasuryDatum
import hydrozoa.multisig.ledger.l1.real.utxo.{DepositUtxo, RolloutUtxo, TreasuryUtxo}
import io.bullet.borer.Cbor
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.TransactionOutput.Babbage

import scala.collection
import scala.language.{implicitConversions, reflectiveCalls}
import scala.util.{Failure, Success, Try}

final case class SettlementTx(
    treasurySpent: TreasuryUtxo,
    treasuryProduced: TreasuryUtxo,
    depositsSpent: List[DepositUtxo],
    rolloutProduced: Option[RolloutUtxo],
    override val tx: Transaction
) extends Tx

object SettlementTx {

    sealed trait ParseError extends Throwable
    case class TxCborDeserializationFailed(e: Throwable) extends ParseError

    def parse(txSerialized: Tx.Serialized): Either[ParseError, SettlementTx] = {
        given OriginalCborByteArray = OriginalCborByteArray(txSerialized)
        Cbor.decode(txSerialized).to[Transaction].valueTry match {
            case Failure(e) => Left(TxCborDeserializationFailed(e))
            case Success(tx) =>
                ???
                for {
                    _ <- Left(???)
                } yield SettlementTx(
                  treasurySpent = ???,
                  treasuryProduced = ???,
                  depositsSpent = ???,
                  rolloutProduced = ???,
                  tx = tx
                )
        }

    }

    case class Recipe(
        majorVersion: Int,
        deposits: List[DepositUtxo],
        utxosWithdrawn: Map[TransactionInput, TransactionOutput],
        treasuryUtxo: TreasuryUtxo,
        headNativeScript: HeadMultisigScript,
        network: Network
    )

    sealed trait BuildError extends Throwable
    case object FailedToBuildTreasuryValue extends BuildError

    def build(recipe: Recipe): Either[BuildError, SettlementTx] = {
        val feeCoin = Coin(10000000)
        val utxos =
            recipe.deposits.map(_.utxo).toBuffer.append(recipe.treasuryUtxo._2)

        val withdrawnValue: Value =
            recipe.utxosWithdrawn.values.map(_.value).foldLeft(Value.zero)((acc, v) => acc + v)
        // FIXME: Make this into a proper rollout utxo
        val rolloutOutput: TransactionOutput = Babbage(
          address = recipe.headNativeScript.address(recipe.network),
          value = withdrawnValue,
          datumOption = None,
          scriptRef = None
        )

        //////////////
        // Datum
        // TODO: Pass the hash of the protocol parameters in the datum
        val treasuryDatum = toData(
          mkMultisigTreasuryDatum(recipe.majorVersion, ByteString.empty).toData
        )

        // The new treasury value should be the sum of all inputs minus withdrawals minus fee
        // -- the inputs will be the deposits and the old treasury utxo
        // FIXME: factor out this calculation
        // TODO: this might not work as expected, since it will produce
        //  lists like that: ada,token,...,ada,...
        val inputsValue: Value = {
            utxos.foldLeft(Value.zero)((b, utxo) => b + utxo._2.value)
        }

        for
            treasuryValue: Value <- Try(
              inputsValue - withdrawnValue - Value(coin = feeCoin)
            ) match {
                case Success(s) => Right(s)
                case Failure(e: IllegalArgumentException) =>
                    Left(
                      FailedToBuildTreasuryValue
                      // s"Malformed value equal to `inputsValue - withdrawnValue - Value(coin = feeCoin)` encountered: inputsValue = ${inputsValue}, withdrawnValue = ${withdrawnValue}, feeCoin = ${feeCoin}"
                    )
                case Failure(otherE) =>
                    Left(FailedToBuildTreasuryValue)
                // s"mkSettlementTxDraft: unable to make treasury value. ${otherE}")
            }

            ////////////////////////////
            // Then construct the output

            treasuryOutput: Sized[TransactionOutput] = Sized(
              TransactionOutput(
                address = recipe.headNativeScript.address(recipe.network),
                value = treasuryValue,
                datumOption = Some(Inline(treasuryDatum))
              )
            )

            ///////////////////////////
            // Finally construct the body
            txBody =
                emptyTxBody.copy(
                  inputs = utxos.toSet.map(_._1),
                  outputs =
                      if recipe.utxosWithdrawn.isEmpty then IndexedSeq(treasuryOutput)
                      else IndexedSeq(treasuryOutput, Sized(rolloutOutput)),
                  // TODO: we set the fee to 1 ada, but this doesn't need to be
                  fee = feeCoin
                )

            txWitSet: TransactionWitnessSet =
                TransactionWitnessSet(
                  nativeScripts = Set(recipe.headNativeScript)
                )

            tx: Transaction = Transaction(
              body = KeepRaw(txBody),
              witnessSet = txWitSet,
              isValid = true,
              auxiliaryData = None
            )
        yield SettlementTx(
          treasurySpent = recipe.treasuryUtxo,
          treasuryProduced = recipe.treasuryUtxo.copy(utxo =
              (
                TransactionInput(transactionId = tx.id, index = 0),
                treasuryOutput.value
              )
          ),
          depositsSpent = recipe.deposits,
          rolloutProduced =
              if recipe.utxosWithdrawn.isEmpty then None
              else Some(RolloutUtxo((TransactionInput(tx.id, 1), rolloutOutput))),
          tx = tx
        )
    }

}
