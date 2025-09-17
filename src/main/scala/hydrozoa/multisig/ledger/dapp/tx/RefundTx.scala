package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.{emptyTxBody, toScalusLedger}
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native

import scala.language.implicitConversions

sealed trait RefundTx {
    def depositSpent: DepositUtxo
}

object RefundTx {
    final case class Immediate(
        override val depositSpent: DepositUtxo,
        override val tx: Transaction
    ) extends Tx,
          RefundTx

    final case class PostDated(
        override val depositSpent: DepositUtxo,
        override val tx: Transaction
    ) extends Tx,
          RefundTx

    object Immediate {
        def build(): Immediate = ???
    }

    object PostDated {
        case class Recipe(
            depositTx: DepositTx,
            txIx: Int,
            network: Network,
            headScript: HeadMultisigScript,
            validityStartSlot: Long
        )

        def build(recipe: Recipe): PostDated = {
            val deposit = recipe.depositTx.depositProduced
            // NB: Fee is paid from deposit itself
            val feeCoin = Coin(1_000_000)
            val depositDatum = deposit._3
            val refundOutput: TransactionOutput =
                TransactionOutput(
                  address = depositDatum.refundAddress.toScalusLedger(network = recipe.network),
                  value = Value(deposit._4 - feeCoin),
                  datumOption = depositDatum.refundDatum.asScala.map(Inline(_))
                )
            val requiredSigners = recipe.headScript.requiredSigners

            val txBody =
                emptyTxBody.copy(
                  inputs = Set(
                    TransactionInput(transactionId = recipe.depositTx.tx.id, index = recipe.txIx)
                  ),
                  outputs = IndexedSeq(refundOutput).map(Sized(_)),
                  // TODO: we set the fee to 1 ada, but this doesn't need to be
                  fee = feeCoin,
                  validityStartSlot = Some(recipe.validityStartSlot),
                  requiredSigners = requiredSigners
                )

            val txWitSet: TransactionWitnessSet =
                TransactionWitnessSet(
                  nativeScripts = Set(recipe.headScript)
                )
            val tx: Transaction = Transaction(
              body = KeepRaw(txBody),
              witnessSet = txWitSet,
              isValid = true,
              auxiliaryData = None
            )
            PostDated(depositSpent = recipe.depositTx.depositProduced, tx = tx)
        }

    }
}
