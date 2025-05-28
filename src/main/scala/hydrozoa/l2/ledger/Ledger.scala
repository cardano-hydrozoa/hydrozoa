package hydrozoa.l2.ledger

import com.bloxbean.cardano.client.spec.Era
import com.bloxbean.cardano.client.transaction.spec.*
import hydrozoa.infra.{Piper, plutusAddressAsL2, txHash}
import hydrozoa.l1.multisig.state.{DepositUtxos, depositDatum}
import hydrozoa.l2.ledger.simple.SimpleL2Ledger
import hydrozoa.*

import scala.jdk.CollectionConverters.*

// TODO: this module uses the Bloxbean dep directly

/** --------------------------------------------------------------------------------------------- L2
  * Ledger implementation
  * ---------------------------------------------------------------------------------------------
  */

// This defined which ledger's implementation is going to be used
val HydrozoaL2Ledger = SimpleL2Ledger

/** --------------------------------------------------------------------------------------------- L2
  * Genesis
  * ---------------------------------------------------------------------------------------------
  */

// TODO: can be simplified, since inputs and outputs are isomorphic
case class L2Genesis(
    depositUtxos: DepositUtxos,
    outputs: List[OutputL2]
) derives CanEqual:
    def volume(): Long = outputs.map(_.coins).sum.toLong

object L2Genesis:
    def apply(ds: DepositUtxos): L2Genesis =
        L2Genesis(
          ds,
          ds.unTag.utxoMap.values
              .map(o =>
                  val datum = depositDatum(o) match
                      case Some(datum) => datum
                      case None =>
                          throw RuntimeException("deposit UTxO doesn't contain a proper datum")
                  Output.apply(datum.address |> plutusAddressAsL2, o.coins)
              )
              .toList
        )

private def mkCardanoTxForL2Genesis(genesis: L2Genesis): TxL2 =

    val depositInputs = genesis.depositUtxos.unTag.utxoMap.keySet.map { utxoId =>
        TransactionInput.builder
            .transactionId(utxoId.txId.hash)
            .index(utxoId.outputIx.ix)
            .build
    }

    val virtualOutputs = genesis.outputs.map { output =>
        TransactionOutput.builder
            .address(output.address.bech32)
            .value(Value.builder.coin(output.coins.bigInteger).build)
            .build
    }

    val body = TransactionBody.builder
        .inputs(depositInputs.toList.asJava)
        .outputs(virtualOutputs.asJava)
        .build

    val tx = Transaction.builder.era(Era.Conway).body(body).build
    TxL2(tx.serialize)

def calculateGenesisHash(genesis: L2Genesis): TxId =
    val cardanoTx = mkCardanoTxForL2Genesis(genesis)
    txHash(cardanoTx)

def mkGenesisOutputs(genesis: L2Genesis, genesisHash: TxId): UtxoSetL2 =

    val utxoDiff = genesis.outputs.zipWithIndex
        .map(output =>
            val txIn = UtxoIdL2(genesisHash, TxIx(output._2.toChar))
            val txOut = Output[L2](output._1.address.asL2, output._1.coins)
            (txIn, txOut)
        )

    UtxoSet.apply(utxoDiff.toMap)

/** --------------------------------------------------------------------------------------------- L2
  * Transaction
  * ---------------------------------------------------------------------------------------------
  */

case class L2Transaction(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[UtxoIdL2],
    outputs: List[OutputL2]
):
    def volume(): Long = outputs.map(_.coins).sum.toLong

/** @param l2Tx
  * @return
  */
private def mkCardanoTxForL2Transaction(l2Tx: L2Transaction): TxL2 =

    val virtualInputs = l2Tx.inputs.map { input =>
        TransactionInput.builder
            .transactionId(input._1.hash)
            .index(input._2.ix.intValue)
            .build
    }

    val virtualOutputs = l2Tx.outputs.map { output =>
        TransactionOutput.builder
            .address(output.address.bech32)
            .value(Value.builder.coin(output.coins.bigInteger).build)
            .build
    }

    val body = TransactionBody.builder
        .inputs(virtualInputs.asJava)
        .outputs(virtualOutputs.asJava)
        .build

    val tx = Transaction.builder.era(Era.Conway).body(body).build
    TxL2(tx.serialize)

def calculateTxHash(tx: L2Transaction): TxId =
    val cardanoTx = mkCardanoTxForL2Transaction(tx)
    val txId = txHash(cardanoTx)
    txId

/** --------------------------------------------------------------------------------------------- L2
  * Withdrawal
  * ---------------------------------------------------------------------------------------------
  */

case class L2Withdrawal(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[UtxoIdL2]
)

/** @param withdrawal
  * @return
  */
private def mkCardanoTxForL2Withdrawal(withdrawal: L2Withdrawal): TxL2 =

    val virtualInputs = withdrawal.inputs.map { input =>
        TransactionInput.builder
            .transactionId(input._1.hash)
            .index(input._2.ix.intValue)
            .build
    }

    val body = TransactionBody.builder
        .inputs(virtualInputs.asJava)
        .build

    val tx = Transaction.builder.era(Era.Conway).body(body).build
    Tx[L2](tx.serialize)

def calculateWithdrawalHash(withdrawal: L2Withdrawal): TxId =
    val cardanoTx = mkCardanoTxForL2Withdrawal(withdrawal)
    val txId = txHash(cardanoTx)
    txId
