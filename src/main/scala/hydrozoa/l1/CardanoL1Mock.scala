package hydrozoa.l1

import hydrozoa.*
import hydrozoa.infra.{txHash, txInputs, txOutputs}
import scalus.ledger.api.v1.PosixTime

import scala.collection.mutable

class CardanoL1Mock() extends CardanoL1:

    val knownTxs: mutable.Map[TxId, TxL1] = mutable.Map()

    val utxosActive: mutable.Map[OutputRefL1, Output[L1]] = mutable.Map() addAll (genesisUtxo)

    override def submit(tx: TxL1): Either[SubmissionError, TxId] = {
        val txId = txHash(tx)
        knownTxs.put(txId, tx)
        utxosActive.subtractAll(txInputs(tx))
        utxosActive.addAll(txOutputs(tx))
        Right(txId)
    }

    // TODO: Add Either
    override def awaitTx(txId: TxId): Unit = ()

    override def network: Network = Network(0, 42)

    override def lastBlockTime: PosixTime = 0

val genesisUtxo: Set[(OutputRefL1, Output[L1])] = Set(
  (
    OutputRefL1(TxId("6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508"), TxIx(0)),
    Output[L1](
      AddressBechL1(
        "addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex"
      ),
      BigInt("10000000000")
    )
  )
)
