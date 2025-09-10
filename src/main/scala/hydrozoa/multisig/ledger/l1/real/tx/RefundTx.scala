package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx
import hydrozoa.multisig.ledger.l1.real.utxo.DepositUtxo
import scalus.cardano.ledger.Transaction

final case class RefundTx(
    depositSpent: DepositUtxo,
    override val tx: Transaction
) extends Tx

object RefundTx {
//    sealed trait ParseError
//
//    def parse(txSerialized: Tx.Serialized.Refund): Either[ParseError, Tx.Refund] = {
//        val deserialized = txCborToScalus(txSerialized.txCbor)
//        Right(
//          Tx.Refund(
//            depositSpent = ???,
//            headAddress = txSerialized.headAddress,
//            headPolicy = txSerialized.headPolicy,
//            txCbor = txSerialized.headPolicy,
//            tx = deserialized
//          )
//        )
//    }
}
