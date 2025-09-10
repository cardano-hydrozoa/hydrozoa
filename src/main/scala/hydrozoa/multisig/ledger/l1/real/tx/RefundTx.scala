package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx
import hydrozoa.multisig.ledger.l1.real.utxo.DepositUtxo
import scalus.cardano.ledger.Transaction

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
        def build(): PostDated = ???
    }
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
