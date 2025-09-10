package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx
import hydrozoa.multisig.ledger.l1.real.txCborToScalus

object RefundTx {
    sealed trait ParseError

    final case class SomeParseError() extends ParseError

    def parse(txSerialized: Tx.Serialized.Refund): Either[ParseError, Tx.Refund] = {
        val deserialized = txCborToScalus(txSerialized.txCbor)
        Right(
          Tx.Refund(
            depositSpent = ???,
            headAddress = txSerialized.headAddress,
            headPolicy = txSerialized.headPolicy,
            txCbor = txSerialized.headPolicy,
            tx = deserialized
          )
        )
    }
}
