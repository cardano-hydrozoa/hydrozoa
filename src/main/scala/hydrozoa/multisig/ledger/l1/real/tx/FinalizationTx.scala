package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx
import hydrozoa.multisig.ledger.l1.real.txCborToScalus

object FinalizationTx {
    sealed trait ParseError

    final case class SomeParseError() extends ParseError

    def parse(txSerialized: Tx.Serialized.Finalization): Either[ParseError, Tx.Finalization] = {
        val deserialized = txCborToScalus(txSerialized.txCbor)
        Right(
          Tx.Finalization(
            treasurySpent = ???,
            headAddress = txSerialized.headAddress,
            headCs = txSerialized.headCs,
            txCbor = txSerialized.headCs,
            txDeserialized = deserialized
          )
        )
    }
}
