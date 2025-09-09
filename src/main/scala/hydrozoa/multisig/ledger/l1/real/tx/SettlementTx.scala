package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx
import hydrozoa.multisig.ledger.l1.real.txCborToScalus

object SettlementTx {
    sealed trait ParseError

    final case class SomeParseError() extends ParseError

    def parse(txSerialized: Tx.Serialized.Settlement): Either[ParseError, Tx.Settlement] = {
        val deserialized = txCborToScalus(txSerialized.txCbor)
        Right(
          Tx.Settlement(
            treasurySpent = ???,
            treasuryProduced = ???,
            depositsSpent = ???,
            rolloutProduced = ???,
            headAddress = txSerialized.headAddress,
            headCs = txSerialized.headCs,
            txCbor = txSerialized.headCs,
            txDeserialized = deserialized
          )
        )
    }
}
