package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx
import hydrozoa.multisig.ledger.l1.real.txCborToScalus

object InitializationTx {
    sealed trait ParseError

    final case class SomeParseError() extends ParseError

    def parse(txSerialized: Tx.Serialized.Initialization): Either[ParseError, Tx.Initialization] = {
        val deserialized = txCborToScalus(txSerialized.txCbor)
        Right(
          Tx.Initialization(
            treasuryProduced = ???,
            headAddress = txSerialized.headAddress,
            headCs = txSerialized.headCs,
            txCbor = txSerialized.headCs,
            txDeserialized = deserialized
          )
        )
    }
}
