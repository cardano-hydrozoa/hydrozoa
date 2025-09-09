package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx

object DepositTx {
    sealed trait ParseError

    final case class SomeParseError()

    def parse(txSerialized: Tx.Serialized.Deposit): Either[ParseError, Tx.Deposit] = {
        Right(
          Tx.Deposit(
            depositProduced = ???,
            headAddress = txSerialized.headAddress,
            headCs = txSerialized.headCs,
            txCbor = txSerialized.headCs
          )
        )
    }

    def foo(x: Tx.Serialized.Deposit): Any =
        x.parse
}
