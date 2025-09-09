package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx

object RolloutTx {
    sealed trait ParseError

    final case class SomeParseError() extends ParseError

    def parse(txSerialized: Tx.Serialized.Rollout): Either[ParseError, Tx.Rollout] = {
        Right(
          Tx.Rollout(
            rolloutSpent = ???,
            rolloutProduced = ???,
            headAddress = txSerialized.headAddress,
            headCs = txSerialized.headCs,
            txCbor = txSerialized.headCs
          )
        )
    }
}
