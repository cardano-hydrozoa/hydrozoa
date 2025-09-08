package hydrozoa.multisig.ledger.l2.trivial

object LedgerL2 {
    sealed trait Event

    final case class Transaction() extends Event
    final case class Withdrawal() extends Event
    final case class Genesis() extends Event
}
