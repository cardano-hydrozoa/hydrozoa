package hydrozoa.multisig.ledger.l1.trivial

object LedgerL1 {
    sealed trait Event
    
    final case class DepositTx() extends Event
    final case class RefundTx() extends Event
}
