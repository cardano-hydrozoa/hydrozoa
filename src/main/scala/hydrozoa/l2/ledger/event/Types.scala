package hydrozoa.l2.ledger.event

sealed abstract class Event[+D, +T, +W] extends Serializable

final case class DepositEvent[+D, +T, +W](deposit: D) extends Event[D, T, W] {}

final case class TransactionEvent[+D, +T, +W](deposit: T) extends Event[D, T, W] {}

final case class WithdrawalEvent[+D, +T, +W](withdrawal: W) extends Event[D, T, W] {}
