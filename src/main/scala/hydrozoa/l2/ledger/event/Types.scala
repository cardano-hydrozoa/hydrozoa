package hydrozoa.l2.ledger.event

sealed abstract class AnyL2Event[+G, +T, +W] extends Serializable

final case class GenesisL2Event[+G, +T, +W](genesis: G) extends AnyL2Event[G, T, W] {}

final case class TransactionL2Event[+G, +T, +W](transaction: T) extends AnyL2Event[G, T, W] {}

final case class WithdrawalL2Event[+G, +T, +W](withdrawal: W) extends AnyL2Event[G, T, W] {}
