package hydrozoa.l2.ledger.event

sealed trait AnyL2Event[+G, +T, +W, U] { type UtxosDiff = U }

final case class GenesisL2Event[+G, +T, +W, U](genesis: G) extends AnyL2Event[G, T, W, U] {}

final case class TransactionL2Event[+G, +T, +W, U](transaction: T) extends AnyL2Event[G, T, W, U] {}

final case class WithdrawalL2Event[+G, +T, +W, U](withdrawal: W) extends AnyL2Event[G, T, W, U] {}
