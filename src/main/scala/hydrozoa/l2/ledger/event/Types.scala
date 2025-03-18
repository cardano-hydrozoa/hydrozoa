package hydrozoa.l2.ledger.event

import hydrozoa.TxId

sealed trait AnyL2Event[+H, +G, +T, +W, U](eventId: H) {
    type UtxosDiff = U
    def getEventId: H = eventId
}

final case class GenesisL2Event[+H, +G, +T, +W, U](eventId: H, genesis: G)
    extends AnyL2Event[H, G, T, W, U](eventId) {}

sealed trait NonGenesisL2Event[+H, +G, +T, +W, U] extends AnyL2Event[H, G, T, W, U] {}

final case class TransactionL2Event[+H, +G, +T, +W, U](
    eventId: H,
    transaction: T
) extends NonGenesisL2Event[H, G, T, W, U]
    with AnyL2Event[H, G, T, W, U](eventId) {}

final case class WithdrawalL2Event[+H, +G, +T, +W, U](
    eventId: H,
    withdrawal: W
) extends NonGenesisL2Event[H, G, T, W, U]
    with AnyL2Event[H, G, T, W, U](eventId) {}
