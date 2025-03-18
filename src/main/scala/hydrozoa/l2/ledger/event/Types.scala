package hydrozoa.l2.ledger.event

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

/** We don't add genesis events to blocks, since they can't be invalid and because they can be
  * calculated from `depositsAbsorbed`.
  */
enum NonGenesisL2EventLabel:
    case TransactionL2EventLabel
    case WithdrawalL2EventLabel
