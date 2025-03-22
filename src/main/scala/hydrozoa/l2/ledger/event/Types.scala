package hydrozoa.l2.ledger.event

sealed trait AnyEventL2[+H, +G, +T, +W, U](eventId: H) {
    type UtxosDiff = U
    def getEventId: H = eventId
}

final case class GenesisEventL2[+H, +G, +T, +W, U](eventId: H, genesis: G)
    extends AnyEventL2[H, G, T, W, U](eventId) {}

sealed trait NonGenesisEventL2[+H, +G, +T, +W, U] extends AnyEventL2[H, G, T, W, U] {}

final case class TransactionEventL2[+H, +G, +T, +W, U](
    eventId: H,
    transaction: T
) extends NonGenesisEventL2[H, G, T, W, U]
    with AnyEventL2[H, G, T, W, U](eventId) {}

final case class WithdrawalEventL2[+H, +G, +T, +W, U](
    eventId: H,
    withdrawal: W
) extends NonGenesisEventL2[H, G, T, W, U]
    with AnyEventL2[H, G, T, W, U](eventId) {}

/** We don't add genesis events to blocks, since they can't be invalid and because they can be
  * calculated from `depositsAbsorbed`.
  */
enum NonGenesisL2EventLabel:
    case TransactionL2EventLabel
    case WithdrawalL2EventLabel
