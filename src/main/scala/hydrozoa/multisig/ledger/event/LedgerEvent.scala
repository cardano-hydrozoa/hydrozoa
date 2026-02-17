package hydrozoa.multisig.ledger.event

import cats.syntax.all.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import scalus.cardano.ledger.Coin

sealed trait LedgerEvent {
    def eventId: LedgerEventId

    final transparent inline def eventNum: LedgerEventNumber = eventId.eventNum
    final transparent inline def peerNum: HeadPeerNumber = eventId.peerNum
}

object LedgerEvent {

    final case class TxL2Event(
        override val eventId: LedgerEventId,
        tx: Array[Byte]
    ) extends LedgerEvent

    // TODO: factor out a true request type - depositTxBytes + refundTxBytes + virtualOutputsBytes + depositFee
    final case class RegisterDeposit(
        override val eventId: LedgerEventId,
        depositTxBytes: Array[Byte],
        refundTxBytes: Array[Byte],
        virtualOutputsBytes: Array[Byte],
        // TODO: explain the name, previously was known as dontationToTreasury
        depositFee: Coin,
    ) extends LedgerEvent

}
