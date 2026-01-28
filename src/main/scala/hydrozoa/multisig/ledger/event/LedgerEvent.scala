package hydrozoa.multisig.ledger.event

import cats.syntax.all.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.consensus.peer.PeerNumber
import hydrozoa.multisig.ledger.dapp.tx.TxTiming
import scalus.cardano.ledger.Coin

sealed trait LedgerEvent {
    def eventId: LedgerEventId

    final transparent inline def eventNum: LedgerEventNumber = eventId.eventNum
    final transparent inline def peerNum: PeerNumber = eventId.peerNum
}

object LedgerEvent {

    final case class TxL2Event(
        override val eventId: LedgerEventId,
        tx: Array[Byte]
    ) extends LedgerEvent

    final case class RegisterDeposit(
        depositTxBytes: Array[Byte],
        refundTxBytes: Array[Byte],
        override val eventId: LedgerEventId,
        virtualOutputsBytes: Array[Byte],
        donationToTreasury: Coin,
        txTiming: TxTiming,
        blockStartTime: QuantizedInstant
    ) extends LedgerEvent
}
