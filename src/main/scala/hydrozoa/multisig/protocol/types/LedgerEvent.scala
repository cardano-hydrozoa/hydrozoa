package hydrozoa.multisig.protocol.types

import cats.effect.IO
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.dapp.tx.TxTiming
import scalus.cardano.ledger.Coin

type LedgerEventId = LedgerEventId.Id

object LedgerEventId {

    /** Used in the transient fields to indicate whether an event is valid or invalid. (This type is
      * solely here to avoid boolean blindness)
      */
    enum ValidityFlag:
        case Valid
        case Invalid

    opaque type Id = (Int, Int)

    def apply(peerId: Int, eventNum: Int): Id = (peerId, eventNum)

    def unapply(self: Id): (Peer.Number, Number) = (Peer.Number(self._1), Number(self._2))

    given Conversion[Id, (Int, Int)] = identity

    given Ordering[Id] with {
        override def compare(x: Id, y: Id): Int =
            x.compare(y)
    }

    extension (self: Id)
        def increment: Id = LedgerEventId(self._1, self._2 + 1)
        def peerNum: Peer.Number = Peer.Number(self._1)
        def eventNum: Number = Number(self._2)

        def precedes(other: Id): Boolean =
            self.peerNum == other.peerNum &&
                self.eventNum.increment == other.eventNum

    type Number = Number.Number

    object Number {
        opaque type Number = Int

        def apply(i: Int): Number = i

        given Conversion[Number, Int] = identity

        given Ordering[Number] with {
            override def compare(x: Number, y: Number): Int =
                x.compare(y)
        }

        extension (self: Number) def increment: Number = Number(self + 1)
    }
}

sealed trait LedgerEvent {
    def eventId: LedgerEventId
}

object LedgerEvent {

    final case class TxL2Event(
        override val eventId: LedgerEventId,
        tx: Array[Byte]
    ) extends LedgerEvent

    final case class RegisterDeposit(
        depositTxBytes: Array[Byte],
        refundTxBytes: Array[Byte],
        eventId: LedgerEventId,
        virtualOutputsBytes: Array[Byte],
        donationToTreasury: Coin,
        txTiming: TxTiming,
        blockStartTime: QuantizedInstant
    ) extends LedgerEvent

    // TODO: do we still need it?
    type Subscriber = ActorRef[IO, LedgerEvent]

}
