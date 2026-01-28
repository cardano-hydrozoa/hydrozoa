package hydrozoa.multisig.ledger.event

import cats.implicits.catsSyntaxOrder
import hydrozoa.multisig.consensus.peer.PeerNumber
import scala.annotation.targetName

type LedgerEventId = LedgerEventId.Id

object LedgerEventId {

    /** Used in the transient fields to indicate whether an event is valid or invalid. (This type is
      * solely here to avoid boolean blindness)
      */
    enum ValidityFlag:
        case Valid
        case Invalid

    opaque type Id = (PeerNumber, LedgerEventNumber)

    def apply(peerNum: PeerNumber, eventNum: LedgerEventNumber): Id = (peerNum, eventNum)

    @targetName("apply_Int")
    def apply(peerNum: Int, eventNum: Int): Id = (PeerNumber(peerNum), LedgerEventNumber(eventNum))

    def unapply(self: Id): (PeerNumber, LedgerEventNumber) =
        (PeerNumber(self._1), LedgerEventNumber(self._2))

    given Conversion[Id, (Int, Int)] = identity

    given Ordering[Id] with {
        override def compare(x: Id, y: Id): Int =
            x.convert.compare(y.convert)
    }

    extension (self: Id)
        def increment: Id = LedgerEventId(self._1, self._2 + 1)
        def peerNum: PeerNumber = PeerNumber(self._1)
        def eventNum: LedgerEventNumber = LedgerEventNumber(self._2)

        def precedes(other: Id): Boolean =
            self.peerNum == other.peerNum &&
                self.eventNum.increment == other.eventNum
}
