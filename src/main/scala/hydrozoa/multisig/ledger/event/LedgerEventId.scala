package hydrozoa.multisig.ledger.event

import cats.implicits.catsSyntaxOrder
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import scala.annotation.targetName

type LedgerEventId = LedgerEventId.Id

object LedgerEventId {

    /** Used in the transient fields to indicate whether an event is valid or invalid. (This type is
      * solely here to avoid boolean blindness)
      */
    enum ValidityFlag:
        case Valid
        case Invalid

    opaque type Id = (HeadPeerNumber, LedgerEventNumber)

    def apply(peerNum: HeadPeerNumber, eventNum: LedgerEventNumber): Id = (peerNum, eventNum)

    @targetName("apply_Int")
    def apply(peerNum: Int, eventNum: Int): Id =
        (HeadPeerNumber(peerNum), LedgerEventNumber(eventNum))

    def unapply(self: Id): (HeadPeerNumber, LedgerEventNumber) =
        (HeadPeerNumber(self._1), LedgerEventNumber(self._2))

    given Conversion[Id, (Int, Int)] = identity

    given Ordering[Id] with {
        override def compare(x: Id, y: Id): Int =
            x.convert.compare(y.convert)
    }

    extension (self: Id)
        def increment: Id = LedgerEventId(self._1, self._2 + 1)
        def peerNum: HeadPeerNumber = HeadPeerNumber(self._1)
        def eventNum: LedgerEventNumber = LedgerEventNumber(self._2)

        def precedes(other: Id): Boolean =
            self.peerNum == other.peerNum &&
                self.eventNum.increment == other.eventNum
}
