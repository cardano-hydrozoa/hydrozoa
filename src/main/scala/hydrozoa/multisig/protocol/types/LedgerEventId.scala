package hydrozoa.multisig.protocol.types

import cats.syntax.all.*

type LedgerEventId = LedgerEventId.Id

object LedgerEventId {

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
