package hydrozoa.multisig.consensus.ack

import cats.implicits.catsSyntaxOrder
import hydrozoa.multisig.consensus.peer.{PeerNumber, PeerWallet}

type AckId = AckId.AckId

object AckId {
    opaque type AckId = (Int, Int)

    def apply(peerNum: Int, ackNum: Int): AckId = (peerNum, ackNum)

    def apply(wallet: PeerWallet, ackNum: Int): AckId = (wallet.getPeerNum, ackNum)

    def unapply(self: AckId): (PeerNumber, AckNumber) = (PeerNumber(self._1), AckNumber(self._2))

    given Conversion[AckId, (Int, Int)] = identity

    given Ordering[AckId] with {
        override def compare(x: AckId, y: AckId): Int =
            x.compare(y)
    }

    extension (self: AckId)
        def increment: AckId = AckId(self._1, self._2 + 1)
        def peerNum: PeerNumber = PeerNumber(self._1)
        def ackNum: AckNumber = AckNumber(self._2)
}
