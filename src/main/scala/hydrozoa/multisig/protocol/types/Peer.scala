package hydrozoa.multisig.protocol.types

import scala.annotation.targetName
import scala.math.Ordered.orderingToOrdered

object Peer {
    type Number = Number.Number

    object Number:
        opaque type Number = Int

        def apply(i: Int): Number = {
            require(i >= 0, "Peer number must be non-negative.")
            i
        }

        given Ordering[Number] with {
            override def compare(x: Number, y: Number): Int =
                x.compare(y)
        }

        given Conversion[Number, Int] = identity

        extension (self: Number) def increment: Number = Number(self + 1)

    final case class Id(peerNum: Peer.Number, nPeers: Int) {
        require(peerNum.convert < nPeers, "Peer ID must be less than the number of peers.")

        /** Is the peer the consensus leader of the given block number? */
        def isLeader(blockNum: Block.Number): Boolean =
            blockNum.convert % nPeers == peerNum.convert

        /** After the given block number, for which block number will the peer next be leader? */
        def nextLeaderBlock(blockNum: Block.Number): Block.Number = {
            val roundNumber = blockNum.convert / nPeers
            val leaderBlockThisRound = roundNumber * nPeers + peerNum.convert

            val result =
                if blockNum.convert < leaderBlockThisRound then leaderBlockThisRound
                else leaderBlockThisRound + nPeers
            Block.Number(result)
        }

        given Ordering[Peer.Id] with {
            override def compare(x: Peer.Id, y: Peer.Id): Int = {
                require(
                  x.nPeers == y.nPeers,
                  "Peer IDs must agree on the total number of peers to be comparable."
                )
                x.peerNum.compare(y.peerNum)
            }
        }
    }

    object Id {
        @targetName("applyInt")
        def apply(peerNum: Int, nPeers: Int): Peer.Id = Peer.Id(Peer.Number(peerNum), nPeers)
    }
}
