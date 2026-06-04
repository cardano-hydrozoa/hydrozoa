package hydrozoa.multisig.consensus.peer

import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber

/** The counterpart a [[hydrozoa.multisig.consensus.PeerLiaison]] is linked to. A head remote
  * carries its full [[HeadPeerId]] (with the head-set size needed for the round-robin leader
  * schedule); a coil remote carries only its [[CoilPeerNumber]], since coil peers never lead and
  * therefore emit no block or stack briefs.
  */
enum RemotePeer {
    case Head(headPeerId: HeadPeerId)
    case Coil(coilPeerNum: CoilPeerNumber)
}

object RemotePeer {
    extension (self: RemotePeer) {

        /** This remote's author id. */
        def peerId: PeerId = self match {
            case Head(h) => PeerId.Head(h.peerNum)
            case Coil(n) => PeerId.Coil(n)
        }

        /** Short label for logger / tracer route names. */
        def label: String = self match {
            case Head(h) => h.peerNum.convert.toString
            case Coil(n) => s"c${n.convert}"
        }

        /** After the given block, the next block this remote leads (and so emits a brief for), or
          * `None` if this remote never leads (coil).
          */
        def nextLeaderBlock(after: BlockNumber): Option[BlockNumber] = self match {
            case Head(h) => Some(h.nextLeaderBlock(after))
            case Coil(_) => None
        }

        /** After the given stack, the next stack this remote leads, or `None` if it never leads. */
        def nextSlowLeaderStack(after: StackNumber): Option[StackNumber] = self match {
            case Head(h) => Some(h.nextSlowLeaderStack(after))
            case Coil(_) => None
        }
    }
}
