package hydrozoa.config.node.owninfo

import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber

/** The own-peer identity + role surface shared by head and coil nodes. Fast- and slow-side actors
  * read their own author id, leadership, and role exclusively through this seam, so the same actor
  * code runs on both peer types. Concrete implementations: [[OwnHeadPeerPublic]] /
  * [[OwnCoilPeerPublic]].
  */
trait OwnPeerPublic {

    /** This peer's author id ([[PeerId.Head]] or [[PeerId.Coil]]). */
    def ownPeerId: PeerId

    /** Is this peer the fast-cycle leader of the given block? Coil peers never lead. */
    def canLeadFast(blockNum: BlockNumber): Boolean

    /** Is this peer the slow-cycle leader of the given stack? Coil peers never lead. */
    def canLeadSlow(stackNum: StackNumber): Boolean

    /** After the given block, the next block this peer leads (and therefore emits a brief for), or
      * `None` if this peer never leads.
      */
    def nextOwnLeaderBlock(after: BlockNumber): Option[BlockNumber]

    /** After the given stack, the next stack this peer leads, or `None` if this peer never leads.
      */
    def nextOwnSlowLeaderStack(after: StackNumber): Option[StackNumber]

    /** Short label for logger / tracer route names (e.g. `0` for head 0, `c0` for coil 0). */
    def ownPeerLabel: String

    /** Numeric peer index for telemetry. Head and coil indices may collide; telemetry only. */
    def ownPeerIndex: Int
}

object OwnPeerPublic {
    trait Section {
        def ownPeerPublic: OwnPeerPublic

        def ownPeerId: PeerId = ownPeerPublic.ownPeerId
        def canLeadFast(blockNum: BlockNumber): Boolean = ownPeerPublic.canLeadFast(blockNum)
        def canLeadSlow(stackNum: StackNumber): Boolean = ownPeerPublic.canLeadSlow(stackNum)
        def nextOwnLeaderBlock(after: BlockNumber): Option[BlockNumber] =
            ownPeerPublic.nextOwnLeaderBlock(after)
        def nextOwnSlowLeaderStack(after: StackNumber): Option[StackNumber] =
            ownPeerPublic.nextOwnSlowLeaderStack(after)
        def ownPeerLabel: String = ownPeerPublic.ownPeerLabel
        def ownPeerIndex: Int = ownPeerPublic.ownPeerIndex
    }
}
