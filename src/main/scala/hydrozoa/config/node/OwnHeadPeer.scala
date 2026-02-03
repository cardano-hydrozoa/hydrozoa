package hydrozoa.config.node

import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber, HeadPeerWallet}

export OwnHeadPeer.ownHeadPeerId

final case class OwnHeadPeer(
    override val ownHeadWallet: HeadPeerWallet,
) extends OwnHeadPeer.Section {
    override transparent inline def ownHeadPeer: OwnHeadPeer = this
}

object OwnHeadPeer {
    trait Section {
        def ownHeadPeer: OwnHeadPeer

        def ownHeadWallet: HeadPeerWallet

        transparent inline def ownHeadPeerNum: HeadPeerNumber = ownHeadWallet.getPeerNum
    }

    extension (config: OwnHeadPeer.Section & HeadPeers.Section)
        def ownHeadPeerId: HeadPeerId = HeadPeerId(config.ownHeadPeerNum, config.nHeadPeers)
}
