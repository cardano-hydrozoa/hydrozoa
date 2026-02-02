package hydrozoa.config.head.peers

import hydrozoa.VerificationKeyBytes
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{PeerId, PeerNumber}

final case class HeadPeers private (
    _peers: IArray[VerificationKeyBytes]
) extends HeadPeers.Section {
    def apply(p: PeerId): VerificationKeyBytes = {
        require(p.nPeers == nPeers)
        _peers(p.peerNum)
    }

    override def getPeerKey(p: PeerId): VerificationKeyBytes = apply(p)

    override def nPeers: PositiveInt = PositiveInt.unsafeApply(_peers.size)
}

object HeadPeers {
    def apply(peers: IArray[VerificationKeyBytes]): HeadPeers = {
        require(peers.size > 0)
        HeadPeers(peers)
    }

    trait Section {
        def getPeerKey(p: PeerId): VerificationKeyBytes

        def nPeers: PositiveInt
    }
}
