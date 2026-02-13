package hydrozoa.config.node.owninfo

import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber, HeadPeerWallet}
import scalus.crypto.ed25519.VerificationKey

final case class OwnHeadPeerPrivate private (
    override val ownHeadWallet: HeadPeerWallet,
    override val ownHeadPeerPublic: OwnHeadPeerPublic,
) extends OwnHeadPeerPrivate.Section {
    override def ownHeadPeerPrivate: OwnHeadPeerPrivate = this
}

object OwnHeadPeerPrivate {
    def apply(ownHeadWallet: HeadPeerWallet, headPeers: HeadPeers): Option[OwnHeadPeerPrivate] =
        val ownPeerNum: HeadPeerNumber = ownHeadWallet.getPeerNum
        val walletKey: VerificationKey = ownHeadWallet.exportVerificationKey
        for {
            ownHeadPeerPublic <- OwnHeadPeerPublic(ownPeerNum, headPeers)
            _ <- Option.when(walletKey == ownHeadPeerPublic.ownHeadVKey)(())
        } yield new OwnHeadPeerPrivate(ownHeadWallet, ownHeadPeerPublic)

    trait Section extends OwnHeadPeerPublic.Section {
        def ownHeadPeerPrivate: OwnHeadPeerPrivate

        def ownHeadWallet: HeadPeerWallet

        override def ownHeadPeerId: HeadPeerId = ownHeadPeerPublic.ownHeadPeerId
        override def ownHeadPeerNum: HeadPeerNumber = ownHeadPeerPublic.ownHeadPeerNum
        override def ownHeadVKey: VerificationKey = ownHeadPeerPublic.ownHeadVKey
    }
}
