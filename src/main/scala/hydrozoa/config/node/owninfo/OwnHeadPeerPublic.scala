package hydrozoa.config.node.owninfo

import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import scalus.crypto.ed25519.VerificationKey

final case class OwnHeadPeerPublic private[owninfo] (
    override val ownHeadPeerId: HeadPeerId,
    override val ownHeadVKey: VerificationKey,
) extends OwnHeadPeerPublic.Section {
    override transparent inline def ownHeadPeerPublic: OwnHeadPeerPublic = this

    override transparent inline def ownHeadPeerNum: HeadPeerNumber = ownHeadPeerId.peerNum
}

object OwnHeadPeerPublic {
    def apply(ownHeadPeerNum: HeadPeerNumber, headPeers: HeadPeers): Option[OwnHeadPeerPublic] =
        for {
            _ <- Option.when(ownHeadPeerNum < headPeers.nHeadPeers)(())
            ownPeerId = HeadPeerId(ownHeadPeerNum, headPeers.nHeadPeers)
            ownHeadVKey <- headPeers.headPeerVKey(ownHeadPeerNum)
        } yield new OwnHeadPeerPublic(ownPeerId, ownHeadVKey)

    trait Section {
        def ownHeadPeerPublic: OwnHeadPeerPublic

        def ownHeadPeerNum: HeadPeerNumber

        def ownHeadPeerId: HeadPeerId

        def ownHeadVKey: VerificationKey
    }
}
