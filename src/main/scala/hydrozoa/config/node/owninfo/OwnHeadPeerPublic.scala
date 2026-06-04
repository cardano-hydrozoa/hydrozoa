package hydrozoa.config.node.owninfo

import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import io.circe.*
import io.circe.generic.semiauto.*
import scalus.crypto.ed25519.VerificationKey

final case class OwnHeadPeerPublic private[owninfo] (
    ownHeadPeerId: HeadPeerId,
    ownHeadVKey: VerificationKey,
) extends OwnPeerPublic {
    def ownHeadPeerNum: HeadPeerNumber = ownHeadPeerId.peerNum

    override def ownPeerId: PeerId = PeerId.Head(ownHeadPeerId.peerNum)
    override def canLeadFast(blockNum: BlockNumber): Boolean = ownHeadPeerId.isLeader(blockNum)
    override def canLeadSlow(stackNum: StackNumber): Boolean = ownHeadPeerId.isSlowLeader(stackNum)
    override def nextOwnLeaderBlock(after: BlockNumber): Option[BlockNumber] =
        Some(ownHeadPeerId.nextLeaderBlock(after))
    override def nextOwnSlowLeaderStack(after: StackNumber): Option[StackNumber] =
        Some(ownHeadPeerId.nextSlowLeaderStack(after))
    override def ownPeerLabel: String = ownHeadPeerNum.convert.toString
    override def ownPeerIndex: Int = ownHeadPeerNum.convert
}

object OwnHeadPeerPublic {

    /** Derive this head's identity by locating its verification key in the head set; the matching
      * index is its [[HeadPeerNumber]] (head peer numbers are contiguous from 0, in vkey-list
      * order). `None` if the key is not a configured head peer. Mirrors
      * [[OwnCoilPeerPublic.apply]].
      */
    def apply(ownHeadVKey: VerificationKey, headPeers: HeadPeers): Option[OwnHeadPeerPublic] =
        headPeers.headPeerVKeys.toList.indexWhere(_ == ownHeadVKey) match {
            case -1 => None
            case index =>
                Some(
                  new OwnHeadPeerPublic(
                    HeadPeerId(HeadPeerNumber(index), headPeers.nHeadPeers),
                    ownHeadVKey
                  )
                )
        }

    given ownHeadPeerPublicEncoder: Encoder[OwnHeadPeerPublic] =
        deriveEncoder[OwnHeadPeerPublic]

    given ownHeadPeerPublicDecoder: Decoder[OwnHeadPeerPublic] =
        deriveDecoder[OwnHeadPeerPublic]
}
