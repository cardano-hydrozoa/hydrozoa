package hydrozoa.config.node.owninfo

import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import scalus.crypto.ed25519.VerificationKey

/** A coil node's own public identity. A coil never leads and authors no soft acks, so the
  * leadership / soft-ack surface is constant. The [[CoilPeerNumber]] is the one [[CoilPeers]] maps
  * to this peer's verification key.
  */
final case class OwnCoilPeerPublic private[owninfo] (
    ownCoilPeerNum: CoilPeerNumber,
    ownCoilVKey: VerificationKey,
) extends OwnPeerPublic {
    override def ownPeerId: PeerId = PeerId.Coil(ownCoilPeerNum)
    override def canLeadFast(blockNum: BlockNumber): Boolean = false
    override def canLeadSlow(stackNum: StackNumber): Boolean = false
    override def nextOwnLeaderBlock(after: BlockNumber): Option[BlockNumber] = None
    override def nextOwnSlowLeaderStack(after: StackNumber): Option[StackNumber] = None
    override def ownPeerLabel: String = s"c${ownCoilPeerNum.convert}"
    override def ownPeerIndex: Int = ownCoilPeerNum.convert
}

object OwnCoilPeerPublic {

    /** Derive this coil's identity by locating its verification key among the `coilPeers`; the
      * matching [[CoilPeerNumber]] is its own. `None` if the key is absent from the configured coil
      * set.
      */
    def apply(
        ownCoilVKey: VerificationKey,
        coilPeers: CoilPeers
    ): Option[OwnCoilPeerPublic] =
        coilPeers
            .coilPeerNumberOf(ownCoilVKey)
            .map(num => new OwnCoilPeerPublic(num, ownCoilVKey))
}
