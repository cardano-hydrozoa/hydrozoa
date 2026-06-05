package hydrozoa.config.node.owninfo

import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import scalus.crypto.ed25519.VerificationKey

/** A coil node's own public identity. A coil never leads and authors no soft acks, so the
  * leadership / soft-ack surface is constant. The [[CoilPeerNumber]] is the coil peer's index in
  * the canonical `coilPeerVKeys` order (sorted by key bytes), located by matching its own
  * verification key.
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

    /** Derive this coil's identity by locating its verification key in the canonical
      * `coilPeerVKeys` order; the matching index is its [[CoilPeerNumber]]. `None` if the key is
      * absent from the configured coil set.
      */
    def apply(
        ownCoilVKey: VerificationKey,
        coilPeerVKeys: List[VerificationKey]
    ): Option[OwnCoilPeerPublic] =
        coilPeerVKeys.indexWhere(_ == ownCoilVKey) match {
            case -1    => None
            case index => Some(new OwnCoilPeerPublic(CoilPeerNumber(index), ownCoilVKey))
        }
}
