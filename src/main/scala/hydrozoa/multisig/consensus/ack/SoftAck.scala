package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import scalus.uplc.builtin.ByteString

/** A head peer's soft acknowledgment of a block brief: the head peer's Ed25519 signature over the
  * brief's [[hydrozoa.multisig.ledger.block.BlockBrief.Section.blockHash]]. See
  * `consensus/fast-consensus` in the spec.
  *
  * One soft-ack per peer per block, regardless of block type (Minor / Major / Final). The `ackNum`
  * is the block number — there is exactly one soft-ack per block, so the two coincide and using
  * `blockNum` keeps the per-peer ack sequence gap-free for
  * [[hydrozoa.multisig.consensus.PeerLiaisonHeadToHead]]'s batch protocol.
  *
  * The `finalizationRequested` flag lets any peer signal that the next block should be a `Final`
  * block; the leader for block N+1 reads the union of these flags across the soft-confirmed block-N
  * acks to decide the next block's type.
  */
final case class SoftAck(
    ackId: SoftAckId,
    blockNum: BlockNumber,
    signature: SoftAck.Signature,
    finalizationRequested: Boolean
) {
    final transparent inline def ackNum: SoftAckNumber = ackId.ackNum
    final transparent inline def peerNum: HeadPeerNumber = ackId.peerNum

    val toContext: Seq[(String, String)] =
        Seq(
          "peer" -> peerNum.toString,
          "ackId" -> ackId.toString,
          "blockNum" -> blockNum.toString
        )
}

object SoftAck {
    def apply(
        peerNum: HeadPeerNumber,
        blockNum: BlockNumber,
        signature: SoftAck.Signature,
        finalizationRequested: Boolean
    ): SoftAck = SoftAck(
      ackId = SoftAckId(peerNum, SoftAckNumber(blockNum: Int)),
      blockNum = blockNum,
      signature = signature,
      finalizationRequested = finalizationRequested
    )

    type Signature = Signature.Signature

    /** A head peer's Ed25519 signature over a block's [[hydrozoa.multisig.ledger.block.BlockHash]]
      * — the 32 digest bytes, and nothing beside them.
      *
      * The digest commits to the whole brief, block number and versions included, so there is
      * nothing to add: a ratchet that needs to order signed statements reads the SEC's own versions
      * (see [[hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment.Signature]]), never a
      * soft-ack's. The digest's domain tag keeps these signatures apart from any other the protocol
      * makes.
      *
      * Built by `PeerWallet.mkSoftAckSignature`; aggregated on `Block.SoftConfirmed`.
      */
    object Signature {
        opaque type Signature = IArray[Byte]

        def apply(signature: IArray[Byte]): Signature = signature

        given Conversion[Signature, IArray[Byte]] = identity

        given Conversion[Signature, Array[Byte]] = sig => IArray.genericWrapArray(sig).toArray

        given Conversion[Signature, ByteString] = sig => ByteString.fromArray(sig)

        extension (signature: Signature) def untagged: IArray[Byte] = identity(signature)
    }
}
