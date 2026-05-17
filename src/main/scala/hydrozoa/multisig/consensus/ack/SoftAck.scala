package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber}

/** A head peer's soft acknowledgment of a block brief: the head peer's Ed25519 signature over the
  * brief's [[BlockHeader.Section.signingBytes]]. See `consensus/fast-consensus` in the spec.
  *
  * One soft-ack per peer per block, regardless of block type (Minor / Major / Final). The `ackNum`
  * (soft-ack number — TODO(rename): `AckNumber` -> `SoftAckNumber`, see AckNumber.scala) is the
  * block number — there is exactly one soft-ack per block, so the two coincide and using `blockNum`
  * keeps the per-peer ack sequence gap-free for [[hydrozoa.multisig.consensus.PeerLiaison]]'s batch
  * protocol.
  *
  * The `finalizationRequested` flag lets any peer signal that the next block should be a `Final`
  * block; the leader for block N+1 reads the union of these flags across the soft-confirmed block-N
  * acks to decide the next block's type.
  */
final case class SoftAck(
    ackId: AckId,
    blockNum: BlockNumber,
    headerSignature: BlockHeader.HeaderSignature,
    finalizationRequested: Boolean
) {
    final transparent inline def ackNum: AckNumber = ackId.ackNum
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
        header: BlockHeader.HeaderSignature,
        finalizationRequested: Boolean
    ): SoftAck = SoftAck(
      ackId = AckId(peerNum, AckNumber(blockNum: Int)),
      blockNum = blockNum,
      headerSignature = header,
      finalizationRequested = finalizationRequested
    )
}
