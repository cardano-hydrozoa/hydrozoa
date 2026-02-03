package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber, BlockType}
import hydrozoa.multisig.ledger.dapp.tx.TxSignature

sealed trait AckBlock {
    def ackId: AckId
    def blockNum: BlockNumber

    final transparent inline def ackNum: AckNumber = ackId.ackNum
    final transparent inline def peerNum: HeadPeerNumber = ackId.peerNum
}

object AckBlock {
    final case class Minor(
        override val ackId: AckId,
        override val blockNum: BlockNumber,
        header: BlockHeader.Minor.HeaderSignature,
        postDatedRefundTxs: List[TxSignature],
        finalizationRequested: Boolean,
    ) extends AckBlock,
          BlockType.Minor,
          AckRound1

    final case class Major1(
        override val ackId: AckId,
        override val blockNum: BlockNumber,
        fallbackTx: TxSignature,
        rolloutTxs: List[TxSignature],
        postDatedRefundTxs: List[TxSignature],
        finalizationRequested: Boolean,
    ) extends AckBlock,
          BlockType.Major,
          AckRound1

    final case class Major2(
        override val ackId: AckId,
        override val blockNum: BlockNumber,
        settlementTx: TxSignature,
    ) extends AckBlock,
          BlockType.Major,
          AckRound2

    final case class Final1(
        override val ackId: AckId,
        override val blockNum: BlockNumber,
        rolloutTxs: List[TxSignature],
        deinitTx: Option[TxSignature],
    ) extends AckBlock,
          BlockType.Final,
          AckRound1 {
        transparent inline def postDatedRefundTxs: List[TxSignature] = List()
    }

    final case class Final2(
        override val ackId: AckId,
        override val blockNum: BlockNumber,
        finalizationTx: TxSignature,
    ) extends AckBlock,
          BlockType.Final,
          AckRound2

    type Intermediate = AckBlock & BlockType.Intermediate
}
