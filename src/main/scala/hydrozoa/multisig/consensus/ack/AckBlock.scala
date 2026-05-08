package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber, BlockType}
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import io.circe.*
import io.circe.syntax.*

sealed trait AckBlock {
    def ackId: AckId
    def blockNum: BlockNumber

    final transparent inline def ackNum: AckNumber = ackId.ackNum
    final transparent inline def peerNum: HeadPeerNumber = ackId.peerNum

    val ackTypeName: String = this match {
        case _: AckBlock.Minor  => "minor"
        case _: AckBlock.Major1 => "major1"
        case _: AckBlock.Major2 => "major2"
        case _: AckBlock.Final1 => "final1"
        case _: AckBlock.Final2 => "final2"
    }

    val toContext: Seq[(String, String)] =
        Seq(
          "ackType" -> ackTypeName,
          "peer" -> peerNum.toString,
          "ackId" -> ackId.toString,
          "blockNum" -> blockNum.toString
        )
}

object AckBlock {

    /** An encoder for logging purposes. It does not round-trip; it only prints the ackTypeName, the
      * ackId, and the blockNum
      */
    val loggingEncoder: Encoder[AckBlock] = Encoder.instance(ack =>
        Json.obj(
          ack.ackTypeName -> Json.obj("ackId" -> ack.ackId.asJson),
          "blockNum" -> ack.blockNum.asJson
        )
    )

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
