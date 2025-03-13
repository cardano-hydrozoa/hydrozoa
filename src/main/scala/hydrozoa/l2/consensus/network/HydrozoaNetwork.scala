package hydrozoa.l2.consensus.network

import hydrozoa.*
import hydrozoa.l2.block.{Block, BlockHeader}

trait HydrozoaNetwork {

    /** @return
      *   verification keys for known participants
      */
    def participantsKeys(): Set[ParticipantVerificationKey]

    def reqInit(req: ReqInit): Set[TxKeyWitness]

    def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness]

    def reqMinor(block: Block): Set[AckMinor]

    def reqMajor(block: Block): Set[AckMajorCombined]

    def reqFinal(block: Block): Set[AckFinalCombined]
}

case class ReqInit(txId: TxId, txIx: TxIx, amount: Long)

case class ReqRefundLater(depositTx: L1Tx, index: TxIx)

case class AckMinor(
    blockHeader: BlockHeader,
    signature: Unit,
    nextBlockFinal: Boolean
)

case class AckMajorCombined(
    blockHeader: BlockHeader,
    rollouts: Set[TxKeyWitness],
    settlement: TxKeyWitness,
    nextBlockFinal: Boolean
)

case class AckFinalCombined(
    blockHeader: BlockHeader,
    rollouts: Set[TxKeyWitness],
    finalization: TxKeyWitness
)
