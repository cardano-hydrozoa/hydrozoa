package hydrozoa.l2.consensus.network

import hydrozoa.*
import hydrozoa.l1.multisig.tx.DepositTx
import hydrozoa.l2.block.{Block, BlockHeader}
import hydrozoa.l2.ledger.state.UtxosDiff

trait HydrozoaNetwork {

    /** @return
      *   verification keys for known participants
      */
    def participantsKeys(): Set[ParticipantVerificationKey]

    def reqInit(req: ReqInit): Set[TxKeyWitness]

    def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness]

    def reqMinor(block: Block): Set[AckMinor]

    // FIXME: remove utxosWithdrawn once we have block validation
    def reqMajor(block: Block, utxosWithdrawn: UtxosDiff): Set[AckMajorCombined]

    // FIXME: remove utxosWithdrawn once we have block validation
    def reqFinal(block: Block, utxosWithdrawn: UtxosDiff): Set[AckFinalCombined]
}

case class ReqInit(seedOutputRef: OutputRefL1, coins: Long)

case class ReqRefundLater(depositTx: DepositTx, index: TxIx)

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
