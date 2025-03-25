package hydrozoa.l2.consensus.network

import hydrozoa.*
import hydrozoa.l1.multisig.tx.DepositTx
import hydrozoa.l2.block.{Block, BlockHeader}
import hydrozoa.l2.ledger.UtxosSet
import hydrozoa.node.state.PeerInfo

trait HeadPeerNetwork {

    /** @return
      *   verification keys for known participants
      */
    def reqPublicKeys(headPeers: Set[PeerInfo]): Set[PeerPublicKeyBytes]

    def reqInit(headPeers: Set[PeerInfo], req: ReqInit): Set[TxKeyWitness]

    def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness]

    def reqMinor(block: Block): Set[AckMinor]

    // FIXME: remove utxosWithdrawn once we have block validation
    def reqMajor(block: Block, utxosWithdrawn: UtxosSet): Set[AckMajorCombined]

    // FIXME: remove utxosWithdrawn once we have block validation
    def reqFinal(block: Block, utxosWithdrawn: UtxosSet): Set[AckFinalCombined]
}

case class ReqInit(seedOutputRef: UtxoIdL1, coins: Long)

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
