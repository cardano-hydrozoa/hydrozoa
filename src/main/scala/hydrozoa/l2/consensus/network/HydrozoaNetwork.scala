package hydrozoa.l2.consensus.network

import hydrozoa.{L1Tx, ParticipantVerificationKey, TxId, TxIx, TxKeyWitness}

trait HydrozoaNetwork {

    /** @return
      *   verification keys for known participants
      */
    def participantsKeys(): Set[ParticipantVerificationKey]

    def reqInit(req: ReqInit): Set[TxKeyWitness]

    def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness]
}

case class ReqInit(txId: TxId, txIx: TxIx, amount: Long)

case class ReqRefundLater(depositTx: L1Tx, index: Long)
