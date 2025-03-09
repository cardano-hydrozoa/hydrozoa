package hydrozoa.l2.consensus.network

import hydrozoa.{ParticipantVerificationKey, TxId, TxIx, TxKeyWitness}

trait HydrozoaNetwork {

    /** @return
      *   verification keys for known participants
      */
    def participantsKeys(): Set[ParticipantVerificationKey]

    def reqInit(req: ReqInit): Set[TxKeyWitness]
}

case class ReqInit(txId: TxId, txIx: TxIx, amount: Long)
