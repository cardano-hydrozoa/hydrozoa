package hydrozoa.head.network

import hydrozoa.head.{TxId, TxIx, TxKeyWitness, ParticipantVerificationKey}

trait HydrozoaNetwork {
  /**
   * @return verification keys for known participants
   */
  def participantsKeys(): Set[ParticipantVerificationKey]

  def reqInit(req: ReqInit): Set[TxKeyWitness]
}

case class ReqInit(txId: TxId, txIx: TxIx, amount: Long)
