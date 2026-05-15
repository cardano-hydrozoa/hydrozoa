package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import scalus.cardano.ledger.VKeyWitness

/** A head peer's hard acknowledgment of a closed stack — see `consensus/slow-consensus` in the
  * spec.
  *
  * Hard-acks are emitted by [[hydrozoa.multisig.consensus.SlowConsensusActor]] in one of three
  * shapes, depending on the stack's [[hydrozoa.multisig.ledger.stack.StackEffects]] variant:
  *
  *   - **2-phase Regular** stacks (containing settlement/finalization): two acks per peer per stack
  *     — round 1 ([[HardAck.Round1Payload.Regular]]) over every effect except the first settlement
  *     / finalization, and round 2 ([[HardAck.Round2Payload.Regular]]) over that first unlock.
  *   - **2-phase Initial** stack 0: round 1 ([[HardAck.Round1Payload.Initial]]) over the locally
  *     derived fallback tx; round 2 ([[HardAck.Round2Payload.Initial]]) over the exogenous init tx
  *     body, plus per-peer individual key witnesses for utxos this peer is funding.
  *   - **1-phase Sole** stacks (minor-only Regular): one ack per peer ([[HardAck.SolePayload]])
  *     over every effect (refund txs + the last evac commit).
  *
  * Wire ordering: for each `(peer, stackNum)`, round-1 / sole hard-acks always precede round-2. The
  * `finalizationRequested` flag mirrors [[SoftAck.finalizationRequested]] — per-peer request, not a
  * confirmation.
  */
final case class HardAck(
    ackId: HardAckId,
    stackNum: StackNumber,
    payload: HardAck.Payload,
    finalizationRequested: Boolean
) {
    final transparent inline def hardAckNum: HardAckNumber = ackId.hardAckNum
    final transparent inline def peerNum: HeadPeerNumber = ackId.peerNum

    val toContext: Seq[(String, String)] =
        Seq(
          "peer" -> peerNum.toString,
          "ackId" -> ackId.toString,
          "stackNum" -> stackNum.toString,
          "round" -> payload.roundLabel
        )
}

object HardAck {

    /** Tag selecting which round of the slow-consensus protocol this hard-ack belongs to.
      *
      *   - [[HardAck.Round.One]] / [[HardAck.Round.Two]] — used in 2-phase stacks (regular with
      *     settlement / finalization, or the initial stack).
      *   - [[HardAck.Round.Sole]] — used in 1-phase minor-only stacks; the only round.
      */
    enum Round:
        case One, Two, Sole

    sealed trait Payload {
        def round: Round
        final transparent inline def roundLabel: String = round.toString
    }

    object Payload {
        sealed trait Round1 extends Payload { final override def round: Round = Round.One }
        sealed trait Round2 extends Payload { final override def round: Round = Round.Two }
    }

    /** Round-1 ack payload variants — varies by [[StackEffects]] shape. */
    object Round1Payload {

        /** Per-effect signatures for everything in a regular stack EXCEPT the first
          * settlement/finalization (the round-2 unlock).
          *
          * Maps are keyed by partition index (one partition per major version in the stack); inner
          * maps with `(Int, Int)` keys are `(partitionIndex, withinPartitionIndex)`.
          */
        final case class Regular(
            settlements: Map[Int, TxSignature],
            fallbacks: Map[Int, TxSignature],
            rollouts: Map[(Int, Int), TxSignature],
            refunds: Map[(Int, Int), TxSignature],
            evacCommits: Map[Int, TxSignature],
            finalization: Map[Int, TxSignature]
        ) extends Payload.Round1

        /** Stack 0 round 1: signature over the locally-derived fallback tx body. */
        final case class Initial(
            fallbackSig: TxSignature
        ) extends Payload.Round1
    }

    /** Round-2 ack payload variants — the "unlock" round. */
    object Round2Payload {

        /** Round-2 in a regular stack: signature over the FIRST settlement / finalization. */
        final case class Regular(
            firstUnlockSig: TxSignature
        ) extends Payload.Round2

        /** Stack 0 round 2: init tx body sig + this peer's individual key witnesses for any utxos
          * it is funding from its individual address (operator-supplied funding).
          */
        final case class Initial(
            initTxSig: TxSignature,
            individualWitnesses: List[VKeyWitness]
        ) extends Payload.Round2
    }

    /** Sole-round ack payload — 1-phase minor-only stacks only. Signs every effect (refund txs +
      * the last evac commit). Same map shape as [[Round1Payload.Regular]] but conceptually a
      * different round.
      */
    final case class SolePayload(
        refunds: Map[(Int, Int), TxSignature],
        evacCommits: Map[Int, TxSignature]
    ) extends Payload {
        override def round: Round = Round.Sole
    }
}
