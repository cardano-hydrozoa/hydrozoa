package hydrozoa.multisig.consensus.ack

import cats.data.NonEmptyList
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import scalus.cardano.ledger.VKeyWitness

/** A head peer's hard acknowledgment of a closed stack — see `consensus/slow-consensus` in the
  * spec.
  *
  * Hard-acks are emitted by [[hydrozoa.multisig.consensus.SlowConsensusActor]] in one of three
  * shapes, depending on the stack's [[hydrozoa.multisig.ledger.stack.StackEffects]] variant:
  *
  *   - **2-phase Initial** stack 0: round 1 ([[HardAck.Round1Payload.Initial]]) over the locally
  *     derived fallback tx; round 2 ([[HardAck.Round2Payload.Initial]]) over the exogenous init tx
  *     body, plus per-peer individual key witnesses for utxos this peer is funding.
  *   - **2-phase Regular** stacks (containing settlement/finalization): two acks per peer per stack
  *     — round 1 ([[HardAck.Round1Payload.Regular]]) over every effect except the first settlement
  *     / finalization, and round 2 ([[HardAck.Round2Payload.Regular]]) over that first unlock.
  *   - **1-phase Sole** stacks (minor-only Regular): one ack per peer ([[HardAck.SolePayload]])
  *     over every effect (refund txs + the last evac commit).
  *
  * Wire ordering: for each `(peer, stackNum)`, round-1 / sole hard-acks always precede round-2.
  *
  * No `finalizationRequested` flag (unlike [[SoftAck]]): by the time a stack reaches hard-ack, all
  * its blocks are already soft-confirmed, so whether the stack finalizes the head is derivable from
  * its contents (`StackEffects.Unsigned.Regular.finalization.isDefined`, or a
  * `Block.SoftConfirmed.Final` in the stack). The slow cycle ratifies effects; it does not decide
  * block types — nothing tallies a per-peer finalization request here.
  */
final case class HardAck(
    ackId: HardAckId,
    stackNum: StackNumber,
    payload: HardAck.Payload
) {
    final transparent inline def hardAckNum: HardAckNumber = ackId.hardAckNum
    final transparent inline def peerNum: HeadPeerNumber = ackId.peerNum

    val toContext: Seq[(String, String)] =
        Seq(
          // The peer that *signed* this hard-ack — distinct from the local peer where the
          // log entry is produced.
          "hardAckSigner" -> peerNum.toString,
          // Disambiguate from a soft-ack id (ackId alone is ambiguous in mixed logs).
          "hardAckId" -> ackId.toString,
          "stackNum" -> stackNum.toString,
          "hardAckRound" -> payload.roundLabel
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

    /** Per-partition signatures — the spine of a regular hard-ack, mirroring
      * [[hydrozoa.multisig.ledger.stack.PartitionEffects]] (one [[PartitionSig]] per partition,
      * same order, same kind). Tx-body signatures are [[TxSignature]]; the standalone evac
      * commitment is signed over a *block header* so its signature is a
      * [[BlockHeader.HeaderSignature]] (not a `TxSignature`).
      *
      * In a round-1 payload the **unlock partition's** settlement / finalization signature is
      * absent (`None`) — it travels in [[Round2Payload.Regular]] instead. Every other slot is
      * present. Which partition is the unlock is the shared
      * [[hydrozoa.multisig.ledger.stack.PartitionEffects.unlock]] rule (not re-derived here).
      */
    sealed trait PartitionSig

    object PartitionSig {

        /** A [[PartitionEffects.Major]] partition's sigs. `settlement` is `None` iff this is the
          * round-2 unlock partition (its sig is the [[Round2Payload.Regular.firstUnlockSig]]).
          */
        final case class Major(
            settlement: Option[TxSignature],
            fallback: TxSignature,
            rollouts: List[TxSignature],
            refunds: List[TxSignature],
            sec: Option[BlockHeader.HeaderSignature]
        ) extends PartitionSig

        /** A [[PartitionEffects.Final]] partition's sigs. `finalization` is `None` iff this Final
          * is the round-2 unlock (no Major preceded it).
          */
        final case class Final(
            finalization: Option[TxSignature],
            rollouts: List[TxSignature]
        ) extends PartitionSig

        /** A [[PartitionEffects.Minor]] partition's sigs. SEC is mandatory (header sig). */
        final case class Minor(
            sec: BlockHeader.HeaderSignature,
            refunds: List[TxSignature]
        ) extends PartitionSig
    }

    /** Round-1 ack payload variants — varies by [[StackEffects]] shape. */
    object Round1Payload {

        /** Stack 0 round 1: signature over the locally-derived fallback tx body. */
        final case class Initial(
            fallbackSig: TxSignature
        ) extends Payload.Round1

        /** Per-partition signatures for a regular stack, EXCEPT the round-2 unlock (the unlock
          * partition's settlement / finalization slot is `None` here — see [[PartitionSig]]). Same
          * partition order as [[StackEffects.Unsigned.Regular.partitions]].
          */
        final case class Regular(
            partitions: NonEmptyList[PartitionSig]
        ) extends Payload.Round1
    }

    /** Round-2 ack payload variants — the "unlock" round. */
    object Round2Payload {

        /** Stack 0 round 2: init tx body sig + this peer's individual key witnesses for any utxos
          * it is funding from its individual address (operator-supplied funding).
          */
        final case class Initial(
            initTxSig: TxSignature,
            individualWitnesses: List[VKeyWitness]
        ) extends Payload.Round2

        /** Round-2 in a regular stack: signature over the FIRST settlement / finalization. */
        final case class Regular(
            firstUnlockSig: TxSignature
        ) extends Payload.Round2
    }

    /** Sole-round ack payload — 1-phase minor-only stacks only. A minor-only stack is exactly one
      * [[PartitionEffects.Minor]] partition (the leading minor run is the whole stack), so this is
      * that single partition's sigs: the mandatory SEC header signature + the minors' refund tx
      * signatures (= a [[PartitionSig.Minor]], inlined for the wire).
      */
    final case class SolePayload(
        sec: BlockHeader.HeaderSignature,
        refunds: List[TxSignature]
    ) extends Payload {
        override def round: Round = Round.Sole
    }

}
