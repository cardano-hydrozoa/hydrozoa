package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import scalus.cardano.ledger.{Transaction, VKeyWitness}

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
  * Wire ordering: for each `(peer, stackNum)`, round-1 / sole hard-acks always precede round-2.
  *
  * No `finalizationRequested` flag (unlike [[SoftAck]]): by the time a stack reaches hard-ack, all
  * its blocks are already soft-confirmed, so whether the stack finalizes the head is derivable from
  * its contents (`StackEffects.Regular.finalization.isDefined`, or a `Block.SoftConfirmed.Final` in
  * the stack). The slow cycle ratifies effects; it does not decide block types — nothing tallies a
  * per-peer finalization request here.
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

    /** Round-1 ack payload variants — varies by [[StackEffects]] shape. */
    object Round1Payload {

        /** Per-effect signatures for everything in a regular stack EXCEPT the first
          * settlement/finalization (the round-2 unlock).
          *
          * `settlements` / `fallbacks` / `finalization` are keyed by partition index (one partition
          * per major version in the stack); `rollouts` / `refunds` use
          * `(partitionIndex, withinPartitionIndex)`.
          *
          * `evacCommits` is keyed by [[BlockNumber]] and carries a [[BlockHeader.HeaderSignature]]
          * (NOT a [[TxSignature]]): a standalone evacuation commitment commits a *block header* —
          * KZG commitments still live on `BlockHeader` (kept there so the rule-based on-chain code
          * is untouched), so a peer signs the full header, the same shape a soft-ack signs.
          */
        final case class Regular(
            settlements: Map[Int, TxSignature],
            fallbacks: Map[Int, TxSignature],
            rollouts: Map[(Int, Int), TxSignature],
            refunds: Map[(Int, Int), TxSignature],
            evacCommits: Map[BlockNumber, BlockHeader.HeaderSignature],
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

    /** Sole-round ack payload — 1-phase minor-only stacks only.
      *
      * A minor-only stack is exactly one partition (minors don't bump the major version), so there
      * is exactly ONE standalone evac commitment — `evacCommit` is a single
      * `(blockNum, headerSig)`, not a map. `refunds` is keyed `(partitionIndex,
      * withinPartitionIndex)` for shape-consistency with [[Round1Payload.Regular]] (partition index
      * is always 0 here).
      */
    final case class SolePayload(
        refunds: Map[(Int, Int), TxSignature],
        evacCommit: (BlockNumber, BlockHeader.HeaderSignature)
    ) extends Payload {
        override def round: Round = Round.Sole
    }

    /** Pre-signing material the [[hydrozoa.multisig.consensus.peer.HeadPeerWallet]] is handed to
      * produce a hard-ack. The wallet maps each entry through its signer and assembles the
      * corresponding [[Payload]] — it never inspects a [[hydrozoa.multisig.ledger.stack.Stack]]
      * itself (the caller does all the walking).
      *
      * Mirrors the payload shapes, but with raw material (tx bodies / header signing bytes) instead
      * of signatures.
      */
    object SigningInputs {
        final case class Round1Regular(
            settlements: Map[Int, Transaction],
            fallbacks: Map[Int, Transaction],
            rollouts: Map[(Int, Int), Transaction],
            refunds: Map[(Int, Int), Transaction],
            evacCommits: Map[BlockNumber, BlockHeader.Minor.Onchain.Serialized],
            finalization: Map[Int, Transaction]
        )

        final case class Round1Initial(fallback: Transaction)

        final case class Round2Regular(unlock: Transaction)

        final case class Round2Initial(
            initTx: Transaction,
            individualWitnesses: List[VKeyWitness]
        )

        final case class Sole(
            refunds: Map[(Int, Int), Transaction],
            evacCommit: (BlockNumber, BlockHeader.Minor.Onchain.Serialized)
        )
    }
}
