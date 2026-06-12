package hydrozoa.multisig.consensus.ack

import cats.data.NonEmptyList
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber

/** A head peer's hard acknowledgment of a closed stack — see `consensus/slow-consensus` in the
  * spec.
  *
  * Hard-acks are emitted by [[hydrozoa.multisig.consensus.SlowConsensusActor]] in the wire shapes
  * listed below by `(stack kind, round, partition layout)`. The two top groups — Initial and Sole —
  * are single-shape; the third (Regular) splits into four cases by partition layout:
  *
  *   - **Initial** (2-phase, stack 0 only):
  *     1. [[HardAck.Round1Payload.Initial]] — signature over the locally derived fallback tx.
  *     2. [[HardAck.Round2Payload.Initial]] — head-multisig signature over the exogenous init tx
  *        body + this peer's individual-address signature for utxos it funds from its individual
  *        address (signature only; the verification key is this peer's known head key).
  *   - **Sole** (1-phase, minor-only Regular stack):
  *     3. [[HardAck.SolePayload]] — mandatory SEC header signature + every refund tx signature for
  *        the leading minor run. Single ack per peer per stack.
  *   - **Regular 2-phase** (Regular stack containing settlement and/or finalization). Round 2 is
  *     always [[HardAck.Round2Payload.Regular]] — one tx signature over the unlock (first Major's
  *     settlement, else the Final's finalization, per
  *     [[hydrozoa.multisig.ledger.stack.PartitionEffects.unlock]]). Round 1 has four shapes:
  *     4. [[OnlyPartial]] — `[Major]` or `[Final]`.
  *     5. [[PartialThenCompletes]] — `[Major, Major* (Major | Final)]`.
  *     6. [[MinorThenPartial]] — `[Minor, Major]` or `[Minor, Final]`.
  *     7. [[MinorThenPartialThenCompletes]] — `[Minor, Major, Major* (Major | Final)]`.
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
    final transparent inline def peerId: PeerId = ackId.peerId

    val toContext: Seq[(String, String)] =
        Seq(
          // The peer that *signed* this hard-ack — distinct from the local peer where the
          // log entry is produced.
          "hardAckSigner" -> peerId.toString,
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

        /** Stack 0 round 1: signature over the locally-derived fallback tx body. */
        final case class Initial(
            fallbackSig: TxSignature
        ) extends Payload.Round1

        /** Round-1 sigs for a regular stack. The key observation here is that depending on the
          * position the same partition may require either complete or partial set of signatures.
          * Four structurally exclusive cases, one per shape the partition list can take given the
          * `[Minor?] [Major]* [Final?]` layout produced by
          * [[hydrozoa.multisig.ledger.stack.StackPartition]] and the unlock rule from
          * [[hydrozoa.multisig.ledger.stack.PartitionEffects.unlock]] (first Major's settlement;
          * else — when no Major — the Final's finalization).
          *
          * Cases listed in order of growing structural complexity (fewest slots first):
          *
          *   1. [[OnlyPartial]] — `[Major]` or `[Final]`
          *   2. [[PartialThenCompletes]] — `[Major, Major* (Major | Final)]`
          *   3. [[MinorThenPartial]] — `[Minor, Major]` or `[Minor, Final]`
          *   4. [[MinorThenPartialThenCompletes]] — `[Minor, Major, Major* (Major | Final)]`
          *
          * The trailing `Major* (Major | Final)` in cases 2 and 4 reads as "zero or more Majors,
          * then exactly one terminal element (a Major or the Final)" — i.e. ≥ 1 trailing Complete,
          * with at most one Final at the very end and nothing after it. Empty trailing is excluded
          * by construction (`completes: NonEmptyList[PartitionSigs.Complete]`) — that's cases 1/3's
          * shape, not 2/4's.
          *
          * Note: a Final partition is always layout-terminal (the `[Minor?] [Major]* [Final?]`
          * grammar forbids anything after a Final). So cases 2 and 4's `completes` field is of
          * shape `Major* (Major | Final)` — ≥ 1 `MajorComplete`s with at most one terminal
          * `FinalComplete` at the very end. And in cases 2 and 4 the partial is ALWAYS a
          * [[PartitionSigs.MajorPartial]] (a `FinalPartial` is only possible when there is no Major
          * at all, which is cases 1 and 3).
          *
          * In every case there is EXACTLY one [[PartitionSigs.Partial]] (the round-1 unlock
          * partition); everything else is the leading [[PartitionSigs.Minor]] (cases 3, 4) and/or
          * post-partial [[PartitionSigs.Complete]] slots (cases 2, 4). The 4 variants make every
          * other arrangement unrepresentable — no slot-list invariants to runtime-check.
          *
          * Field order in each variant matches the partition order of the corresponding
          * [[hydrozoa.multisig.ledger.stack.StackEffects.Unsigned.Regular.partitions]] list:
          * leading Minor (if any) → Partial → Completes (if any).
          */
        sealed trait Regular extends Payload.Round1

        object Regular {

            /** Case 1: the entire stack is exactly one partition (a Major or Final), which IS the
              * round-1 unlock. Effects' partition list: `[Major]` (`partial` = `MajorPartial`) or
              * `[Final]` (`partial` = `FinalPartial`, only fires when the stack contains no Major
              * at all).
              */
            final case class OnlyPartial(
                partial: PartitionSigs.Partial
            ) extends Regular

            /** Case 2: the unlock Major at index 0 followed by ≥ 1 further Complete partitions —
              * zero or more `MajorComplete` optionally followed by exactly one terminal
              * `FinalComplete` (Final is layout-terminal). Effects' partition list:
              * `[Major, Major* (Major | Final)]`. `partial` is always a
              * [[PartitionSigs.MajorPartial]] here.
              */
            final case class PartialThenCompletes(
                partial: PartitionSigs.MajorPartial,
                completes: NonEmptyList[PartitionSigs.Complete]
            ) extends Regular

            /** Case 3: leading Minor partition then the round-1 unlock partition (a single Major
              * or Final), nothing further. Effects' partition list: `[Minor, Major]` (`partial`
              * = `MajorPartial`) or `[Minor, Final]` (`partial` = `FinalPartial`, only fires when
              * the stack contains no Major at all).
              */
            final case class MinorThenPartial(
                minor: PartitionSigs.Minor,
                partial: PartitionSigs.Partial
            ) extends Regular

            /** Case 4: leading Minor partition, then the unlock Major, then ≥ 1 further Complete
              * partitions. The completes are zero or more `MajorComplete` optionally followed by
              * exactly one terminal `FinalComplete` (Final is layout-terminal). Effects' partition
              * list: `[Minor, Major, Major* (Major | Final)]`. `partial` is always a
              * [[PartitionSigs.MajorPartial]] here — there is at least one Major in the stack, so
              * the unlock rule selects it (not the Final).
              */
            final case class MinorThenPartialThenCompletes(
                minor: PartitionSigs.Minor,
                partial: PartitionSigs.MajorPartial,
                completes: NonEmptyList[PartitionSigs.Complete]
            ) extends Regular

            /** Flatten a `Regular` variant back to the partition-aligned slot sequence (leading
              * Minor if any, then Partial, then Completes if any). Used by the verifier to zip sigs
              * vs effects partition-by-partition without re-dispatching on the variant.
              */
            def asSlots(r: Regular): NonEmptyList[PartitionSigs] = r match {
                case OnlyPartial(p)              => NonEmptyList.one(p)
                case PartialThenCompletes(p, cs) => NonEmptyList(p, cs.toList)
                case MinorThenPartial(m, p)      => NonEmptyList.of(m, p)
                case MinorThenPartialThenCompletes(m, p, cs) =>
                    NonEmptyList(m, p :: cs.toList)
            }
        }

        /** A single partition's round-1 sigs. Three categories — [[PartitionSigs.Partial]] (the
          * round-1 unlock partition's sig set, missing the unlocking sig),
          * [[PartitionSigs.Complete]] (a non-unlock Major or Final, all sigs present), and
          * [[PartitionSigs.Minor]] (always complete; structurally distinct because Minor can only
          * appear as the leading partition and is never the round-1 unlock).
          *
          * Round-coupling (unlock vs not) lives at this round-1-payload-scoped slot level — NOT on
          * partition-level data — because being "the unlock partition" is a property of the stack's
          * round-1 slicing, not of the partition itself. Tx-body sigs are [[TxSignature]]; the
          * standalone evac commitment is signed over a block header so its signature is a
          * [[BlockHeader.HeaderSignature]].
          */
        sealed trait PartitionSigs

        object PartitionSigs {

            /** Partial = the round-1 unlock partition's sigs, MISSING the unlocking sig (settlement
              * for [[MajorPartial]], finalization for [[FinalPartial]]). The missing sig travels in
              * [[Round2Payload.Regular.firstUnlockSig]].
              */
            sealed trait Partial extends PartitionSigs

            /** Complete = a non-unlock Major or Final partition's sigs, ALL present. Does NOT
              * include [[Minor]] — Minor stands alone because it can never be the round-1 unlock
              * and can only appear as the leading partition (see [[PartitionSigs]]'s scaladoc).
              */
            sealed trait Complete extends PartitionSigs

            final case class MajorComplete(
                settlement: TxSignature,
                fallback: TxSignature,
                rollouts: List[TxSignature],
                refunds: List[TxSignature],
                sec: Option[BlockHeader.HeaderSignature]
            ) extends Complete

            final case class MajorPartial(
                fallback: TxSignature,
                rollouts: List[TxSignature],
                refunds: List[TxSignature],
                sec: Option[BlockHeader.HeaderSignature]
            ) extends Partial

            final case class FinalComplete(
                finalization: TxSignature,
                rollouts: List[TxSignature]
            ) extends Complete

            final case class FinalPartial(
                rollouts: List[TxSignature]
            ) extends Partial

            /** Minor: leading-partition slot (every sig present). Never the unlock; never appears
              * mid-sequence (trailing minors are absorbed into their Major partition's `sec`).
              */
            final case class Minor(
                sec: BlockHeader.HeaderSignature,
                refunds: List[TxSignature]
            ) extends PartitionSigs
        }
    }

    /** Round-2 ack payload variants — the "unlock" round.
      *
      * ===Atomicity invariant===
      *
      * Round 2 covers '''exactly one transaction''' (its multisig sig comes together in this
      * round). This is load-bearing for protocol atomicity: round-1 effects are released only after
      * round-2 saturates, so round 2 is the single point where the last signer commits. If round 2
      * covered TWO independent txs, the last signer could publish a sig for one and withhold the
      * other — breaking the all-or-nothing release that the round split exists to provide. Every
      * variant below therefore carries sigs for ONE tx only.
      *
      * Concretely:
      *   - [[Regular]]: one sig over the unlock tx (settlement OR finalization — exactly one, per
      *     [[hydrozoa.multisig.ledger.stack.PartitionEffects.unlock]]).
      *   - [[Initial]]: BOTH fields ([[Initial.initTxSig]] and [[Initial.individualSig]]) are
      *     signatures over the SAME initialization transaction — head-multisig contribution + this
      *     peer's individual-address signature for utxos it funds from its individual address. One
      *     tx, two witness kinds, one round-2 commit point.
      */
    object Round2Payload {

        /** Stack 0 round 2 — both signatures here are over the SAME initialization tx (one tx, two
          * witness kinds; see the round-2 atomicity invariant above):
          *   - [[initTxSig]]: this peer's head-multisig signature over the init tx body.
          *   - [[individualSig]]: this peer's individual-address signature for any utxos it funds
          *     from its individual address (operator-supplied funding) — `Some` iff it funds such
          *     an input, also over the same init tx. Signature only: the verification key is this
          *     peer's known head key (the aggregator rebuilds the `VKeyWitness` from `peerNum`), so
          *     a peer can only ever contribute a witness under its own key.
          */
        final case class Initial(
            initTxSig: TxSignature,
            individualSig: Option[TxSignature]
        ) extends Payload.Round2

        /** Round-2 in a regular stack: signature over the FIRST settlement / finalization (the
          * single unlock tx per [[hydrozoa.multisig.ledger.stack.PartitionEffects.unlock]]; see the
          * round-2 atomicity invariant above).
          */
        final case class Regular(
            firstUnlockSig: TxSignature
        ) extends Payload.Round2
    }

    /** Sole-round ack payload — 1-phase minor-only stacks only. A minor-only stack is exactly one
      * [[hydrozoa.multisig.ledger.stack.PartitionEffects.Minor]] partition (the leading minor run
      * is the whole stack), so this is that single partition's sigs: the mandatory SEC header
      * signature + the minors' refund tx signatures (same shape as
      * [[Round1Payload.PartitionSigs.Minor]], inlined for the wire).
      */
    final case class SolePayload(
        sec: BlockHeader.HeaderSignature,
        refunds: List[TxSignature]
    ) extends Payload {
        override def round: Round = Round.Sole
    }

}
