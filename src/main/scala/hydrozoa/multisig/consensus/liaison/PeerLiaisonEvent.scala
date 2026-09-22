package hydrozoa.multisig.consensus.liaison

import cats.Eval
import hydrozoa.multisig.consensus.ack.HardAckNumber
import hydrozoa.multisig.ledger.stack.StackNumber

/** Typed events emitted by the liaison actors ([[PeerLiaisonHeadToHead]], [[PeerLiaisonCoilToHub]],
  * [[PeerLiaisonHubToCoil]]) and their shared [[Puller]] engine. Pure data; formatters in
  * [[PeerLiaisonEventFormat]] decide how each variant is rendered to a particular sink.
  *
  * One event type covers all three liaison kinds — they speak the same batch protocol, and the
  * per-liaison / per-remote identity comes from the wiring layer's `contramap` wrapper (e.g.
  * `HeadMultisigRegimeManagerEvent.PL`), not from separate event ADTs.
  *
  * [[BatchRequested]] and [[BatchReceived]] are the exception to "pure data": their `detail` is an
  * `Eval`, which compares by reference, so those two variants have no value equality. Two of them
  * with the same `batchNum` and identical detail text are unequal — do not assert on them by
  * construction, and do not `distinct` or dedupe a collection of them.
  */
sealed trait PeerLiaisonEvent

object PeerLiaisonEvent:

    /** Emitted once from a liaison's pre-start, after its connections resolve. */
    case object Started extends PeerLiaisonEvent

    /** A `GetMsgBatch` pull sent to the remote (initial, retransmit, or the next after a reply).
      * `detail` summarizes the requested cursors — including the backpressure `requestCeiling` — so
      * the mesh's request-flow throttling is visible. High-frequency: DEBUG.
      */
    final case class BatchRequested(batchNum: BatchNumber, detail: Eval[String])
        extends PeerLiaisonEvent

    /** A `NewMsgBatch` reply accepted from the remote. `detail` summarizes the per-lane payload
      * (requests / soft-ack / block / …) so co-arriving lanes are visible — e.g. requests and acks
      * delivered in the same batch. High-frequency: DEBUG.
      */
    final case class BatchReceived(batchNum: BatchNumber, detail: Eval[String])
        extends PeerLiaisonEvent

    /** A reply whose batch number does not match the outstanding request — a stale duplicate the
      * [[Puller]] drops.
      */
    final case class StaleBatchDropped(
        receivedBatchNum: BatchNumber,
        outstandingBatchNum: BatchNumber
    ) extends PeerLiaisonEvent

    /** A reply that failed lane verification; the [[Puller]] rejects it and the retransmit tick
      * keeps the chain alive. `reason` names the failing lane predicate.
      */
    final case class BatchRejected(batchNum: BatchNumber, reason: String) extends PeerLiaisonEvent

    /** A coil peer's link came up and the hub decided to seed it at `startStack`. `ownHardAck` is
      * the index the hub will now ask that coil for — the value that fixes a coil whose store is
      * too far behind to walk forward from. Rare and consequential: INFO.
      */
    final case class CoilSeeded(
        startStack: StackNumber,
        ownHardAck: HardAckNumber
    ) extends PeerLiaisonEvent

    /** A coil peer's link came up close enough behind to walk forward over the population lanes.
      * The ordinary outcome of a reconnect — nothing is transferred.
      */
    case object CoilCaughtUp extends PeerLiaisonEvent

    /** A coil peer's link came up but the hub has no start point to offer, so the coil bootstraps
      * stack 0 and catches up. `reason` names which of the two cases it is.
      */
    final case class CoilNotSeeded(reason: String) extends PeerLiaisonEvent

    /** A `Join.Offer` reached a coil peer whose actors are already running, so it was declined. A
      * start point is adopted at boot or not at all. Worth WARN: it means the hub decided this coil
      * needed seeding at a moment when seeding was no longer possible.
      */
    final case class JoinOfferTooLate(startStack: StackNumber) extends PeerLiaisonEvent

    /** A hub refused the ack **at a coil peer's cursor** on a `coilHardAck` lane, because its
      * `stackNum` is above the coil peer's ceiling. That lane is contiguous, so refusing its head
      * stops it: nothing behind that ack can be served until the coil peer's hard-confirmed stack
      * advances and lifts the ceiling.
      *
      * This is the precondition for the deadlock the window is sized to avoid
      * (docs/spec/liaison-backpressure.md), so it is worth seeing. A few are normal — a coil peer
      * behind on blocks parks its ack lanes, which is the ceiling working. A lane that never
      * resumes is not.
      */
    final case class CoilHardAckHeadRefused(
        hub: String,
        askedStack: String,
        ceilingStack: String
    ) extends PeerLiaisonEvent
