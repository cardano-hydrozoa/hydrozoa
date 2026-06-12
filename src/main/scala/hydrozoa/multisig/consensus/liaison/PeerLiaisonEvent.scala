package hydrozoa.multisig.consensus.liaison

/** Typed events emitted by the liaison actors ([[PeerLiaisonHeadToHead]], [[PeerLiaisonCoilToHub]],
  * [[PeerLiaisonHubToCoil]]) and their shared [[Puller]] engine. Pure data; formatters in
  * [[PeerLiaisonEventFormat]] decide how each variant is rendered to a particular sink.
  *
  * One event type covers all three liaison kinds — they speak the same batch protocol, and the
  * per-liaison / per-remote identity comes from the wiring layer's `contramap` wrapper (e.g.
  * `MultisigRegimeManagerEvent.PL`), not from separate event ADTs.
  */
sealed trait PeerLiaisonEvent

object PeerLiaisonEvent:

    /** Emitted once from a liaison's pre-start, after its connections resolve. */
    case object Started extends PeerLiaisonEvent

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
