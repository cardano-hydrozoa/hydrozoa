package hydrozoa.multisig.consensus.liaison

/** Typed events emitted by the liaison actors ([[PeerLiaisonHeadToHead]], [[PeerLiaisonCoilToHub]],
  * [[PeerLiaisonHubToCoil]]) and their shared [[Puller]] engine. Pure data; formatters in
  * [[PeerLiaisonEventFormat]] decide how each variant is rendered to a particular sink.
  *
  * One event type covers all three liaison kinds — they speak the same batch protocol, and the
  * per-liaison / per-remote identity comes from the wiring layer's `contramap` wrapper (e.g.
  * `HeadMultisigRegimeManagerEvent.PL`), not from separate event ADTs.
  */
sealed trait PeerLiaisonEvent

object PeerLiaisonEvent:

    /** Emitted once from a liaison's pre-start, after its connections resolve. */
    case object Started extends PeerLiaisonEvent

    /** A `GetMsgBatch` pull sent to the remote (initial, retransmit, or the next after a reply).
      * `detail` summarizes the requested cursors — including the backpressure `requestCeiling` — so
      * the mesh's request-flow throttling is visible. High-frequency: DEBUG.
      */
    final case class BatchRequested(batchNum: BatchNumber, detail: String) extends PeerLiaisonEvent

    /** A `NewMsgBatch` reply accepted from the remote. `detail` summarizes the per-lane payload
      * (requests / soft-ack / block / …) so co-arriving lanes are visible — e.g. requests and acks
      * delivered in the same batch. High-frequency: DEBUG.
      */
    final case class BatchReceived(batchNum: BatchNumber, detail: String) extends PeerLiaisonEvent

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
