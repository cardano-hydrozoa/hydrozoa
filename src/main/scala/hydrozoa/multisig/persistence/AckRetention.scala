package hydrozoa.multisig.persistence

/** Whether an ack key may be physically deleted yet.
  *
  * Confirmation makes a per-peer ack signature redundant to *consensus* — the confirmation record
  * carries the aggregated multisig that subsumes it — so the aggregator drops it in the same
  * `WriteBatch` that writes the record (§7, "confirmation-driven ack-pruning"). But an attached
  * archiver keeps the per-peer signatures, and it reads the store on its own schedule: pruning at
  * confirmation would delete them seconds after they are written, well inside the archiver's tail
  * interval, so an archive would systematically lack every ack journal.
  *
  * This is the seam that reconciles the two. It is a *value* the aggregators are handed rather than
  * a registry they consult, so the confirmation path states its dependency instead of reaching for
  * global state, and so a test can pin the decision without an archiver anywhere in sight.
  */
trait AckRetention {

    /** Whether the entry at `index` in `cf` may be deleted now.
      *
      * Asked per key rather than per block or stack, because the two ack journals number
      * differently: a soft-ack's index *is* its block number, while a hard-ack's is an independent
      * per-peer cursor that advances once per `(stackNum, round)`. Both are answerable here because
      * an archiver's watermark is already per column family, so each index is compared against a
      * watermark in its own numbering space and no conversion arises.
      */
    def mayPrune(cf: Cf, index: Long): Boolean
}

object AckRetention {

    /** No archiver is attached, so nothing but consensus holds an ack: prune on confirmation.
      *
      * The default, and the behaviour of every node that declares no archiver.
      */
    val unconstrained: AckRetention = (_, _) => true

    /** An archiver is attached: an ack survives until the archive holds it.
      *
      * A family the archiver has never reported prunes nothing — including before its first report
      * — which is the same conservative reading retention takes everywhere else: an archiver was
      * declared and is not yet accounted for, so nothing may be deleted on its behalf.
      *
      * The cost is real and worth stating where it is decided: a declared archiver that dies stops
      * ack pruning, and the ack families are the highest-churn in the store. That is the safe
      * direction, but it is not a free one — it is why a stale watermark has to raise an alert
      * rather than merely stall a floor.
      */
    def archivedUpTo(watermarks: ArchiveWatermarks): AckRetention =
        (cf, index) => watermarks.watermark(cf).exists(_ >= index)

    /** The retention an attached archiver implies, or [[unconstrained]] when none is. */
    def forArchiver(watermarks: Option[ArchiveWatermarks]): AckRetention =
        watermarks.fold(unconstrained)(archivedUpTo)
}
