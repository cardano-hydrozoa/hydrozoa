package hydrozoa.multisig.persistence

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

/** How far an attached archiver has durably copied each column family, as it last reported.
  *
  * The archiver reads this node's store as a RocksDB secondary and is never dialed; the watermark
  * it posts to `POST /api/admin/archive/watermark` is the only thing it ever tells the node, and
  * the only reason the node may delete anything. This holds what arrived. Acting on it — taking the
  * minimum with what consensus still needs, and trimming — belongs to retention.
  *
  * **Monotone, never lowered.** A rebuilt or truncated archive will report a watermark below one
  * already seen, and adopting it would be meaningless: the node cannot un-delete what it removed on
  * the strength of the earlier report. A regression is therefore recorded as [[Report.regressed]]
  * for the caller to log and otherwise ignored.
  *
  * **Not persisted.** A restart forgets every watermark, so the node retains everything until the
  * archiver reports again — seconds, at the archiver's tail cadence. The failure direction is
  * "retain more", which is the safe one, and persisting would cost either new [[Cf.Meta]] keys or a
  * column family plus a [[StoreVersion]] bump to buy nothing.
  *
  * Written by the HTTP layer and read by retention, so state lives in one
  * [[java.util.concurrent.atomic.AtomicReference AtomicReference]] over an immutable map — the same
  * publication idiom as `PeerMetrics`, and enough here because reports are rare.
  */
final class ArchiveWatermarks private (
    state: AtomicReference[ArchiveWatermarks.State]
) {
    import ArchiveWatermarks.*

    /** Merge a report, keeping the higher index per family.
      *
      * `reportedAt` is recorded even when nothing advanced: a report that repeats the previous
      * watermarks still proves the archiver is running, and liveness is the other half of what
      * retention needs from this.
      */
    def record(reported: Map[Cf, Long], reportedAt: Instant): Report = {
        @tailrec def merge(): Report = {
            val current = state.get()
            val regressed = reported.filter { case (cf, index) =>
                current.watermarks.get(cf).exists(_ > index)
            }
            val advanced = reported.filterNot { case (cf, index) =>
                current.watermarks.get(cf).exists(_ >= index)
            }
            val merged = State(
              watermarks = current.watermarks ++ advanced,
              lastReportAt = Some(reportedAt)
            )
            if state.compareAndSet(current, merged) then
                Report(accepted = merged.watermarks, advanced = advanced, regressed = regressed)
            else merge()
        }
        merge()
    }

    /** The highest index reported for `cf`, if any. */
    def watermark(cf: Cf): Option[Long] = state.get().watermarks.get(cf)

    /** Every watermark held. */
    def watermarks: Map[Cf, Long] = state.get().watermarks

    /** When a report last arrived, or `None` if none has since this process started. */
    def lastReportAt: Option[Instant] = state.get().lastReportAt

    /** Whether the archiver counts as running, judged against `staleAfter` from the node's
      * [[hydrozoa.config.node.ArchiverConfig]].
      *
      * `false` before the first report as well as after a silence, and both mean the same thing to
      * retention: an archiver was declared and is not currently accounted for, so nothing may be
      * deleted on its behalf. Distinguishing "not yet" from "not any more" is a matter for the
      * operator's alert, not for the decision.
      */
    def isFresh(staleAfter: java.time.Duration, now: Instant): Boolean =
        state.get().lastReportAt.exists(at => !at.plus(staleAfter).isBefore(now))
}

object ArchiveWatermarks {
    private final case class State(watermarks: Map[Cf, Long], lastReportAt: Option[Instant])

    /** What one [[ArchiveWatermarks.record]] did.
      *
      * `regressed` is separated from `advanced` rather than folded away because the two have very
      * different meanings to an operator: one is an archiver making progress, the other an archive
      * that has lost ground, which usually means it was rebuilt and now holds less than the node
      * assumed when it last deleted.
      */
    final case class Report(
        accepted: Map[Cf, Long],
        advanced: Map[Cf, Long],
        regressed: Map[Cf, Long]
    )

    def empty(): ArchiveWatermarks =
        new ArchiveWatermarks(new AtomicReference(State(Map.empty, None)))
}
