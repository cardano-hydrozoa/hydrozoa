package hydrozoa.multisig.persistence.recovery

import cats.effect.IO
import cats.syntax.traverse.*
import hydrozoa.multisig.persistence.{BackendStore, LaneKey, LaneValue}

/** Range-scan one lane from a [[LaneKey]] cursor, yielding its tail as [[RawLaneEntry]]s in
  * within-lane index order (§5.3 derives the cursor; this reads from it).
  *
  * The underlying [[BackendStore.cursor]] iterates the **whole column family** from the seek key to
  * the end — it does no prefix bounding. For the **satellite** lanes (Request / SoftAck / HardAck)
  * the CF multiplexes every author behind a `[peer : 1]` key prefix (§7.1), so a raw scan from
  * `(peer, n)` would run on into the next peer's entries. We therefore stop as soon as a decoded
  * key leaves the cursor's lane (`key.laneId != from.laneId`). The satellite CFs are **peer-major**
  * (the peer byte leads the key, so the store's unsigned-lex order groups each author's entries
  * into a contiguous run — see [[LaneKey]]), so that single mismatch is a definitive end-of-lane.
  * For the **spines** (Block / Stack) the whole CF *is* the lane, so the scan runs to the end.
  *
  * Payloads are returned **encoded** (the framed `[stamp : 12][wire …]` bytes) — recovery ordering
  * needs only the stamp, and the typed payload is decoded later by the consumer with the key's own
  * codec. See `design/recovery-implementation-plan.md` R1.
  */
object LaneScan:

    /** Scan `from`'s lane, starting at `from`'s index (inclusive) and running to the end of the
      * lane. Returns the entries in ascending index order.
      */
    def scan(backend: BackendStore[IO], from: LaneKey): IO[List[RawLaneEntry]] =
        val cf = from.cf
        val laneId = from.laneId
        backend.cursor(cf, from.encode).use { cursor =>
            def loop(acc: List[RawLaneEntry]): IO[List[RawLaneEntry]] =
                cursor.next.flatMap {
                    case None => IO.pure(acc.reverse)
                    case Some((keyBytes, valueBytes)) =>
                        val key = LaneKey.decode(cf, keyBytes)
                        if key.laneId != laneId then IO.pure(acc.reverse)
                        else loop(RawLaneEntry(key, LaneValue.stamp(valueBytes), valueBytes) :: acc)
                }
            loop(Nil)
        }

    /** Scan every lane named by `cursors` (the `2 + 3N` scan floors; §5.3 — the spines collapse to
      * their lower `confirmed` floor here, see [[ReplayCursors.scanFloors]]), returning one entry
      * list per lane. The caller flattens + merges these by arrival stamp
      * ([[ArrivalOrderedMerge]]), then (R3) slices each spine's `acked` consumer feed from the
      * merged tail.
      */
    def scanLanes(backend: BackendStore[IO], cursors: ReplayCursors): IO[List[List[RawLaneEntry]]] =
        cursors.scanFloors.traverse(scan(backend, _))
