package hydrozoa.multisig.persistence.recovery

import cats.effect.IO
import cats.syntax.traverse.*
import hydrozoa.multisig.persistence.{BackendStore, FamilyKey, FamilyValue}

/** Range-scan one family from a [[FamilyKey]] cursor, yielding its tail as [[RawFamilyEntry]]s in
  * within-family index order (§5.3 derives the cursor; this reads from it).
  *
  * The underlying [[BackendStore.cursor]] iterates the **whole column family** from the seek key to
  * the end — it does no prefix bounding. For the **satellite** families (Request / SoftAck /
  * HardAck) the CF multiplexes every author behind a `[peer : 1]` key prefix (§7.1), so a raw scan
  * from `(peer, n)` would run on into the next peer's entries. We therefore stop as soon as a
  * decoded key leaves the cursor's family (`key.familyId != from.familyId`). The satellite CFs are
  * **peer-major** (the peer byte leads the key, so the store's unsigned-lex order groups each
  * author's entries into a contiguous run — see [[FamilyKey]]), so that single mismatch is a
  * definitive end-of-family. For the **spines** (Block / Stack) the whole CF *is* the family, so
  * the scan runs to the end.
  *
  * Payloads are returned **encoded** (the framed `[stamp : 12][wire …]` bytes) — recovery ordering
  * needs only the stamp, and the typed payload is decoded later by the consumer with the key's own
  * codec. See `design/recovery-implementation-plan.md` R1.
  */
object FamilyScan:

    /** Scan `from`'s family, starting at `from`'s index (inclusive) and running to the end of the
      * family. Returns the entries in ascending index order.
      */
    def scan(backend: BackendStore[IO], from: FamilyKey): IO[List[RawFamilyEntry]] =
        val cf = from.cf
        val familyId = from.familyId
        backend.cursor(cf, from.encode).use { cursor =>
            def loop(acc: List[RawFamilyEntry]): IO[List[RawFamilyEntry]] =
                cursor.next.flatMap {
                    case None => IO.pure(acc.reverse)
                    case Some((keyBytes, valueBytes)) =>
                        val key = FamilyKey.decode(cf, keyBytes)
                        if key.familyId != familyId then IO.pure(acc.reverse)
                        else
                            loop(
                              RawFamilyEntry(key, FamilyValue.stamp(valueBytes), valueBytes) :: acc
                            )
                }
            loop(Nil)
        }

    /** Scan every family named by `cursors` (the `2 + 3N + H` scan floors; §5.3 — the spines
      * collapse to their lower `confirmed` floor here, see [[ReplayCursors.scanFloors]]), returning
      * one entry list per family. The caller flattens + merges these by arrival stamp
      * ([[ArrivalOrderedMerge]]), then (R3) slices each spine's `acked` consumer feed from the
      * merged tail.
      */
    def scanFamilies(
        backend: BackendStore[IO],
        cursors: ReplayCursors
    ): IO[List[List[RawFamilyEntry]]] =
        scanFamilies(backend, cursors.scanFloors)

    /** Scan every family named by `floors`, returning one entry list per family — the
      * family-agnostic core used by both the head [[ReplayCursors.scanFloors]] and the coil
      * [[CoilReplayCursors.scanFloors]].
      */
    def scanFamilies(
        backend: BackendStore[IO],
        floors: List[FamilyKey]
    ): IO[List[List[RawFamilyEntry]]] =
        floors.traverse(scan(backend, _))
