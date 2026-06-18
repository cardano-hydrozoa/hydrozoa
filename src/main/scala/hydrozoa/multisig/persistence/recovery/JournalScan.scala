package hydrozoa.multisig.persistence.recovery

import cats.effect.IO
import cats.syntax.traverse.*
import hydrozoa.multisig.persistence.{BackendStore, JournalKey, JournalValue}

/** Range-scan one journal from a [[JournalKey]] cursor, yielding its tail as [[RawJournalEntry]]s
  * in within-journal index order (§5.3 derives the cursor; this reads from it).
  *
  * The underlying [[BackendStore.cursor]] iterates from the seek key to the end of the **column
  * journal**. With the per-author CF split (§3.2) each satellite CF (Request / SoftAck / HardAck)
  * holds exactly one author's journal, and the spines (Block / Stack) are head-global — so in both
  * cases the cursor's run to end-of-CF *is* the whole journal. The
  * `key.journalId != from.journalId` stop is a belt-and-braces journal boundary (every key in a
  * per-author CF shares one [[JournalId]]), not a multiplexed-peer split.
  *
  * Payloads are returned **encoded** (the framed `[stamp : 12][wire …]` bytes) — recovery ordering
  * needs only the stamp, and the typed payload is decoded later by the consumer with the key's own
  * codec. See `design/recovery-implementation-plan.md` R1.
  */
object JournalScan:

    /** Hot-load up to `limit` payloads from `from`'s journal, starting at `from`'s index
      * (inclusive), decoding each value with `decode` and keeping only those `keep` accepts. Stops
      * at the first `limit` kept payloads or end-of-journal, whichever comes first — a bounded read
      * (the early stop makes it cheap even on a long journal). Returns the kept payloads in
      * ascending index order.
      *
      * Used by the liaison outbound lanes ([[hydrozoa.multisig.consensus.liaison.LaneOutbound]]) to
      * serve a remote's `*.Get` from the store when the requested entry is below the in-memory
      * outbox floor — so a liaison reseeds only its live/replayed tail and reads the older prefix
      * on demand rather than eagerly loading its whole own production. `keep` filters a spine
      * journal to this peer's own-led entries (the satellites are already single-author per CF).
      */
    def loadFrom[T](
        backend: BackendStore[IO],
        from: JournalKey,
        decode: Array[Byte] => T,
        keep: T => Boolean,
        limit: Int
    ): IO[List[T]] =
        val cf = from.cf
        val journalId = from.journalId
        backend.cursor(cf, from.encode).use { cursor =>
            def loop(acc: List[T]): IO[List[T]] =
                if acc.length >= limit then IO.pure(acc.reverse)
                else
                    cursor.next.flatMap {
                        case None => IO.pure(acc.reverse)
                        case Some((keyBytes, valueBytes)) =>
                            val key = JournalKey.decode(cf, keyBytes)
                            if key.journalId != journalId then IO.pure(acc.reverse)
                            else
                                val item = decode(valueBytes)
                                loop(if keep(item) then item :: acc else acc)
                    }
            loop(Nil)
        }

    /** Scan `from`'s journal, starting at `from`'s index (inclusive) and running to the end of the
      * journal. Returns the entries in ascending index order.
      */
    def scan(backend: BackendStore[IO], from: JournalKey): IO[List[RawJournalEntry]] =
        val cf = from.cf
        val journalId = from.journalId
        backend.cursor(cf, from.encode).use { cursor =>
            def loop(acc: List[RawJournalEntry]): IO[List[RawJournalEntry]] =
                cursor.next.flatMap {
                    case None => IO.pure(acc.reverse)
                    case Some((keyBytes, valueBytes)) =>
                        val key = JournalKey.decode(cf, keyBytes)
                        if key.journalId != journalId then IO.pure(acc.reverse)
                        else
                            loop(
                              RawJournalEntry(
                                key,
                                JournalValue.stamp(valueBytes),
                                valueBytes
                              ) :: acc
                            )
                }
            loop(Nil)
        }

    /** Scan every journal named by `cursors` (the `2 + 3N + H` scan floors; §5.3 — the spines
      * collapse to their lower `confirmed` floor here, see [[ReplayCursors.scanFloors]]), returning
      * one entry list per journal. The caller flattens + merges these by arrival stamp
      * ([[ArrivalOrderedMerge]]), then (R3) slices each spine's `acked` consumer feed from the
      * merged tail.
      */
    def scanJournals(
        backend: BackendStore[IO],
        cursors: ReplayCursors
    ): IO[List[List[RawJournalEntry]]] =
        scanJournals(backend, cursors.scanFloors)

    /** Scan every journal named by `floors`, returning one entry list per journal — the
      * journal-agnostic core behind [[ReplayCursors.scanFloors]].
      */
    def scanJournals(
        backend: BackendStore[IO],
        floors: List[JournalKey]
    ): IO[List[List[RawJournalEntry]]] =
        floors.traverse(scan(backend, _))
