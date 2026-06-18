package hydrozoa.multisig.persistence.recovery

import cats.effect.IO
import hydrozoa.multisig.persistence.{BackendStore, Cf}

/** The shared cursor-fold behind every recovery range-scan: open a cursor on `cf` from
  * `seekInclusive`, decode each `(key, value)` and accumulate to a `List` in ascending key order,
  * stopping at end-of-CF or as soon as `stop(key)` holds (e.g. a per-author family boundary).
  *
  * The loop is the only thing the scans share — each supplies its own key decode, entry decode, and
  * stop predicate. [[FamilyScan.scan]] decodes a [[hydrozoa.multisig.persistence.FamilyKey]] into a
  * stamped [[RawFamilyEntry]] and stops at the family boundary; [[BlockResultScan]] /
  * [[HardConfirmationScan]] decode a `StoreKey` into the typed value and run to end-of-CF (these
  * are single-author, non-family CFs — §3). Keeping this loop generic, rather than collapsing every
  * CF into one stamped family abstraction, preserves the family-vs-reconstruction split (see
  * [[hydrozoa.multisig.persistence.FamilyKey]]).
  */
object CursorScan:

    /** Walk `cf` from `seekInclusive` to end-of-CF (or until `stop(key)`), decoding each entry with
      * `decodeEntry`. Returns the entries in ascending key order.
      */
    def cursorWalk[K, V](
        backend: BackendStore[IO],
        cf: Cf,
        seekInclusive: Array[Byte],
        decodeKey: Array[Byte] => K,
        stop: K => Boolean = (_: K) => false
    )(decodeEntry: (K, Array[Byte]) => V): IO[List[V]] =
        backend.cursor(cf, seekInclusive).use { cursor =>
            def loop(acc: List[V]): IO[List[V]] =
                cursor.next.flatMap {
                    case None => IO.pure(acc.reverse)
                    case Some((keyBytes, valueBytes)) =>
                        val key = decodeKey(keyBytes)
                        if stop(key) then IO.pure(acc.reverse)
                        else loop(decodeEntry(key, valueBytes) :: acc)
                }
            loop(Nil)
        }
