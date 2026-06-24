package hydrozoa.multisig.consensus.liaison

import cats.effect.{IO, Ref}

/** An **inbound** next-expected lane (§4.2 of `design/coil-network.md`) [doc-ref]: a lane we only
  * receive on. Wraps an [[inboundCursor]] naming the next number we expect from the remote; we
  * [[verify]] a received slice against it (pure) and [[advanceTo]] its successor on accept. On a
  * link that produces *and* receives on this number space it is paired with a [[LaneOutbound]] (see
  * [[LaneBidirectional]]); on a receive-only link it stands alone.
  *
  * A lane is **author-agnostic**: a per-author lane family is a `Map[author, LaneInbound[T, N]]`,
  * so "this item is from peer P" is encoded by *which* lane it lives in, not by an in-lane check.
  *
  * Lanes differ only in their successor function:
  *   - '''Contiguous''' (acks, requests, re-sequenced relay lanes): successor is `+1`.
  *   - '''Sparse''' (block / stack briefs on the head mesh): successor is the remote's leader
  *     schedule.
  *
  * @tparam T
  *   the item type carried on the lane (a brief, ack, request, …).
  * @tparam N
  *   the item number — the value the lane sequences on (block / request / hard-ack number, …).
  * @param numberOf
  *   the number of an item.
  * @param next
  *   the successor of the inbound cursor after receiving a given number (`None` = no successor; the
  *   cursor then stays put — a sparse lane with no further remote-led item).
  * @param initialCursor
  *   the first number we expect inbound (the remote's first wire-eligible item).
  */
final class LaneInbound[T, N] private (
    numberOf: T => N,
    next: N => Option[N],
    initialCursor: N
) {
    import LaneInbound.*

    private val inboundCursor = Ref.unsafe[IO, N](initialCursor)

    /** The next inbound number we expect from the remote (used to assemble the outgoing cursor). */
    def cursor: IO[N] = inboundCursor.get

    /** Verify a received inbound slice against the current cursor — **pure**, no mutation. The
      * first item must equal the cursor; any following items must be consecutive (each the [[next]]
      * of its predecessor). On success returns the cursor to advance to; on failure the first
      * offending `(expected, received)` pair. An empty slice trivially succeeds and leaves the
      * cursor unchanged.
      */
    def verify(items: List[T], current: N): Either[Mismatch[N], N] =
        items match {
            case Nil => Right(current)
            case head :: _ if numberOf(head) != current =>
                Left(Mismatch(current, numberOf(head)))
            case _ =>
                // Walk the slice: each successor must match next of its predecessor.
                val numbers = items.map(numberOf)
                val consecutive = numbers.sliding(2).collectFirst {
                    case Seq(a, b) if !next(a).contains(b) => Mismatch(a, b)
                }
                consecutive.toLeft(next(numbers.last).getOrElse(current))
        }

    /** Commit a verified cursor. Call only with the `Right` value from [[verify]]. */
    def advanceTo(next: N): IO[Unit] = inboundCursor.set(next)

    /** Restore the receive cursor on recovery from the last durably-received number — the cursor
      * becomes its [[next]] (`max(persisted journal) + 1` for a contiguous lane; the remote's
      * next-led number for a sparse one), or the cold [[initialCursor]] when nothing was received.
      * So after a crash the remote is re-pulled only for **new** items, and [[verify]] rejects any
      * stale re-serve of what we already hold (which would otherwise be re-dispatched to the
      * consensus actors that `ReplayActor` already re-fed).
      */
    def restoreCursor(lastReceived: Option[N]): IO[Unit] =
        inboundCursor.set(lastReceived.flatMap(next).getOrElse(initialCursor))
}

object LaneInbound {

    /** A verify failure: the cursor we expected vs the number actually received. */
    final case class Mismatch[N](expected: N, received: N)

    /** A contiguous inbound lane whose first expected number is `first` and whose successor is
      * `+1`. `increment` supplies the `+1`.
      */
    def contiguous[T, N](
        numberOf: T => N,
        first: N,
        increment: N => N
    ): LaneInbound[T, N] =
        new LaneInbound[T, N](
          numberOf = numberOf,
          next = n => Some(increment(n)),
          initialCursor = first
        )

    /** A sparse inbound lane: only the round-robin leader emits, so the successor is the remote's
      * leader schedule. `next(after)` is the remote's next-led number after `after`; `zero` is
      * "before the first". `initialCursor = next(zero)`.
      */
    def sparse[T, N](
        numberOf: T => N,
        zero: N,
        next: N => Option[N]
    ): LaneInbound[T, N] =
        new LaneInbound[T, N](
          numberOf = numberOf,
          next = next,
          initialCursor = next(zero).getOrElse(zero)
        )
}
