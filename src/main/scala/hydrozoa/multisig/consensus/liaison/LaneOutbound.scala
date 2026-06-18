package hydrozoa.multisig.consensus.liaison

import cats.effect.{IO, Ref}
import scala.collection.immutable.Queue

/** An **outbound** next-expected lane (§4.2 of `design/coil-network.md`) [doc-ref]: a lane we only
  * produce on. It owns an [[outbox]] queue of items we append plus the high-water number ever
  * appended; the remote pulls with its [[LaneInbound]] counterpart's cursor, we prune what it has
  * already seen, and we hand back the head ([[reply]]). On a link that produces *and* receives on
  * this number space it is paired with that [[LaneInbound]] (see [[LaneBidirectional]]); on a
  * produce-only link it stands alone.
  *
  * A lane is **author-agnostic**: a per-author lane family is a `Map[author, LaneOutbound[T, N]]`,
  * so "this item is from peer P" is encoded by *which* lane it lives in, not by an in-lane check.
  *
  * Lanes differ only in their successor function:
  *   - '''Contiguous''' (acks, requests, re-sequenced relay lanes): successor is `+1`.
  *   - '''Sparse''' (block / stack briefs on the head mesh): successor is this side's leader
  *     schedule.
  *
  * @tparam T
  *   the item type carried on the lane (a brief, ack, request, …).
  * @tparam N
  *   the item number — the value the lane sequences on (block / request / hard-ack number, …).
  * @param numberOf
  *   the number of an item.
  * @param next
  *   the next number this side may append, given the last appended (`None` = nothing yet). A `None`
  *   result means no further item is legal on this lane. Drives the append gap-check and the
  *   out-of-bounds guard.
  * @param maxPerReply
  *   how many items a single [[reply]] may carry (1 for single-item lanes; the request lane
  *   batches).
  */
final class LaneOutbound[T, N] private (
    numberOf: T => N,
    next: Option[N] => Option[N],
    maxPerReply: Int,
    backfill: (N, Int) => IO[List[T]]
)(using ord: Ordering[N]) {
    import LaneOutbound.*
    import ord.mkOrderingOps

    private val lastAppended = Ref.unsafe[IO, Option[N]](None)
    private val outbox = Ref.unsafe[IO, Queue[T]](Queue.empty)

    /** Append an item we produce to the outbox, enforcing gap-free monotonic numbering for *new*
      * items. An item whose number is **at or below the high-water is a no-op**: it is something we
      * already produced and persisted (everything that reaches a lane is durable, CR4), so the lane
      * serves it from the journal on demand ([[reply]]'s hot-load) and need not re-hold it. This
      * absorbs a replay re-broadcast — e.g. `SlowConsensusActor` re-emitting the in-flight stack's
      * round-1 ack (number `n1`) after the lane has restored its high-water to round-2 (`n1 + 1`) —
      * which would otherwise be a spurious out-of-order error. A *new* item (above the high-water)
      * must still be exactly the expected next ([[next]] of the last appended); a gap raises.
      */
    def append(item: T): IO[Unit] =
        lastAppended.get.flatMap { last =>
            val n = numberOf(item)
            last match
                case Some(hw) if n <= hw => IO.unit
                case _ =>
                    val expected = next(last)
                    IO.raiseUnless(expected.contains(n))(
                      AppendOutOfOrder(last.toString, n.toString, expected.toString)
                    ) >> lastAppended.set(Some(n)) >> outbox.update(_ :+ item)
        }

    /** Restore only the lane's high-water on recovery, leaving the outbox empty: the lane serves
      * the older prefix from the store via [[backfill]] and lets replay / live production re-append
      * the tail on top. This is the gap-free baseline an [[append]] continues from (the first
      * post-boot append must be `next(high-water)`), and the [[reply]] out-of-bounds guard. `None`
      * is the cold state.
      */
    def seedHighWater(highWater: Option[N]): IO[Unit] =
        lastAppended.set(highWater) >> outbox.set(Queue.empty)

    /** Serve the remote's pull. Prune everything strictly below its cursor (retransmit-safe — the
      * head is kept until the remote moves past it). If the in-memory outbox head *is* the
      * requested number, serve up to [[maxPerReply]] items from it (the live / replayed tail,
      * including not-yet-persisted entries); otherwise the requested entry is below the outbox
      * floor (older, already durable, or the remote regressed across a restart), so hot-load it
      * from the store via [[backfill]]. Returns [[OutOfBounds]] if the remote's cursor is ahead of
      * what we could have produced (protocol desync — the caller raises), else the (possibly empty)
      * slice.
      */
    def reply(remoteCursor: N): IO[Reply[T]] =
        lastAppended.get.flatMap { last =>
            // The remote may never legitimately ask past our next-producible number.
            val bound = next(last)
            if bound.exists(_ < remoteCursor) then
                IO.pure(
                  OutOfBounds(
                    asked = remoteCursor.toString,
                    bound = bound.fold("none")(_.toString),
                    lastAppended = last.fold("none")(_.toString)
                  )
                )
            else
                outbox
                    .modify { q =>
                        val pruned = q.dropWhile(item => numberOf(item) < remoteCursor)
                        (pruned, pruned)
                    }
                    .flatMap { pruned =>
                        pruned.headOption match
                            case Some(head) if numberOf(head) == remoteCursor =>
                                IO.pure(Items(pruned.take(maxPerReply).toList))
                            case _ =>
                                backfill(remoteCursor, maxPerReply).map(Items(_))
                    }
        }

    /** Whether the outbox currently holds nothing (for the link's empty-batch bookkeeping). */
    def outboxIsEmpty: IO[Boolean] = outbox.get.map(_.isEmpty)
}

object LaneOutbound {

    /** Result of [[LaneOutbound.reply]]: the desync sentinel or the (possibly empty) slice to send.
      */
    enum Reply[+T]:
        /** The remote's cursor is ahead of our next-producible number — protocol desync. Carries
          * the diagnostic indices (stringified — a lane is generic over its number type `N`): what
          * the remote `asked` for, our `bound` (`next(lastAppended)`), and `lastAppended` itself.
          */
        case OutOfBounds(asked: String, bound: String, lastAppended: String)
        case Items(items: List[T])
    export Reply.*

    /** The first out-of-bounds reply among the given `labeled` lanes, rendered as a diagnostic
      * string naming the lane and its asked/bound indices — the `detail` a `Server` reports on a
      * [[Server.Served.OutOfBounds]]. `None` if no lane is out of bounds.
      */
    def firstOutOfBounds(labeled: (String, Reply[?])*): Option[String] =
        labeled.collectFirst { case (name, OutOfBounds(asked, bound, lastAppended)) =>
            s"lane '$name' (asked=$asked bound=$bound lastAppended=$lastAppended)"
        }

    final case class AppendOutOfOrder(last: String, attempted: String, expected: String)
        extends Throwable(
          s"append out of order: last=$last attempted=$attempted expected=$expected"
        )

    /** A contiguous outbound lane whose first number is `first` and whose successor is `+1` (acks,
      * requests, re-sequenced relay lanes). `increment` supplies the `+1`; `backfill` reads the
      * prefix below the in-memory outbox floor from the store on [[reply]].
      */
    def contiguous[T, N: Ordering](
        numberOf: T => N,
        first: N,
        increment: N => N,
        maxPerReply: Int = 1,
        backfill: (N, Int) => IO[List[T]]
    ): LaneOutbound[T, N] =
        new LaneOutbound[T, N](
          numberOf = numberOf,
          next = _.fold(Some(first))(last => Some(increment(last))),
          maxPerReply = maxPerReply,
          backfill = backfill
        )

    /** A sparse outbound lane: only the round-robin leader emits, so the successor is this side's
      * leader schedule. `next(after)` is this side's next-led number after `after`; `zero` is
      * "before the first". `backfill` reads the prefix below the in-memory outbox floor from the
      * store on [[reply]].
      */
    def sparse[T, N: Ordering](
        numberOf: T => N,
        zero: N,
        next: N => Option[N],
        backfill: (N, Int) => IO[List[T]]
    ): LaneOutbound[T, N] =
        new LaneOutbound[T, N](
          numberOf = numberOf,
          next = last => next(last.getOrElse(zero)),
          maxPerReply = 1,
          backfill = backfill
        )
}
