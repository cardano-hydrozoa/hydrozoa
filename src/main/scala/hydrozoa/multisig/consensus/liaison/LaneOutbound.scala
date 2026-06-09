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
    maxPerReply: Int
)(using ord: Ordering[N]) {
    import LaneOutbound.*
    import ord.mkOrderingOps

    private val lastAppended = Ref.unsafe[IO, Option[N]](None)
    private val outbox = Ref.unsafe[IO, Queue[T]](Queue.empty)

    /** Append an item we produce to the outbox, enforcing gap-free monotonic numbering. Raises if
      * the item's number is not the expected next ([[next]] of the last appended).
      */
    def append(item: T): IO[Unit] =
        lastAppended.get.flatMap { last =>
            val n = numberOf(item)
            val expected = next(last)
            IO.raiseUnless(expected.contains(n))(
              AppendOutOfOrder(last.toString, n.toString, expected.toString)
            ) >> lastAppended.set(Some(n)) >> outbox.update(_ :+ item)
        }

    /** Serve the remote's pull: prune everything strictly below its cursor (retransmit-safe — the
      * head is kept until the remote moves past it) and return up to [[maxPerReply]] items from the
      * head. Returns [[OutOfBounds]] if the remote's cursor is ahead of what we could have produced
      * (protocol desync — the caller raises), else the (possibly empty) slice.
      */
    def reply(remoteCursor: N): IO[Reply[T]] =
        lastAppended.get.flatMap { last =>
            // The remote may never legitimately ask past our next-producible number.
            val bound = next(last)
            if bound.exists(_ < remoteCursor) then IO.pure(OutOfBounds)
            else
                outbox.modify { q =>
                    val pruned = q.dropWhile(item => numberOf(item) < remoteCursor)
                    (pruned, Items(pruned.take(maxPerReply).toList))
                }
        }

    /** Whether the outbox currently holds nothing (for the link's empty-batch bookkeeping). */
    def outboxIsEmpty: IO[Boolean] = outbox.get.map(_.isEmpty)
}

object LaneOutbound {

    /** Result of [[LaneOutbound.reply]]: the desync sentinel or the (possibly empty) slice to send.
      */
    enum Reply[+T]:
        case OutOfBounds
        case Items(items: List[T])
    export Reply.*

    final case class AppendOutOfOrder(last: String, attempted: String, expected: String)
        extends Throwable(
          s"append out of order: last=$last attempted=$attempted expected=$expected"
        )

    /** A contiguous outbound lane whose first number is `first` and whose successor is `+1` (acks,
      * requests, re-sequenced relay lanes). `increment` supplies the `+1`.
      */
    def contiguous[T, N: Ordering](
        numberOf: T => N,
        first: N,
        increment: N => N,
        maxPerReply: Int = 1
    ): LaneOutbound[T, N] =
        new LaneOutbound[T, N](
          numberOf = numberOf,
          next = _.fold(Some(first))(last => Some(increment(last))),
          maxPerReply = maxPerReply
        )

    /** A sparse outbound lane: only the round-robin leader emits, so the successor is this side's
      * leader schedule. `next(after)` is this side's next-led number after `after`; `zero` is
      * "before the first".
      */
    def sparse[T, N: Ordering](
        numberOf: T => N,
        zero: N,
        next: N => Option[N]
    ): LaneOutbound[T, N] =
        new LaneOutbound[T, N](
          numberOf = numberOf,
          next = last => next(last.getOrElse(zero)),
          maxPerReply = 1
        )
}
