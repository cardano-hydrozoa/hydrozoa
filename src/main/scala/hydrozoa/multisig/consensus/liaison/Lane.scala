package hydrozoa.multisig.consensus.liaison

import cats.effect.{IO, Ref}
import scala.collection.immutable.Queue

/** One **next-expected** lane of a peer-liaison link (§8 of `design/coil-network.md`).
  *
  * A link's batch protocol is a product of independent lanes; this is the reusable unit the three
  * liaison shapes compose (rather than inherit). It owns the state for a single lane in **both**
  * directions of one link:
  *
  *   - '''Outbound''' — an [[outbox]] queue of items we produce + the high-water number ever
  *     appended. The remote pulls with a cursor; we prune what it has already seen and hand back
  *     the head ([[reply]]).
  *   - '''Inbound''' — a [[inboundCursor]] naming the next number we expect from the remote. We
  *     [[verify]] a received slice against it (pure) and [[advanceTo]] its successor on accept.
  *
  * A lane is **author-agnostic**: a per-author lane family is a `Map[author, Lane[T, N]]`, so "this
  * ack is from peer P" is encoded by *which* lane it lives in, not by an in-lane check. The old fat
  * batch's `author == remote` checks are therefore gone — the keying replaces them.
  *
  * All lanes share the next-expected contract; they differ only in their successor functions:
  *   - '''Contiguous''' lanes (acks, requests, re-sequenced relay lanes): successor is `+1`.
  *   - '''Sparse''' lanes (block / stack briefs on the head mesh): successor is the leader
  *     schedule.
  *
  * @param extract
  *   the lane number of an item.
  * @param nextOutbound
  *   the next number this side may append, given the last appended (`None` = nothing yet). `None`
  *   result = no further item is legal on this lane. Drives the append gap-check and the
  *   out-of-bounds guard.
  * @param nextInbound
  *   the successor of the inbound cursor after receiving a given number (`None` = no successor; the
  *   cursor then stays put — a sparse lane with no further remote-led item).
  * @param initialCursor
  *   the first number we expect inbound (the remote's first wire-eligible item).
  * @param maxPerReply
  *   how many items a single [[reply]] may carry (1 for single-item lanes; the request lane
  *   batches).
  */
final class Lane[T, N] private (
    extract: T => N,
    nextOutbound: Option[N] => Option[N],
    nextInbound: N => Option[N],
    initialCursor: N,
    maxPerReply: Int
)(using ord: Ordering[N]) {
    import Lane.*
    import ord.mkOrderingOps

    private val lastAppended = Ref.unsafe[IO, Option[N]](None)
    private val outbox = Ref.unsafe[IO, Queue[T]](Queue.empty)
    private val inboundCursor = Ref.unsafe[IO, N](initialCursor)

    /** Append an item we produce to the outbox, enforcing gap-free monotonic numbering. Raises if
      * the item's number is not the expected next ([[nextOutbound]] of the last appended).
      */
    def append(item: T): IO[Unit] =
        lastAppended.get.flatMap { last =>
            val n = extract(item)
            val expected = nextOutbound(last)
            IO.raiseUnless(expected.contains(n))(
              Lane.AppendOutOfOrder(last.toString, n.toString, expected.toString)
            ) >> lastAppended.set(Some(n)) >> outbox.update(_ :+ item)
        }

    /** The next inbound number we expect from the remote (used to assemble the outgoing cursor). */
    def cursor: IO[N] = inboundCursor.get

    /** Serve the remote's pull: prune everything strictly below its cursor (retransmit-safe — the
      * head is kept until the remote moves past it) and return up to [[maxPerReply]] items from the
      * head. Returns [[OutOfBounds]] if the remote's cursor is ahead of what we could have produced
      * (protocol desync — the caller raises), else the (possibly empty) slice.
      */
    def reply(remoteCursor: N): IO[Reply[T]] =
        lastAppended.get.flatMap { last =>
            // The remote may never legitimately ask past our next-producible number.
            val bound = nextOutbound(last)
            if bound.exists(_ < remoteCursor) then IO.pure(OutOfBounds)
            else
                outbox.modify { q =>
                    val pruned = q.dropWhile(item => extract(item) < remoteCursor)
                    (pruned, Items(pruned.take(maxPerReply).toList))
                }
        }

    /** Verify a received inbound slice against the current cursor — **pure**, no mutation. The
      * first item must equal the cursor; any following items must be consecutive (each the
      * [[nextInbound]] of its predecessor). On success returns the cursor to advance to; on failure
      * the first offending `(expected, received)` pair. An empty slice trivially succeeds and
      * leaves the cursor unchanged.
      */
    def verify(items: List[T], current: N): Either[Mismatch[N], N] =
        items match {
            case Nil => Right(current)
            case head :: _ if extract(head) != current =>
                Left(Mismatch(current, extract(head)))
            case _ =>
                // Walk the slice: each successor must match nextInbound of its predecessor.
                val numbers = items.map(extract)
                val consecutive = numbers.sliding(2).collectFirst {
                    case Seq(a, b) if !nextInbound(a).contains(b) => Mismatch(a, b)
                }
                consecutive.toLeft(nextInbound(numbers.last).getOrElse(current))
        }

    /** Commit a verified cursor. Call only with the `Right` value from [[verify]]. */
    def advanceTo(next: N): IO[Unit] = inboundCursor.set(next)

    /** Whether the outbox currently holds nothing (for the link's empty-batch bookkeeping). */
    def outboxIsEmpty: IO[Boolean] = outbox.get.map(_.isEmpty)
}

object Lane {

    /** Result of [[Lane.reply]]: either the desync sentinel or the (possibly empty) slice to send.
      */
    enum Reply[+T]:
        case OutOfBounds
        case Items(items: List[T])
    export Reply.*

    /** A verify failure: the cursor we expected vs the number actually received. */
    final case class Mismatch[N](expected: N, received: N)

    final case class AppendOutOfOrder(last: String, attempted: String, expected: String)
        extends Throwable(
          s"append out of order: last=$last attempted=$attempted expected=$expected"
        )

    /** A contiguous lane whose first number is `first` and whose successor is `+1` (acks, requests,
      * re-sequenced relay lanes). `incr` supplies the `+1`.
      */
    def contiguous[T, N: Ordering](
        extract: T => N,
        first: N,
        incr: N => N,
        maxPerReply: Int = 1
    ): Lane[T, N] =
        new Lane[T, N](
          extract = extract,
          nextOutbound = _.fold(Some(first))(last => Some(incr(last))),
          nextInbound = n => Some(incr(n)),
          initialCursor = first,
          maxPerReply = maxPerReply
        )

    /** A sparse lane: only the round-robin leader emits, so the successor is the leader schedule.
      * `ownNext(after)` is this side's next-led number after `after`; `remoteNext` the remote's.
      * Both take the lane's zero as "before the first". `initialCursor = remoteNext(zero)`.
      */
    def sparse[T, N: Ordering](
        extract: T => N,
        zero: N,
        ownNext: N => Option[N],
        remoteNext: N => Option[N]
    ): Lane[T, N] =
        new Lane[T, N](
          extract = extract,
          nextOutbound = last => ownNext(last.getOrElse(zero)),
          nextInbound = remoteNext,
          initialCursor = remoteNext(zero).getOrElse(zero),
          maxPerReply = 1
        )
}
