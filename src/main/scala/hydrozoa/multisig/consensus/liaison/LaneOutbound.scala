package hydrozoa.multisig.consensus.liaison

import cats.effect.{IO, Ref}
import cats.syntax.foldable.*
import scala.collection.immutable.TreeMap

/** An **outbound** next-expected lane (§4.2 of `docs/spec/coil-network.md`) [doc-ref]: a lane we
  * only produce on. It owns a bounded [[outbox]] of items we append plus the high-water number ever
  * appended; the remote pulls with its [[LaneInbound]] counterpart's cursor, we prune what it has
  * already seen, and we hand back the head ([[reply]]). On a link that produces *and* receives on
  * this number space it is paired with that [[LaneInbound]] (see [[LaneBidirectional]]); on a
  * produce-only link it stands alone.
  *
  * ==The outbox is a cache, not a buffer==
  *
  * Everything that reaches this lane is already durable — it is persisted before it is appended
  * (CR4 write-before-send, §4 of `docs/spec/persistence-and-crash-recovery.md`) — so the journal,
  * not the outbox, is what makes a pull servable. The outbox only saves a store read for a remote
  * that is roughly current, and it is capped at [[capacity]]: appending past the cap evicts the
  * oldest entry, and a pull below the remaining floor is served from the journal instead
  * ([[serveFromJournal]]). Recovery already runs in exactly that state — [[seedHighWater]] restores
  * the high-water and leaves the outbox empty, so every pull after a restart is a journal serve
  * until live production refills the window.
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
  * @param outboxCap
  *   how many items to keep in memory; see [[capacity]] for the floor applied to it.
  * @param serveFromJournal
  *   read up to `limit` durable items from a number, for a pull below the outbox floor.
  */
final class LaneOutbound[T, N] private (
    numberOf: T => N,
    next: Option[N] => Option[N],
    maxPerReply: Int,
    outboxCap: Int,
    serveFromJournal: (N, Int) => IO[List[T]]
)(using ord: Ordering[N]) {
    import LaneOutbound.*
    import ord.mkOrderingOps

    /** The outbox's item cap: `outboxCap`, floored at [[maxPerReply]].
      *
      * The floor is what keeps the outbox worth having. A reply may carry `maxPerReply` items, so a
      * cap below that could not serve even a perfectly current remote from memory — every pull on
      * the request lane (`maxPerReply` = `peerLiaisonMaxRequestsPerBatch`) would go to the store.
      */
    private val capacity: Int = math.max(outboxCap, maxPerReply)

    private val lastAppended = Ref.unsafe[IO, Option[N]](None)
    private val outbox = Ref.unsafe[IO, TreeMap[N, T]](TreeMap.empty)

    /** The highest number [[append]] has evicted, or `None` if nothing has been evicted yet.
      *
      * Held only so eviction can fail loudly. An evicted item was appended, and everything appended
      * is durable (CR4), so the journal owes us every number at or below this mark; a pull there
      * that the journal cannot answer is a broken backing, not an empty lane. Without the mark that
      * mistake reads as "nothing new to send" and the remote's lane stalls in silence.
      */
    private val evictedThrough = Ref.unsafe[IO, Option[N]](None)

    /** Append an item we produce to the outbox, enforcing gap-free monotonic numbering for *new*
      * items. An item whose number is **at or below the high-water is a no-op**: it is something we
      * already produced and persisted (everything that reaches a lane is durable, CR4), so the lane
      * serves it from the journal on demand ([[reply]]) and need not re-hold it. This absorbs a
      * replay re-broadcast — e.g. `SlowConsensusActor` re-emitting the in-flight stack's round-1
      * ack (number `n1`) after the lane has restored its high-water to round-2 (`n1 + 1`) — which
      * would otherwise be a spurious out-of-order error. A *new* item (above the high-water) must
      * still be exactly the expected next ([[next]] of the last appended); a gap raises.
      *
      * Appending past [[capacity]] evicts the oldest held item. Eviction costs nothing but a store
      * read later: the evicted item is durable by CR4, so it stays servable via
      * [[serveFromJournal]].
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
                    ) >> lastAppended.set(Some(n)) >> outbox
                        .modify { held =>
                            val grown = held.updated(n, item)
                            val excess = grown.size - capacity
                            if excess <= 0 then (grown, None)
                            else (grown.drop(excess), grown.take(excess).lastOption.map(_._1))
                        }
                        .flatMap(
                          _.traverse_(highest =>
                              evictedThrough.update(
                                _.fold(Some(highest))(prev => Some(ord.max(prev, highest)))
                              )
                          )
                        )
        }

    /** Restore only the lane's high-water on recovery, leaving the outbox empty: the lane serves
      * the older prefix from the store via [[serveFromJournal]] and lets replay / live production
      * re-append the tail on top. This is the gap-free baseline an [[append]] continues from (the
      * first post-boot append must be `next(high-water)`), and the [[reply]] out-of-bounds guard.
      * `None` is the cold state.
      */
    def seedHighWater(highWater: Option[N]): IO[Unit] =
        lastAppended.set(highWater) >> outbox.set(TreeMap.empty) >> evictedThrough.set(None)

    /** Serve the remote's pull. Prune everything strictly below its cursor — retransmit-safe: the
      * requested entry stays *servable* until the remote moves past it, from memory if it is still
      * within the outbox window and from the journal otherwise. If the outbox holds the requested
      * number, serve up to [[maxPerReply]] items from there; otherwise it is below the outbox floor
      * (evicted, restored-but-not-re-appended, or the remote regressed across a restart) and the
      * journal serves it — **capped at the released high-water `lastAppended`**: the store may hold
      * entries persisted but not yet released onto this lane (a postponed own soft-ack), and
      * serving those would push the remote's cursor past our bound. Returns [[OutOfBounds]] if the
      * remote's cursor is ahead of what we could have produced (protocol desync — the caller
      * raises), else the (possibly empty) slice.
      */
    def reply(remoteCursor: N, ceiling: Option[N] = None): IO[Reply[T]] =
        // `ceiling`, when set, is an absolute upper bound on the item numbers we may serve this reply
        // (on top of `maxPerReply` and the released high-water) — the request lane passes the
        // puller's backpressure ceiling. Items are monotonic ascending, so a `takeWhile`/`filter`
        // keeps the contiguous prefix at or below it.
        def withinCeiling(items: List[T]): List[T] =
            ceiling.fold(items)(c => items.takeWhile(item => numberOf(item) <= c))
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
                last match
                    // Nothing released on this lane yet, so nothing is servable whatever the store
                    // holds — and this is the steady state of a lane the peer never produces on
                    // (a non-hub's `HubHardAck` lane). Answer without touching the store: the
                    // journal read below would be discarded by the high-water filter anyway.
                    case None => IO.pure(Items(Nil))
                    case Some(highWater) =>
                        outbox
                            .modify { held =>
                                val kept = held.rangeFrom(remoteCursor)
                                (kept, kept)
                            }
                            .flatMap { kept =>
                                if kept.contains(remoteCursor) then
                                    IO.pure(
                                      Items(withinCeiling(kept.values.take(maxPerReply).toList))
                                    )
                                else
                                    // The store can hold entries already persisted but **not yet
                                    // released** onto this lane (e.g. a postponed own soft-ack that
                                    // the producer persisted at block time but `announceAck`s only
                                    // when the previous block's cell completes). Serve only up to
                                    // the announced high-water — never past `lastAppended` — or the
                                    // remote's cursor would run ahead of our bound and the next pull
                                    // would be a false out-of-bounds.
                                    for {
                                        fromStore <- serveFromJournal(remoteCursor, maxPerReply)
                                        evicted <- evictedThrough.get
                                        // We held this number and dropped it; CR4 says the journal
                                        // has it. Nothing coming back means the backing is wrong
                                        // (missing CF, over-strict `keep`), which would otherwise
                                        // present as a permanently idle remote.
                                        _ <- IO.raiseWhen(
                                          fromStore.isEmpty &&
                                              evicted.exists(mark => remoteCursor <= mark)
                                        )(
                                          EvictedButUnservable(
                                            asked = remoteCursor.toString,
                                            evictedThrough = evicted.fold("none")(_.toString),
                                            lastAppended = highWater.toString
                                          )
                                        )
                                    } yield Items(
                                      withinCeiling(
                                        fromStore.filter(item => numberOf(item) <= highWater)
                                      )
                                    )
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
        extends RuntimeException(
          s"append out of order: last=$last attempted=$attempted expected=$expected"
        )

    /** The outbox evicted an entry the journal then could not serve. Everything appended to a lane
      * is persisted first (CR4), so this means the lane's backing is wrong — not that there is
      * nothing to send. Raised rather than answered with an empty batch, which would leave the
      * remote's lane stalled with nothing in the log to say why.
      */
    final case class EvictedButUnservable(
        asked: String,
        evictedThrough: String,
        lastAppended: String
    ) extends RuntimeException(
          s"outbox evicted through $evictedThrough but the journal served nothing from $asked " +
              s"(lastAppended=$lastAppended)"
        )

    /** A contiguous outbound lane whose first number is `first` and whose successor is `+1` (acks,
      * requests, re-sequenced relay lanes). `increment` supplies the `+1`; `serveFromJournal` reads
      * the entries below the in-memory outbox floor from the store on [[reply]].
      */
    def contiguous[T, N: Ordering](
        numberOf: T => N,
        first: N,
        increment: N => N,
        maxPerReply: Int = 1,
        outboxCap: Int,
        serveFromJournal: (N, Int) => IO[List[T]]
    ): LaneOutbound[T, N] =
        new LaneOutbound[T, N](
          numberOf = numberOf,
          next = _.fold(Some(first))(last => Some(increment(last))),
          maxPerReply = maxPerReply,
          outboxCap = outboxCap,
          serveFromJournal = serveFromJournal
        )

    /** A sparse outbound lane: only the round-robin leader emits, so the successor is this side's
      * leader schedule. `next(after)` is this side's next-led number after `after`; `zero` is
      * "before the first". `serveFromJournal` reads the entries below the in-memory outbox floor
      * from the store on [[reply]].
      */
    def sparse[T, N: Ordering](
        numberOf: T => N,
        zero: N,
        next: N => Option[N],
        outboxCap: Int,
        serveFromJournal: (N, Int) => IO[List[T]]
    ): LaneOutbound[T, N] =
        new LaneOutbound[T, N](
          numberOf = numberOf,
          next = last => next(last.getOrElse(zero)),
          maxPerReply = 1,
          outboxCap = outboxCap,
          serveFromJournal = serveFromJournal
        )
}
