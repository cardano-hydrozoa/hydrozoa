package hydrozoa.multisig.consensus.liaison

import cats.effect.IO

// TODO: T and N might be restricted to pull in things like numberOf, increment and so on.

/** A full-duplex lane — a [[LaneOutbound]] (our production) paired with a [[LaneInbound]] (the
  * remote's), numbered the **same** way (both key on the item number `N`). Only the symmetric
  * head-peer mesh ([[PeerLiaisonHeadToHead]]) produces *and* receives on a given lane; the
  * asymmetric hub↔coil links use a standalone [[LaneOutbound]] or [[LaneInbound]] per lane, so no
  * lane carries a dead direction.
  *
  * The two lanes hold independent state (our outbox + high-water vs the remote's cursor); this is
  * just the pairing, delegating each call to the relevant one.
  *
  * @tparam T
  *   the item type carried on the lane (a brief, ack, request, …).
  * @tparam N
  *   the item number — the value the lane sequences on (block / request / hard-ack number, …).
  */
final class LaneBidirectional[T, N] private (
    out: LaneOutbound[T, N],
    in: LaneInbound[T, N]
) {
    // ---- Outbound ----
    def append(item: T): IO[Unit] = out.append(item)
    def reply(remoteCursor: N): IO[LaneOutbound.Reply[T]] = out.reply(remoteCursor)
    def outboxIsEmpty: IO[Boolean] = out.outboxIsEmpty
    def seedHighWaterOutbox(highWater: Option[N]): IO[Unit] = out.seedHighWater(highWater)

    // ---- Inbound ----
    def cursor: IO[N] = in.cursor
    def verify(items: List[T], current: N): Either[LaneInbound.Mismatch[N], N] =
        in.verify(items, current)
    def advanceTo(next: N): IO[Unit] = in.advanceTo(next)
    def restoreInbound(lastReceived: Option[N]): IO[Unit] = in.restoreCursor(lastReceived)
}

object LaneBidirectional {

    /** A contiguous bidirectional lane: both directions start at `first`, successor `+1`.
      * `backfill` reads the outbound prefix below the in-memory outbox floor from the store on a
      * reply.
      */
    def contiguous[T, N: Ordering](
        numberOf: T => N,
        first: N,
        increment: N => N,
        maxPerReply: Int = 1,
        backfill: (N, Int) => IO[List[T]]
    ): LaneBidirectional[T, N] =
        new LaneBidirectional[T, N](
          LaneOutbound.contiguous(numberOf, first, increment, maxPerReply, backfill),
          LaneInbound.contiguous(numberOf, first, increment)
        )

    /** A sparse bidirectional lane: outbound follows this side's leader schedule (`outboundNext`),
      * inbound the remote's (`inboundNext`). `zero` is "before the first" for both. `backfill`
      * reads the outbound prefix below the in-memory outbox floor from the store on a reply.
      */
    def sparse[T, N: Ordering](
        numberOf: T => N,
        zero: N,
        outboundNext: N => Option[N],
        inboundNext: N => Option[N],
        backfill: (N, Int) => IO[List[T]]
    ): LaneBidirectional[T, N] =
        new LaneBidirectional[T, N](
          LaneOutbound.sparse(numberOf, zero, outboundNext, backfill),
          LaneInbound.sparse(numberOf, zero, inboundNext)
        )
}
