package hydrozoa.multisig.consensus.liaison

import cats.effect.IO

/** A full-duplex lane — a [[LaneOutbound]] (our production) paired with a [[LaneInbound]] (the
  * remote's), over the **same** lane number space. Only the symmetric head-peer mesh
  * ([[PeerLiaisonHeadToHead]]) produces *and* receives on a given lane; the asymmetric hub↔coil
  * links use a standalone [[LaneOutbound]] or [[LaneInbound]] per lane, so no lane carries a dead
  * direction.
  *
  * The two lanes hold independent state (our outbox + high-water vs the remote's cursor); this is
  * just the pairing, delegating each call to the relevant one.
  */
final class LaneBidirectional[T, N] private (
    out: LaneOutbound[T, N],
    in: LaneInbound[T, N]
) {
    // ---- Outbound ----
    def append(item: T): IO[Unit] = out.append(item)
    def reply(remoteCursor: N): IO[LaneOutbound.Reply[T]] = out.reply(remoteCursor)
    def outboxIsEmpty: IO[Boolean] = out.outboxIsEmpty

    // ---- Inbound ----
    def cursor: IO[N] = in.cursor
    def verify(items: List[T], current: N): Either[LaneInbound.Mismatch[N], N] =
        in.verify(items, current)
    def advanceTo(next: N): IO[Unit] = in.advanceTo(next)
}

object LaneBidirectional {

    /** A contiguous bidirectional lane: both directions start at `first`, successor `+1`. */
    def contiguous[T, N: Ordering](
        numberOf: T => N,
        first: N,
        incr: N => N,
        maxPerReply: Int = 1
    ): LaneBidirectional[T, N] =
        new LaneBidirectional[T, N](
          LaneOutbound.contiguous(numberOf, first, incr, maxPerReply),
          LaneInbound.contiguous(numberOf, first, incr)
        )

    /** A sparse bidirectional lane: outbound follows this side's leader schedule (`ownNext`),
      * inbound the remote's (`remoteNext`). `zero` is "before the first" for both.
      */
    def sparse[T, N: Ordering](
        numberOf: T => N,
        zero: N,
        ownNext: N => Option[N],
        remoteNext: N => Option[N]
    ): LaneBidirectional[T, N] =
        new LaneBidirectional[T, N](
          LaneOutbound.sparse(numberOf, zero, ownNext),
          LaneInbound.sparse(numberOf, zero, remoteNext)
        )
}
