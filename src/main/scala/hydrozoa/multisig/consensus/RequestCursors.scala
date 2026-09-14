package hydrozoa.multisig.consensus

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}

/** How far each head peer's request stream has advanced, as [[BlockWeaver]] has seen it.
  *
  * Requests are monotonic per author. A peer numbers its own requests from [[RequestNumber.zero]],
  * consumes a number only for a request it then persists and sends ([[RequestSequencer]]), and the
  * `request` lane delivers them contiguously (`docs/spec/fast-consensus.md`). So every peer's
  * stream reaches the weaver as `0, 1, 2, …`: no gap, no reordering, no repeat. Recovery depends on
  * the same rule — each Request journal resumes at its author's high-water `+ 1`
  * (`docs/spec/persistence-and-crash-recovery.md` §5.3) — and a stream that skipped a number would
  * leave that cursor pointing at a request no block ever included.
  *
  * This is the gate that states the rule instead of assuming it: the weaver consumes the requests,
  * so it is where the invariant is checked (a liaison is pure transport and checks nothing).
  *
  * @param highWater
  *   the highest request number accepted from each head peer; absent until that peer's first
  *   request arrives
  */
final case class RequestCursors private (highWater: Map[HeadPeerNumber, RequestNumber]) {

    /** The request number `peerNum`'s next request must carry. */
    def nextExpected(peerNum: HeadPeerNumber): RequestNumber =
        highWater.get(peerNum).fold(RequestNumber.zero)(_.increment)

    /** Advance `requestId`'s author by one, or name the break in its stream.
      *
      * One comparison covers all three failures: a gap (a number was skipped), a reordering (an
      * earlier number arrives after a later one) and a repeat (the same number twice) each show up
      * as a request number that is not the expected next one.
      */
    def accept(requestId: RequestId): Either[String, RequestCursors] = {
        val peerNum = requestId.peerNum
        val expected = nextExpected(peerNum)
        Either.cond(
          requestId.requestNum == expected,
          copy(highWater = highWater.updated(peerNum, requestId.requestNum)),
          s"Request stream of head peer $peerNum is broken: expected request number" +
              s" ${expected: Long}, got ${requestId.requestNum: Long}"
        )
    }
}

object RequestCursors {

    /** A cold store: every peer's stream is expected to open at [[RequestNumber.zero]]. */
    val cold: RequestCursors = RequestCursors(Map.empty)

    /** Resume from the per-peer high-water persisted at the fast anchor
      * ([[hydrozoa.multisig.persistence.StoreKey.RequestHighWater]]) — the same value `ReplayActor`
      * floors each Request journal with, so the first entry replay feeds for a peer is exactly the
      * one this gate expects next.
      */
    def resume(highWater: Map[HeadPeerNumber, RequestNumber]): RequestCursors =
        RequestCursors(highWater)
}
