package hydrozoa.multisig.consensus.mempool

import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestId
import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

/** Simple immutable mempool. Duplicate ledger request IDs are NOT allowed; a duplicate add returns
  * `None`. Other components — particularly the peer liaison — keep the incoming stream of messages
  * consistent.
  *
  * Arrival order is indexed by a monotonic sequence number rather than a `Vector`, so removing an
  * arbitrary request (the follower replay path, one id at a time) is `O(log n)` instead of the
  * `O(n)` a `Vector.filterNot` would cost. `bySeq` iterates in arrival order; `seqOf` maps a
  * request id to its arrival slot for dedup and removal.
  *
  * @param bySeq
  *   requests keyed by arrival sequence number, so iteration is in arrival order
  * @param seqOf
  *   the arrival sequence number of each live request id
  * @param nextSeq
  *   the sequence number the next arrival will take (monotonic, never reused)
  */
final case class Mempool private (
    bySeq: TreeMap[Long, UserRequestWithId],
    seqOf: Map[RequestId, Long],
    nextSeq: Long
) {

    def isEmpty: Boolean = seqOf.isEmpty

    /** The number of live requests. */
    def size: Int = seqOf.size

    /** Add a request to the mempool.
      *
      * @param request
      *   a request to add
      * @return
      *   the updated mempool, or `None` if a request with the same id is already present
      */
    def addRequest(request: UserRequestWithId): Option[Mempool] = {
        val requestId = request.requestId
        Option.when(!seqOf.contains(requestId))(
          copy(
            bySeq = bySeq.updated(nextSeq, request),
            seqOf = seqOf.updated(requestId, nextSeq),
            nextSeq = nextSeq + 1
          )
        )
    }

    /** Retrieve a request from the mempool, by the request's ID.
      * @param requestId
      *   the request's ID
      */
    def getRequest(requestId: RequestId): Option[UserRequestWithId] =
        seqOf.get(requestId).map(bySeq)

    /** Given a list of request IDs, extract the corresponding requests from the mempool until a
      * request ID is encountered that is missing from the mempool.
      *
      * @param requestIds
      *   a list of request IDs
      * @return
      *   An extraction result, which is complete if all request IDs were found or incomplete if a
      *   request ID was missing from the mempool. If incomplete, the result indicates the first
      *   request ID encountered in the list that was missing.
      */
    def extractRequestsWhile(requestIds: IterableOnce[RequestId]): Mempool.Extraction.Result =
        extractRequestsWhile(Mempool.Extraction.start(this), requestIds.iterator)

    @tailrec
    private def extractRequestsWhile(
        acc: Mempool.Extraction,
        requestIds: Iterator[RequestId]
    ): Mempool.Extraction.Result = acc match {
        case result: Mempool.Extraction.Result => result
        case inProgress: Mempool.Extraction.InProgress =>
            import inProgress.*
            if !requestIds.hasNext then
                Mempool.Extraction.Complete(
                  extractedRequests,
                  survivingMempool
                )
            else {
                val requestId = requestIds.next()
                survivingMempool.extractRequest(requestId) match {
                    case None =>
                        Mempool.Extraction.Incomplete(
                          extractedRequests,
                          survivingMempool,
                          requestId
                        )
                    case Some((newSurvivingMempool, extractedRequest)) =>
                        val newAcc = Mempool.Extraction.InProgress(
                          extractedRequests :+ extractedRequest,
                          newSurvivingMempool
                        )
                        extractRequestsWhile(newAcc, requestIds)
                }
            }
    }

    private def extractRequest(requestId: RequestId): Option[(Mempool, UserRequestWithId)] =
        seqOf.get(requestId).map { seq =>
            val request = bySeq(seq)
            (copy(bySeq = bySeq - seq, seqOf = seqOf - requestId), request)
        }

    /** Extract up to `limit` requests for block production, preferring those authored by
      * `preferredPeer` so a leader's own users are not starved when the mempool is full (fairness).
      * Within each author the arrival (i.e. request-number) order is preserved — only the
      * cross-author interleaving changes — so the per-author stream ordering is unaffected.
      *
      * @return
      *   the chosen requests, in the order they should enter the block, and the surviving mempool
      *   with those requests removed.
      */
    def extractInOrderPreferring(
        preferredPeer: HeadPeerNumber,
        limit: Int
    ): (List[UserRequestWithId], Mempool) = {
        // `bySeq.toVector` is (seq, request) in arrival order; `partition` is stable, so each side
        // keeps that order and own requests lead.
        val (own, others) = bySeq.toVector.partition(_._2.requestId.peerNum == preferredPeer)
        val chosen = (own ++ others).take(limit)
        val survivingMempool = copy(
          bySeq = bySeq -- chosen.iterator.map(_._1),
          seqOf = seqOf -- chosen.iterator.map(_._2.requestId)
        )
        (chosen.map(_._2).toList, survivingMempool)
    }
}

object Mempool {
    val empty: Mempool = Mempool(TreeMap.empty, Map.empty, 0L)

    enum Extraction:
        def extractedRequests: List[UserRequestWithId]
        def survivingMempool: Mempool

        private[Mempool] case InProgress(
            extractedRequests: List[UserRequestWithId],
            survivingMempool: Mempool
        )

        case Incomplete(
            override val extractedRequests: List[UserRequestWithId],
            override val survivingMempool: Mempool,
            awaitingRequestId: RequestId
        )

        case Complete(
            override val extractedRequests: List[UserRequestWithId],
            override val survivingMempool: Mempool,
        )

    object Extraction {
        type Result = Extraction.Complete | Extraction.Incomplete

        private[Mempool] def start(mempool: Mempool): Extraction.InProgress =
            Extraction.InProgress(List(), mempool)
    }
}
