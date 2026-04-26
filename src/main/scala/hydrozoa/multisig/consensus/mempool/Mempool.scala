package hydrozoa.multisig.consensus.mempool

import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.ledger.event.RequestId
import scala.annotation.tailrec

/** Simple immutable mempool implementation. Duplicate ledger request IDs are NOT allowed and a
  * runtime exception is thrown since this should never happen. Other components, particularly the
  * peer liaison is in charge or maintaining the integrity of the stream of messages.
  *
  * @param requests
  *   map to store requests
  * @param arrivalOrder
  *   vector to store order of request ids
  */
final case class Mempool(
    requests: Map[RequestId, UserRequestWithId] = Map.empty,
    arrivalOrder: Vector[RequestId] = Vector.empty
) {

    private val logger = Logging.logger("Mempool")

    def isEmpty: Boolean = requests.isEmpty

    /** Add a request to the mempool
      *
      * @param request
      *   a request to add
      * @return
      *   an updated mempool
      * @throws IllegalArgumentException
      *   if a duplicate is detected
      */
    def addRequest(
        request: UserRequestWithId
    ): Option[Mempool] = {
        val requestId = request.requestId

        Option.when(!requests.contains(requestId))(
          copy(
            requests = requests + (requestId -> request),
            arrivalOrder = arrivalOrder :+ requestId
          )
        )
    }

    /** Retrieve a request from the mempool, by the request's ID.
      * @param requestId
      *   the request's ID
      */
    def getRequest(requestId: RequestId): Option[UserRequestWithId] = requests.get(requestId)

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

    private def extractRequest(requestId: RequestId): Option[(Mempool, UserRequestWithId)] = {
        val mRequestWithId = requests.get(requestId)
        mRequestWithId.map(request => {
            val newMempool = copy(
              requests = requests - requestId,
              arrivalOrder = arrivalOrder.filterNot(_ == requestId)
            )
            (newMempool, request)
        })
    }

    /** Retrieve all mempool requests by order of arrival.
      * @throws RuntimeException
      *   If the mempool's [[arrivalOrder]] vector contains a request ID that is missing from its
      *   [[requests]] map, violating the mempool's invariant property.
      */
    def extractRequestsInOrder: List[UserRequestWithId] =
        arrivalOrder.iterator
            .map(requestId => {
                requests.getOrElse(
                  requestId, {
                      val msg =
                          s"Panic: the mempool's `arrivalOrder` vector has a request ID (${requestId.asI64}) that" +
                              " is missing from the mempool's `requests` map."
                      logger.error(msg)
                      throw RuntimeException(msg)
                  }
                )
            })
            .toList
}

object Mempool {
    val empty: Mempool = Mempool()

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
