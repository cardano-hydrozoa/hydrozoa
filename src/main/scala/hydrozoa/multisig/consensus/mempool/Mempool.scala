package hydrozoa.multisig.consensus.mempool

import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.ledger.event.RequestId

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

    /** Retrieve a request from the mempool, by the request's ID.
     * @param requestId
     *   the request's ID
     */
    def getRequest(requestId: RequestId): Option[UserRequestWithId] = requests.get(requestId)

    /** Add a request to the mempool
      *
      * @param request
      *   a request to add
      * @return
      *   an updated mempool
      * @throws IllegalArgumentException
      *   if a duplicate is detected
      */
    def unsafeAddRequest(
        request: UserRequestWithId
    ): Mempool = {

        val requestId = request.requestId

        require(
          !requests.contains(requestId), {
              val msg =
                  s"Panic: attempting to add a duplicate request ID (${requestId.asI64}) into the mempool: $requestId"
              logger.error(msg)
              msg
          }
        )

        copy(
          requests = requests + (requestId -> request),
          arrivalOrder = arrivalOrder :+ requestId
        )
    }

    /** Retrieve all mempool requests by order of arrival.
     * @throws RuntimeException
     *   if the mempool's [[arrivalOrder]] vector contains a request ID that is missing from its
     *   [[requests]] map.
     */
    private def unsafeGetRequestsInOrder: IterableOnce[UserRequestWithId] =
        arrivalOrder.iterator.map(requestId => {
            requests.getOrElse(
                requestId, {
                    val msg = s"Panic: the mempool's `arrivalOrder` vector has a request ID (${requestId.asI64}) that" +
                        s" is missing from the mempool's `requests` map."
                    logger.error(msg)
                    throw RuntimeException(msg)
                }
            )
        })

    /** Remove a request (by request ID) from the mempool.
      * @param requestId
      *   the request's ID
      * @return
      *   an updated mempool
      * @throws IllegalArgumentException
      *   if the [[requestId]] is already missing from the mempool.
      */
    private def unsafeExtractRequest(requestId: RequestId): (Mempool, UserRequestWithId) = {
        val requestWithId = requests.getOrElse(requestId, {
            val msg =
                s"Panic: attempting to remove a request ID (${requestId.asI64}) that is missing from the mempool: $requestId"
            logger.error(msg)
            throw IllegalArgumentException(msg)
        })

        val newMempool = copy(
          requests = requests - requestId,
          arrivalOrder = arrivalOrder.filterNot(_ == requestId)
        )

        (newMempool, requestWithId)
    }
}

object Mempool {
    val empty: Mempool = Mempool()

    def apply(events: Seq[UserRequestWithId]): Mempool =
        events.foldLeft(Mempool.empty)((mempool, request) => mempool.unsafeAddRequest(request))
}
