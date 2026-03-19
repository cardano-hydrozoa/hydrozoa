package hydrozoa.multisig.consensus.mempool

import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.ledger.event.RequestId

/** Simple immutable mempool implementation. Duplicate ledger request IDs are NOT allowed and a
  * runtime exception is thrown since this should never happen. Other components, particularly the
  * peer liaison is in charge or maintaining the integrity of the stream of messages.
  *
  * @param requests
  *   map to store requests
  * @param receivingOrder
  *   vector to store order of request ids
  */
final case class Mempool(
    requests: Map[RequestId, UserRequestWithId] = Map.empty,
    receivingOrder: Vector[RequestId] = Vector.empty
) {

    /** Throws if a duplicate is detected.
      *
      * @param request
      *   a request to add
      * @return
      *   an updated mempool
      */
    def add(
        request: UserRequestWithId
    ): Mempool = {

        val requestId = request.requestId

        require(
          !requests.contains(requestId),
          s"panic - duplicate event id in the pool: $requestId"
        )

        copy(
          requests = requests + (requestId -> request),
          receivingOrder = receivingOrder :+ requestId
        )
    }

    // Remove event - returns new state
    def remove(id: RequestId): Mempool = {
        require(
          requests.contains(id),
          "panic - an attempt to remove a missing event from the mempool"
        )
        copy(
          requests = requests - id,
          receivingOrder = receivingOrder.filterNot(_ == id)
        )
    }

    // Find by ID
    def findById(id: RequestId): Option[UserRequestWithId] = requests.get(id)

    // Iterate in insertion order
    def inOrder: Iterator[UserRequestWithId] =
        receivingOrder.iterator.flatMap(requests.get)

    def isEmpty: Boolean = requests.isEmpty
}

object Mempool {
    val empty: Mempool = Mempool()

    def apply(events: Seq[UserRequestWithId]): Mempool =
        events.foldLeft(Mempool.empty)((mempool, request) => mempool.add(request))
}
