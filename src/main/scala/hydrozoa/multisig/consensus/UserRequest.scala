package hydrozoa.multisig.consensus

import cats.effect.IO
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.ledger.event.RequestId
import java.time.Instant
import scalus.cardano.ledger.{Hash, Hash32}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString
import scalus.|>

// TODO: move away from server, it doesn't belong in here
/** A parsed user request wrapping its body. There is no separate header: the L2 payload in the body
  * is a native, self-authenticating Cardano transaction that carries its own validity interval and
  * headId pin, and its signatures are verified by the ledger's stateless screening.
  */
enum UserRequest extends SyncRequest[IO, UserRequest, Either[UserRequest.Rejected, RequestId]] {

    export UserRequest.Sync
    def ?: : this.Send = SyncRequest.send(_, this)

    def body: UserRequestBody

    case DepositRequest private (
        override val body: UserRequestBody.DepositRequestBody
    ) extends UserRequest

    case TransactionRequest private (
        override val body: UserRequestBody.TransactionRequestBody
    ) extends UserRequest
}

object UserRequest {

    object DepositRequest {
        def apply(body: DepositRequestBody): DepositRequest =
            new UserRequest.DepositRequest(body)
    }

    object TransactionRequest {
        def apply(body: TransactionRequestBody): TransactionRequest =
            new UserRequest.TransactionRequest(body)
    }

    type Sync = SyncRequest.Envelope[IO, UserRequest, Either[Rejected, RequestId]]

    /** A request rejected at stateless screening (docs/l2-isomorphism.md), before a `RequestId` is
      * assigned — the ledger judged it malformed or replay-pinned. Surfaced to the submitter
      * instead of an id.
      */
    final case class Rejected(reason: String)

}

enum UserRequestBody {

    /** @param l1Payload
      *   The cbor-encoded depositTx
      * @param l2Payload
      *   And opaque byte array passed unmodified to the L2
      */
    case DepositRequestBody(
        l1Payload: ByteString,
        l2Payload: ByteString
    )
    case TransactionRequestBody(
        l2Payload: ByteString
    )

    /** To keep the hash injective, we hash deposits twice, to avoid collapsing liek hash(abc + def) ==
      * hash(ab + cdef)
      */
    def hash: Hash32 = {
        val preimage = this match {
            case UserRequestBody.DepositRequestBody(l1Payload, l2Payload) =>
                blake2b_256(l1Payload)
                    .concat(blake2b_256(l2Payload))
            case UserRequestBody.TransactionRequestBody(l2Payload) => l2Payload
        }

        // println(s"preimage: $preimage")

        preimage |> blake2b_256 |> Hash.apply
    }
}

enum UserRequestWithId {
    def requestId: RequestId
    def request: UserRequest

    /** Wall-clock time the author peer received the request, stamped at id assignment (CR1) and
      * consensus-replicated with the request.
      */
    def receivedAt: Instant

    case DepositRequest(
        override val requestId: RequestId,
        override val request: UserRequest.DepositRequest,
        override val receivedAt: Instant,
    )

    case TransactionRequest(
        override val requestId: RequestId,
        override val request: UserRequest.TransactionRequest,
        override val receivedAt: Instant,
    )
}

object UserRequestWithId {
    def apply(
        userRequest: UserRequest,
        requestId: RequestId,
        receivedAt: Instant
    ): UserRequestWithId = userRequest match {
        case req: UserRequest.DepositRequest =>
            UserRequestWithId.DepositRequest(requestId, req, receivedAt)
        case req: UserRequest.TransactionRequest =>
            UserRequestWithId.TransactionRequest(requestId, req, receivedAt)
    }
}
