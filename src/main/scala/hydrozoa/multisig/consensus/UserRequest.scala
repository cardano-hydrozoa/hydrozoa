package hydrozoa.multisig.consensus

import cats.effect.IO
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.ledger.event.RequestId
import scalus.cardano.ledger.{Hash, Hash32}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString
import scalus.|>

// TODO: move away from server, it doesn't belong in here
/** A parsed user request: a header bound to its body by the header's body hash. Authentication is
  * not carried here — the L2 payload in the body is a native, self-authenticating Cardano
  * transaction, and its signatures are verified by the ledger's stateless screening (§5.4 Phase 4).
  */
enum UserRequest extends SyncRequest[IO, UserRequest, Either[UserRequest.Rejected, RequestId]] {

    export UserRequest.Sync
    def ?: : this.Send = SyncRequest.send(_, this)

    def header: UserRequestHeader
    def body: UserRequestBody

    case DepositRequest private (
        override val header: UserRequestHeader,
        override val body: UserRequestBody.DepositRequestBody
    ) extends UserRequest

    case TransactionRequest private (
        override val header: UserRequestHeader,
        override val body: UserRequestBody.TransactionRequestBody
    ) extends UserRequest
}

object UserRequest {

    object DepositRequest {
        def apply(
            header: UserRequestHeader,
            body: DepositRequestBody
        ): DepositRequest = new UserRequest.DepositRequest(header, body)
    }

    object TransactionRequest {
        def apply(
            header: UserRequestHeader,
            body: TransactionRequestBody
        ): TransactionRequest = new UserRequest.TransactionRequest(header, body)
    }

    type Sync = SyncRequest.Envelope[IO, UserRequest, Either[Rejected, RequestId]]

    /** A request rejected at stateless screening (§4.1), before a `RequestId` is assigned — the
      * ledger judged it malformed or replay-pinned. Surfaced to the submitter instead of an id.
      */
    final case class Rejected(reason: String)

}

/** @param headId
  *   The blake2b_224 hash of the cbor-encoded seed utxo [[TransactionInput]] appended to the CIP-67
  *   prefix HYDR. This is the asset name of the treasury token
  * @param bodyHash
  *   blake2b_256 hash of the Cbor-encoded
  * @param validityStart
  *   Epoch time in seconds, block creation start time must be no earlier than this time in order
  *   for the request to be actionable
  * @param validityEnd
  *   Epoch time in seconds, block creation start time must be before this time in order for the
  *   request to be actionable
  */
case class UserRequestHeader(
    headId: HeadId,
    validityStart: RequestValidityStartTime,
    validityEnd: RequestValidityEndTime,
    bodyHash: Hash32
)

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

    case DepositRequest(
        override val requestId: RequestId,
        override val request: UserRequest.DepositRequest,
    )

    case TransactionRequest(
        override val requestId: RequestId,
        override val request: UserRequest.TransactionRequest,
    )
}

object UserRequestWithId {
    def apply(
        userRequest: UserRequest,
        requestId: RequestId
    ): UserRequestWithId = userRequest match {
        case req: UserRequest.DepositRequest => UserRequestWithId.DepositRequest(requestId, req)
        case req: UserRequest.TransactionRequest =>
            UserRequestWithId.TransactionRequest(requestId, req)
    }
}
