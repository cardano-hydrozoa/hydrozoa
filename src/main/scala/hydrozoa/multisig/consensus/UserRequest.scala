package hydrozoa.multisig.consensus

import cats.effect.IO
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.ledger.event.{RequestHash, RequestId}
import scalus.uplc.builtin.ByteString

// TODO: move away from server, it doesn't belong in here
/** A parsed user request wrapping its body. There is no separate header: the L2 payload in the body
  * is a native, self-authenticating Cardano transaction that carries its own validity interval and
  * headId pin, and its signatures are verified by the ledger's stateless screening.
  */
enum UserRequest extends SyncRequest[IO, UserRequest, Either[UserRequest.Rejected, RequestId]] {

    export UserRequest.Sync
    def ?: : this.Send = SyncRequest.send(_, this)

    def body: UserRequestBody

    /** The digest of [[body]] **as the submitter computed it**, not as this node derived it.
      *
      * It is a claim until [[RequestSequencer]] re-derives [[UserRequestBody.mkHash]] from the body
      * it received and refuses the request if the two differ — an end-to-end check that the request
      * the head holds is the request the user built. Nothing downstream trusts it: every peer that
      * needs a request's digest hashes the bytes in front of it (see `JointLedger`).
      */
    def requestHash: RequestHash

    /** Re-derive [[UserRequestBody.mkHash]] from [[body]] and compare it against [[requestHash]],
      * naming both digests when they differ.
      *
      * There is one hash function, run by the submitter to produce the value and here to verify it;
      * a second implementation would be a second thing to disagree about. A mismatch means the head
      * and the submitter disagree about what was submitted, so the request is refused rather than
      * corrected — see [[RequestSequencer]], which runs this before assigning a [[RequestId]].
      */
    def checkRequestHash: Either[String, Unit] = {
        val derived = body.mkHash
        Either.cond(
          derived == requestHash,
          (),
          "requestHash does not match the submitted body:" +
              s" submitted=${requestHash.toHex}, derived=${derived.toHex}"
        )
    }

    case DepositRequest private (
        override val body: UserRequestBody.DepositRequestBody,
        override val requestHash: RequestHash
    ) extends UserRequest

    case TransactionRequest private (
        override val body: UserRequestBody.TransactionRequestBody,
        override val requestHash: RequestHash
    ) extends UserRequest
}

object UserRequest {

    object DepositRequest {

        /** Build the request a submitter sends: the digest is derived from the body it is about to
          * carry, which is what a client does before it has a [[RequestId]] to name the request by.
          */
        def apply(body: DepositRequestBody): DepositRequest =
            new UserRequest.DepositRequest(body, body.mkHash)

        /** Rebuild a request as received, keeping the submitter's own digest for the head to check.
          */
        def apply(body: DepositRequestBody, requestHash: RequestHash): DepositRequest =
            new UserRequest.DepositRequest(body, requestHash)
    }

    object TransactionRequest {

        /** Build the request a submitter sends — see [[DepositRequest.apply]]. */
        def apply(body: TransactionRequestBody): TransactionRequest =
            new UserRequest.TransactionRequest(body, body.mkHash)

        /** Rebuild a request as received, keeping the submitter's own digest for the head to check.
          */
        def apply(body: TransactionRequestBody, requestHash: RequestHash): TransactionRequest =
            new UserRequest.TransactionRequest(body, requestHash)
    }

    type Sync = SyncRequest.Envelope[IO, UserRequest, Either[Rejected, RequestId]]

    /** A request rejected at stateless screening (docs/spec/l2-isomorphism.md), before a
      * `RequestId` is assigned — the ledger judged it malformed or replay-pinned. Surfaced to the
      * submitter instead of an id.
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

    /** This body's content digest, and the single entry point to it: the submitter runs this to
      * produce the value it sends, and the head runs it over the body it received to verify that
      * value. A second implementation would be a second thing to disagree about.
      *
      * [[RequestHash]] carries the preimage layout and why it is shaped that way.
      */
    def mkHash: RequestHash = this match {
        case UserRequestBody.DepositRequestBody(l1Payload, l2Payload) =>
            RequestHash.hashDeposit(l1Payload, l2Payload)
        case UserRequestBody.TransactionRequestBody(l2Payload) =>
            RequestHash.hashTransaction(l2Payload)
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
