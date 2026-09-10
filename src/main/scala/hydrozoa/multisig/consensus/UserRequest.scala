package hydrozoa.multisig.consensus

import cats.effect.IO
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.lib.crypto.Preimage
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.ledger.event.RequestId
import java.nio.charset.StandardCharsets.UTF_8
import scalus.cardano.ledger.Hash32
import scalus.uplc.builtin.Builtins.blake2b_256
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
      * It is a claim until [[RequestSequencer]] re-derives [[UserRequestBody.hash]] from the body
      * it received and refuses the request if the two differ — an end-to-end check that the request
      * the head holds is the request the user built. Nothing downstream trusts it: every peer that
      * needs a request's digest hashes the bytes in front of it (see `JointLedger`).
      */
    def requestHash: Hash32

    /** Re-derive [[UserRequestBody.hash]] from [[body]] and compare it against [[requestHash]],
      * naming both digests when they differ.
      *
      * There is one hash function, run by the submitter to produce the value and here to verify it;
      * a second implementation would be a second thing to disagree about. A mismatch means the head
      * and the submitter disagree about what was submitted, so the request is refused rather than
      * corrected — see [[RequestSequencer]], which runs this before assigning a [[RequestId]].
      */
    def checkRequestHash: Either[String, Unit] = {
        val derived = body.hash
        Either.cond(
          derived == requestHash,
          (),
          "requestHash does not match the submitted body:" +
              s" submitted=${requestHash.toHex}, derived=${derived.toHex}"
        )
    }

    case DepositRequest private (
        override val body: UserRequestBody.DepositRequestBody,
        override val requestHash: Hash32
    ) extends UserRequest

    case TransactionRequest private (
        override val body: UserRequestBody.TransactionRequestBody,
        override val requestHash: Hash32
    ) extends UserRequest
}

object UserRequest {

    object DepositRequest {

        /** Build the request a submitter sends: the digest is derived from the body it is about to
          * carry, which is what a client does before it has a [[RequestId]] to name the request by.
          */
        def apply(body: DepositRequestBody): DepositRequest =
            new UserRequest.DepositRequest(body, body.hash)

        /** Rebuild a request as received, keeping the submitter's own digest for the head to check.
          */
        def apply(body: DepositRequestBody, requestHash: Hash32): DepositRequest =
            new UserRequest.DepositRequest(body, requestHash)
    }

    object TransactionRequest {

        /** Build the request a submitter sends — see [[DepositRequest.apply]]. */
        def apply(body: TransactionRequestBody): TransactionRequest =
            new UserRequest.TransactionRequest(body, body.hash)

        /** Rebuild a request as received, keeping the submitter's own digest for the head to check.
          */
        def apply(body: TransactionRequestBody, requestHash: Hash32): TransactionRequest =
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

    /** The request's content digest — `requestHash` in `design/block-hash.md`, and the value a
      * submitter sends alongside the request for the head to verify.
      *
      * ```
      * requestHash = blake2b_256(
      *      "gummiworm-request-v1"
      *   || u8(variant)                                    -- 0 deposit, 1 transaction
      *   || deposit:     blake2b_256(l1Payload) || blake2b_256(l2Payload)
      *   || transaction: l2Payload
      * )
      * ```
      *
      * The variant tag is what keeps the two kinds apart: without it a transaction whose
      * `l2Payload` is exactly a deposit's 64-byte pair of digests hashes to the same value as that
      * deposit. It leads the payload, so neither variant needs length framing — a deposit
      * contributes two fixed-width digests, a transaction one trailing payload.
      *
      * A deposit's two payloads are hashed before being concatenated so the pair stays injective:
      * hashing them raw would collapse `hash(abc + def) == hash(ab + cdef)`.
      *
      * The [[RequestId]] is deliberately absent. The same bytes hash the same however they were
      * sequenced, which is what lets a submitter compute the digest before the head has assigned an
      * id — and lets two peers that received the same request agree on its digest without agreeing
      * on anything else.
      *
      * The construction is a public interface: a client that cannot reproduce it cannot get a
      * request accepted. It is written out for clients in `docs/user-guide/REQUEST-HASH.md`.
      */
    def hash: Hash32 = {
        val out = Preimage()
        out.raw(UserRequestBody.domainTag)
        this match {
            case UserRequestBody.DepositRequestBody(l1Payload, l2Payload) =>
                out.u8(UserRequestBody.depositVariantTag)
                out.raw(blake2b_256(l1Payload).bytes)
                out.raw(blake2b_256(l2Payload).bytes)
            case UserRequestBody.TransactionRequestBody(l2Payload) =>
                out.u8(UserRequestBody.transactionVariantTag)
                out.raw(l2Payload.bytes)
        }
        out.digest
    }
}

object UserRequestBody {

    /** Mixed in before anything else so this digest can never collide with a hash of the same bytes
      * taken for another purpose. ASCII, no terminator — the variant tag that follows is
      * fixed-width, so the boundary is unambiguous.
      */
    val domainTag: Array[Byte] = "gummiworm-request-v1".getBytes(UTF_8)

    /** Variant tags, in the order the cases are declared. They separate the two request kinds
      * inside [[domainTag]]'s namespace.
      */
    val depositVariantTag: Int = 0x00
    val transactionVariantTag: Int = 0x01
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
