package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.server.JsonCodecs.given_Encoder_UserRequestHeader
import io.circe.*
import io.circe.syntax.*
import scalus.cardano.ledger.{Hash, Hash32}
import scalus.crypto.ed25519.{Signature, VerificationKey}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.{ByteString, JVMPlatformSpecific}
import scalus.|>

// TODO: move away from server, it doesn't belong in here
/** A parsed user request with a valid signature and body hash
  *
  * @param signature
  *   Signature of the header, encoded as the bytestring of the UTF-8 representation of json string,
  *   verifiable by userVK
  */
enum UserRequest extends SyncRequest[IO, UserRequest, RequestId] {

    export UserRequest.Sync
    def ?: : this.Send = SyncRequest.send(_, this)

    def header: UserRequestHeader
    def body: UserRequestBody
    def userVk: VerificationKey

    case DepositRequest private (
        override val header: UserRequestHeader,
        override val body: UserRequestBody.DepositRequestBody,
        override val userVk: VerificationKey
    ) extends UserRequest

    case TransactionRequest private (
        override val header: UserRequestHeader,
        override val body: UserRequestBody.TransactionRequestBody,
        override val userVk: VerificationKey
    ) extends UserRequest
}

object UserRequest {

    object DepositRequest {
        def apply(
            header: UserRequestHeader,
            body: DepositRequestBody,
            userVk: VerificationKey
        ): DepositRequest = new UserRequest.DepositRequest(header, body, userVk)
    }

    object TransactionRequest {
        def apply(
            header: UserRequestHeader,
            body: TransactionRequestBody,
            userVk: VerificationKey
        ): TransactionRequest = new UserRequest.TransactionRequest(header, body, userVk)
    }

    type Sync = SyncRequest.Envelope[IO, UserRequest, RequestId]

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
) {
    def signEd25519(privateKey: ByteString): Signature =
        Signature.unsafeFromByteString(JVMPlatformSpecific.signEd25519(privateKey, this.byteString))
    // TODO: do we want to remove it finally?
    def bytes: Array[Byte] = this.asJson(given_Encoder_UserRequestHeader).toString.getBytes("UTF-8")
    private def byteString: ByteString = ByteString.fromArray(this.bytes)
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

        println(s"preimage: $preimage")

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
