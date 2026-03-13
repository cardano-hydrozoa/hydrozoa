package hydrozoa.multisig.server

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.server.JsonCodecs.{given_Encoder_UserRequestBody, given_Encoder_UserRequestHeader}
import hydrozoa.multisig.server.UserRequest.Error.{BodyHashMismatch, SignatureMismatch}
import hydrozoa.multisig.server.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import io.circe.*
import io.circe.syntax.*
import scalus.cardano.ledger.{Hash, Hash32}
import scalus.cardano.onchain.plutus.v3.PosixTime
import scalus.crypto.ed25519.{Signature, VerificationKey}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.JVMPlatformSpecific.verifyEd25519Signature
import scalus.uplc.builtin.{ByteString, JVMPlatformSpecific}
import scalus.|>

// TODO: move away from server, it doesn't belong in here
/** A parsed user request with a valid signature and body hash
  *
  * @param signature
  *   Signature of the header, encoded as the bytestring of the UTF-8 representation of json string,
  *   verifiable by userVK
  */
enum UserRequest {
    def header: UserRequestHeader
    def body: UserRequestBody
    def userVk: VerificationKey
    def signature: Signature

    case DepositRequest private (
        override val header: UserRequestHeader,
        override val body: UserRequestBody.DepositRequestBody,
        override val userVk: VerificationKey,
        override val signature: Signature
    ) extends UserRequest

    case TransactionRequest private (
        override val header: UserRequestHeader,
        override val body: UserRequestBody.TransactionRequestBody,
        override val userVk: VerificationKey,
        override val signature: Signature
    ) extends UserRequest
}

//extends SyncRequest[IO, UserRequest, RequestId] {
//    export UserRequest.Sync
//    def ?: : this.Send = SyncRequest.send(_, this)
//}

object UserRequest {
    object DepositRequest {
        def apply(
            header: UserRequestHeader,
            body: DepositRequestBody,
            userVk: VerificationKey,
            signature: Signature
        ): Either[Error.ValidationError, UserRequest] =
            for {
                _ <-
                    if body.hash == header.bodyHash
                    then Right(header)
                    else Left(BodyHashMismatch)
                _ <-
                    if verifyEd25519Signature(userVk, header.byteString, signature)
                    then Right(signature)
                    else Left(SignatureMismatch)
            } yield new UserRequest.DepositRequest(header, body, userVk, signature)
    }

    object TransactionRequest {
        def apply(
            header: UserRequestHeader,
            body: TransactionRequestBody,
            userVk: VerificationKey,
            signature: Signature
        ): Either[Error.ValidationError, UserRequest] =
            for {
                _ <-
                    if body.hash == header.bodyHash
                    then Right(header)
                    else Left(BodyHashMismatch)
                _ <-
                    if verifyEd25519Signature(userVk, header.byteString, signature)
                    then Right(signature)
                    else Left(SignatureMismatch)
            } yield new UserRequest.TransactionRequest(header, body, userVk, signature)
    }

    type Sync = SyncRequest.Envelope[IO, UserRequest, RequestId]

    object Error {
        trait ValidationError extends Throwable

        /** The [[UserRequestHeader.body]] does not match the [[blake2b_256]] hash of the
          * [[UserRequestBody]]
          */
        case object BodyHashMismatch extends ValidationError

        /** The ed25519 signature of the header does not match
          */
        case object SignatureMismatch extends ValidationError
    }
}

/** @param headId
  *   The blake2b_224 hash of the cbor-encoded seed utxo [[TransactionInput]] appended to the CIP-67
  *   prefix HYDR. This is the asset name of the treasury token
  * @param bodyHash
  *   blake2b_256 hash of the Cbor-encoded
  * @param validityStart
  *   Block creation start time must be no earlier than this time in order for the request to be
  *   actionable
  * @param validityEnd
  *   Block creation start time must be before this time in order for the request to be actionable
  */
case class UserRequestHeader(
    headId: HeadId,
    validityStart: PosixTime,
    validityEnd: PosixTime,
    bodyHash: Hash32
)

// Note: extension method because it complains about uninitialized value if I do it as a method(?)
extension (urh: UserRequestHeader) {
    def bytes: Array[Byte] = urh.asJson(given_Encoder_UserRequestHeader).toString.getBytes("UTF-8")
    def byteString: ByteString = ByteString.fromArray(urh.bytes)
    def signEd25519(privateKey: ByteString): Signature =
        Signature.unsafeFromByteString(JVMPlatformSpecific.signEd25519(privateKey, urh.byteString))
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
}

extension (urb: UserRequestBody) {
    def hash: Hash32 =
        urb.asJson(given_Encoder_UserRequestBody).toString.getBytes("UTF-8")
            |> ByteString.fromArray
            |> blake2b_256
            |> Hash.apply
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
