package hydrozoa.multisig.server

import cats.data.*
import cats.*
import cats.syntax.all.*
import cats.data.Validated.{Invalid, Valid}
import cats.effect.IO
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.server.UserRequest.Error.{BodyHashMismatch, SignatureMismatch}
import io.bullet.borer.derivation.MapBasedCodecs.derived
import io.bullet.borer.{Cbor, Decoder, Encoder}
import scalus.cardano.ledger.{Coin, Hash28, Hash32, Value}
import scalus.cardano.onchain.plutus.v3.PosixTime
import scalus.crypto.ed25519.{Signature, VerificationKey}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.JVMPlatformSpecific.{signEd25519, verifyEd25519Signature}

import java.util.UUID

/** A parsed user request with a valid signature and body hash
  * @param signature
  *   Signature of the header, verifiable by userVK
  */
case class UserRequest[BodyType <: UserRequestBody] private (
    header: UserRequestHeader,
    body: BodyType,
    userVk: VerificationKey,
    signature: Signature
)

object UserRequest {
    object Error {
        trait ParseError extends Throwable

        /** The [[UserRequestHeader.body]] does not match the [[blake2b_256]] hash of the
          * [[UserRequestBody]]
          */
        case object BodyHashMismatch extends ParseError

        /** The ed25519 signature of the header does not match
          */
        case object SignatureMismatch extends ParseError
    }

    def apply[AnyBody <: UserRequestBody](
        header: UserRequestHeader,
        body: UserRequestBody,
        userVk: VerificationKey,
        signature: Signature
    ): Either[Error.ParseError, UserRequest[AnyBody]] =
        for {
            _ <-
                if body.hash == header.bodyHash
                then Right(header)
                else Left(BodyHashMismatch)
            _ <-
                if verifyEd25519Signature(userVk, header.bytes, signature)
                then Right(signature)
                else Left(SignatureMismatch)
        } yield new UserRequest(header, body, userVk, signature)
}

given Encoder[UserRequestHeader] = Encoder.derived

case class UserRequestHeader(
    headId: Hash28,
    validityStart: PosixTime,
    validityEnd: PosixTime,
    bodyHash: Hash32
) {
    val bytes: ByteString = ByteString.fromArray(Cbor.encode(this).toByteArray)
}

trait UserRequestBody {
    val hash: Hash32
}

given Encoder[DepositRequest] = Encoder.derived

case class L1DepositRequestPayload(depositTxBytes : Array[Byte],
                                   refundTxBytes: Array[Byte],
                                   l2Value : Value,
                                   depositFee : Coin)

case class DepositRequest(
    l1Payload: L1DepositRequestPayload,
    l2Payload: Array[Byte]
) extends UserRequestBody {
    override val hash: Hash32 = Hash32.fromArray(Cbor.encode(this).toByteArray)
}

given Encoder[TransactionRequest] = Encoder.derived

case class TransactionRequest(
    l2Payload: Array[Byte]
) extends UserRequestBody {
    override val hash: Hash32 = Hash32.fromArray(Cbor.encode(this).toByteArray)
}

case class UserRequestWithId[BodyType <: UserRequestBody] private (
    requestId: RequestId,
    requestBody: BodyType,
    signature: Signature
)

object UserRequestWithId {
    def apply[AnyBody <: UserRequestBody](userRequest: UserRequest[AnyBody], requestId : RequestId): UserRequestWithId[AnyBody] =
        UserRequestWithId(requestId, userRequest.body, userRequest.signature)

}
