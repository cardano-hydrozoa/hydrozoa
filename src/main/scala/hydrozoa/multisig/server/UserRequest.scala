package hydrozoa.multisig.server

import cats.syntax.all.*
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.server.UserRequest.Error.{BodyHashMismatch, SignatureMismatch}
import io.bullet.borer.derivation.MapBasedCodecs.derived
import io.bullet.borer.{Cbor, Encoder}
import scalus.cardano.ledger.{Coin, Hash28, Hash32, Value}
import scalus.cardano.onchain.plutus.v3.PosixTime
import scalus.crypto.ed25519.{Signature, VerificationKey}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.JVMPlatformSpecific.verifyEd25519Signature

/** A parsed user request with a valid signature and body hash
  *
  * @param signature
  *   Signature of the header, encoded as the bytestring of the UTF-8 representation of json string,
  *   verifiable by userVK
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
        body: AnyBody,
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
        } yield new UserRequest[AnyBody](header, body, userVk, signature)
}

// TODO: Make this a JSON instance instead
given io.bullet.borer.Encoder[UserRequestHeader] = Encoder.derived

/** @param headId
  *   The blake2b_224 hash of the cbor-encoded seed [[Utxo]] appended to the CIP-67 prefix HYDR
  * @param bodyHash
  *   blake2b_256 hash of the Cbor-encoded
  * @param validityStart
  *   Block creation start time must be no earlier than this time in order for the request to be
  *   actionable
  * @param validityEnd
  *   Block creation start time must be before this time in order for the request to be actionable
  */
case class UserRequestHeader(
    headId: Hash28, // FIXME: Not a hash28, see doc comment
    validityStart: PosixTime,
    validityEnd: PosixTime,
    bodyHash: Hash32
) {
    //  TODO: UTF-8 JSON
    val bytes: ByteString = ByteString.fromArray(Cbor.encode(this).toByteArray)
}

// TODO: Make into Enum rather than trait
trait UserRequestBody {
    val hash: Hash32
}

// TODO: Json codec
given io.bullet.borer.Encoder[DepositRequest] = Encoder.derived

given io.bullet.borer.Encoder[L1DepositRequestPayload] = Encoder.derived

case class L1DepositRequestPayload(
    depositTxBytes: Array[Byte],
    // TODO: This will be removed in the future
    refundTxBytes: Array[Byte],
    // TODO: Remove. Deposit value - depositFee = l2Value
    l2Value: Value,
    // TODO: Remove: deposit fee will be put in DepositTx Metadata
    depositFee: Coin
)

case class DepositRequest(
    l1Payload: L1DepositRequestPayload,
    l2Payload: Array[Byte]
) extends UserRequestBody {
    // TODO: Hash of UTF-8 JSON, not cbor
    override val hash: Hash32 = Hash32.fromArray(Cbor.encode(this).toByteArray)
}

// TODO: JSON Codec
given io.bullet.borer.Encoder[TransactionRequest] = Encoder.derived

case class TransactionRequest(
    l2Payload: Array[Byte]
) extends UserRequestBody {
    // TODO: Hash of UTF-8 JSON, not cbor
    override val hash: Hash32 = Hash32.fromArray(Cbor.encode(this).toByteArray)
}

case class UserRequestWithId[BodyType <: UserRequestBody] private (
    requestId: RequestId,
    request: UserRequest[BodyType],
)

object UserRequestWithId {
    def apply[AnyBody <: UserRequestBody](
        userRequest: UserRequest[AnyBody],
        requestId: RequestId
    ): UserRequestWithId[AnyBody] =
        UserRequestWithId(requestId, userRequest)

}
