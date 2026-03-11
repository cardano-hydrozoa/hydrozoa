package hydrozoa.multisig.server

import hydrozoa.multisig.ledger.event.LedgerEventId
import scalus.cardano.ledger.{AssetName, Hash32}
import scalus.crypto.ed25519.{Signature, VerificationKey}

/** Response types for the HTTP API */
class ApiRequest {
    import UserRequestBody.*

    type DepositRequest = UserRequest[DepositRequestBody]
    type TransactionRequest = UserRequest[TransactionRequestBody]

    case class UserRequest[Body <: UserRequestBody](
        header: UserRequestHeader,
        body: Body,
        userVk: VerificationKey,
        signature: Signature // Signature of header, verifiable by userVk.
    )

    case class UserRequestHeader(
        headId: AssetName,
        validityStart: BigInt, // PosixTime
        validityEnd: BigInt, // PosixTime
        bodyHash: Hash32 // Hash of UserRequestBody
    )

    enum UserRequestBody {
        case DepositRequestBody(
            l1Payload: Array[Byte],
            l2Payload: Array[Byte]
        )

        case TransactionRequestBody(
            l2Payload: Array[Byte]
        )
    }

    case class UserRequestWithId(
        requestId: LedgerEventId,
        requestBody: UserRequestBody,
        signature: Signature
    )
}
