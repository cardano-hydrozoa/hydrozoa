package hydrozoa.multisig.server

import hydrozoa.multisig.consensus.{UserRequest, UserRequestBody}
import io.circe.parser.decode
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** The submit-body wire contract: the internally-tagged JSON the [[SubmissionClient]] posts must
  * decode as [[ApiDto.SubmitRequestView]] and [[ApiDto.toUserRequest]] must recover the request.
  * (Guards the CLI ⇄ `POST /head/requests` round-trip, which has no HTTP-level test.)
  */
class SubmitRequestBodyTest extends AnyFunSuite:

    private val l1Hex = "deadbeef"
    private val l2Hex = "cafe00"

    test("a `type: transaction` body decodes to a TransactionRequest") {
        val body = s"""{ "type": "transaction", "l2Payload": "$l2Hex" }"""
        val request = decode[ApiDto.SubmitRequestView](body).flatMap(ApiDto.toUserRequest)
        assert(
          request == Right(
            UserRequest.TransactionRequest(
              UserRequestBody.TransactionRequestBody(ByteString.fromHex(l2Hex))
            )
          )
        )
    }

    test("a `type: deposit` body decodes to a DepositRequest") {
        val body = s"""{ "type": "deposit", "l1Payload": "$l1Hex", "l2Payload": "$l2Hex" }"""
        val request = decode[ApiDto.SubmitRequestView](body).flatMap(ApiDto.toUserRequest)
        assert(
          request == Right(
            UserRequest.DepositRequest(
              UserRequestBody.DepositRequestBody(
                ByteString.fromHex(l1Hex),
                ByteString.fromHex(l2Hex)
              )
            )
          )
        )
    }

    test("an unknown `type` is a client error") {
        val body = """{ "type": "nonsense", "l2Payload": "cafe00" }"""
        assert(decode[ApiDto.SubmitRequestView](body).isLeft)
    }
