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

    private val transactionBody: UserRequestBody.TransactionRequestBody =
        UserRequestBody.TransactionRequestBody(ByteString.fromHex(l2Hex))
    private val depositBody: UserRequestBody.DepositRequestBody =
        UserRequestBody.DepositRequestBody(ByteString.fromHex(l1Hex), ByteString.fromHex(l2Hex))

    test("a `type: transaction` body decodes to a TransactionRequest") {
        val body =
            s"""{ "type": "transaction", "l2Payload": "$l2Hex",
               |  "requestHash": "${transactionBody.hash.toHex}" }""".stripMargin
        val request = decode[ApiDto.SubmitRequestView](body).flatMap(ApiDto.toUserRequest)
        assert(request == Right(UserRequest.TransactionRequest(transactionBody)))
    }

    test("a `type: deposit` body decodes to a DepositRequest") {
        val body =
            s"""{ "type": "deposit", "l1Payload": "$l1Hex", "l2Payload": "$l2Hex",
               |  "requestHash": "${depositBody.hash.toHex}" }""".stripMargin
        val request = decode[ApiDto.SubmitRequestView](body).flatMap(ApiDto.toUserRequest)
        assert(request == Right(UserRequest.DepositRequest(depositBody)))
    }

    test("an unknown `type` is a client error") {
        val body =
            s"""{ "type": "nonsense", "l2Payload": "$l2Hex",
               |  "requestHash": "${transactionBody.hash.toHex}" }""".stripMargin
        assert(decode[ApiDto.SubmitRequestView](body).isLeft)
    }

    test("a body with no `requestHash` is a client error") {
        val body = s"""{ "type": "transaction", "l2Payload": "$l2Hex" }"""
        assert(decode[ApiDto.SubmitRequestView](body).isLeft)
    }

    /** A wrong-width digest is rejected at decode, before the request reaches the sequencer that
      * would compare it: a 32-byte field is a shape question, not a verdict on the body.
      */
    test("a `requestHash` that is not 32 bytes is a client error") {
        val body =
            s"""{ "type": "transaction", "l2Payload": "$l2Hex", "requestHash": "deadbeef" }"""
        val request = decode[ApiDto.SubmitRequestView](body).flatMap(ApiDto.toUserRequest)
        assert(request.left.exists(_.toString.contains("32 bytes")))
    }

    /** A digest that is well-formed but wrong for the body still decodes here. Deciding whether it
      * describes the body is [[hydrozoa.multisig.consensus.RequestSequencer]]'s job, and deriving
      * it at this layer would answer that question by construction.
      */
    test("a well-formed `requestHash` that does not match the body still decodes") {
        val body =
            s"""{ "type": "transaction", "l2Payload": "$l2Hex",
               |  "requestHash": "${depositBody.hash.toHex}" }""".stripMargin
        val request = decode[ApiDto.SubmitRequestView](body).flatMap(ApiDto.toUserRequest)
        assert(request == Right(UserRequest.TransactionRequest(transactionBody, depositBody.hash)))
    }
