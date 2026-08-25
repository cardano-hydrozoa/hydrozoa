package hydrozoa.multisig.consensus.transport

import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.transport.Codecs.given
import hydrozoa.multisig.consensus.{UserRequest, UserRequestBody, UserRequestWithId}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import io.circe.syntax.*
import io.circe.{Decoder, Json}
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** The request lane's wire codec, which has to read two forms at once.
  *
  * A peer on this build must understand a peer on the next one, which will send the protobuf form.
  * The two are distinguished by JSON shape — a string against an object — so this pins that nothing
  * can be read as both, and that each form survives its own round trip.
  */
class UserRequestWireCodecTest extends AnyFunSuite {

    private def bytes(n: Int, fill: Byte): ByteString =
        ByteString.fromArray(Array.fill[Byte](n)(fill))

    private val transaction: UserRequestWithId = UserRequestWithId.TransactionRequest(
      RequestId(HeadPeerNumber(2), RequestNumber(913L)),
      UserRequest.TransactionRequest(UserRequestBody.TransactionRequestBody(bytes(800, 3)))
    )

    private val deposit: UserRequestWithId = UserRequestWithId.DepositRequest(
      RequestId(HeadPeerNumber(0), RequestNumber(0L)),
      UserRequest.DepositRequest(
        UserRequestBody.DepositRequestBody(bytes(220, 5), bytes(96, 7))
      )
    )

    private val cases = List("transaction" -> transaction, "deposit" -> deposit)

    test("the emitted form still round-trips") {
        cases.foreach { (name, request) =>
            assert(
              Decoder[UserRequestWithId].decodeJson(request.asJson) == Right(request),
              s"round trip changed $name"
            )
        }
    }

    test("the protobuf form round-trips through the same decoder the mesh uses") {
        cases.foreach { (name, request) =>
            val wire = Codecs.userRequestProtobufEncoder(request)
            val _ = assert(wire.isString, s"$name did not encode to a JSON string")
            assert(
              Decoder[UserRequestWithId].decodeJson(wire) == Right(request),
              s"protobuf round trip changed $name"
            )
        }
    }

    test("the two forms are distinguishable by shape alone") {
        cases.foreach { (_, request) =>
            val emitted = request.asJson
            val protobuf = Codecs.userRequestProtobufEncoder(request)
            // Neither can be mistaken for the other, so the decoder never has to guess.
            val _ = assert(emitted.isObject && !emitted.isString)
            assert(protobuf.isString && !protobuf.isObject)
        }
    }

    test("the protobuf form is smaller than the form it will replace") {
        // Hex in JSON is two characters per byte; base64 is four per three. The point of the change.
        val emitted = transaction.asJson.noSpaces.length
        val protobuf = Codecs.userRequestProtobufEncoder(transaction).noSpaces.length
        assert(
          protobuf * 4 < emitted * 3,
          s"protobuf form is $protobuf bytes against $emitted; expected roughly two thirds"
        )
    }

    test(
      "a malformed protobuf payload reports what was wrong with it, not that a string was wanted"
    ) {
        val notProtobuf = Json.fromString("Zm9vYmFyYmF6")
        val failure = Decoder[UserRequestWithId].decodeJson(notProtobuf)
        assert(
          failure.left.exists(_.getMessage.contains("protobuf is malformed")),
          s"unhelpful failure: $failure"
        )
    }

    test("a string that is not base64 says so") {
        val failure = Decoder[UserRequestWithId].decodeJson(Json.fromString("not base64 !!!"))
        assert(
          failure.left.exists(_.getMessage.contains("not base64")),
          s"unhelpful failure: $failure"
        )
    }
}
