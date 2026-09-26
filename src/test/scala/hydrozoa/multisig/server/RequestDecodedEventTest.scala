package hydrozoa.multisig.server

import hydrozoa.multisig.consensus.{UserRequest, UserRequestBody}
import hydrozoa.multisig.server.HydrozoaHttpEvent.RequestDecoded
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

/** What the ingress debug event carries.
  *
  * The event is built on every accepted request, whatever the log level. Carrying the rendered
  * request meant `toString` on its payloads, and a `ByteString`'s `toString` is its hex — cached on
  * the instance by scalus, so a payload that goes on to be retained is retained at three times its
  * size. Its type now admits only the request's shape, which is what keeps that from coming back:
  * there is no field a rendered request would fit in.
  */
class RequestDecodedEventTest extends AnyFunSuite {

    private def bytes(n: Int): ByteString = ByteString.fromArray(Array.fill[Byte](n)(7))

    test("a transaction request reports its kind and payload size") {
        val request =
            UserRequest.TransactionRequest(UserRequestBody.TransactionRequestBody(bytes(1234)))
        assert(
          HydrozoaRoutes.decodedEvent("/api/L2/submit", request) ==
              RequestDecoded("/api/L2/submit", "Transaction", 1234)
        )
    }

    test("a deposit request counts both payloads") {
        val request =
            UserRequest.DepositRequest(UserRequestBody.DepositRequestBody(bytes(600), bytes(96)))
        assert(
          HydrozoaRoutes.decodedEvent("/api/L1/deposit", request) ==
              RequestDecoded("/api/L1/deposit", "Deposit", 696)
        )
    }
}
