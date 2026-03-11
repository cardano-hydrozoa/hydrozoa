package hydrozoa.multisig.server

import hydrozoa.multisig.server.JsonCodecs.{DepositRequest, TransactionRequest}
import io.circe.syntax.*
import scalus.cardano.ledger.{AssetName, Blake2b_256, Hash}
import scalus.crypto.ed25519.{Signature, VerificationKey}
import scalus.uplc.builtin.ByteString

/** Tool to generate JSON examples for API request types.
  *
  * Usage: Run this object to print example JSON for DepositRequest and TransactionRequest.
  */
object JsonExampleGenerator {

    import JsonCodecs.apiRequest
    import JsonCodecs.given

    /** Generate example bytes of specified length */
    private def exampleBytes(length: Int, seed: Byte = 0): Array[Byte] =
        Array.fill(length)((seed + 1).toByte)

    /** Generate example DepositRequest */
    def exampleDepositRequest: DepositRequest = {
        val header = apiRequest.UserRequestHeader(
          headId = AssetName.fromHex("deadbeef"),
          validityStart = BigInt(1234567890000L), // Example POSIX time
          validityEnd = BigInt(1234567999999L),
          bodyHash = Hash[Blake2b_256, Any](
            ByteString.fromArray(exampleBytes(32, seed = 1))
          )
        )

        val body: apiRequest.UserRequestBody.DepositRequestBody =
            apiRequest.UserRequestBody.DepositRequestBody(
              l1Payload = exampleBytes(100, seed = 2),
              l2Payload = exampleBytes(50, seed = 3)
            )

        val userVk = VerificationKey.unsafeFromByteString(
          ByteString.fromArray(exampleBytes(32, seed = 4))
        )

        val signature = Signature.unsafeFromByteString(
          ByteString.fromArray(exampleBytes(64, seed = 5))
        )

        apiRequest.UserRequest[apiRequest.UserRequestBody.DepositRequestBody](
          header,
          body,
          userVk,
          signature
        )
    }

    /** Generate example TransactionRequest */
    def exampleTransactionRequest: TransactionRequest = {
        val header = apiRequest.UserRequestHeader(
          headId = AssetName.fromHex("cafebabe"),
          validityStart = BigInt(1234567890000L),
          validityEnd = BigInt(1234567999999L),
          bodyHash = Hash[Blake2b_256, Any](
            ByteString.fromArray(exampleBytes(32, seed = 10))
          )
        )

        val body: apiRequest.UserRequestBody.TransactionRequestBody =
            apiRequest.UserRequestBody.TransactionRequestBody(
              l2Payload = exampleBytes(75, seed = 11)
            )

        val userVk = VerificationKey.unsafeFromByteString(
          ByteString.fromArray(exampleBytes(32, seed = 12))
        )

        val signature = Signature.unsafeFromByteString(
          ByteString.fromArray(exampleBytes(64, seed = 13))
        )

        apiRequest.UserRequest[apiRequest.UserRequestBody.TransactionRequestBody](
          header,
          body,
          userVk,
          signature
        )
    }

    /** Pretty-print JSON string */
    private def prettyJson(json: io.circe.Json): String =
        json.spaces2

    def main(args: Array[String]): Unit = {
        println("=" * 80)
        println("DepositRequest JSON Example:")
        println("=" * 80)
        val depositReq = exampleDepositRequest
        println(prettyJson(depositReq.asJson))

        println()
        println("=" * 80)
        println("TransactionRequest JSON Example:")
        println("=" * 80)
        val txReq = exampleTransactionRequest
        println(prettyJson(txReq.asJson))

        println()
        println("=" * 80)
        println("Examples generated successfully!")
        println("=" * 80)
    }
}
