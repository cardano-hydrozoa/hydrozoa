package hydrozoa.multisig.server

import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.multisig.consensus.{RequestValidityEndTimeRaw, RequestValidityStartTimeRaw, UserRequest, UserRequestBody, UserRequestHeader}
import io.circe.syntax.*
import java.time.Instant
import scalus.cardano.ledger.AssetName
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.ByteString

/** Tool to generate JSON examples for API request types.
  *
  * Usage: Run this object to print example JSON for DepositRequest and TransactionRequest.
  */
object JsonExampleGenerator {
    // Generated via https://cyphr.me/ed25519_tool/ed.html
    private val privateKey =
        ByteString.fromHex("CECF833065416B2E571B9563F94910C7FCC13D1ACE102843660FA202F8C0D126")
    private val pubKey =
        ByteString.fromHex("8A82B1C6792A44BEE2CCF45DB585E250D89D05AF43B75EEC6796749FA29A18DE")

    private val userVk: VerificationKey = VerificationKey.unsafeFromByteString(pubKey)

    import JsonCodecs.given

    /** Generate example bytes of specified length */
    private def exampleBytes(length: Int, seed: Byte = 0): ByteString =
        ByteString.fromArray(Array.fill(length)((seed + 1).toByte))

    /** Generate example DepositRequest */
    private def exampleDepositRequest: UserRequest = {
        val body: UserRequestBody.DepositRequestBody =
            UserRequestBody.DepositRequestBody(
              l1Payload = exampleBytes(100, seed = 2),
              l2Payload = exampleBytes(50, seed = 3)
            )

        val header = UserRequestHeader(
          headId = HeadId(AssetName.fromHex("deadbeef")),
          validityStart = RequestValidityStartTimeRaw.apply(Instant.now().getEpochSecond - 10),
          validityEnd = RequestValidityEndTimeRaw.apply(Instant.now().getEpochSecond + 600),
          bodyHash = body.hash
        )

        val signature = header.signEd25519(privateKey)

        val req = UserRequest.DepositRequest(
          header,
          body,
          userVk,
          signature
        )
        req.getOrElse(throw new RuntimeException("failed to build DepositRequest"))
    }

    /** Generate example TransactionRequest */
    private def exampleTransactionRequest: UserRequest = {
        val body: UserRequestBody.TransactionRequestBody =
            UserRequestBody.TransactionRequestBody(
              l2Payload = exampleBytes(75, seed = 11)
            )

        val header = UserRequestHeader(
          headId = HeadId(AssetName.fromHex("cafebabe")),
          validityStart = RequestValidityStartTimeRaw.apply(Instant.now().getEpochSecond - 10),
          validityEnd = RequestValidityEndTimeRaw.apply(Instant.now().getEpochSecond + 600),
          bodyHash = body.hash
        )

        val signature = header.signEd25519(privateKey)

        val req = UserRequest.TransactionRequest(
          header,
          body,
          userVk,
          signature
        )
        req.getOrElse(throw new RuntimeException("failed to build TransactionRequest"))
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
