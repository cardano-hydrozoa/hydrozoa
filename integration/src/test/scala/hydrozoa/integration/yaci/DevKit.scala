package hydrozoa.integration.yaci

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Coin, ProtocolParams}
import sttp.client4.*
import sttp.model.{StatusCode, Uri}

given backend: sttp.client4.Backend[scala.concurrent.Future] = DefaultFutureBackend()

/** Handle to a running Yaci DevKit devnet, holding its Blockfrost-compatible and admin API base
  * URIs. Use [[DevKit.localhost]] for a manually-started devnet on the conventional dev ports, or a
  * [[YaciDevnet]]-managed instance whose URIs point at the container's mapped ports.
  */
final case class DevKit(blockfrostApiBaseUri: String, yaciApiBaseUri: Uri) {

    /** Partial `/admin/devnet` response. */
    def devnetInfo(): DevKit.DevnetInfo =
        val request = basicRequest
            .get(uri"$yaciApiBaseUri/admin/devnet")
            .send(backend)
            .map(resp =>
                if resp.code == StatusCode.Ok then {
                    resp.body match {
                        case Right(body) =>
                            val jobj = ujson.read(body, trace = false).obj
                            DevKit.DevnetInfo(
                              slotLength = jobj.get("slotLength").get.num.longValue,
                              startTime = jobj.get("startTime").get.num.longValue,
                              protocolMagic = jobj.get("protocolMagic").get.num.intValue,
                            )
                        case Left(error) =>
                            throw RuntimeException(
                              s"Failed to fetch /devnet. Status: ${resp.code}, Body: $error"
                            )
                    }
                } else {
                    throw RuntimeException(
                      s"Failed to fetch /devnet. Status: ${resp.code}, Body: ${resp.body}"
                    )
                }
            )
        Await.result(request, 10.seconds)

    def reset(timeout: FiniteDuration = 30.seconds): Unit = {
        val request = basicRequest
            .post(uri"$yaciApiBaseUri/admin/devnet/reset")
            .send(backend)
            .map(resp =>
                if resp.code == StatusCode.Ok then ()
                else throw RuntimeException(s"Cannot reset Yaci: ${resp.body}")
            )
        Await.result(request, timeout)
    }

    /** Fetch the running devnet's live protocol parameters (including cost models) from its
      * Blockfrost `/epochs/parameters`. Prefer this over the bundled [[DevKit.yaciParams]] when
      * building transactions against the devnet: the tx builder's cost models must match the
      * image's, or every Plutus tx fails `PPViewHashesDontMatch` (script-integrity-hash mismatch).
      */
    def protocolParams(timeout: FiniteDuration = 10.seconds): ProtocolParams = {
        val request = basicRequest
            .get(Uri.unsafeParse(s"$blockfrostApiBaseUri/epochs/parameters"))
            .send(backend)
            .map(resp =>
                resp.body match {
                    case Right(body) =>
                        ProtocolParams.fromBlockfrostJson(
                          new java.io.ByteArrayInputStream(
                            body.getBytes(java.nio.charset.StandardCharsets.UTF_8)
                          )
                        )
                    case Left(error) =>
                        throw RuntimeException(
                          s"Failed to fetch protocol params. Status: ${resp.code}, Body: $error"
                        )
                }
            )
        Await.result(request, timeout)
    }

    /** Fund `address` from the devnet faucet. The faucet returns a transient `500 "Topup failed"`
      * while the devnet is still warming (the admin API and Blockfrost come up before the faucet is
      * usable), so retry on any non-`200` before giving up — a single POST flakes on CI.
      */
    def topup(
        address: ShelleyAddress,
        coins: Coin,
        timeout: FiniteDuration = 30.seconds,
        maxAttempts: Int = 8,
        retryDelay: FiniteDuration = 2.seconds,
    ): Unit = {
        val adaAmount = coins.value / 1_000_000.0
        val jsonBody = s"""{"address": "${address.toBech32.get}", "adaAmount": $adaAmount}"""

        def attempt(): Either[String, Unit] =
            val request = basicRequest
                .post(uri"$yaciApiBaseUri/addresses/topup")
                .header("Content-Type", "application/json")
                .body(jsonBody)
                .send(backend)
                .map(resp =>
                    if resp.code == StatusCode.Ok then Right(())
                    else Left(s"status ${resp.code}: ${resp.body}")
                )
            try Await.result(request, timeout)
            catch case e: Throwable => Left(s"request error: ${e.getMessage}")

        @annotation.tailrec
        def loop(attemptsLeft: Int): Unit =
            attempt() match
                case Right(()) => ()
                case Left(_) if attemptsLeft > 1 =>
                    Thread.sleep(retryDelay.toMillis)
                    loop(attemptsLeft - 1)
                case Left(err) =>
                    throw new RuntimeException(s"Topup failed after $maxAttempts attempts: $err")

        loop(maxAttempts)
    }
}

object DevKit {

    val yaciParams: ProtocolParams = ProtocolParams.fromBlockfrostJson(
      this.getClass
          .getResourceAsStream("/yaci-params.json")
    )

    /** Conventional dev ports of a manually-started (`yaci-devkit up`) local devnet. */
    val defaultBlockfrostApiBaseUri: String = "http://localhost:8080/api/v1"
    val defaultYaciApiBaseUri: Uri = uri"http://localhost:10000/local-cluster/api"

    /** A [[DevKit]] bound to the conventional local dev ports. */
    val localhost: DevKit = DevKit(defaultBlockfrostApiBaseUri, defaultYaciApiBaseUri)

    /** Partial response.
      *
      * @param slotLength
      *   NB: in seconds!
      * @param startTime
      *   NB: in seconds!
      * @param protocolMagic
      */
    case class DevnetInfo(
        slotLength: Long,
        startTime: Long,
        protocolMagic: Int
    )
}
