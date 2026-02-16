package hydrozoa.integration.yaci

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}
import scalus.cardano.ledger.ProtocolParams
import sttp.client4.*
import sttp.model.{StatusCode, Uri}

given backend: sttp.client4.Backend[scala.concurrent.Future] = DefaultFutureBackend()

object DevKit {

    val yaciParams: ProtocolParams = ProtocolParams.fromBlockfrostJson(
      this.getClass
          .getResourceAsStream("/yaci-params.json")
    )

    val blockfrostApiBaseUri = "http://localhost:8080/api/v1"
    val adminApiBaseUri: Uri = uri"http://localhost:10000/local-cluster/api/admin"

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

    def devnetInfo(): DevnetInfo =

        // Obtain the cluster information
        val request = basicRequest
            .get(uri"$adminApiBaseUri/devnet")
            .send(backend)
            .map(resp =>
                if resp.code == StatusCode.Ok then {
                    resp.body match {
                        case Right(body) =>
                            val jobj = ujson.read(body, trace = false).obj
                            DevnetInfo(
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
            .post(uri"$adminApiBaseUri/devnet/reset")
            .send(backend)
            .map(resp =>
                if resp.code == StatusCode.Ok then ()
                else throw RuntimeException(s"Cannot reset Yaci: ${resp.body}")
            )
        Await.result(request, timeout)
    }
}
