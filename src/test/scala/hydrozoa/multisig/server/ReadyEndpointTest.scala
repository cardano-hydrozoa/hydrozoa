package hydrozoa.multisig.server

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.GenerateSampleConfig.{defaultSpec, testPeersSpec}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.NodeStatus
import hydrozoa.multisig.consensus.{BlockWeaver, RequestSequencer}
import hydrozoa.multisig.persistence.ConsensusStoreReader
import io.circe.Json
import org.http4s.circe.*
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite

/** `GET /ready`: `200` only while the node is [[NodeStatus.Active]], `503` otherwise — and the body
  * carries the lifecycle status label either way (the verdict is the status code; the body is
  * diagnostic).
  */
class ReadyEndpointTest extends AnyFunSuite:

    private val spec = defaultSpec.copy(nPeers = 1)

    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig
            .generate(testPeersSpec(spec))()
            .pureApply(Gen.Parameters.default, Seed(spec.generationSeed))

    /** Build the routes with the given lifecycle status and run `GET /ready`. */
    private def getReady(status: NodeStatus): (Status, Json) =
        ActorSystem[IO]("ReadyEndpointTest")
            .use { system =>
                for {
                    requestSequencerStub <- system.actorOf(
                      new Actor[IO, RequestSequencer.Request] {
                          override def receive: Receive[IO, RequestSequencer.Request] =
                              _ => IO.pure(())
                      }
                    )
                    blockWeaverStub <- system.actorOf(
                      new Actor[IO, BlockWeaver.Request] {
                          override def receive: Receive[IO, BlockWeaver.Request] = _ => IO.pure(())
                      }
                    )
                    routes <- HydrozoaRoutes(
                      requestSequencerStub,
                      blockWeaverStub,
                      IO.pure(status),
                      ConsensusStoreReader.empty,
                      None,
                      multiNodeConfig.headConfig,
                      HydrozoaServer.Config(adminUsername = "admin", adminPassword = "admin"),
                      ContraTracer[IO, HydrozoaHttpEvent](_ => IO.unit)
                    )
                    resp <- routes.routes.orNotFound.run(Request[IO](Method.GET, uri"/ready"))
                    body <- resp.as[Json]
                } yield (resp.status, body)
            }
            .unsafeRunSync()

    private def statusLabel(body: Json): Option[String] =
        body.hcursor.downField("status").as[String].toOption

    test("GET /ready is 200 with status \"active\" when the node is Active") {
        val (status, body) = getReady(NodeStatus.Active)
        assert(
          status == Status.Ok && statusLabel(body).contains("active"),
          s"expected 200 + active, got $status + ${statusLabel(body)}"
        )
    }

    test("GET /ready is 503 with the lifecycle status when the node is not Active") {
        val (status, body) = getReady(NodeStatus.Initializing)
        assert(
          status == Status.ServiceUnavailable && statusLabel(body).contains("initializing"),
          s"expected 503 + initializing, got $status + ${statusLabel(body)}"
        )
    }

end ReadyEndpointTest
