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
import hydrozoa.multisig.metrics.PeerMetrics
import hydrozoa.multisig.persistence.{ArchiveWatermarks, ConsensusStoreReader}
import io.circe.Json
import io.circe.syntax.*
import org.http4s.circe.*
import org.http4s.headers.Authorization
import org.http4s.implicits.*
import org.http4s.{BasicCredentials, Method, Request, Status}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite

/** `POST /api/admin/archive/watermark`: an attached archiver reporting how far it has durably
  * copied each column family.
  *
  * The endpoint records and answers; deciding what may be deleted on the strength of it is
  * retention's job, so these tests are about what the node accepts, refuses, and holds.
  */
class ArchiveWatermarkEndpointTest extends AnyFunSuite:

    private val spec = defaultSpec.copy(nPeers = 1)

    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig
            .generate(testPeersSpec(spec))()
            .pureApply(Gen.Parameters.default, Seed(spec.generationSeed))

    private val admin = BasicCredentials("admin", "admin")

    private def body(watermarks: (String, Long)*): Json =
        Json.obj("watermarks" -> watermarks.toMap.asJson)

    /** Post `payload` against routes built with `watermarks`, and return the outcome.
      *
      * `watermarks = None` is a node whose private config declares no archiver — the case where the
      * route is not mounted at all.
      */
    private def post(
        watermarks: Option[ArchiveWatermarks],
        payload: Json,
        credentials: Option[BasicCredentials] = Some(admin)
    ): (Status, Json) =
        ActorSystem[IO]("ArchiveWatermarkEndpointTest")
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
                      Some(requestSequencerStub),
                      blockWeaverStub,
                      IO.pure(NodeStatus.Active),
                      ConsensusStoreReader.empty,
                      None,
                      watermarks,
                      multiNodeConfig.headConfig,
                      HydrozoaServer.Config(adminUsername = "admin", adminPassword = "admin"),
                      PeerMetrics.create(0L, Vector.empty),
                      ContraTracer[IO, HydrozoaHttpEvent](_ => IO.unit)
                    )
                    request = Request[IO](Method.POST, uri"/api/admin/archive/watermark")
                        .withEntity(payload)
                    authorized = credentials.fold(request)(c =>
                        request.putHeaders(Authorization(c))
                    )
                    resp <- routes.routes.orNotFound.run(authorized)
                    json <- resp.as[Json].handleError(_ => Json.Null)
                } yield (resp.status, json)
            }
            .unsafeRunSync()

    private def floor(json: Json, family: String): Option[Long] =
        json.hcursor.downField("effectiveFloor").downField(family).as[Long].toOption

    test("a report is accepted and held") {
        val held = ArchiveWatermarks.empty()
        val (status, json) = post(Some(held), body("Block" -> 472021L, "Request:0" -> 510941L))

        assert(status == Status.Ok, s"expected 200, got $status"): Unit
        assert(floor(json, "Block").contains(472021L)): Unit
        assert(floor(json, "Request:0").contains(510941L))
    }

    /** A rebuilt archive holds less than it did, and the node cannot un-delete what it removed on
      * the strength of the earlier report — so the higher figure stands and the lower is ignored.
      */
    test("a watermark below one already held does not lower it") {
        val held = ArchiveWatermarks.empty()
        post(Some(held), body("Block" -> 500L)): Unit
        val (status, json) = post(Some(held), body("Block" -> 100L))

        assert(status == Status.Ok, s"expected 200, got $status"): Unit
        assert(floor(json, "Block").contains(500L), "the node lowered a watermark it already held")
    }

    /** Families are resolved against this node's own set. An archiver reading a different node's
      * store would otherwise get a 200 and a watermark nobody will ever read.
      */
    test("a column family this node does not have is refused") {
        val (status, json) = post(Some(ArchiveWatermarks.empty()), body("Request:99" -> 1L))

        assert(status == Status.BadRequest, s"expected 400, got $status"): Unit
        assert(
          json.hcursor.downField("error").as[String].exists(_.contains("Request:99")),
          s"the error should name the family it refused: $json"
        )
    }

    test("a valid family alongside an unknown one refuses the whole report") {
        val held = ArchiveWatermarks.empty()
        val (status, _) = post(Some(held), body("Block" -> 5L, "NotAFamily" -> 1L))

        assert(status == Status.BadRequest, s"expected 400, got $status"): Unit
        assert(
          held.watermarks.isEmpty,
          "a refused report must record nothing, not the half it understood"
        )
    }

    test("an unauthenticated report is refused and re-advertises the challenge") {
        val (status, _) =
            post(Some(ArchiveWatermarks.empty()), body("Block" -> 1L), credentials = None)
        assert(status == Status.Unauthorized, s"expected 401, got $status")
    }

    test("a report with the wrong password is refused") {
        val (status, _) = post(
          Some(ArchiveWatermarks.empty()),
          body("Block" -> 1L),
          credentials = Some(BasicCredentials("admin", "not-the-password"))
        )
        assert(status == Status.Unauthorized, s"expected 401, got $status")
    }

    /** No archiver declared, no route. The node then deletes as soon as consensus allows rather
      * than waiting on a report that would never come.
      */
    test("the route is absent when no archiver is declared") {
        val (status, _) = post(None, body("Block" -> 1L))
        assert(status == Status.NotFound, s"expected 404, got $status")
    }

end ArchiveWatermarkEndpointTest
