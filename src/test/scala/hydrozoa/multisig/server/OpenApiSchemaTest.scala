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
import java.nio.file.{Files, Path}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite

/** Golden tests pinning the two committed OpenAPI documents to the schemas generated from the tapir
  * endpoint definitions: `docs/openapi.yaml` (the core node API) and `docs/openapi-eutxo-l2.yaml`
  * (the EUTXO L2-ledger query API, served only on a node running the EUTXO ledger). Each
  * regenerates its document and fails if the committed copy is stale, so the checked-in schemas can
  * never silently drift from the routes. Regenerate by re-running these tests (a stale run
  * overwrites the file) and committing the result.
  */
class OpenApiSchemaTest extends AnyFunSuite:

    private val coreSchema: Path = Path.of("docs/openapi.yaml")
    private val l2Schema: Path = Path.of("docs/openapi-eutxo-l2.yaml")

    private val spec = defaultSpec.copy(nPeers = 1)

    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig
            .generate(testPeersSpec(spec))()
            .pureApply(Gen.Parameters.default, Seed(spec.generationSeed))

    /** Build the routes and hand them to `use`. No L2 reader is needed: both schemas derive from
      * the endpoint definitions, not their handlers, so the L2 document is generated even with
      * `None`.
      */
    private def withRoutes[A](use: HydrozoaRoutes => A): A =
        ActorSystem[IO]("OpenApiSchemaTest")
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
                      IO.pure(NodeStatus.Active),
                      None,
                      multiNodeConfig.headConfig,
                      HydrozoaServer.Config(adminUsername = "admin", adminPassword = "admin"),
                      ContraTracer[IO, HydrozoaHttpEvent](_ => IO.unit)
                    )
                } yield use(routes)
            }
            .unsafeRunSync()

    /** Fail (after rewriting the file) if `committed` is out of date with `generated`. */
    private def checkGolden(path: Path, generated: String): Unit =
        val committed = if Files.exists(path) then Files.readString(path) else ""
        if committed != generated then
            Files.writeString(path, generated)
            fail(
              s"$path was out of date with the endpoint definitions and has been regenerated. " +
                  s"Review and commit the updated $path."
            )

    test("docs/openapi.yaml is up to date with the core tapir endpoint definitions") {
        checkGolden(coreSchema, withRoutes(_.openApiYaml))
    }

    test("docs/openapi-eutxo-l2.yaml is up to date with the EUTXO L2 endpoint definitions") {
        checkGolden(l2Schema, withRoutes(_.l2OpenApiYaml))
    }

end OpenApiSchemaTest
