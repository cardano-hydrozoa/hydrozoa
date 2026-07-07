package hydrozoa.multisig.server

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.GenerateSampleConfig.{defaultSpec, testPeersSpec}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.{BlockWeaver, RequestSequencer}
import hydrozoa.multisig.ledger.l2.{L2LedgerReader, L2TxSummary}
import java.nio.file.{Files, Path}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.Utxos

/** Golden test pinning the committed `docs/openapi.yaml` to the schema generated from the tapir
  * endpoint definitions. It regenerates the document and fails if the committed copy is stale, so
  * the checked-in schema can never silently drift from the routes. Regenerate by re-running this
  * test (a stale run overwrites the file) and committing the result.
  */
class OpenApiSchemaTest extends AnyFunSuite:

    private val committedSchema: Path = Path.of("docs/openapi.yaml")

    private val spec = defaultSpec.copy(nPeers = 1)

    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig
            .generate(testPeersSpec(spec))()
            .pureApply(Gen.Parameters.default, Seed(spec.generationSeed))

    /** A no-op reader — the schema depends only on the endpoint descriptions, not the handlers. */
    private val emptyReader: L2LedgerReader[IO] = new L2LedgerReader[IO] {
        override def utxosByAddress(address: Address): IO[Utxos] = IO.pure(Map.empty)
        override def recentTransactions(limit: Int): IO[Vector[L2TxSummary]] = IO.pure(Vector.empty)
    }

    private def generatedYaml: String =
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
                      emptyReader,
                      multiNodeConfig.headConfig,
                      HydrozoaServer.Config(adminUsername = "admin", adminPassword = "admin"),
                      ContraTracer[IO, HydrozoaHttpEvent](_ => IO.unit)
                    )
                } yield routes.openApiYaml
            }
            .unsafeRunSync()

    test("docs/openapi.yaml is up to date with the tapir endpoint definitions") {
        val generated = generatedYaml
        val committed =
            if Files.exists(committedSchema) then Files.readString(committedSchema) else ""
        if committed != generated then
            Files.writeString(committedSchema, generated)
            fail(
              "docs/openapi.yaml was out of date with the endpoint definitions and has been " +
                  "regenerated. Review and commit the updated docs/openapi.yaml."
            )
    }

end OpenApiSchemaTest
