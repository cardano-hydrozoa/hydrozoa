package hydrozoa.app

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import hydrozoa.config.GenerateSampleConfig.{defaultSpec, testPeersSpec}
import hydrozoa.config.head.HeadConfig.headConfigEncoder
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.config.node.NodePrivateConfig.nodePrivateConfigEncoder
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.server.HydrozoaHttpEvent
import io.circe.Printer
import io.circe.syntax.*
import java.nio.file.Files
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt
import test.TestPeers

/** End-to-end sanity check: generates a 1-peer sample config in memory, writes it to a temp dir
  * (the head peer's mesh `webSocketAddress` carries port 0 from the test fixture, and `httpPort` is
  * patched to "0", so the OS picks free ephemeral ports), and runs [[Serve.runNode]] against the
  * on-disk files. A mock [[hydrozoa.multisig.backend.cardano.CardanoBackend]] is pre-seeded with
  * the head's script-ref UTxOs so config decoding doesn't try to hit Blockfrost.
  *
  * The test passes when the HTTP server binds — [[HydrozoaHttpEvent.ServerStarted]] is the deepest
  * milestone we can reach without real network IO. It implies that all earlier startup steps
  * succeeded: actors spawned, `WatchingActors` fired, `connectionsDeferred` resolved, and Ember
  * bound on its port.
  *
  * Reaching that milestone is necessary but not sufficient: the node's consensus bootstrap runs
  * concurrently with the bind, so the test also requires it to still be alive afterwards. Racing
  * the milestone against `runNode` returning would pass whenever binding merely won, which is how a
  * node that died of a hard-ack `BadSignature` on every run went unnoticed for two months.
  *
  * The peers are keyed [[TestPeers.KeyScheme.Ed25519]] rather than the default BIP32 for the same
  * reason the node has to survive: a BIP32 wallet does not survive the JSON round trip below, and
  * the node would load a signing key its own verification key does not match.
  */
class MainSmokeTest extends AnyFunSuite:

    test("Serve.runNode reaches ServerStarted for a generated single-peer config") {
        val rootTmp = Files.createTempDirectory("hydrozoa-smoke-")
        val configDir = rootTmp.resolve("config")
        val dataDir = rootTmp.resolve("data")
        Files.createDirectories(configDir)
        Files.createDirectories(dataDir)

        val spec = defaultSpec.copy(outDir = configDir, nPeers = 1)
        val headPath = configDir.resolve("head-config.json")
        val privatePath = configDir.resolve("peer-0").resolve("private.json")

        val testPeers = TestPeers(
          seedPhrase = spec.seedPhrase,
          network = testPeersSpec(spec).network,
          peersNumber = spec.nPeers,
          keyScheme = TestPeers.KeyScheme.Ed25519
        )

        // Generate the multi-node config in memory so we can both (a) write it to disk and (b)
        // pre-seed the mock backend with the same script-ref / seed UTxOs the decoder will ask
        // about.
        val mnc: MultiNodeConfig = MultiNodeConfig
            .generateWith(testPeers)()
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(spec.generationSeed))

        // Both the inter-peer mesh server (which binds where the head config advertises this peer —
        // the test fixture uses port 0) and the HTTP admin server (httpPort) bind OS-ephemeral
        // ports, so the test doesn't collide with whatever holds the generator's defaults.
        val peerPrivate = mnc
            .nodePrivateConfigs(HeadPeerNumber(0))
            .copy(httpPort = "0")

        val printer = Printer.spaces2.copy(dropNullValues = true)

        val testIO = for {
            _ <- IO.blocking(
              Files.writeString(headPath, printer.print(mnc.headConfig.asJson))
            )
            _ <- IO.blocking(Files.createDirectories(privatePath.getParent))
            _ <- IO.blocking(
              Files.writeString(privatePath, printer.print(peerPrivate.asJson))
            )

            mockBackend <- CardanoBackendMock.mockIO(
              MockState(initialUtxos =
                  mnc.headConfig.initializationTx.resolvedUtxos.utxos
                      ++ Map.from(mnc.headConfig.scriptReferenceUtxos.toList.map(_.toTuple))
              )
            )

            startedD <- Deferred[IO, Unit]
            observer = ContraTracer[IO, HydrozoaHttpEvent] {
                case _: HydrozoaHttpEvent.ServerStarted => startedD.complete(()).void
                case _                                  => IO.unit
            }

            fiber <- Serve
                .runNode(
                  headPath,
                  privatePath,
                  dataDir,
                  observer,
                  backendOverride = Some(mockBackend),
                )
                .start

            outcome <- IO
                .race(startedD.get, fiber.join)
                .timeoutTo(
                  30.seconds,
                  IO.raiseError(
                    new AssertionError(
                      "Serve.runNode did not reach HydrozoaHttpEvent.ServerStarted within 30s"
                    )
                  )
                )
            _ <- outcome match {
                case Left(()) => IO.unit
                case Right(o) =>
                    IO.raiseError(
                      new AssertionError(
                        s"Serve.runNode terminated before reaching ServerStarted: $o"
                      )
                    )
            }

            // The bind says nothing about the actors started alongside it. Give the consensus
            // bootstrap room to fail and require the node to outlive it: an escalation to the
            // guardian ends `runNode`, whatever exit code it carries.
            _ <- IO.sleep(2.seconds)
            died <- fiber.join.as(true).timeoutTo(1.second, IO.pure(false))
            _ <- IO.raiseWhen(died)(
              new AssertionError(
                "Serve.runNode reached ServerStarted but the node then terminated; " +
                    "look for an [EventBus] => Error line naming kukku://hydrozoa-demo"
              )
            )

            // Fire-and-forget: awaiting this never returns. Cancelling a *live* node deadlocks —
            // `Fiber.cancel` back-pressures and is itself uncancelable, so no timeout can bound it.
            // Until a node can be shut down on demand, the daemon threads go when the JVM does.
            _ <- fiber.cancel.start.void
        } yield ()

        testIO.unsafeRunSync()
    }

end MainSmokeTest
