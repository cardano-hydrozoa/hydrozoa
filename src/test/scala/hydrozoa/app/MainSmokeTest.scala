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

/** End-to-end sanity check: generates a 1-peer sample config in memory, writes it to a temp dir
  * (the head peer's mesh `webSocketAddress` carries port 0 from the test fixture, and `httpPort` is
  * patched to "0", so the OS picks free ephemeral ports), and runs [[Main.runNode]] against the
  * on-disk files. A mock [[hydrozoa.multisig.backend.cardano.CardanoBackend]] is pre-seeded with
  * the head's script-ref UTxOs so config decoding doesn't try to hit Blockfrost.
  *
  * The test passes when the HTTP server binds — [[HydrozoaHttpEvent.ServerStarted]] is the deepest
  * milestone we can reach without real network IO. It implies that all earlier startup steps
  * succeeded: actors spawned, `WatchingActors` fired, `connectionsDeferred` resolved, and Ember
  * bound on its port.
  */
class MainSmokeTest extends AnyFunSuite:

    test("Main.runNode reaches ServerStarted for a generated single-peer config") {
        val rootTmp = Files.createTempDirectory("hydrozoa-smoke-")
        val configDir = rootTmp.resolve("config")
        val dataDir = rootTmp.resolve("data")
        Files.createDirectories(configDir)
        Files.createDirectories(dataDir)

        val spec = defaultSpec.copy(outDir = configDir, nPeers = 1)
        val headPath = configDir.resolve("head-config.json")
        val privatePath = configDir.resolve("peer-0").resolve("private.json")

        // Generate the multi-node config in memory so we can both (a) write it to disk and (b)
        // pre-seed the mock backend with the same script-ref / seed UTxOs the decoder will ask
        // about.
        val mnc: MultiNodeConfig = MultiNodeConfig
            .generate(testPeersSpec(spec))()
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
                  Map(mnc.headConfig.seedUtxo.toTuple)
                      ++ mnc.headConfig.additionalFundingUtxos
                      ++ Map.from(mnc.headConfig.scriptReferenceUtxos.toList.map(_.toTuple))
              )
            )

            startedD <- Deferred[IO, Unit]
            observer = ContraTracer[IO, HydrozoaHttpEvent] {
                case _: HydrozoaHttpEvent.ServerStarted => startedD.complete(()).void
                case _                                  => IO.unit
            }

            fiber <- Main
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
                      "Main.runNode did not reach HydrozoaHttpEvent.ServerStarted within 30s"
                    )
                  )
                )
            _ <- outcome match {
                case Left(()) => IO.unit
                case Right(o) =>
                    IO.raiseError(
                      new AssertionError(
                        s"Main.runNode terminated before reaching ServerStarted: $o"
                      )
                    )
            }
            _ <- fiber.cancel
        } yield ()

        testIO.unsafeRunSync()
    }

end MainSmokeTest
