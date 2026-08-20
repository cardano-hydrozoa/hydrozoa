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
import io.circe.syntax.*
import io.circe.{Json, Printer}
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
  * The bind alone is not enough to assert on: the consensus bootstrap runs concurrently with it, so
  * a test that only raced the milestone against `runNode` returning would pass whenever binding
  * merely won. This one also requires the node to outlive the bind.
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

        // The stock config encoders deliberately withhold signing keys (PeerWallet's encoder writes
        // an all-zero placeholder), so a serialized private config boots a node that cannot sign —
        // its stack-0 hard-ack self-verification then fails and terminates the actor system before
        // the HTTP server binds. Splice peer 0's real signing key back in — the same key TestPeers
        // derived for the generated config — so the on-disk config is actually runnable.
        val (_, peerSigningKey) = TestPeers.deriveScalusKeypair(spec.seedPhrase.mnemonic, 0)
        val runnablePrivateJson = peerPrivate.asJson.deepMerge(
          Json.obj(
            "ownPeerPrivate" -> Json.obj(
              "ownHeadWallet" -> Json.obj(
                "signingKey" -> Json.fromString(scodec.bits.ByteVector(peerSigningKey.bytes).toHex)
              )
            )
          )
        )

        val testIO = for {
            _ <- IO.blocking(
              Files.writeString(headPath, printer.print(mnc.headConfig.asJson))
            )
            _ <- IO.blocking(Files.createDirectories(privatePath.getParent))
            _ <- IO.blocking(
              Files.writeString(privatePath, printer.print(runnablePrivateJson))
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

            // The bind says nothing about the actors started alongside it. An escalation to the
            // guardian ends `runNode` whatever exit code it carries, so give it room to happen.
            _ <- IO.sleep(2.seconds)
            died <- fiber.join.as(true).timeoutTo(1.second, IO.pure(false))
            _ <- IO.raiseWhen(died)(
              new AssertionError(
                "Serve.runNode reached ServerStarted but the node then terminated; " +
                    "look for an [EventBus] => Error line naming kukku://hydrozoa-demo"
              )
            )

            // ⚠️ Do not await this. Cancelling a *live* node never returns, and `Fiber.cancel` is
            // itself uncancelable, so no timeout bounds it. The daemon threads go with the JVM.
            _ <- fiber.cancel.start.void
        } yield ()

        testIO.unsafeRunSync()
    }

end MainSmokeTest
