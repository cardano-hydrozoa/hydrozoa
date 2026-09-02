package hydrozoa.app

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import hydrozoa.config.GenerateSampleConfig.{defaultSpec, testPeersSpec}
import hydrozoa.config.head.HeadConfig.headConfigEncoder
import hydrozoa.config.node.NodePrivateConfig.nodePrivateConfigEncoder
import hydrozoa.config.node.{MultiNodeConfig, PrivateSecrets}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.server.HydrozoaHttpEvent
import io.circe.syntax.*
import io.circe.{Json, Printer}
import java.nio.file.{Files, Path}
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
        val generatedPrivate = mnc.nodePrivateConfigs(HeadPeerNumber(0))
        // ⛔ Clear the transplant tag. `generateNodeOperationMultisigConfig` draws it from
        // `Gen.option` on purpose — a codec round-trip has to cover both shapes of the field — but
        // this test boots a NORMAL peer, and a normal peer carries no transplant tag. Left in, the
        // config claims to be a transplant of a stack no empty store can hold, which is a refusal,
        // and whether it appears at all depends on the generator seed.
        val peerPrivate = generatedPrivate.copy(
          httpPort = "0",
          nodeOperationMultisigConfig = generatedPrivate.nodeOperationMultisigConfig
              .copy(transplantStackNumber = None)
        )

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
              Files.writeString(headPath, printer.print(mnc.headConfig.asJson)): Unit
            )
            _ <- IO.blocking(writePrivatePair(runnablePrivateJson, privatePath))

            mockBackend <- CardanoBackendMock.mockIO(
              MockState(initialUtxos =
                  mnc.headConfig.initializationTx.resolvedUtxos.utxos
                      ++ Map.from(mnc.headConfig.scriptReferenceUtxos.toList.map(_.toTuple))
              ),
              // The node refuses to start when the chain's protocol parameters differ from the ones
              // its config asserts, so the mock must report the config's. Without this it reports
              // scalus's `UtxoEnv.testMainnet` fixture, which matches no real network's CardanoInfo
              // -- the boot then refuses, correctly, on an incoherent fixture.
              reportedParams = Some(mnc.headConfig.cardanoProtocolParams)
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

    /** Split a private config into the file a node reads and the credentials beside it.
      *
      * Credentials no longer live in `private.json` — the node reads them from the environment or
      * from a `private.env` sibling — so a test that writes a runnable config has to write both
      * halves. Doing it here rather than hand-rolling per test keeps every test on the real path.
      */
    private def writePrivatePair(json: Json, privatePath: Path): Unit = {
        val walletField =
            if json.hcursor.downField("ownPeerPrivate").downField("ownHeadWallet").succeeded then
                "ownHeadWallet"
            else "ownCoilWallet"
        val paths = List(
          "HYDROZOA_SIGNING_KEY" -> List("ownPeerPrivate", walletField, "signingKey"),
          "HYDROZOA_RULE_BASED_SIGNING_KEY" ->
              List("nodeOperationEvacuationConfig", "ruleBasedWallet", "signingKey"),
          "HYDROZOA_BLOCKFROST_API_KEY" -> List("blockfrostApiKey"),
          "HYDROZOA_ADMIN_PASSWORD" -> List("adminPassword")
        )
        def get(j: Json, path: List[String]): Option[String] =
            path.foldLeft(Option(j))((acc, k) => acc.flatMap(_.asObject).flatMap(_.apply(k)))
                .flatMap(_.asString)
        def drop(j: Json, path: List[String]): Json = path match {
            case Nil         => j
            case last :: Nil => j.asObject.fold(j)(o => Json.fromJsonObject(o.remove(last)))
            case head :: rest =>
                j.asObject.fold(j)(o =>
                    Json.fromJsonObject(o.add(head, drop(o(head).getOrElse(Json.obj()), rest)))
                )
        }
        val found = paths.flatMap((env, path) => get(json, path).map(env -> _))
        val stripped = paths.foldLeft(json)((acc, kv) => drop(acc, kv._2))
        val printer = Printer.spaces2.copy(dropNullValues = true)
        Files.createDirectories(privatePath.getParent)
        Files.writeString(privatePath, printer.print(stripped)): Unit
        Files.writeString(
          privatePath.resolveSibling(PrivateSecrets.defaultFileName),
          found.map((k, v) => s"$k=$v").mkString("", "\n", "\n")
        ): Unit
    }

    // A `transplantStackNumber` names the stack this peer elects to ADOPT: everything at or below it
    // is taken on trust from the donor committee and never verified. So the tag has to name a stack
    // the store actually holds. Here the store is empty, so no tag can be honoured -- the node must
    // refuse rather than silently ignore the tag and boot as though none had been set.
    //
    // Asserting on the REASON, not just the type: this refusal has to be distinguishable from the
    // other ways a node can refuse to start, or the test would pass on the wrong one.
    test("Serve.runNode refuses a transplantStackNumber the store does not contain") {
        val rootTmp = Files.createTempDirectory("hydrozoa-transplant-tag-")
        val configDir = rootTmp.resolve("config")
        val dataDir = rootTmp.resolve("data")
        Files.createDirectories(configDir)
        Files.createDirectories(dataDir)

        val spec = defaultSpec.copy(outDir = configDir, nPeers = 1)
        val headPath = configDir.resolve("head-config.json")
        val privatePath = configDir.resolve("peer-0").resolve("private.json")
        val mnc: MultiNodeConfig = MultiNodeConfig
            .generate(testPeersSpec(spec))()
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(spec.generationSeed))
        val peerPrivate = mnc.nodePrivateConfigs(HeadPeerNumber(0)).copy(httpPort = "0")
        val printer = Printer.spaces2.copy(dropNullValues = true)
        val (_, peerSigningKey) = TestPeers.deriveScalusKeypair(spec.seedPhrase.mnemonic, 0)
        val runnablePrivateJson = peerPrivate.asJson
            .deepMerge(
              Json.obj(
                "ownPeerPrivate" -> Json.obj(
                  "ownHeadWallet" -> Json.obj(
                    "signingKey" -> Json.fromString(
                      scodec.bits.ByteVector(peerSigningKey.bytes).toHex
                    )
                  )
                )
              )
            )
            .deepMerge(
              Json.obj(
                "nodeOperationMultisigConfig" -> Json.obj(
                  "transplantStackNumber" -> Json.fromInt(999999)
                )
              )
            )

        val testIO = for {
            _ <- IO.blocking(Files.writeString(headPath, printer.print(mnc.headConfig.asJson)))
            _ <- IO.blocking(writePrivatePair(runnablePrivateJson, privatePath))
            mockBackend <- CardanoBackendMock.mockIO(
              MockState(initialUtxos =
                  mnc.headConfig.initializationTx.resolvedUtxos.utxos
                      ++ Map.from(mnc.headConfig.scriptReferenceUtxos.toList.map(_.toTuple))
              )
            )
            outcome <- Serve
                .runNode(headPath, privatePath, dataDir, backendOverride = Some(mockBackend))
                .attempt
                .timeoutTo(
                  60.seconds,
                  IO.raiseError(new AssertionError("runNode neither refused nor returned in 60s"))
                )
            _ <- outcome match {
                case Left(r: StartupRefusal) if r.reason.contains("transplantStackNumber") =>
                    IO.unit
                case Left(r: StartupRefusal) =>
                    IO.raiseError(
                      new AssertionError(s"refused, but for another reason: ${r.reason}")
                    )
                case Left(other) =>
                    IO.raiseError(new AssertionError(s"expected a StartupRefusal, got: $other"))
                case Right(code) =>
                    IO.raiseError(
                      new AssertionError(
                        s"expected a StartupRefusal, but the node started and exited $code"
                      )
                    )
            }
        } yield ()

        testIO.unsafeRunSync()
    }

    // The positive test above hands the mock the config's own protocol parameters, so the boot-time
    // comparison always agrees. That would leave the REFUSAL path untested -- a check that cannot
    // fail is not a check -- so this is its negative control: the same node, booted against a chain
    // that reports DIFFERENT parameters, must refuse rather than start and quietly build L1
    // transactions the chain may reject.
    test("Serve.runNode refuses to start when the chain's protocol parameters differ") {
        val rootTmp = Files.createTempDirectory("hydrozoa-pparams-mismatch-")
        val configDir = rootTmp.resolve("config")
        val dataDir = rootTmp.resolve("data")
        Files.createDirectories(configDir)
        Files.createDirectories(dataDir)

        val spec = defaultSpec.copy(outDir = configDir, nPeers = 1)
        val headPath = configDir.resolve("head-config.json")
        val privatePath = configDir.resolve("peer-0").resolve("private.json")
        val mnc: MultiNodeConfig = MultiNodeConfig
            .generate(testPeersSpec(spec))()
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(spec.generationSeed))
        val peerPrivate = mnc.nodePrivateConfigs(HeadPeerNumber(0)).copy(httpPort = "0")
        val printer = Printer.spaces2.copy(dropNullValues = true)
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
            _ <- IO.blocking(Files.writeString(headPath, printer.print(mnc.headConfig.asJson)))
            _ <- IO.blocking(writePrivatePair(runnablePrivateJson, privatePath))

            // No `reportedParams`: the mock falls back to scalus's `UtxoEnv.testMainnet` fixture,
            // which matches no real network's `CardanoInfo`. That IS the mismatch under test.
            mockBackend <- CardanoBackendMock.mockIO(
              MockState(initialUtxos =
                  mnc.headConfig.initializationTx.resolvedUtxos.utxos
                      ++ Map.from(mnc.headConfig.scriptReferenceUtxos.toList.map(_.toTuple))
              )
            )

            outcome <- Serve
                .runNode(headPath, privatePath, dataDir, backendOverride = Some(mockBackend))
                .attempt
                .timeoutTo(
                  60.seconds,
                  IO.raiseError(new AssertionError("runNode neither refused nor returned in 60s"))
                )
            _ <- outcome match {
                case Left(_: StartupRefusal) => IO.unit
                case Left(other) =>
                    IO.raiseError(
                      new AssertionError(s"expected a StartupRefusal, got: $other")
                    )
                case Right(code) =>
                    IO.raiseError(
                      new AssertionError(
                        s"expected a StartupRefusal, but the node started and exited $code"
                      )
                    )
            }
        } yield ()

        testIO.unsafeRunSync()
    }

end MainSmokeTest
