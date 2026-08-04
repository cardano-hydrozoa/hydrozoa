package hydrozoa.integration.e2e

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.BuildInfo
import hydrozoa.app.cli.{DemoConfig, SubmitL2Transaction}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.integration.yaci.DevKit
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.multisig.consensus.UserRequest
import hydrozoa.multisig.consensus.UserRequestBody.TransactionRequestBody
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.server.ApiDto.{L2TxSummaryView, L2UtxoView, RequestIdView, mkRequestIdView, given}
import hydrozoa.multisig.server.SubmissionClient
import io.circe.Json
import io.circe.parser.parse
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Comparator
import org.http4s.Uri
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*
import scala.sys.process.{Process, ProcessLogger}
import scala.util.Try
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.{Coin, Value}
import scalus.uplc.builtin.ByteString

/** End-to-end L2-propagation test: stand up **four real head-peer containers** on a local Yaci
  * devnet, form a head, then submit an L2 transaction to head-0 over HTTP and assert it propagates
  * to every peer's L2 ledger. Unlike the in-process `MultiPeerHeadHarness`, this drives the shipped
  * artifacts black-box — the packaged image, `docker compose`, the mesh, the HTTP API, and L2
  * consensus across four distinct identities. See the E2E section of `docs/integration-stages.md`.
  *
  * It is **heavy** (minutes-long, needs Docker + a Yaci container + the built image) and is
  * **hard-excluded from CI** via `Tests.Exclude` in `build.sbt`, exactly like
  * `Stage1PropertiesYaci`. Run it with `just integration-e2e-docker`, which builds the image,
  * stages the launcher, then:
  * {{{
  *   sbt "integration/testOnly hydrozoa.integration.e2e.DockerPropagationTest"
  * }}}
  *
  * The flow mirrors DEPLOYMENT.md §4→§6 against Yaci (custom network):
  *   1. bring up the Yaci devnet (the `docker-compose.yaci.yml` overlay) and create its node;
  *   2. `keygen-fleet 4 0 0` — keys, roster, `defaults.json`, opening L2 state, per-peer configs;
  *   3. `topup` head peer 0 on Yaci (no faucet);
  *   4. `deploy-scripts-and-g2-setup` — deploy the treasury/dispute validators (+ G2 ladder);
  *   5. `build-head-config` — resolve the ref UTxOs + the custom `CardanoInfo` live from Yaci;
  *   6. `docker compose up` the four `serve` containers;
  *   7. wait for `/ready` on all four, submit an L2 tx to head-0, poll all four until convergence.
  *
  * URL split (the containers and the host reach the same Yaci at different addresses): the peers
  * reach it in-mesh at `http://yaci:8080` (written into each `private.json` via the template's
  * `blockfrostApiUrl`), while the host-side generation steps reach it at Yaci's host-mapped port
  * (`localhost:18080`) via `--blockfrost-url`, which overrides the in-mesh URL recorded in
  * `defaults.json`.
  *
  * '''Incomplete:''' nothing yet writes the devnet's `CardanoInfo` into `defaults.json`, so this
  * suite cannot form a head as written. The Yaci harness that produces it, and the rework of this
  * suite onto the real `docker-compose.yml`, are tracked as WI-010 / WI-011.
  */
class DockerPropagationTest extends AnyFunSuite:

    import DockerPropagationTest.*

    test("an L2 tx submitted to head-0 propagates to all four Yaci-backed head peers") {
        // Prerequisites the `just integration-e2e-docker` recipe guarantees; cancel (not fail) when a
        // stray `testOnly *` reaches this excluded suite without them.
        if !commandSucceeds(Seq("docker", "--version")) then
            cancel("docker is not available on PATH")
        if !Files.isExecutable(launcher) then
            cancel(
              s"the staged launcher is missing at $launcher — run `just stage` (or `just integration-e2e-docker`)"
            )
        program.unsafeRunSync()
    }

    /** Create a throwaway head workspace, run the whole scenario against a fresh client, and always
      * tear the compose project down. On success the workspace is deleted; on failure it is kept
      * (and each container's logs dumped) for debugging.
      */
    private def program: IO[Unit] =
        makeHome.flatMap { home =>
            EmberClientBuilder.default[IO].build.use { client =>
                runScenario(home, client)
                    .onError(e =>
                        log(s"scenario failed: ${e.getMessage} — configs kept at $home") *>
                            dumpLogs(home).attempt.void
                    )
                    .guarantee(compose(home, "down", "-v", "--remove-orphans").attempt.void)
                    <* deleteRecursively(home).attempt.void
            }
        }

    private def runScenario(home: Path, client: Client[IO]): IO[Unit] =
        for {
            _ <- log(s"home=$home image=$image compose=${composeFiles.mkString(" + ")}")
            _ <- writePrivateTemplate(home)

            _ <- log("bringing up the Yaci devnet…")
            _ <- compose(home, "up", "-d", "yaci")
            _ <- createYaciDevnet(home)
            _ <- pollUntil("the Yaci devnet admin API", 2.minutes, 3.seconds)(
              IO.blocking(DevKit.devnetInfo()).as(true)
            )
            // The Blockfrost store API comes up a little after the node; the host-side CLI steps
            // (deploy-scripts / build-head-config) query it, so wait until it serves protocol params.
            _ <- log("waiting for the Yaci Blockfrost store API…")
            _ <- pollUntil("the Yaci Blockfrost store API", 3.minutes, 3.seconds)(
              yaciBlockfrostReady(client)
            )

            _ <- log("keygen-fleet 4 0 0…")
            _ <- cli(
              "keygen-fleet",
              "4",
              "0",
              "0",
              "--home",
              home.toString,
              "--template",
              templatePath(home).toString
            )

            head0Funding <- cliCapture("head-zero-address", "--home", home.toString)
                .flatMap(out =>
                    IO.fromOption(lastNonBlankLine(out))(
                      RuntimeException("head-zero-address printed no address")
                    )
                )
            _ <- log(s"topping up head-0 ($head0Funding) with ${TopupLovelace / 1_000_000L} ADA…")
            _ <- IO.blocking(DevKit.topup(parseShelley(head0Funding), Coin(TopupLovelace)))
            // Wait until the store has indexed the topup — deploy-scripts fetches head-0's utxos
            // once and hard-fails if none are present yet.
            _ <- pollUntil("head-0's funds to be indexed", 2.minutes, 3.seconds)(
              yaciAddressFunded(client, head0Funding)
            )

            _ <- log("deploy-scripts-and-g2-setup…")
            _ <- cli(
              "deploy-scripts-and-g2-setup",
              "--home",
              home.toString,
              "--blockfrost-url",
              HostBlockfrostUrl
            )

            _ <- log("build-head-config…")
            _ <- cli(
              "build-head-config",
              "--home",
              home.toString,
              "--blockfrost-url",
              HostBlockfrostUrl
            )

            _ <- log("docker compose up the four head peers…")
            _ <- compose(home, (Seq("up", "-d") ++ headServices)*)

            _ <- log("waiting for /ready on all four peers (head must initialize on L1 first)…")
            _ <- pollUntil("all four peers to become ready", ReadyTimeout, 5.seconds)(
              allReady(client)
            )

            _ <- submitAndAssertPropagation(home, client)
            _ <- log("propagation confirmed on all four peers ✓")
        } yield ()

    /** Build the same zero-fee L2 tx `submit-l2-tx` would (spend head-0's opening output, send
      * [[SendAda]] to head-1), submit it to head-0, then poll every peer until the resulting utxo
      * and feed entry appear everywhere. Loads head-0's wallet/headId/network offline, plus
      * head-1's wallet for the destination address.
      */
    private def submitAndAssertPropagation(home: Path, client: Client[IO]): IO[Unit] =
        for {
            demo0 <- DemoConfig.loadOffline(headConfigPath(home), privatePath(home, "head-0"))
            head1Wallet <- DemoConfig.readWallet(privatePath(home, "head-1"))
            _ <- runSubmit(demo0, head1Wallet, client)
        } yield ()

    private def runSubmit(
        demo0: DemoConfig.L2Demo,
        head1Wallet: PeerWallet,
        client: Client[IO]
    ): IO[Unit] =
        given CardanoNetwork.Section = demo0.cardanoNetwork
        val head0Address = demo0.wallet.exportVerificationKey.shelleyAddress()
        val head1Address = head1Wallet.exportVerificationKey.shelleyAddress()
        for {
            head0Bech32 <- IO.fromOption(head0Address.toBech32.toOption)(
              RuntimeException("head-0 address is not bech32-renderable")
            )
            head1Bech32 <- IO.fromOption(head1Address.toBech32.toOption)(
              RuntimeException("head-1 address is not bech32-renderable")
            )

            views <- client.expect[List[L2UtxoView]](
              headUri(0) / "l2" / "cardano-eutxo" / "utxos" / head0Bech32
            )
            parsed <- IO.fromEither(
              views
                  .traverse(SubmitL2Transaction.parseUtxoView)
                  .left
                  .map(e => RuntimeException(s"could not parse head-0's L2 utxos: $e"))
            )
            selected <- IO.fromOption(parsed.headOption)(
              RuntimeException(s"head-0 has no opening L2 utxo at $head0Bech32")
            )
            (input, output) = selected

            tx <- IO.fromEither(
              SubmitL2Transaction
                  .buildTx(demo0.headId, input, output, head1Address, Value.ada(SendAda))
                  .left
                  .map(e => RuntimeException(s"could not build the L2 tx: $e"))
            )
            signed = demo0.wallet.signTx(tx)
            txIdHex = signed.id.toHex
            _ <- log(s"submitting L2 tx $txIdHex ($SendAda ADA head-0 → head-1) to head-0…")
            requestId <- SubmissionClient
                .http(client, headUri(0))
                .submit(
                  UserRequest.TransactionRequest(
                    TransactionRequestBody(ByteString.fromArray(signed.toCbor))
                  )
                )
            expectedRequest = mkRequestIdView(requestId)

            _ <- log("polling all four peers for the propagated utxo…")
            _ <- pollUntil(s"utxo $txIdHex at head-1 on every peer", ConvergeTimeout, 3.seconds)(
              allPeersShowUtxo(client, head1Bech32, txIdHex)
            )
            _ <- pollUntil("our tx in every peer's /transactions feed", ConvergeTimeout, 3.seconds)(
              allPeersShowTransaction(client, expectedRequest)
            )
        } yield ()

    // ---- HTTP probes -------------------------------------------------------------------------

    /** The Yaci Blockfrost store is ready once it serves protocol parameters — the exact query the
      * host-side `deploy-scripts` / `build-head-config` make first.
      */
    private def yaciBlockfrostReady(client: Client[IO]): IO[Boolean] =
        client.get(Uri.unsafeFromString(s"$HostBlockfrostUrl/epochs/latest/parameters"))(r =>
            IO.pure(r.status.isSuccess)
        )

    /** The store has indexed funds for `addr` once its Blockfrost utxos endpoint returns a
      * non-empty list (404 while the address is still unseen raises and is retried by the caller's
      * poll).
      */
    private def yaciAddressFunded(client: Client[IO], addr: String): IO[Boolean] =
        client
            .expect[List[Json]](Uri.unsafeFromString(s"$HostBlockfrostUrl/addresses/$addr/utxos"))
            .map(_.nonEmpty)

    /** `GET /ready` returns 200 on every peer (head initialized and active). */
    private def allReady(client: Client[IO]): IO[Boolean] =
        peerIndices
            .traverse(i => client.get(headUri(i) / "ready")(r => IO.pure(r.status.code == 200)))
            .map(_.forall(identity))

    /** Every peer's `GET /l2/cardano-eutxo/utxos/{head-1}` lists a utxo minted by our tx. */
    private def allPeersShowUtxo(client: Client[IO], addr: String, txIdHex: String): IO[Boolean] =
        peerIndices
            .traverse(i =>
                client.expect[List[L2UtxoView]](
                  headUri(i) / "l2" / "cardano-eutxo" / "utxos" / addr
                )
            )
            .map(_.forall(_.exists(_.input.transaction_id == txIdHex)))

    /** Every peer's `GET /l2/cardano-eutxo/transactions` feed carries the entry for our submitted
      * request — matched by its request id, so the check is specific to the tx we sent.
      */
    private def allPeersShowTransaction(client: Client[IO], request: RequestIdView): IO[Boolean] =
        peerIndices
            .traverse(i =>
                client.expect[List[L2TxSummaryView]](
                  (headUri(i) / "l2" / "cardano-eutxo" / "transactions").withQueryParam("count", 50)
                )
            )
            .map(_.forall(_.exists(_.requestId == request)))

    // ---- config authoring --------------------------------------------------------------------

    /** Write the peer-private template `keygen-fleet` fills, from the packaged scaffold template
      * with only the two Yaci URL fields overridden to the in-mesh addresses the containers use.
      */
    private def writePrivateTemplate(home: Path): IO[Unit] =
        IO.blocking {
            val base = parse(readResource("/scaffold/peer-private.template.json"))
                .fold(e => throw RuntimeException(s"bad scaffold template: $e"), identity)
            val patched = base.deepMerge(
              Json.obj("blockfrostApiUrl" -> Json.fromString(MeshBlockfrostUrl))
            )
            val path = templatePath(home)
            Files.createDirectories(path.getParent)
            Files.writeString(path, patched.spaces2)
            ()
        }

    // ---- process orchestration ---------------------------------------------------------------

    /** Create + start the devnet inside the idle `yaci` container (fast 1s block/slot for a
      * wall-clock test). Run **detached** (`exec -d`): `create-node --start` runs the node in the
      * foreground, so a blocking `exec` would never return — detaching launches it and returns
      * immediately, and the subsequent admin-API readiness poll arbitrates success. Lenient: a
      * non-zero exit (e.g. a devnet already exists) is left to that poll.
      */
    private def createYaciDevnet(home: Path): IO[Unit] =
        log("creating the Yaci devnet (create-node --start, detached)…") *>
            runProcessLenient(
              composeCmd(
                "exec",
                "-d",
                "yaci",
                "/app/yaci-cli.sh",
                "create-node",
                "-o",
                "--start",
                "--block-time",
                "1",
                "--slot-length",
                "1"
              ),
              composeEnv(home)
            )

    /** Dump each container's recent logs (best-effort) — the failure diagnostic. */
    private def dumpLogs(home: Path): IO[Unit] =
        (List("yaci") ++ headServices).traverse_ { svc =>
            log(s"──────── docker logs: $svc ────────") *>
                runProcessLenient(
                  composeCmd("logs", "--no-color", "--tail", "200", svc),
                  composeEnv(home)
                )
        }

    private def compose(home: Path, args: String*): IO[Unit] =
        runProcess(composeCmd(args*), cwd = None, extraEnv = composeEnv(home))

    /** The shipped `docker-compose.yml` plus the Yaci overlay — the same pair, in the same order,
      * that DEPLOYMENT.md tells an operator to run, so this suite exercises the real deployment
      * rather than a test-only copy of it. `-p` isolates the run from an operator's own project.
      */
    private def composeCmd(args: String*): Seq[String] =
        Seq("docker", "compose", "-p", ComposeProject) ++
            composeFiles.flatMap(f => Seq("-f", f.toString)) ++ args

    private def composeEnv(home: Path): Seq[(String, String)] =
        Seq("HYDROZOA_HOME" -> home.toString, "HYDROZOA_IMAGE" -> image)

    /** Run a staged-launcher subcommand from the repo root, failing on a non-zero exit.
      *
      * `BLOCKFROST_API_KEY` is set so `deploy-scripts` / `build-head-config` take the key from the
      * env instead of falling back to reading the default `head/template/…json.local` (absent
      * here); a keyless Yaci devnet ignores the value, and `--blockfrost-url` (not the key's
      * prefix) selects the Custom network.
      */
    private def cli(args: String*): IO[Unit] =
        runProcess(
          launcher.toString +: args,
          cwd = Some(repoRoot.toFile),
          extraEnv = Seq("BLOCKFROST_API_KEY" -> DummyBlockfrostKey)
        )

    /** Run a staged-launcher subcommand and return its stdout (for `head-zero-address`). */
    private def cliCapture(args: String*): IO[String] =
        IO.blocking {
            Process(launcher.toString +: args, repoRoot.toFile)
                .!!(ProcessLogger(line => println(s"$Tag $line")))
        }

    private def runProcess(
        cmd: Seq[String],
        cwd: Option[File],
        extraEnv: Seq[(String, String)]
    ): IO[Unit] =
        IO.blocking {
            val captured = new StringBuilder
            val logger = ProcessLogger { line =>
                val _ = captured.append(line).append('\n')
                println(s"$Tag $line")
            }
            val code = Process(cmd, cwd, extraEnv*).!(logger)
            if code != 0 then
                throw RuntimeException(
                  s"command failed (exit $code): ${cmd.mkString(" ")}\n$captured"
                )
        }

    private def runProcessLenient(cmd: Seq[String], extraEnv: Seq[(String, String)]): IO[Unit] =
        IO.blocking {
            val code =
                Process(cmd, None, extraEnv*).!(ProcessLogger(line => println(s"$Tag $line")))
            if code != 0 then println(s"$Tag (non-fatal) exit $code: ${cmd.mkString(" ")}")
        }

    // ---- small utilities ---------------------------------------------------------------------

    /** Repeat `check` until it returns true, or raise after `timeout`. Exceptions (a peer not up
      * yet, a connection refused) count as "not ready" and are retried until the deadline.
      */
    private def pollUntil(what: String, timeout: FiniteDuration, interval: FiniteDuration)(
        check: IO[Boolean]
    ): IO[Unit] =
        IO.monotonic.flatMap { start =>
            def loop: IO[Unit] =
                check.attempt.flatMap {
                    case Right(true) => IO.unit
                    case other =>
                        IO.monotonic.flatMap { now =>
                            if now - start >= timeout then
                                IO.raiseError(
                                  RuntimeException(
                                    s"timed out after $timeout waiting for $what" +
                                        other.left.toOption.fold("")(e =>
                                            s" (last error: ${e.getMessage})"
                                        )
                                  )
                                )
                            else IO.sleep(interval) *> loop
                        }
                }
            loop
        }

    private def log(msg: String): IO[Unit] = IO.println(s"$Tag $msg")

    private def makeHome: IO[Path] = IO.blocking(Files.createTempDirectory("hydrozoa-e2e"))

    /** Best-effort recursive delete (deepest-first) of the throwaway workspace. */
    private def deleteRecursively(dir: Path): IO[Unit] =
        IO.blocking {
            if Files.exists(dir) then {
                val walk = Files.walk(dir)
                try walk.sorted(Comparator.reverseOrder()).forEach(p => Files.delete(p))
                finally walk.close()
            }
        }

end DockerPropagationTest

object DockerPropagationTest:

    private val Tag = "[e2e]"
    private val HeadCount = 4

    /** Yaci's host-mapped Blockfrost port (`docker-compose.yaci.yml`). */
    private val HostBlockfrostUrl = "http://localhost:18080/api/v1"

    /** The in-mesh URL the containers use (compose service name `yaci`). */
    private val MeshBlockfrostUrl = "http://yaci:8080/api/v1"

    private val TopupLovelace = 100_000_000_000L // 100k ADA to head-0 on the devnet
    private val SendAda = 2L // ADA moved head-0 → head-1 on L2

    // Placeholder Blockfrost key for the keyless Yaci devnet — value is ignored (see `cli`).
    private val DummyBlockfrostKey = "preview00000000000000000000000000000000"

    // Generous, real-wall-clock budgets: head initialization lands on L1 before /ready flips.
    private val ReadyTimeout = 8.minutes
    private val ConvergeTimeout = 3.minutes

    private val peerIndices: List[Int] = (0 until HeadCount).toList
    private val headServices: List[String] = peerIndices.map(i => s"head-$i")

    /** The local image the compose file runs; the `just` recipe builds it via `Docker/publishLocal`
      * at [[BuildInfo.version]]. `HYDROZOA_IMAGE` overrides it.
      */
    private val image: String =
        sys.env.getOrElse("HYDROZOA_IMAGE", s"cardano-hydrozoa/hydrozoa:${BuildInfo.version}")

    private def headUri(i: Int): Uri = Uri.unsafeFromString(s"http://localhost:${8080 + i}")

    private def templatePath(home: Path): Path =
        home.resolve("template").resolve("peer-private.template.json")

    private def headConfigPath(home: Path): Path =
        home.resolve("head-config").resolve("head-config.json")

    private def privatePath(home: Path, peer: String): Path =
        home.resolve("private").resolve(peer).resolve("private.json")

    /** The repo root — the nearest ancestor of the working directory carrying a `build.sbt`. */
    private lazy val repoRoot: Path =
        var dir = Paths.get("").toAbsolutePath
        while dir != null && !Files.exists(dir.resolve("build.sbt")) do dir = dir.getParent
        if dir == null then
            throw RuntimeException("could not locate the repo root (no build.sbt above cwd)")
        dir

    private lazy val launcher: Path =
        repoRoot
            .resolve("target")
            .resolve("universal")
            .resolve("stage")
            .resolve("bin")
            .resolve("hydrozoa")

    private val ComposeProject = "hydrozoa-e2e"

    /** The real deployment file plus the Yaci overlay, both at the repo root. */
    private lazy val composeFiles: List[Path] =
        List("docker-compose.yml", "docker-compose.yaci.yml").map { name =>
            val path = repoRoot.resolve(name)
            if !Files.exists(path) then throw RuntimeException(s"$path is missing")
            path
        }

    private def parseShelley(bech32: String): ShelleyAddress =
        Address.fromBech32(bech32) match
            case sa: ShelleyAddress => sa
            case other              => throw RuntimeException(s"not a Shelley address: $other")

    private def lastNonBlankLine(s: String): Option[String] =
        s.linesIterator.map(_.trim).filter(_.nonEmpty).toList.lastOption

    private def readResource(name: String): String =
        val is = getClass.getResourceAsStream(name)
        if is == null then throw RuntimeException(s"classpath resource $name not found")
        try new String(is.readAllBytes(), StandardCharsets.UTF_8)
        finally is.close()

    private def commandSucceeds(cmd: Seq[String]): Boolean =
        Try(Process(cmd).!(ProcessLogger(_ => (), _ => ())) == 0).getOrElse(false)

end DockerPropagationTest
