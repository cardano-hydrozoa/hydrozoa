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
import hydrozoa.multisig.server.ApiDto.{L2TxKindView, L2TxSummaryView, L2UtxoView, given}
import hydrozoa.multisig.server.SubmissionClient
import io.circe.Json
import io.circe.parser.parse
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
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
  * consensus across four distinct identities. See `docs/local/integration/design.md`.
  *
  * It is **heavy** (minutes-long, needs Docker + a Yaci container + the built image) and is
  * **hard-excluded from CI** via `Tests.Exclude` in `build.sbt`, exactly like `Stage1PropertiesYaci`.
  * Run it with `just integration-e2e-docker`, which builds the image, stages the launcher, then:
  * {{{
  *   sbt "integration/testOnly hydrozoa.integration.e2e.DockerPropagationTest"
  * }}}
  *
  * The flow mirrors DEPLOYMENT.md §4→§6 against Yaci (custom network):
  *   1. bring up the Yaci devnet (compose `yaci` profile) and create its node;
  *   2. `keygen-fleet 4 0 0` — keys, roster, `defaults.json`, opening L2 state, per-peer configs;
  *   3. `topup` head peer 0 on Yaci (no faucet);
  *   4. `deploy-scripts-and-g2-setup` — deploy the treasury/dispute validators (+ G2 ladder);
  *   5. `build-head-config` — resolve the ref UTxOs + the custom `CardanoInfo` live from Yaci;
  *   6. `docker compose up` the four `serve` containers;
  *   7. wait for `/ready` on all four, submit an L2 tx to head-0, poll all four until convergence.
  *
  * URL split (the containers and the host reach the same Yaci at different addresses): the peers
  * reach it in-mesh at `http://yaci:8080` (written into each `private.json` via the template's
  * `cardanoBackendUrl`), while the host-side generation steps (`deploy-scripts`, `build-head-config`)
  * reach it at Yaci's host-mapped ports (`localhost:18080` / `localhost:10000`). Because
  * `keygen-fleet` copies the one template URL into both the peers' `private.json` and
  * `defaults.json`'s pending-custom marker, this test keeps the template on the mesh URL and rewrites
  * only the `defaults.json` marker to the host ports before the host-side build steps
  * ([[rewriteDefaultsMarkerToHost]]).
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
      * tear the compose project down; on failure, dump each container's logs and keep the workspace.
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
            }
        }

    private def runScenario(home: Path, client: Client[IO]): IO[Unit] =
        for {
            _ <- log(s"home=$home image=$image compose=$composeFile")
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
            _ <- rewriteDefaultsMarkerToHost(home)

            head0Funding <- cliCapture("head-zero-address", "--home", home.toString)
                .flatMap(out =>
                    IO.fromOption(lastNonBlankLine(out))(
                      RuntimeException("head-zero-address printed no address")
                    )
                )
            _ <- log(s"topping up head-0 ($head0Funding) with ${TopupLovelace / 1_000_000L} ADA…")
            _ <- IO.blocking(DevKit.topup(parseShelley(head0Funding), Coin(TopupLovelace)))

            _ <- log("deploy-scripts-and-g2-setup…")
            _ <- cli(
              "deploy-scripts-and-g2-setup",
              "--home",
              home.toString,
              "--blockfrost-url",
              HostBlockfrostUrl,
              "--yaci-admin-url",
              HostYaciAdminUrl
            )

            _ <- log("build-head-config…")
            _ <- cli("build-head-config", "--home", home.toString)

            _ <- log("docker compose up the four head peers…")
            _ <- compose(home, (Seq("up", "-d") ++ headServices)*)

            _ <- log("waiting for /ready on all four peers (head must initialize on L1 first)…")
            _ <- pollUntil("all four peers to become ready", ReadyTimeout, 5.seconds)(allReady(client))

            _ <- submitAndAssertPropagation(home, client)
            _ <- log("propagation confirmed on all four peers ✓")
        } yield ()

    /** Build the same zero-fee L2 tx `submit-l2-tx` would (spend head-0's opening output, send
      * [[SendAda]] to head-1), submit it to head-0, then poll every peer until the resulting utxo and
      * feed entry appear everywhere. Loads head-0's wallet/headId/network offline, plus head-1's
      * wallet for the destination address.
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
            _ <- SubmissionClient
                .http(client, headUri(0))
                .submit(
                  UserRequest.TransactionRequest(
                    TransactionRequestBody(ByteString.fromArray(signed.toCbor))
                  )
                )

            _ <- log("polling all four peers for the propagated utxo…")
            _ <- pollUntil(s"utxo $txIdHex at head-1 on every peer", ConvergeTimeout, 3.seconds)(
              allPeersShowUtxo(client, head1Bech32, txIdHex)
            )
            _ <- pollUntil("the tx in every peer's /transactions feed", ConvergeTimeout, 3.seconds)(
              allPeersShowTransaction(client)
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

    /** Every peer's `GET /l2/cardano-eutxo/transactions` feed carries a `transaction` entry. */
    private def allPeersShowTransaction(client: Client[IO]): IO[Boolean] =
        peerIndices
            .traverse(i =>
                client.expect[List[L2TxSummaryView]](
                  (headUri(i) / "l2" / "cardano-eutxo" / "transactions").withQueryParam("count", 50)
                )
            )
            .map(_.forall(_.exists(_.kind == L2TxKindView.Transaction)))

    // ---- config authoring --------------------------------------------------------------------

    /** Write the peer-private template `keygen-fleet` fills, from the packaged scaffold template with
      * only the two Yaci URL fields overridden to the in-mesh addresses the containers use.
      */
    private def writePrivateTemplate(home: Path): IO[Unit] =
        IO.blocking {
            val base = parse(readResource("/scaffold/peer-private.template.json"))
                .fold(e => throw RuntimeException(s"bad scaffold template: $e"), identity)
            val patched = base.deepMerge(
              Json.obj(
                "cardanoBackendUrl" -> Json.fromString(MeshBlockfrostUrl),
                "yaciAdminUrl" -> Json.fromString(MeshYaciAdminUrl)
              )
            )
            val path = templatePath(home)
            Files.createDirectories(path.getParent)
            Files.writeString(path, patched.spaces2)
            ()
        }

    /** Point `defaults.json`'s pending-custom marker at Yaci's host-mapped ports, so the host-side
      * `deploy-scripts` / `build-head-config` resolve the custom `CardanoInfo` from the host while the
      * peers keep the in-mesh URL in their `private.json` (see the class doc's URL-split note).
      */
    private def rewriteDefaultsMarkerToHost(home: Path): IO[Unit] =
        IO.blocking {
            val path = home.resolve("bootstrap").resolve("defaults.json")
            val json = parse(Files.readString(path))
                .fold(e => throw RuntimeException(s"bad defaults.json: $e"), identity)
            val patched = json.deepMerge(
              Json.obj(
                "cardanoNetwork" -> Json.obj(
                  "customBackend" -> Json.obj(
                    "blockfrostUrl" -> Json.fromString(HostBlockfrostUrl),
                    "yaciAdminUrl" -> Json.fromString(HostYaciAdminUrl)
                  )
                )
              )
            )
            Files.writeString(path, patched.spaces2)
            ()
        } *> log("rewrote defaults.json's custom-backend marker to Yaci's host-mapped ports")

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

    private def composeCmd(args: String*): Seq[String] =
        Seq("docker", "compose", "-f", composeFile.toString, "--profile", "yaci") ++ args

    private def composeEnv(home: Path): Seq[(String, String)] =
        Seq("HYDROZOA_HOME" -> home.toString, "HYDROZOA_IMAGE" -> image)

    /** Run a staged-launcher subcommand from the repo root, failing on a non-zero exit.
      *
      * `BLOCKFROST_API_KEY` is set so `deploy-scripts` / `build-head-config` take the key from the
      * env instead of falling back to reading the default `head/template/…json.local` (absent here);
      * a keyless Yaci devnet ignores the value, and `--blockfrost-url` (not the key's prefix) selects
      * the Custom network.
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
        cwd: Option[java.io.File],
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
            val code = Process(cmd, None, extraEnv*).!(ProcessLogger(line => println(s"$Tag $line")))
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

end DockerPropagationTest

object DockerPropagationTest:

    private val Tag = "[e2e]"
    private val HeadCount = 4

    /** Yaci's host-mapped ports (`docker-compose.yaci.yml`): Blockfrost API 18080, admin 10000. */
    private val HostBlockfrostUrl = "http://localhost:18080/api/v1"
    private val HostYaciAdminUrl = "http://localhost:10000/local-cluster/api"

    /** The in-mesh URLs the containers use (compose service name `yaci`). */
    private val MeshBlockfrostUrl = "http://yaci:8080/api/v1"
    private val MeshYaciAdminUrl = "http://yaci:10000/local-cluster/api"

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
        if dir == null then throw RuntimeException("could not locate the repo root (no build.sbt above cwd)")
        dir

    private lazy val launcher: Path =
        repoRoot.resolve("target").resolve("universal").resolve("stage").resolve("bin").resolve("hydrozoa")

    private lazy val composeFile: Path =
        val url = getClass.getResource("/e2e/docker-compose.yaci.yml")
        if url == null then
            throw RuntimeException("resource /e2e/docker-compose.yaci.yml is not on the test classpath")
        Paths.get(url.toURI)

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
