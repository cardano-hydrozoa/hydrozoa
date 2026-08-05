package hydrozoa.integration.e2e

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.BuildInfo
import hydrozoa.app.cli.{DemoConfig, SubmitL2Transaction}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.multisig.consensus.UserRequest
import hydrozoa.multisig.consensus.UserRequestBody.TransactionRequestBody
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.server.ApiDto.{RequestIdView, mkRequestIdView}
import hydrozoa.multisig.server.{EutxoL2QueryClient, SubmissionClient}
import io.circe.Json
import io.circe.parser.parse
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Comparator
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*
import scala.sys.process.{Process, ProcessLogger}
import scala.util.Try
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.Value
import scalus.uplc.builtin.ByteString

/** Docker smoke-test: stand up **the head `docker-compose.yml` describes** — 2 head peers and 4
  * coil peers — on a local Yaci devnet, form a head, submit an L2 transaction to head-0 over HTTP,
  * and assert it reaches both head peers' L2 ledgers. Unlike the in-process `MultiPeerHeadHarness`,
  * this drives the shipped artifacts black-box: the packaged image, `docker compose`, the mesh, the
  * HTTP API, and L2 consensus across six distinct identities.
  *
  * It checks rather more than propagation — that the deployment procedure in DEPLOYMENT.md works at
  * all — hence "smoke test". See the E2E section of `docs/integration-stages.md`.
  *
  * '''The shipped topology, not a test-shaped one.''' `keygen-fleet 2 4 2` and the real
  * `docker-compose.yml` (plus the `docker-compose.yaci.yml` overlay for the devnet) are what an
  * operator runs, so a failure here is a failure of the documented path. Only head peers publish
  * the HTTP API — `runCoilNode` starts no `HydrozoaServer` — so the assertions cover head-0 and
  * head-1. The four coil peers are not idle: the head cannot initialize without `coilQuorum` of
  * them signing, so a broken coil surfaces as a `/ready` timeout rather than passing unnoticed.
  *
  * '''Devnet bring-up goes through `scripts/yaci-devnet.sh`''' — the same script DEPLOYMENT.md
  * hands an operator, rather than a Scala reimplementation that could drift from it. That script
  * owns every devnet-specific step: creating the devnet, describing its chain, and funding head-0
  * (a devnet has no faucet).
  *
  * It is **heavy** (minutes-long, needs Docker + a Yaci container + the built image) and is
  * **hard-excluded from CI** via `Tests.Exclude` in `build.sbt`, exactly like
  * `Stage1PropertiesYaci`. Run it with `just integration-e2e-docker`, which builds the image,
  * stages the launcher, then:
  * {{{
  *   sbt "integration/testOnly hydrozoa.integration.e2e.DockerSmokeTest"
  * }}}
  *
  * The flow mirrors DEPLOYMENT.md against a devnet:
  *   1. `yaci-devnet.sh up` — the devnet container, and a node inside it;
  *   2. `yaci-devnet.sh network` — the chain description, since a devnet has no baked-in one;
  *   3. `keygen-fleet 2 4 2 --cardano-network-file` — keys, roster, `defaults.json`, opening L2
  *      state, per-peer configs;
  *   4. `yaci-devnet.sh topup` — fund head peer 0;
  *   5. `deploy-scripts-and-g2-setup` — deploy the treasury/dispute validators (+ G2 ladder);
  *   6. `build-head-config` — resolve the ref UTxOs into the shared head config;
  *   7. `docker compose up` — all six peers;
  *   8. wait for `/ready`, submit an L2 tx to head-0, poll both head peers until convergence.
  *
  * URL split (the containers and the host reach the same devnet at different addresses): the peers
  * reach it in-mesh at `http://yaci:8080` (written into each `private.json` via the template's
  * `blockfrostApiUrl`), while the host-side generation steps reach it at the devnet's host-mapped
  * port (`localhost:18080`) via `--blockfrost-url`.
  */
class DockerSmokeTest extends AnyFunSuite:

    import DockerSmokeTest.*

    test("a head forms on the shipped topology and an L2 tx reaches both head peers") {
        // Prerequisites the `just integration-e2e-docker` recipe guarantees; cancel (not fail) when
        // a stray `testOnly *` reaches this excluded suite without them.
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

            // Steps 1-2 both come from the script, so this drives the documented commands rather
            // than a copy of them.
            _ <- log("bringing up the Yaci devnet…")
            _ <- devnet(home, "up")
            _ <- log("describing the devnet chain…")
            _ <- devnet(home, "network", networkPath(home).toString)

            _ <- log(s"keygen-fleet $HeadCount $CoilCount $CoilQuorum…")
            _ <- cli(
              "keygen-fleet",
              HeadCount.toString,
              CoilCount.toString,
              CoilQuorum.toString,
              "--home",
              home.toString,
              "--template",
              templatePath(home).toString,
              "--cardano-network-file",
              networkPath(home).toString
            )

            head0Funding <- cliCapture("head-zero-address", "--home", home.toString)
                .flatMap(out =>
                    IO.fromOption(lastNonBlankLine(out))(
                      RuntimeException("head-zero-address printed no address")
                    )
                )
            // `topup` returns only once the store has indexed the funds — deploy-scripts fetches
            // head-0's utxos once and hard-fails if none are present yet.
            _ <- log(s"topping up head-0 ($head0Funding) with $TopupAda ADA…")
            _ <- devnet(home, "topup", head0Funding, TopupAda.toString)

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

            _ <- log(s"docker compose up the $HeadCount head + $CoilCount coil peers…")
            _ <- compose(home, (Seq("up", "-d") ++ peerServices)*)

            _ <- log("waiting for /ready on both head peers (head must initialize on L1 first)…")
            _ <- pollUntil("the head peers to become ready", ReadyTimeout, 5.seconds)(
              allReady(client)
            )

            _ <- submitAndAssertPropagation(home, client)
            _ <- log("propagation confirmed on both head peers ✓")
        } yield ()

    /** Build the same zero-fee L2 tx `submit-l2-tx` would (spend head-0's opening output, send
      * [[SendAda]] to head-1), submit it to head-0, then poll both head peers until the resulting
      * utxo and feed entry appear on each. Loads head-0's wallet/headId/network offline, plus
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

            parsed <- EutxoL2QueryClient.http(client, headUri(0)).utxos(head0Address)
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

            _ <- log("polling both head peers for the propagated utxo…")
            _ <- pollUntil(s"utxo $txIdHex at head-1 on every peer", ConvergeTimeout, 3.seconds)(
              allPeersShowUtxo(client, head1Address, txIdHex)
            )
            _ <- pollUntil("our tx in every peer's /transactions feed", ConvergeTimeout, 3.seconds)(
              allPeersShowTransaction(client, expectedRequest)
            )
        } yield ()

    // ---- HTTP probes -------------------------------------------------------------------------

    /** `GET /ready` returns 200 on every HTTP-observable peer (head initialized and active). */
    private def allReady(client: Client[IO]): IO[Boolean] =
        headPeerIndices
            .traverse(i => client.get(headUri(i) / "ready")(r => IO.pure(r.status.code == 200)))
            .map(_.forall(identity))

    /** Every head peer's `GET /l2/cardano-eutxo/utxos/{head-1}` lists a utxo minted by our tx. */
    private def allPeersShowUtxo(
        client: Client[IO],
        addr: ShelleyAddress,
        txIdHex: String
    ): IO[Boolean] =
        headPeerIndices
            .traverse(i => EutxoL2QueryClient.http(client, headUri(i)).utxos(addr))
            .map(_.forall(_.exists((input, _) => input.transactionId.toHex == txIdHex)))

    /** Every head peer's `GET /l2/cardano-eutxo/transactions` feed carries the entry for our
      * submitted request — matched by its request id, so the check is specific to the tx we sent.
      */
    private def allPeersShowTransaction(client: Client[IO], request: RequestIdView): IO[Boolean] =
        headPeerIndices
            .traverse(i => EutxoL2QueryClient.http(client, headUri(i)).transactions(RecentTxWindow))
            .map(_.forall(_.exists(_.requestId == request)))

    // ---- config authoring --------------------------------------------------------------------

    /** Write the peer-private template `keygen-fleet` fills, from the packaged scaffold template
      * with only the backend URL overridden to the in-mesh address the containers use.
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

    /** Run `scripts/yaci-devnet.sh`, the same entry point DEPLOYMENT.md documents.
      *
      * `COMPOSE_PROJECT_NAME` points it at this run's project, so the devnet it creates is the one
      * the peers join, and `HYDROZOA_BIN` at the staged launcher the recipe just built.
      */
    private def devnet(home: Path, args: String*): IO[Unit] =
        runProcess(
          devnetScript.toString +: args,
          cwd = Some(repoRoot.toFile),
          extraEnv = composeEnv(home) ++ Seq(
            "COMPOSE_PROJECT_NAME" -> ComposeProject,
            "HYDROZOA_BIN" -> launcher.toString
          )
        )

    /** Dump each container's recent logs (best-effort) — the failure diagnostic. */
    private def dumpLogs(home: Path): IO[Unit] =
        (List("yaci") ++ peerServices).traverse_ { svc =>
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
      * here); a keyless devnet ignores the value.
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

end DockerSmokeTest

object DockerSmokeTest:

    private val Tag = "[smoke]"

    /** The topology `docker-compose.yml` describes and DEPLOYMENT.md walks through:
      * `keygen-fleet 2 4 2`. Changing these means changing the shipped compose file too — nothing
      * derives one from the other.
      */
    private val HeadCount = 2
    private val CoilCount = 4
    private val CoilQuorum = 2

    /** The devnet's host-mapped Blockfrost port (`docker-compose.yaci.yml`). */
    private val HostBlockfrostUrl = "http://localhost:18080/api/v1"

    /** The in-mesh URL the containers use (compose service name `yaci`). */
    private val MeshBlockfrostUrl = "http://yaci:8080/api/v1"

    private val TopupAda = 100_000L // ADA to head-0 on the devnet
    private val SendAda = 2L // ADA moved head-0 → head-1 on L2
    private val RecentTxWindow = 50 // entries pulled from each peer's /transactions feed

    // Placeholder Blockfrost key for the keyless devnet — value is ignored (see `cli`).
    private val DummyBlockfrostKey = "preview00000000000000000000000000000000"

    // Generous, real-wall-clock budgets: head initialization lands on L1 before /ready flips.
    private val ReadyTimeout = 8.minutes
    private val ConvergeTimeout = 3.minutes

    /** Head peers publish the user HTTP API; coil peers dial out only (`runCoilNode` starts no
      * `HydrozoaServer`), so only these are observable over HTTP.
      */
    private val headPeerIndices: List[Int] = (0 until HeadCount).toList

    private val peerServices: List[String] =
        headPeerIndices.map(i => s"head-$i") ++ (0 until CoilCount).map(i => s"coil-$i").toList

    /** The local image the compose file runs; the `just` recipe builds it via `Docker/publishLocal`
      * at [[BuildInfo.version]]. `HYDROZOA_IMAGE` overrides it.
      */
    private val image: String =
        sys.env.getOrElse("HYDROZOA_IMAGE", s"cardano-hydrozoa/hydrozoa:${BuildInfo.version}")

    private def headUri(i: Int): Uri = Uri.unsafeFromString(s"http://localhost:${8080 + i}")

    private def templatePath(home: Path): Path =
        home.resolve("template").resolve("peer-private.template.json")

    /** Where `yaci-devnet.sh network` writes the devnet's chain description. */
    private def networkPath(home: Path): Path = home.resolve("network.json")

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

    /** The devnet harness DEPLOYMENT.md documents; the suite drives it rather than reimplementing
      * its steps.
      */
    private lazy val devnetScript: Path = {
        val path = repoRoot.resolve("scripts").resolve("yaci-devnet.sh")
        if !Files.isExecutable(path) then throw RuntimeException(s"$path is missing")
        path
    }

    private val ComposeProject = "hydrozoa-e2e"

    /** The real deployment file plus the Yaci overlay, both at the repo root. */
    private lazy val composeFiles: List[Path] =
        List("docker-compose.yml", "docker-compose.yaci.yml").map { name =>
            val path = repoRoot.resolve(name)
            if !Files.exists(path) then throw RuntimeException(s"$path is missing")
            path
        }

    private def lastNonBlankLine(s: String): Option[String] =
        s.linesIterator.map(_.trim).filter(_.nonEmpty).toList.lastOption

    private def readResource(name: String): String =
        val is = getClass.getResourceAsStream(name)
        if is == null then throw RuntimeException(s"classpath resource $name not found")
        try new String(is.readAllBytes(), StandardCharsets.UTF_8)
        finally is.close()

    private def commandSucceeds(cmd: Seq[String]): Boolean =
        Try(Process(cmd).!(ProcessLogger(_ => (), _ => ())) == 0).getOrElse(false)

end DockerSmokeTest
