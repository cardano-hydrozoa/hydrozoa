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

/** Stand up a whole head in containers on a local Yaci devnet and form it, then hand the formed
  * head to a subclass's [[scenario]]. Unlike the in-process `MultiPeerHeadHarness`, this drives the
  * shipped artifacts black-box: the packaged image, `docker compose`, the mesh, the HTTP API, and
  * L2 consensus across distinct node identities.
  *
  * Bringing a head up costs minutes, so everything up to "every head peer reports `/ready`" lives
  * here and every subclass shares it; what varies is the [[DockerTopology]] it runs on and the
  * [[scenario]] it then asserts. Subclasses also inherit the L2 traffic helpers ([[sendAda]],
  * [[awaitPropagation]]) and the container controls ([[killPeer]], [[startPeer]]). See
  * [[DockerSmokeTest]] for L2 propagation on the deployment we ship, [[DockerRecoveryTest]] for
  * killing a head peer and recovering it, and the E2E section of `docs/spec/integration-stages.md`
  * for where this level sits among the others.
  *
  * '''Only head peers are observable.''' `runCoilNode` starts no `HydrozoaServer`, so the HTTP
  * assertions cover head peers alone. Coil peers are still load-bearing: the head cannot initialize
  * without `coilQuorum` of them signing, so a broken coil surfaces as a `/ready` timeout rather
  * than passing unnoticed.
  *
  * '''Devnet bring-up goes through `scripts/yaci-devnet.sh`''' — the same script
  * docs/user-guide/DEPLOYMENT.md hands an operator, rather than a Scala reimplementation that could
  * drift from it. That script owns every devnet-specific step: creating the devnet, describing its
  * chain, and funding head-0 (a devnet has no faucet).
  *
  * These suites are **heavy** (minutes-long, needing Docker + a Yaci container + the built image)
  * and **hard-excluded from CI** by FQN via `Tests.Exclude` in `build.sbt`, like
  * `Stage1PropertiesYaci` — so a new subclass must be added there too, or CI will start running it.
  *
  * The flow mirrors docs/user-guide/DEPLOYMENT.md against a devnet:
  *   1. `scaffold` — the head workspace, `docker-compose.yml` included;
  *   2. `yaci-devnet.sh up` — the devnet container, and a node inside it;
  *   3. `yaci-devnet.sh network` — the chain description, since a devnet has no baked-in one;
  *   4. `keygen-fleet <heads> <coils> <quorum> --cardano-network-file` — keys, roster,
  *      `defaults.json`, opening L2 state, per-peer configs;
  *   5. `yaci-devnet.sh topup` — fund head peer 0;
  *   6. `deploy-scripts-and-g2-setup` — deploy the treasury/dispute validators (+ G2 ladder);
  *   7. `build-head-config` — resolve the ref UTxOs into the shared head config;
  *   8. `docker compose up` — every peer;
  *   9. wait for `/ready` on every head peer — after which the subclass's [[scenario]] runs.
  *
  * URL split (the containers and the host reach the same devnet at different addresses): the peers
  * reach it in-mesh at `http://yaci:8080` (written into each `private.json` via the template's
  * `blockfrostApiUrl`), while the host-side generation steps reach it at the devnet's host-mapped
  * port (`localhost:18080`) via `--blockfrost-url`.
  */
abstract class DockerHeadSuite(topology: DockerTopology, scenarioName: String) extends AnyFunSuite:

    import DockerHeadSuite.*

    private val HeadCount = topology.heads
    private val CoilCount = topology.coils
    private val CoilQuorum = topology.coilQuorum
    private val ComposeProject = topology.project
    private val Tag = topology.tag
    private val composeOverlays = topology.composeOverlays

    /** Head peers publish the user HTTP API; coil peers dial out only (`runCoilNode` starts no
      * `HydrozoaServer`), so only these are observable over HTTP.
      */
    private val headPeerIndices: List[Int] = (0 until HeadCount).toList

    private val peerServices: List[String] =
        headPeerIndices.map(i => s"head-$i") ++ (0 until CoilCount).map(i => s"coil-$i").toList

    /** What this suite asserts once the head has formed and every head peer reports `/ready`. The
      * workspace is the one [[formHead]] scaffolded, so a scenario can read the generated configs
      * and drive `docker compose` against the running project.
      */
    protected def scenario(home: Path, client: Client[IO]): IO[Unit]

    test(s"a head forms on the ${topology.name} topology and $scenarioName") {
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
                (formHead(home, client) *> scenario(home, client))
                    .onError(e =>
                        log(s"scenario failed: ${e.getMessage} — configs kept at $home") *>
                            dumpLogs(home).attempt.void
                    )
                    .guarantee(compose(home, "down", "-v", "--remove-orphans").attempt.void)
                    <* deleteRecursively(home).attempt.void
            }
        }

    /** Steps 1-9 of the deployment flow: scaffold the workspace, bring the devnet up, generate the
      * fleet, deploy the scripts, start every peer, and wait for the head to open on L1.
      */
    private def formHead(home: Path, client: Client[IO]): IO[Unit] =
        for {
            _ <- log(s"home=$home image=$image compose=${composeFiles(home).mkString(" + ")}")
            _ <- writePrivateTemplate(home)

            // The workspace is scaffolded exactly as an operator's is, so the compose file the
            // rest of the run drives is the shipped one, materialized the documented way.
            _ <- log("scaffolding the head workspace…")
            _ <- cli("scaffold", home.toString)

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
        } yield ()

    // ---- L2 traffic --------------------------------------------------------------------------

    /** Load the two ends of the suite's L2 traffic from the generated workspace: head-0's offline
      * demo config (wallet, head id, network) and head-1's wallet, whose address every [[sendAda]]
      * pays. Both read files, so they survive a peer being down.
      */
    protected def loadL2Wallets(home: Path): IO[L2Wallets] =
        for {
            sender <- DemoConfig.loadOffline(headConfigPath(home), privatePath(home, "head-0"))
            recipient <- DemoConfig.readWallet(privatePath(home, "head-1"))
        } yield L2Wallets(sender, recipient)

    /** Build the same zero-fee L2 tx `submit-l2-tx` would — spend head-0's largest L2 output, send
      * [[SendAda]] to head-1, keep the change — sign it, and submit it to head-0. Returns as soon
      * as head-0 has assigned a request id; landing it on every peer is [[awaitPropagation]]'s job,
      * so a scenario can submit while consensus cannot progress.
      *
      * The input is re-read from head-0's live utxo set on every call, so repeated sends chain off
      * the previous one's change.
      */
    protected def sendAda(wallets: L2Wallets, client: Client[IO]): IO[SentTransaction] =
        given CardanoNetwork.Section = wallets.sender.cardanoNetwork
        val head0Address = wallets.sender.wallet.exportVerificationKey.shelleyAddress()
        val head1Address = wallets.recipient.exportVerificationKey.shelleyAddress()
        for {
            head0Bech32 <- IO.fromOption(head0Address.toBech32.toOption)(
              RuntimeException("head-0 address is not bech32-renderable")
            )

            parsed <- EutxoL2QueryClient.http(client, headUri(0)).utxos(head0Address)
            selected <- IO.fromOption(parsed.maxByOption(_._2.value.coin.value))(
              RuntimeException(s"head-0 has no spendable L2 utxo at $head0Bech32")
            )
            (input, output) = selected

            tx <- IO.fromEither(
              SubmitL2Transaction
                  .buildTx(wallets.sender.headId, input, output, head1Address, Value.ada(SendAda))
                  .left
                  .map(e => RuntimeException(s"could not build the L2 tx: $e"))
            )
            signed = wallets.sender.wallet.signTx(tx)
            txIdHex = signed.id.toHex
            _ <- log(s"submitting L2 tx $txIdHex ($SendAda ADA head-0 → head-1) to head-0…")
            requestId <- SubmissionClient
                .http(client, headUri(0))
                .submit(
                  UserRequest.TransactionRequest(
                    TransactionRequestBody(ByteString.fromArray(signed.toCbor))
                  )
                )
        } yield SentTransaction(txIdHex, mkRequestIdView(requestId), head1Address)

    /** Poll every head peer until `sent` shows up in both of its L2 views — the utxo it paid to
      * head-1 and the entry in its `/transactions` feed — or [[ConvergeTimeout]] elapses.
      */
    protected def awaitPropagation(client: Client[IO], sent: SentTransaction): IO[Unit] =
        for {
            _ <- log(s"polling every head peer for L2 tx ${sent.txIdHex}…")
            _ <- pollUntil(
              s"utxo ${sent.txIdHex} at head-1 on every peer",
              ConvergeTimeout,
              3.seconds
            )(allPeersShowUtxo(client, sent.destination, sent.txIdHex))
            _ <- pollUntil("our tx in every peer's /transactions feed", ConvergeTimeout, 3.seconds)(
              allPeersShowTransaction(client, sent.request)
            )
        } yield ()

    // ---- HTTP probes -------------------------------------------------------------------------

    /** `GET /ready` returns 200 on every HTTP-observable peer (head initialized and active). */
    protected def allReady(client: Client[IO]): IO[Boolean] =
        headPeerIndices
            .traverse(i => client.get(headUri(i) / "ready")(r => IO.pure(r.status.code == 200)))
            .map(_.forall(identity))

    /** Whether head peer `peer` answers `GET /health` at all — false while its container is down,
      * so a scenario can tell "killed" from "still serving".
      */
    protected def peerResponds(client: Client[IO], peer: Int): IO[Boolean] =
        client
            .get(headUri(peer) / "health")(r => IO.pure(r.status.code == 200))
            .attempt
            .map(_.getOrElse(false))

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

    // ---- container controls ------------------------------------------------------------------

    /** SIGKILL `service`'s container — an abrupt crash, with no SIGTERM and no chance to flush, so
      * what the peer left in its RocksDB store is what a real crash would have left.
      *
      * `docker compose kill` is a manual stop as far as the daemon is concerned, so the compose
      * file's `restart: on-failure` policy does *not* resurrect the container and the peer stays
      * down until [[startPeer]].
      */
    protected def killPeer(home: Path, service: String): IO[Unit] =
        compose(home, "kill", service)

    /** Start `service`'s existing container again. This is a restart, not a re-creation: the same
      * container comes back on the same named volume, so the peer boots off the store its crash
      * left behind.
      */
    protected def startPeer(home: Path, service: String): IO[Unit] =
        compose(home, "start", service)

    /** `service`'s container exit code, as the daemon recorded it — 137 (128 + SIGKILL) after a
      * [[killPeer]], which is how a scenario confirms the peer really died rather than exiting on
      * its own.
      */
    protected def peerExitCode(home: Path, service: String): IO[Int] =
        for {
            id <- composeCapture(home, "ps", "-a", "-q", service).flatMap(out =>
                IO.fromOption(lastNonBlankLine(out))(
                  RuntimeException(s"docker compose ps found no container for $service")
                )
            )
            raw <- captureProcess(
              Seq("docker", "inspect", "-f", "{{.State.ExitCode}}", id),
              extraEnv = Seq.empty
            )
            code <- IO.fromOption(lastNonBlankLine(raw).flatMap(_.toIntOption))(
              RuntimeException(s"could not read $service's exit code from: $raw")
            )
        } yield code

    // ---- process orchestration ---------------------------------------------------------------

    /** Run `scripts/yaci-devnet.sh`, the same entry point the deployment guide documents.
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
                  composeCmd(home, "logs", "--no-color", "--tail", "200", svc),
                  composeEnv(home)
                )
        }

    private def compose(home: Path, args: String*): IO[Unit] =
        runProcess(composeCmd(home, args*), cwd = None, extraEnv = composeEnv(home))

    /** [[compose]], but returning the command's stdout instead of only echoing it. */
    private def composeCapture(home: Path, args: String*): IO[String] =
        captureProcess(composeCmd(home, args*), composeEnv(home))

    /** The head directory's scaffolded `docker-compose.yml` plus the Yaci overlay — the same pair,
      * in the same order, that the deployment guide tells an operator to run, so this suite
      * exercises the real deployment rather than a test-only copy of it. The first `-f` also fixes
      * the project directory, so the file's `./head-config` and `./private` mounts resolve under
      * the workspace. `-p` isolates the run from an operator's own project.
      */
    private def composeFiles(home: Path): List[Path] =
        home.resolve("docker-compose.yml") :: composeOverlays

    private def composeCmd(home: Path, args: String*): Seq[String] =
        Seq("docker", "compose", "-p", ComposeProject) ++
            composeFiles(home).flatMap(f => Seq("-f", f.toString)) ++ args

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

    /** [[runProcess]] that returns stdout. Stderr still goes to the console, so a failing command
      * reports itself the same way.
      */
    private def captureProcess(cmd: Seq[String], extraEnv: Seq[(String, String)]): IO[String] =
        IO.blocking {
            val captured = new StringBuilder
            val logger = ProcessLogger(
              line => { val _ = captured.append(line).append('\n') },
              line => println(s"$Tag $line")
            )
            val code = Process(cmd, None, extraEnv*).!(logger)
            if code != 0 then
                throw RuntimeException(s"command failed (exit $code): ${cmd.mkString(" ")}")
            captured.toString
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
    protected def pollUntil(what: String, timeout: FiniteDuration, interval: FiniteDuration)(
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

    /** Fail the scenario unless `condition` holds, with `message` as the reason. */
    protected def ensure(condition: Boolean, message: String): IO[Unit] =
        IO.raiseUnless(condition)(RuntimeException(message))

    protected def log(msg: String): IO[Unit] = IO.println(s"$Tag $msg")

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

end DockerHeadSuite

object DockerHeadSuite:

    /** The two ends of the L2 traffic a scenario sends: head-0, whose offline demo config carries
      * the wallet, head id, and network needed to build and sign, and head-1's wallet, whose
      * address every [[DockerHeadSuite.sendAda]] pays.
      */
    private[e2e] final case class L2Wallets(sender: DemoConfig.L2Demo, recipient: PeerWallet)

    /** An L2 transaction a scenario submitted, and everything needed to recognize it afterwards: in
      * a peer's utxo set (a `txIdHex` output at `destination`) and in its `/transactions` feed (the
      * `request` head-0 assigned it).
      */
    private[e2e] final case class SentTransaction(
        txIdHex: String,
        request: RequestIdView,
        destination: ShelleyAddress
    )

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
    private[e2e] lazy val repoRoot: Path =
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

    /** The devnet harness docs/user-guide/DEPLOYMENT.md documents; the suite drives it rather than
      * reimplementing its steps.
      */
    private lazy val devnetScript: Path = {
        val path = repoRoot.resolve("scripts").resolve("yaci-devnet.sh")
        if !Files.isExecutable(path) then throw RuntimeException(s"$path is missing")
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

end DockerHeadSuite
