package hydrozoa.app

import cats.effect.unsafe.IORuntimeConfig
import cats.effect.{ExitCode, IO}
import com.monovore.decline.effect.CommandIOApp
import com.monovore.decline.{Command, Opts}
import hydrozoa.BuildInfo
import hydrozoa.app.cli.{Scaffold, SubmitDeposit, SubmitL2Transaction}
import hydrozoa.bootstrap.{BuildHeadConfig, GenerateKeyPair, InitBootstrapFiles, KeygenFleet, Migrate, PrintHeadZeroAddress}
import hydrozoa.multisig.consensus.transport.ProtocolVersion
import hydrozoa.multisig.persistence.StoreVersion
import scala.concurrent.duration.DurationInt

/** The `hydrozoa` command-line entry point: a single dispatcher over every deployment and runtime
  * command. Each subcommand is defined next to its logic and surfaced here as a `Command` value:
  *
  *   - `serve` — run a head node ([[Serve]])
  *   - `evacuate` — run the rule-based regime standalone from a left-over database ([[Evacuate]])
  *   - `keygen-fleet` / `keygen` / `init-bootstrap-files` / `head-zero-address` /
  *     `deploy-scripts-and-g2-setup` / `build-head-config` — the bootstrap ladder ([[KeygenFleet]],
  *     [[GenerateKeyPair]], [[InitBootstrapFiles]], [[PrintHeadZeroAddress]],
  *     [[hydrozoa.app.DeployScriptsAndG2Setup]], [[BuildHeadConfig]])
  *   - `submit-deposit` / `submit-l2-tx` — drive a running head ([[SubmitDeposit]],
  *     [[SubmitL2Transaction]])
  *   - `migrate` — sweep a wallet ([[Migrate]])
  *   - `scaffold` — write the Docker workspace files for a repo-less user ([[Scaffold]])
  *   - `version` — print the three versions this build carries (also available as `--version`)
  *
  * This is the single main class packaged by native-packager and the Docker image entrypoint, so
  * `hydrozoa <subcommand> …` and `docker run hydrozoa <subcommand> …` share one vocabulary.
  */
object Main
    extends CommandIOApp(
      name = "hydrozoa",
      header = "Hydrozoa — multi-party state channels for Cardano",
      version = BuildInfo.version
    ):

    /** Bound how long shutdown may hang after a signal.
      *
      * cats-effect defaults `shutdownHookTimeout` to `Duration.Inf`, so a finalizer that cannot
      * complete makes the process ignore SIGTERM outright — it then needs SIGKILL, and anything
      * downstream of the stuck finalizer never releases. A wedged node holding its RocksDB LOCK is
      * the case that bit us: `docker stop -t 120` waits the full two minutes and kills it anyway.
      *
      * A finite bound converts that into a slower-than-usual exit. Finalizers past the deadline are
      * skipped, which is safe for the store specifically: the OS releases the lock on process death
      * and RocksDB recovers from its WAL, which is the same path any crash takes.
      *
      * This is a backstop, not a fix — a shutdown that needs it has a bug worth finding.
      */
    override protected def runtimeConfig: IORuntimeConfig =
        super.runtimeConfig.copy(shutdownHookTimeout = 30.seconds)

    /** The `version` subcommand: print the build identity baked in at compile time (see
      * [[BuildInfo]]) plus the protocol and store versions this build speaks.
      */
    private lazy val versionCommand: Command[IO[ExitCode]] =
        Command(
          name = "version",
          header = "Print the build identity plus the protocol and store versions"
        )(Opts.unit.map(_ => printVersion))

    override def main: Opts[IO[ExitCode]] =
        Opts.subcommands(
          Serve.command,
          Evacuate.command,
          KeygenFleet.command,
          GenerateKeyPair.command,
          InitBootstrapFiles.command,
          PrintHeadZeroAddress.command,
          DeployScriptsAndG2Setup.command,
          BuildHeadConfig.command,
          SubmitDeposit.command,
          SubmitL2Transaction.command,
          Migrate.command,
          Scaffold.command,
          versionCommand
        )

    /** The three versions this build carries (`docs/spec/versioning.md`). The protocol and store
      * versions are what an operator needs before an upgrade: they say which peers this build can
      * talk to and which data directories it can open, neither of which follows from the release
      * number.
      */
    private def printVersion: IO[ExitCode] =
        IO.println(
          s"hydrozoa ${BuildInfo.version}\n" +
              s"git:      ${BuildInfo.gitCommit}\n" +
              s"built:    ${BuildInfo.builtAtString}\n" +
              s"protocol: ${ProtocolVersion.current}\n" +
              s"store:    ${StoreVersion.current}"
        ).as(ExitCode.Success)

end Main
