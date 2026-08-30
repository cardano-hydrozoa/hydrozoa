package hydrozoa.app

import cats.effect.{ExitCode, IO}
import com.monovore.decline.effect.CommandIOApp
import com.monovore.decline.{Command, Opts}
import hydrozoa.BuildInfo
import hydrozoa.app.cli.{Scaffold, SubmitDeposit, SubmitL2Transaction}
import hydrozoa.bootstrap.{BuildHeadConfig, GenerateKeyPair, InitBootstrapFiles, KeygenFleet, Migrate, PrintHeadZeroAddress}
import hydrozoa.rulebased.evacuator.DeinitSweep

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
  *   - `version` — print the baked build identity (also available as `--version`)
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

    /** The `version` subcommand: print the version, git revision, and build time baked in at
      * compile time (see [[BuildInfo]]).
      */
    private lazy val versionCommand: Command[IO[ExitCode]] =
        Command(
          name = "version",
          header = "Print the version, git commit, and build time"
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
          DeinitSweep.command,
          Scaffold.command,
          versionCommand
        )

    private def printVersion: IO[ExitCode] =
        IO.println(
          s"hydrozoa ${BuildInfo.version}\n" +
              s"git:   ${BuildInfo.gitCommit}\n" +
              s"built: ${BuildInfo.builtAtString}"
        ).as(ExitCode.Success)

end Main
