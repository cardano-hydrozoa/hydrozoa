package hydrozoa.app.cli

import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.monovore.decline.{Command, Opts}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.backend.cardano.CardanoNetworkDiscovery
import io.circe.syntax.*
import java.nio.file.{Files, Path, StandardCopyOption}

/** Print the chain a Blockfrost-compatible backend serves, as the `cardanoNetwork` block
  * `defaults.json` expects.
  *
  * For a chain that is not one of the three standard networks — a private or consortium devnet, a
  * self-hosted backend — there is no baked-in description to fall back on, and hand-writing a
  * `CardanoInfo` (protocol parameters, cost models, slot geometry) is not reasonable. This asks the
  * backend and prints the answer:
  *
  * {{{
  *   hydrozoa discover-network --blockfrost-url http://localhost:18080/api/v1 --out network.json
  *   hydrozoa init-bootstrap-files roster.json --cardano-network-file network.json \
  *     --blockfrost-url http://localhost:18080/api/v1
  * }}}
  *
  * The endpoint is named twice because it answers two questions: which chain to describe, and — in
  * `defaults.json` — which endpoint serves it from then on. A chain outside the three standard ones
  * has no public endpoint to fall back on, so omitting the second one fails at `build-head-config`.
  *
  * Deliberately a separate step rather than something `build-head-config` does implicitly: pinning
  * the value keeps a rebuild reproducible, and keeps the config-read path free of network
  * resolution. The result is refused if it carries a standard chain's protocol magic — configure
  * that chain by name instead, so its baked-in (Byron-aware) slot geometry is used.
  */
object DiscoverNetwork:

    private val blockfrostUrlOpt: Opts[String] =
        Opts.option[String](
          "blockfrost-url",
          "Blockfrost-compatible API base URL of the chain to describe"
        )

    private val apiKeyOpt: Opts[Option[String]] =
        Opts.option[String](
          "api-key",
          "Blockfrost API key (omit for a keyless endpoint)",
          short = "k"
        ).orElse(
          Opts.env[String]("BLOCKFROST_API_KEY", "Blockfrost API key for the Cardano backend")
        ).orNone

    private val outOpt: Opts[Option[Path]] =
        Opts.option[String]("out", "Write to this file instead of stdout", short = "o")
            .map(Path.of(_))
            .orNone

    /** The `discover-network` subcommand. */
    lazy val command: Command[IO[ExitCode]] =
        Command(
          name = "discover-network",
          header = "Describe the chain a Blockfrost-compatible backend serves, as JSON"
        )((blockfrostUrlOpt, apiKeyOpt, outOpt).mapN(discoverNetwork))

    private def discoverNetwork(
        blockfrostUrl: String,
        mbApiKey: Option[String],
        out: Option[Path]
    ): IO[ExitCode] =
        for {
            custom <- CardanoNetworkDiscovery.discover(blockfrostUrl, mbApiKey.getOrElse(""))
            json = (custom: CardanoNetwork).asJson.spaces2
            _ <- out.fold(IO.println(json))(path =>
                writeAtomically(path, json) *>
                    IO.println(s"Wrote the $blockfrostUrl chain description to $path")
            )
        } yield ExitCode.Success

    /** Write `content` to `path`, creating its parent directories, via a temporary file in the same
      * directory. A run interrupted mid-write leaves the previous file intact rather than a
      * truncated one that `--cardano-network-file` would later fail to parse.
      */
    private def writeAtomically(path: Path, content: String): IO[Unit] =
        IO.blocking {
            val parent = Option(path.toAbsolutePath.getParent)
            parent.foreach(Files.createDirectories(_))
            val tmp = Files.createTempFile(parent.get, path.getFileName.toString, ".tmp")
            try {
                Files.writeString(tmp, content)
                Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING)
                ()
            } catch { case e: Throwable => Files.deleteIfExists(tmp); throw e }
        }

end DiscoverNetwork
