package hydrozoa.app.cli

import cats.data.Validated
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

    /** The chain's slot geometry and magic, for a backend that serves no `/genesis` — all four
      * together or none at all.
      *
      * All-or-nothing rather than field-by-field defaulting: a chain half described by the backend
      * and half by the command line is the patched-together artifact this exists to avoid, and
      * there is no sound default for the missing half. Either the backend describes its chain, or
      * the caller does.
      */
    private val geometryOpt: Opts[Option[CardanoNetworkDiscovery.ChainGeometry]] =
        (
          Opts.option[Long](
            "system-start",
            "When slot 0 began, in epoch seconds (as `/genesis` reports it)"
          ).orNone,
          Opts.option[Double]("slot-length", "Seconds per slot; may be fractional").orNone,
          Opts.option[Long]("epoch-length", "Slots per epoch").orNone,
          Opts.option[Long]("protocol-magic", "The chain's network magic").orNone
        ).mapN((start, slot, epoch, magic) => (start, slot, epoch, magic))
            .mapValidated {
                case (Some(start), Some(slot), Some(epoch), Some(magic)) =>
                    Validated.validNel(
                      Some(CardanoNetworkDiscovery.ChainGeometry(start, slot, epoch, magic))
                    )
                case (None, None, None, None) => Validated.validNel(None)
                case _ =>
                    Validated.invalidNel(
                      "--system-start, --slot-length, --epoch-length and --protocol-magic must be " +
                          "given together (or all omitted, to read them from the backend's /genesis)"
                    )
            }

    /** The `discover-network` subcommand. */
    lazy val command: Command[IO[ExitCode]] =
        Command(
          name = "discover-network",
          header = "Describe the chain a Blockfrost-compatible backend serves, as JSON"
        )((blockfrostUrlOpt, apiKeyOpt, outOpt, geometryOpt).mapN(discoverNetwork))

    private def discoverNetwork(
        blockfrostUrl: String,
        mbApiKey: Option[String],
        out: Option[Path],
        geometry: Option[CardanoNetworkDiscovery.ChainGeometry]
    ): IO[ExitCode] =
        for {
            custom <- CardanoNetworkDiscovery
                .discover(blockfrostUrl, mbApiKey.getOrElse(""), geometry)
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
