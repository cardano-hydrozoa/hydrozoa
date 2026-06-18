package hydrozoa.config

import cats.effect.{ExitCode, IO}
import cats.syntax.apply.*
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp
import fs2.io.file.{Files as Fs2Files, Path as Fs2Path}
import fs2.{Stream, text}
import hydrozoa.config.head.HeadConfig.headConfigEncoder
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.config.node.NodePrivateConfig.nodePrivateConfigEncoder
import io.circe.Printer
import io.circe.syntax.*
import java.nio.file.Path
import org.scalacheck.Gen
import test.{PeersNumberSpec, SeedPhrase, TestPeersSpec}

/** Dev-time tool: generate a sample multi-peer Hydrozoa config and write it as JSON to disk.
  *
  * Files written:
  *   - `${outDir}/head-config.json` — shared HeadConfig.
  *   - `${outDir}/peer-${n}/private.json` — that peer's NodePrivateConfig.
  *
  * Invoke via:
  * `sbt "Test/runMain hydrozoa.config.GenerateSampleConfig [--out-dir DIR] [--peers N]"`.
  *
  * Synthetic-mode only for now; a `--blockfrost-key` flag for real-UTXO mode is a follow-up.
  */
object GenerateSampleConfig
    extends CommandIOApp(
      name = "generate-sample-config",
      header = "Generate a sample multi-peer Hydrozoa config and write it as JSON to disk."
    ) {

    /** Fully determines the generated config — deterministic given `generationSeed`. */
    final case class Spec(
        outDir: Path,
        nPeers: Int,
        seedPhrase: SeedPhrase,
        generationSeed: Long
    )

    /** Used when CLI args are absent. */
    val defaultSpec: Spec = Spec(
      outDir = Path.of(".scratch/config-sample"),
      nPeers = 3,
      seedPhrase = SeedPhrase.Yaci,
      generationSeed = 0xdeadbeefL
    )

    private val outDirOpt: Opts[Path] =
        Opts.option[String]("out-dir", "Directory to write generated config files", short = "o")
            .orNone
            .map(_.map(Path.of(_)).getOrElse(defaultSpec.outDir))

    private val nPeersOpt: Opts[Int] =
        Opts.option[Int]("peers", "Number of head peers to generate", short = "n")
            .orNone
            .map(_.getOrElse(defaultSpec.nPeers))

    override def main: Opts[IO[ExitCode]] =
        (outDirOpt, nPeersOpt).mapN { (outDir, nPeers) =>
            val spec = defaultSpec.copy(outDir = outDir, nPeers = nPeers)
            generateAndWrite(spec).as(ExitCode.Success)
        }

    /** Project the user `Spec` onto the generator's `TestPeersSpec` — fixed phrase, exact N. */
    def testPeersSpec(spec: Spec): TestPeersSpec =
        TestPeersSpec(spec.seedPhrase, CardanoNetwork.Preprod, PeersNumberSpec.Exact(spec.nPeers))

    /** Drive `MultiNodeConfig.generate(testPeersSpec(spec))()` exactly once; `generationSeed`
      * controls reproducibility via ScalaCheck's seed mechanism.
      */
    def generateAndWrite(spec: Spec): IO[Unit] =
        IO(
          MultiNodeConfig
              .generate(testPeersSpec(spec))()
              .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(spec.generationSeed))
        ).flatMap(writeAll(spec, _))

    /** Serialize the shared head config + per-peer private configs under `spec.outDir`.
      *
      * NOTE: signing keys are written as dummy all-zero bytes — the BIP32 extended keys used
      * internally by TestPeers cannot round-trip through the 32-byte Scalus codec. Replace them
      * before running a node.
      */
    def writeAll(spec: Spec, mnc: MultiNodeConfig): IO[Unit] = {
        val printer = Printer.spaces2.copy(dropNullValues = true)
        val fs = Fs2Files[IO]

        def toFsPath(p: Path): Fs2Path = Fs2Path.fromNioPath(p)

        def writeFile(p: Path, content: String): IO[Unit] =
            Stream
                .emit(content)
                .through(text.utf8.encode)
                .through(fs.writeAll(toFsPath(p)))
                .compile
                .drain

        for {
            _ <- fs.createDirectories(toFsPath(spec.outDir))
            _ <- writeFile(
              spec.outDir.resolve("head-config.json"),
              printer.print(mnc.headConfig.asJson)
            )
            _ <- Stream
                .iterable(mnc.nodePrivateConfigs)
                .evalMap { case (peerNum, npc) =>
                    val peerDir = spec.outDir.resolve(s"peer-$peerNum")
                    fs.createDirectories(toFsPath(peerDir)) >>
                        writeFile(peerDir.resolve("private.json"), printer.print(npc.asJson))
                }
                .compile
                .drain
        } yield ()
    }
}
