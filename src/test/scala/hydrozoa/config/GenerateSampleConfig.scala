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
import test.{PeersNumberSpec, SeedPhrase, TestPeers, TestPeersSpec}

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

    /** Drive the generator exactly once; `generationSeed` controls reproducibility via ScalaCheck's
      * seed mechanism.
      *
      * Peers are keyed [[TestPeers.KeyScheme.Ed25519]] so the written configs are usable as-is: a
      * BIP32 wallet cannot be serialized (see [[writeAll]]), and a node reading a config where it
      * could not would sign with a key its own verification key does not match.
      */
    def generateAndWrite(spec: Spec): IO[Unit] =
        IO(
          MultiNodeConfig
              .generateWith(
                TestPeers(
                  seedPhrase = spec.seedPhrase,
                  network = testPeersSpec(spec).network,
                  peersNumber = spec.nPeers,
                  keyScheme = TestPeers.KeyScheme.Ed25519
                )
              )()
              .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(spec.generationSeed))
        ).flatMap(writeAll(spec, _))

    /** Serialize the shared head config + per-peer private configs under `spec.outDir`.
      *
      * NOTE: a peer whose wallet holds a BIP32 extended key — [[TestPeers.KeyScheme.Bip32]], the
      * default everywhere else — gets dummy all-zero signing-key bytes, because that key has no
      * 32-byte form to write. Such a config is not runnable: replace the keys first, or generate
      * with [[TestPeers.KeyScheme.Ed25519]] as [[generateAndWrite]] does.
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
