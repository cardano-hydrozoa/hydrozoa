package hydrozoa.examples.demo

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import io.circe.parser
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.Value
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.ByteString

/** Console prompts shared by the interactive demo targets ([[SubmitL2Transaction]],
  * [[SubmitDeposit]]): peer selection from the private-config directory, numbered list pickers, and
  * the destination/value reader both flows use for L2 outputs.
  */
object Prompts {

    /** List the peers under `privateDir` (the `private/` half of the config layout — one
      * `<peer>/private.json` per peer) and let the user pick one. Returns the peer name and its
      * private-config path.
      */
    def selectPeer(privateDir: Path): IO[(String, Path)] =
        for {
            peers <- IO.blocking {
                Files
                    .list(privateDir)
                    .iterator()
                    .asScala
                    .filter(p => Files.isRegularFile(p.resolve("private.json")))
                    .map(p => (p.getFileName.toString, p.resolve("private.json")))
                    .toList
                    .sortBy(_._1)
            }
            _ <- IO.raiseWhen(peers.isEmpty)(
              RuntimeException(s"no <peer>/private.json found under $privateDir")
            )
            picked <- selectFromList("Select a peer (its key signs everything below)", peers)(_._1)
        } yield picked

    /** Print `items` as a numbered list under `title` and read a 1-based selection. */
    def selectFromList[A](title: String, items: List[A])(render: A => String): IO[A] =
        for {
            _ <- IO.println(s"\n$title:")
            _ <- items.zipWithIndex.traverse_ { case (item, i) =>
                IO.println(f"  ${i + 1}%3d) ${render(item)}")
            }
            selected <- promptRetrying(s"Enter 1..${items.size}") { line =>
                line.toIntOption
                    .filter(n => n >= 1 && n <= items.size)
                    .map(n => items(n - 1))
                    .toRight(s"expected a number in 1..${items.size}")
            }
        } yield selected

    /** Read one or more `(destination, value)` L2 outputs — the shared input flow for an L2
      * transaction's outputs and a deposit's opening obligations.
      */
    def promptOutputs(privateDir: Path)(using
        CardanoNetwork.Section
    ): IO[NonEmptyList[(ShelleyAddress, Value)]] =
        for {
            head <- promptOutput(privateDir)
            tail <- promptMoreOutputs(privateDir)
        } yield NonEmptyList(head, tail)

    /** Read a single `(destination, value)` pair. */
    def promptOutput(privateDir: Path)(using
        CardanoNetwork.Section
    ): IO[(ShelleyAddress, Value)] =
        (promptDestination(privateDir), promptValue).tupled

    /** Read a destination: a bech32 address, or a peer name (`head-0`, `coil-2`, …) resolved to
      * that peer's own address via its private config.
      */
    def promptDestination(privateDir: Path)(using
        network: CardanoNetwork.Section
    ): IO[ShelleyAddress] =
        promptRetryingIO("Destination (bech32 address, or a peer name like head-1)") { line =>
            if line.startsWith("head-") || line.startsWith("coil-") then
                readPeerAddress(privateDir.resolve(line).resolve("private.json"))
            else
                IO.fromEither(
                  Try(Address.fromBech32(line)).toEither
                      .flatMap {
                          case sa: ShelleyAddress => Right(sa)
                          case other => Left(RuntimeException(s"not a Shelley address: $other"))
                      }
                      .left
                      .map(e => RuntimeException(s"invalid bech32 address: ${e.getMessage}"))
                )
        }

    /** Read a value in whole ADA. */
    def promptValue: IO[Value] =
        promptRetrying("Value (whole ADA)") { line =>
            line.toLongOption
                .filter(_ > 0)
                .map(Value.ada)
                .toRight("expected a positive integer ADA amount")
        }

    /** Derive a peer's own L1/L2 address from the verification key in its private config. */
    def readPeerAddress(privateConfigPath: Path)(using
        CardanoNetwork.Section
    ): IO[ShelleyAddress] =
        for {
            json <- IO
                .blocking(Files.readString(privateConfigPath))
                .flatMap(s => IO.fromEither(parser.parse(s)))
            ownPeer = json.hcursor.downField("ownPeerPrivate")
            vkeyHex <- IO.fromEither(
              ownPeer
                  .downField("ownHeadWallet")
                  .get[String]("verificationKey")
                  .orElse(ownPeer.downField("ownCoilWallet").get[String]("verificationKey"))
            )
            vkey = VerificationKey.unsafeFromByteString(ByteString.fromHex(vkeyHex))
        } yield vkey.shelleyAddress()

    private def promptMoreOutputs(privateDir: Path)(using
        CardanoNetwork.Section
    ): IO[List[(ShelleyAddress, Value)]] =
        for {
            more <- promptRetrying("Add another output? [y/N]") {
                case "" | "n" | "N" => Right(false)
                case "y" | "Y"      => Right(true)
                case _              => Left("expected y or n")
            }
            outputs <-
                if more then (promptOutput(privateDir), promptMoreOutputs(privateDir)).mapN(_ :: _)
                else IO.pure(List.empty)
        } yield outputs

    /** Render a value as ADA + a native-asset count, for utxo listings. */
    def renderValue(value: Value): String = {
        val ada = value.coin.value / 1_000_000.0
        val assets =
            if value.assets.isEmpty then ""
            else s" + ${value.assets.assets.values.map(_.size).sum} native asset(s)"
        f"$ada%.6f ADA$assets"
    }

    /** Prompt until `parse` accepts the input. */
    def promptRetrying[A](prompt: String)(parse: String => Either[String, A]): IO[A] =
        promptRetryingIO(prompt)(line => IO.fromEither(parse(line).left.map(RuntimeException(_))))

    private def promptRetryingIO[A](prompt: String)(parse: String => IO[A]): IO[A] =
        (IO.print(s"$prompt: ") >> IO.readLine.map(_.trim).flatMap(parse)).handleErrorWith {
            // Retrying on a closed stdin would loop forever — abort instead.
            case _: java.io.EOFException =>
                IO.raiseError(RuntimeException("stdin closed — aborting"))
            case e =>
                IO.println(s"  ✗ ${e.getMessage}") >> promptRetryingIO(prompt)(parse)
        }
}
