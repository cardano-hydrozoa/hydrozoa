package hydrozoa.app

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.{HydrozoaBlueprint, ScriptReferenceUtxos}
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.ledger.l1.tx.RawTx
import hydrozoa.rulebased.ledger.l1.script.plutus.DeploymentTx
import io.circe.parser
import io.circe.syntax.*
import java.nio.file.{Files, Path}
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.{ScriptRef, TransactionInput, Utxo}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

/** Deploy the rule-based treasury and dispute validators as reference scripts on L1.
  *
  * Usage:
  * {{{
  *   sbt "runMain hydrozoa.app.DeployReferenceScripts \
  *     --wallet config/demo/head-0/private.json [--out script-refs.json]"
  * }}}
  *
  * Builds and submits two chained [[DeploymentTx]]s funded from the wallet carried by the given
  * keygen private config (change returns to the wallet, so the head funding survives): each locks
  * one script at the unspendable burn address. Waits until both reference UTxOs are visible on L1,
  * then writes their inputs as `--out` in the shape [[BuildHeadConfig]] consumes via
  * `--script-refs`.
  *
  * Reference UTxOs at the burn address can never be spent, so one deployment serves every head on
  * the network until the compiled scripts change (a hash mismatch at config-build or node start
  * means: redeploy). The Blockfrost key comes from `--blockfrost-key` or `$BLOCKFROST_API_KEY`, and
  * the target network is derived from the key's network prefix (`preview…` / `preprod…` /
  * `mainnet…`).
  */
object DeployReferenceScripts
    extends CommandIOApp(
      name = "deploy-reference-scripts",
      header = "Deploy the treasury + dispute validators as reference scripts on L1"
    ):

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(
          Slf4jMsgFormat.humanFormat("hydrozoa.app.DeployReferenceScripts")
        )

    private val walletOpt: Opts[Path] =
        Opts.option[String](
          "wallet",
          "A keygen private config whose ownHeadWallet funds the deployment (e.g. head-0's)",
          short = "w"
        ).map(Path.of(_))

    private val blockfrostKeyOpt: Opts[String] =
        Opts.option[String](
          "blockfrost-key",
          "Blockfrost API key for the Cardano backend (falls back to $BLOCKFROST_API_KEY)",
          short = "k"
        ).orElse(
          Opts.env[String]("BLOCKFROST_API_KEY", "Blockfrost API key for the Cardano backend")
        )

    private val outOpt: Opts[Path] =
        Opts.option[String]("out", "Output path (default script-refs.json)", short = "o")
            .map(Path.of(_))
            .withDefault(Path.of("script-refs.json"))

    override def main: Opts[IO[ExitCode]] =
        (walletOpt, blockfrostKeyOpt, outOpt).mapN(deployReferenceScripts)

    private def deployReferenceScripts(
        walletPath: Path,
        blockfrostKey: String,
        outPath: Path
    ): IO[ExitCode] = IO
        .fromEither[StandardCardanoNetwork](
          networkOfBlockfrostKey(blockfrostKey)
        )
        .flatMap { (cardanoNetwork: StandardCardanoNetwork) =>
            given CardanoNetwork.Section = cardanoNetwork
            deployOn(cardanoNetwork, walletPath, blockfrostKey, outPath)
        }

    private def deployOn(
        cardanoNetwork: StandardCardanoNetwork,
        walletPath: Path,
        blockfrostKey: String,
        outPath: Path
    )(using CardanoNetwork.Section): IO[ExitCode] = {
        for {
            _ <- log.info(s"Target network (from the Blockfrost key): $cardanoNetwork")

            wallet <- readWallet(walletPath)
            address = wallet.exportVerificationKey.shelleyAddress()(using cardanoNetwork)
            _ <- log.info(s"Funding wallet address: ${address.toBech32.get}")

            backend <- CardanoBackendBlockfrost(
              Left(cardanoNetwork),
              blockfrostKey,
              tracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)
            )

            utxosMap <- backend
                .utxosAt(address)
                .flatMap(r =>
                    IO.fromEither(r.left.map(e => RuntimeException(s"Failed to fetch UTxOs: $e")))
                )
            fundingUtxos <- IO.fromOption(
              NonEmptyList.fromList(utxosMap.toList.map(Utxo(_, _)))
            )(RuntimeException(s"No UTxOs at ${address.toBech32.get}; fund the wallet first"))

            // Treasury deployment spends the wallet UTxOs; the dispute deployment chains off
            // its change output (index 1; index 0 is the reference script).
            treasuryTx <- IO.fromEither(
              DeploymentTx
                  .Build(fundingUtxos, ScriptRef(HydrozoaBlueprint.treasuryScript))
                  .result
                  .left
                  .map(e => RuntimeException(s"Failed to build treasury deployment tx: $e"))
            )
            treasurySigned = wallet.signTx(treasuryTx.tx)
            treasuryChange = Utxo(
              TransactionInput(treasurySigned.id, 1),
              treasurySigned.body.value.outputs(1).value
            )
            disputeTx <- IO.fromEither(
              DeploymentTx
                  .Build(
                    NonEmptyList.one(treasuryChange),
                    ScriptRef(HydrozoaBlueprint.disputeScript)
                  )
                  .result
                  .left
                  .map(e => RuntimeException(s"Failed to build dispute deployment tx: $e"))
            )
            disputeSigned = wallet.signTx(disputeTx.tx)

            _ <- log.info(s"Submitting treasury deployment tx: ${treasurySigned.id}")
            _ <- submit(backend, RawTx(treasurySigned))
            _ <- log.info(s"Submitting dispute deployment tx: ${disputeSigned.id}")
            _ <- submit(backend, RawTx(disputeSigned))

            _ <- log.info("Waiting for the reference UTxOs to appear on L1...")
            _ <- awaitUtxo(backend, treasuryTx.refScriptUtxo)
            _ <- awaitUtxo(backend, disputeTx.refScriptUtxo)

            unresolved = ScriptReferenceUtxos.Unresolved(
              rulebasedTreasuryScriptInput = treasuryTx.refScriptUtxo,
              disputeResolutionScriptInput = disputeTx.refScriptUtxo
            )
            _ <- IO.blocking {
                Option(outPath.getParent).foreach(Files.createDirectories(_))
                Files.writeString(outPath, unresolved.asJson.spaces2)
            }
            _ <- log.info(s"Wrote script reference inputs to $outPath")
            _ <- log.info(
              s"Explorer: https://$explorerHost/tx/${treasuryTx.refScriptUtxo.transactionId.toHex}"
            )
        } yield ExitCode.Success
    }

    /** Derive the target network from the Blockfrost key's network prefix. */
    private def networkOfBlockfrostKey(key: String): Either[Throwable, StandardCardanoNetwork] =
        if key.startsWith("preview") then Right(CardanoNetwork.Preview)
        else if key.startsWith("preprod") then Right(CardanoNetwork.Preprod)
        else if key.startsWith("mainnet") then Right(CardanoNetwork.Mainnet)
        else
            Left(
              RuntimeException(
                "cannot derive the network from the Blockfrost key: expected a preview…/preprod…/" +
                    "mainnet… project key"
              )
            )

    /** The cexplorer host for the target network. */
    private def explorerHost(using network: CardanoNetwork.Section): String =
        network.cardanoNetwork match {
            case CardanoNetwork.Preview => "preview.cexplorer.io"
            case CardanoNetwork.Preprod => "preprod.cexplorer.io"
            case _                      => "cexplorer.io"
        }

    /** Load the signing wallet from a keygen private config, reading only the `ownHeadWallet`
      * fields — a full [[hydrozoa.config.node.NodePrivateConfig]] decode would require the head
      * config, which does not exist yet at deployment time.
      */
    private def readWallet(path: Path): IO[PeerWallet] = for {
        json <- IO.blocking(Files.readString(path)).flatMap(s => IO.fromEither(parser.parse(s)))
        wallet <- IO.fromEither(
          (for {
              obj <- json.hcursor.downField("ownPeerPrivate").downField("ownHeadWallet").focus
              vKeyHex <- obj.hcursor.downField("verificationKey").focus.flatMap(_.asString)
              sKeyHex <- obj.hcursor.downField("signingKey").focus.flatMap(_.asString)
          } yield PeerWallet.scalusWallet(
            VerificationKey.unsafeFromByteString(ByteString.fromHex(vKeyHex)),
            SigningKey.unsafeFromByteString(ByteString.fromHex(sKeyHex))
          )).toRight(
            RuntimeException(
              s"$path does not carry ownPeerPrivate.ownHeadWallet (is it a keygen head private config?)"
            )
          )
        )
    } yield wallet

    private def submit(backend: CardanoBackend[IO], rawTx: RawTx): IO[Unit] =
        backend
            .submitTx(rawTx)
            .flatMap(r =>
                IO.fromEither(r.left.map(e => RuntimeException(s"Failed to submit tx: $e"))).void
            )

    /** Poll the backend until the reference UTxO is visible, for up to three minutes. */
    private def awaitUtxo(
        backend: CardanoBackend[IO],
        input: TransactionInput,
        attemptsLeft: Int = 36
    ): IO[Unit] =
        backend.resolve(input).flatMap {
            case Right(Some(_)) => log.info(s"Reference UTxO $input is on L1")
            case _ if attemptsLeft > 0 =>
                IO.sleep(5.seconds) *> awaitUtxo(backend, input, attemptsLeft - 1)
            case other =>
                IO.raiseError(
                  RuntimeException(s"Reference UTxO $input not visible on L1: last result $other")
                )
        }

end DeployReferenceScripts
