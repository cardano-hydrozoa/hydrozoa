package hydrozoa.app

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import hydrozoa.config.ScriptReferenceUtxos.given
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.{HydrozoaBlueprint, ScriptReferenceUtxos}
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.ledger.l1.tx.RawTx
import hydrozoa.rulebased.ledger.l1.script.plutus.{DeploymentTx, SetupLadder}
import io.circe.parser
import io.circe.syntax.*
import java.nio.file.{Files, Path}
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.{ScriptRef, TransactionInput, Utxo}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

/** Deploy the rule-based treasury and dispute validators as reference scripts, and (once) the G2
  * setup ladder as inline-datum utxos, on L1.
  *
  * Usage:
  * {{{
  *   sbt "runMain hydrozoa.app.DeployScriptsAndG2Setup \
  *     --wallet config/demo/head-0/private.json \
  *     [--ladder-refs script-refs.json] [--out script-refs.json]"
  * }}}
  *
  * The G2 setup ladder never changes, so it is deployed exactly once; the validator scripts change
  * per release. Pass `--ladder-refs <existing script-refs.json>` to reuse an already-deployed
  * ladder and redeploy only the two validators. Without it, the ladder is deployed too (bootstrap).
  *
  * Builds and submits chained [[DeploymentTx]]s funded from the wallet carried by the given keygen
  * private config (change returns to the wallet, so the head funding survives): one per validator
  * script, plus — unless the ladder is reused — one carrying all seven [[SetupLadder]] rungs at
  * outputs 0-6, all locked at the unspendable burn address. Waits until every reference UTxO is
  * visible on L1, then writes their inputs as `--out` in the shape [[BuildHeadConfig]] consumes via
  * `--script-refs`.
  *
  * Reference UTxOs at the burn address can never be spent, so one deployment serves every head on
  * the network until the compiled scripts change (a hash mismatch at config-build or node start
  * means: redeploy). The Blockfrost key comes from `--blockfrost-key` or `$BLOCKFROST_API_KEY`, and
  * the target network is derived from the key's network prefix (`preview…` / `preprod…` /
  * `mainnet…`).
  */
object DeployScriptsAndG2Setup
    extends CommandIOApp(
      name = "deploy-scripts-and-g2-setup",
      header = "Deploy the treasury + dispute validators, and the G2 setup ladder, on L1"
    ):

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(
          Slf4jMsgFormat.humanFormat("hydrozoa.app.DeployScriptsAndG2Setup")
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

    private val ladderRefsOpt: Opts[Option[Path]] =
        Opts.option[String](
          "ladder-refs",
          "Existing script-refs.json whose G2 setup ladder to reuse (skips redeploying it)",
          short = "l"
        ).map(Path.of(_))
            .orNone

    private val outOpt: Opts[Path] =
        Opts.option[String]("out", "Output path (default script-refs.json)", short = "o")
            .map(Path.of(_))
            .withDefault(Path.of("script-refs.json"))

    override def main: Opts[IO[ExitCode]] =
        (walletOpt, blockfrostKeyOpt, ladderRefsOpt, outOpt).mapN(deployScriptsAndG2Setup)

    private def deployScriptsAndG2Setup(
        walletPath: Path,
        blockfrostKey: String,
        ladderRefsPath: Option[Path],
        outPath: Path
    ): IO[ExitCode] = IO
        .fromEither[StandardCardanoNetwork](
          networkOfBlockfrostKey(blockfrostKey)
        )
        .flatMap { (cardanoNetwork: StandardCardanoNetwork) =>
            given CardanoNetwork.Section = cardanoNetwork
            deployOn(cardanoNetwork, walletPath, blockfrostKey, ladderRefsPath, outPath)
        }

    private def deployOn(
        cardanoNetwork: StandardCardanoNetwork,
        walletPath: Path,
        blockfrostKey: String,
        ladderRefsPath: Option[Path],
        outPath: Path
    )(using CardanoNetwork.Section): IO[ExitCode] = {
        for {
            _ <- log.info(s"Target network (from the Blockfrost key): $cardanoNetwork")

            wallet <- readWallet(walletPath)
            address = wallet.exportVerificationKey.shelleyAddress()(using cardanoNetwork)
            _ <- log.info(s"Funding wallet address: ${address.toBech32.get}")

            reusedLadderInputs <- ladderRefsPath.traverse(readLadderInputs)

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

            // Treasury deployment spends the wallet UTxOs; the dispute deployment chains off its
            // change output, and the setup-ladder deployment (when not reused) off the dispute tx's
            // change (a deployment tx's change is its last output, after the payload outputs).
            treasuryTx <- IO.fromEither(
              DeploymentTx
                  .Build(
                    fundingUtxos,
                    NonEmptyList.one(
                      DeploymentTx.DeployedPayload
                          .script(ScriptRef(HydrozoaBlueprint.treasuryScript))
                    )
                  )
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
                    NonEmptyList.one(
                      DeploymentTx.DeployedPayload
                          .script(ScriptRef(HydrozoaBlueprint.disputeScript))
                    )
                  )
                  .result
                  .left
                  .map(e => RuntimeException(s"Failed to build dispute deployment tx: $e"))
            )
            disputeSigned = wallet.signTx(disputeTx.tx)
            disputeChange = Utxo(
              TransactionInput(disputeSigned.id, 1),
              disputeSigned.body.value.outputs(1).value
            )

            // Deploy the ladder only when not reusing an existing one.
            ladderTxOpt <- reusedLadderInputs match {
                case Some(_) => IO.none
                case None =>
                    IO.fromEither(
                      DeploymentTx
                          .Build(
                            NonEmptyList.one(disputeChange),
                            SetupLadder.rungDatums.map(DeploymentTx.DeployedPayload.data(_))
                          )
                          .result
                          .left
                          .map(e =>
                              RuntimeException(s"Failed to build setup-ladder deployment tx: $e")
                          )
                    ).map(Some(_))
            }
            ladderSignedOpt = ladderTxOpt.map(t => wallet.signTx(t.tx))
            ladderInputs = reusedLadderInputs.getOrElse(ladderTxOpt.get.deployedUtxos.toList)

            _ <- log.info(s"Submitting treasury deployment tx: ${treasurySigned.id}")
            _ <- submit(backend, RawTx(treasurySigned))
            _ <- log.info(s"Submitting dispute deployment tx: ${disputeSigned.id}")
            _ <- submit(backend, RawTx(disputeSigned))
            _ <- ladderSignedOpt match {
                case Some(s) =>
                    log.info(s"Submitting setup-ladder deployment tx: ${s.id}") *>
                        submit(backend, RawTx(s))
                case None => log.info("Reusing the already-deployed G2 setup ladder")
            }

            _ <- log.info("Waiting for the reference UTxOs to appear on L1...")
            _ <- awaitUtxo(backend, treasuryTx.deployedUtxos.head)
            _ <- awaitUtxo(backend, disputeTx.deployedUtxos.head)
            _ <- ladderInputs.traverse_(awaitUtxo(backend, _))

            unresolved = ScriptReferenceUtxos.Unresolved(
              rulebasedTreasuryScriptInput = treasuryTx.deployedUtxos.head,
              disputeResolutionScriptInput = disputeTx.deployedUtxos.head,
              setupLadderInputs = ladderInputs
            )
            _ <- IO.blocking {
                Option(outPath.getParent).foreach(Files.createDirectories(_))
                Files.writeString(outPath, unresolved.asJson.spaces2)
            }
            _ <- log.info(s"Wrote script reference inputs to $outPath")
            _ <- log.info(
              s"Explorer: https://$explorerHost/tx/${treasuryTx.deployedUtxos.head.transactionId.toHex}"
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

    /** Read the G2 setup ladder's reference inputs from an existing `script-refs.json`, to reuse
      * the already-deployed ladder instead of redeploying it.
      */
    private def readLadderInputs(path: Path): IO[List[TransactionInput]] = for {
        json <- IO.blocking(Files.readString(path)).flatMap(s => IO.fromEither(parser.parse(s)))
        unresolved <- IO.fromEither(json.as[ScriptReferenceUtxos.Unresolved])
    } yield unresolved.setupLadderInputs

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

end DeployScriptsAndG2Setup
