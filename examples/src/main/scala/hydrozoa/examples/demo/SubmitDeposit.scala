package hydrozoa.examples.demo

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.RequestValidityEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.UserRequest
import hydrozoa.multisig.consensus.UserRequestBody.DepositRequestBody
import hydrozoa.multisig.ledger.eutxol2.tx.GenesisObligation
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l1.tx.RawTx
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import hydrozoa.multisig.server.SubmissionClient
import java.nio.file.Path
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.{Coin, TransactionInput, Utxo, Value}
import scalus.cardano.onchain.plutus.prelude.Option as SOption
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString

/** Interactive demo target: deposit funds from L1 into a running head.
  *
  * Select a peer (its key funds and signs), pick one of the peer's L1 utxos (fetched via the peer's
  * own Blockfrost backend), enter the L2 outputs the deposit should spawn on absorption (same
  * destination/value flow as [[SubmitL2Transaction]]), and the tool builds the L2 payload,
  * COSE-signs its hash with the peer wallet (the depositor's endorsement carried in the deposit tx
  * metadata, design note §5.5), registers the deposit with the head (`POST /api/deposit/register`),
  * then signs the deposit tx and submits it to L1 via Blockfrost, polling until the deposit utxo
  * lands. The head absorbs it after maturity.
  *
  * Usage:
  * {{{
  *   sbt "examples/runMain hydrozoa.examples.demo.SubmitDeposit [--config-dir config/demo] \
  *     [--head-uri http://localhost:8080]"
  * }}}
  */
object SubmitDeposit
    extends CommandIOApp(
      name = "deposit",
      header = "Interactively build, register, and submit a deposit into a running head"
    ):

    /** The accept-by margin: the head must learn the deposit within this window, so it has to
      * comfortably exceed the interactive prompting + L1 submission time. Absorption is anchored
      * on the accept-by time, so a wider margin directly delays the deposit landing on L2.
      */
    private val acceptByMargin = 3.minutes

    override def main: Opts[IO[ExitCode]] =
        (DemoOptions.configDirOpt, DemoOptions.headUriOpt).mapN(run)

    private def run(configDir: Path, headUri: Uri): IO[ExitCode] =
        EmberClientBuilder.default[IO].build.use { client =>
            for {
                picked <- Prompts.selectPeer(configDir.resolve("private"))
                (peerName, privateConfigPath) = picked
                loaded <- NodeConfig.load(
                  configDir.resolve("head-config").resolve("head-config.json"),
                  privateConfigPath
                )
                (config, backend) = loaded
                given CardanoNetwork.Section = config
                ownAddress = config.ownWallet.exportVerificationKey.shelleyAddress()
                ownBech32 <- IO.fromOption(ownAddress.toBech32.toOption)(
                  RuntimeException("could not render the peer address as bech32")
                )
                _ <- IO.println(s"\nPeer $peerName, L1 address: $ownBech32")

                utxos <- backend
                    .utxosAt(ownAddress)
                    .flatMap(r =>
                        IO.fromEither(
                          r.left.map(e => RuntimeException(s"could not fetch L1 utxos: $e"))
                        )
                    )
                    .map(_.toList)
                _ <- IO.raiseWhen(utxos.isEmpty)(
                  RuntimeException(s"no L1 utxos at $ownBech32 — fund it first")
                )
                selected <- Prompts.selectFromList(s"L1 utxos at $peerName", utxos)(u =>
                    s"${u._1.transactionId.toHex.take(16)}…#${u._1.index}  " +
                        Prompts.renderValue(u._2.value)
                )

                _ <- IO.println("\nL2 outputs the deposit spawns on absorption:")
                outputs <- Prompts.promptOutputs(configDir.resolve("private"))
                obligations = outputs.map((address, value) =>
                    GenesisObligation(
                      l2OutputPaymentAddress = address.payment,
                      l2OutputNetwork = config.network,
                      l2OutputDatum = SOption.None,
                      l2OutputValue = value,
                      l2OutputRefScript = None
                    )
                )
                l2Payload = GenesisObligation.serialize(obligations)
                l2Value = Value.combine(obligations.map(_.l2OutputValue).toList)

                // The depositor's endorsement of the L2 payload (design note §5.5), carried in
                // the deposit tx metadata and verified by the head's deposit pre-screening.
                l2PayloadCose = config.ownWallet.signCoseCip30(blake2b_256(l2Payload).bytes)

                now <- IO.realTimeInstant
                requestValidityEndTime = RequestValidityEndTime(
                  QuantizedInstant.ofEpochSeconds(
                    config.slotConfig,
                    now.getEpochSecond + acceptByMargin.toSeconds
                  )
                )

                depositRefundTxSeq <- IO.fromEither(
                  DepositRefundTxSeq
                      .Build(
                        l2Payload = l2Payload,
                        l2PayloadCose = l2PayloadCose,
                        l2Value = l2Value,
                        depositFee = Coin.zero,
                        utxosFunding = NonEmptyList.one(Utxo(selected._1, selected._2)),
                        changeAddress = ownAddress,
                        requestValidityEndTime = requestValidityEndTime,
                        refundAddress = ownAddress,
                        refundDatum = None,
                        // The id is a correlation field on the returned refund object only — it
                        // reaches neither the deposit tx bytes nor the register body; the head
                        // assigns the real id at registration.
                        requestId = RequestId(0, 0L)
                      )(using config)
                      .result
                      .left
                      .map(e => RuntimeException(s"could not build the deposit tx: $e"))
                )
                depositTx = depositRefundTxSeq.depositTx
                _ <- IO.println(
                  s"Built deposit ${depositTx.depositProduced.utxoId} " +
                      s"(${Prompts.renderValue(l2Value)} to L2, accept-by $requestValidityEndTime)"
                )

                requestId <- SubmissionClient
                    .http(client, headUri)
                    .submit(
                      UserRequest.DepositRequest(
                        DepositRequestBody(
                          l1Payload = ByteString.fromArray(depositTx.tx.toCbor),
                          l2Payload = l2Payload
                        )
                      )
                    )
                _ <- IO.println(s"Registered with the head: requestId=$requestId")

                signed = config.ownWallet.signTx(depositTx.tx)
                _ <- backend
                    .submitTx(RawTx(signed))
                    .flatMap(r =>
                        IO.fromEither(
                          r.left.map(e => RuntimeException(s"L1 submission failed: $e"))
                        )
                    )
                _ <- IO.println(s"Submitted deposit tx ${signed.id} to L1; waiting for the utxo…")
                _ <- awaitUtxo(backend, depositTx.depositProduced.utxoId)
                _ <- IO.println(
                  "Deposit is on L1. The head absorbs it after maturity — watch " +
                      s"GET $headUri/api/l2/utxos/{destination} for the spawned outputs."
                )
            } yield ExitCode.Success
        }

    /** Poll L1 until the deposit utxo is visible (5s intervals, 3 minutes). */
    private def awaitUtxo(
        backend: CardanoBackend[IO],
        input: TransactionInput,
        attemptsLeft: Int = 36
    ): IO[Unit] =
        backend.resolve(input).flatMap {
            case Right(Some(_)) => IO.unit
            case _ if attemptsLeft > 0 =>
                IO.sleep(5.seconds) *> awaitUtxo(backend, input, attemptsLeft - 1)
            case other =>
                IO.raiseError(
                  RuntimeException(s"deposit utxo $input not visible on L1 after 3min: $other")
                )
        }

end SubmitDeposit
