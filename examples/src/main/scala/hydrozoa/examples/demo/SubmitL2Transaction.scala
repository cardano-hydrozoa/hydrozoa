package hydrozoa.examples.demo

import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.scalus.ledger.withZeroFees
import hydrozoa.lib.cardano.scalus.txbuilder.DiffHandler.prebalancedLovelaceDiffHandler
import hydrozoa.multisig.consensus.UserRequest
import hydrozoa.multisig.consensus.UserRequestBody.TransactionRequestBody
import hydrozoa.multisig.ledger.eutxol2.HeadIdPin
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.server.ApiDto.{L2UtxoView, given}
import hydrozoa.multisig.server.SubmissionClient
import java.nio.file.Path
import org.http4s.Uri
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.ember.client.EmberClientBuilder
import scala.util.Try
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AssetName, AuxiliaryData, Coin, Metadatum, ScriptHash, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value, Word64}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, ModifyAuxiliaryData, Send, Spend}
import scalus.cardano.txbuilder.{PubKeyWitness, TransactionBuilder}
import scalus.uplc.builtin.ByteString

/** Interactive demo target: submit a native L2 transaction to a running head.
  *
  * Select a peer (its key signs), pick one of the peer's L2 utxos (fetched from the head's
  * `GET /l2/cardano-eutxo/utxos/{address}`), enter a destination + value, and the tool builds the
  * zero-fee native tx (destination output + change back to the sender, the CIP-67 all-L2 output
  * designation and the headId pin in the metadata), signs it with the peer wallet, and posts it to
  * `POST /head/requests`.
  *
  * Usage:
  * {{{
  *   sbt "examples/runMain hydrozoa.examples.demo.SubmitL2Transaction [--config-dir config/demo] \
  *     [--head-uri http://localhost:8080]"
  * }}}
  */
object SubmitL2Transaction
    extends CommandIOApp(
      name = "submit-l2-tx",
      header = "Interactively build, sign, and submit an L2 transaction to a running head"
    ):

    override def main: Opts[IO[ExitCode]] =
        (DemoOptions.configDirOpt, DemoOptions.headUriOpt).mapN(run)

    private def run(configDir: Path, headUri: Uri): IO[ExitCode] =
        EmberClientBuilder.default[IO].build.use { client =>
            for {
                picked <- Prompts.selectPeer(configDir.resolve("private"))
                (peerName, privateConfigPath) = picked
                // Offline load — an L2 submission never touches L1, so no Blockfrost backend.
                demo <- DemoConfig.loadOffline(
                  configDir.resolve("head-config").resolve("head-config.json"),
                  privateConfigPath
                )
                given CardanoNetwork.Section = demo.cardanoNetwork
                ownAddress = demo.wallet.exportVerificationKey.shelleyAddress()
                ownBech32 <- IO.fromOption(ownAddress.toBech32.toOption)(
                  RuntimeException("could not render the peer address as bech32")
                )
                _ <- IO.println(s"\nPeer $peerName, L2 address: $ownBech32")

                views <- client.expect[List[L2UtxoView]](
                  headUri / "l2" / "cardano-eutxo" / "utxos" / ownBech32
                )
                utxos <- IO.fromEither(
                  views
                      .traverse(parseUtxoView)
                      .left
                      .map(e => RuntimeException(s"could not parse the L2 utxos response: $e"))
                )
                _ <- IO.raiseWhen(utxos.isEmpty)(
                  RuntimeException(
                    s"no L2 utxos at $ownBech32 — deposit first (see SubmitDeposit), or pick the " +
                        "peer that holds the opening L2 outputs"
                  )
                )
                selected <- Prompts.selectFromList(s"L2 utxos at $peerName", utxos)(renderUtxo)
                (input, output) = selected

                destination <- Prompts.promptDestination(configDir.resolve("private"))
                value <- promptSpendValue(output.value)

                txSigned <- IO
                    .fromEither(
                      buildTx(demo.headId, input, output, destination, value).left
                          .map(e => RuntimeException(s"could not build the L2 tx: $e"))
                    )
                    .map(demo.wallet.signTx)
                _ <- IO.println(s"Built + signed L2 tx ${txSigned.id}")

                requestId <- SubmissionClient
                    .http(client, headUri)
                    .submit(
                      UserRequest.TransactionRequest(
                        TransactionRequestBody(ByteString.fromArray(txSigned.toCbor))
                      )
                    )
                _ <- IO.println(
                  s"Accepted: requestId=$requestId. Watch GET $headUri/l2/cardano-eutxo/utxos/$ownBech32 " +
                      "for the result."
                )
            } yield ExitCode.Success
        }

    /** Prompt for the value to send from the selected utxo: it must fit, and the change left behind
      * must be either zero or at least 1 ADA (and never strand the utxo's native assets in a
      * zero-coin change).
      */
    private def promptSpendValue(available: Value): IO[Value] =
        Prompts.promptRetrying(
          s"Value to send (whole ADA, available ${Prompts.renderValue(available)})"
        ) { line =>
            line.toLongOption
                .filter(_ > 0)
                .map(Value.ada)
                .toRight("expected a positive ADA amount")
                .flatMap { value =>
                    val change = available - value
                    if change.coin.value < 0 then Left("exceeds the selected utxo")
                    else if change.isZero then Right(value)
                    else if change.coin.value == 0 && !change.assets.isEmpty then
                        Left("would strand the utxo's native assets in a zero-ADA change")
                    else if change.coin.value > 0 && change.coin.value < 1_000_000 then
                        Left("would leave dust change — send the full utxo or leave >= 1 ADA")
                    else Right(value)
                }
        }

    /** Build the unsigned zero-fee L2 tx: the selected input, the destination output, change back
      * to the sender when nonzero, and the mandatory metadata (the CIP-67 all-L2 output designation
      * + the headId pin).
      */
    private def buildTx(
        headId: HeadId,
        input: TransactionInput,
        output: TransactionOutput,
        destination: ShelleyAddress,
        value: Value
    )(using config: CardanoNetwork.Section): Either[String, scalus.cardano.ledger.Transaction] = {
        val change = output.value - value
        val outputs =
            Babbage(destination, value, None, None) ::
                (if change.isZero then Nil else List(Babbage(output.address, change, None, None)))
        val metadata = AuxiliaryData.Metadata(
          Map(
            // Every output stays on L2 (Int(2)); Int(1) would mark a withdrawal.
            Word64(CIP67.Tags.head) ->
                Metadatum.List(outputs.map(_ => Metadatum.Int(2)).toIndexedSeq),
            HeadIdPin.metadatum(headId)
          )
        )
        TransactionBuilder
            .build(
              config.network,
              Spend(Utxo(input, output), PubKeyWitness)
                  :: (outputs.map(Send.apply) :+ Fee(Coin.zero)
                      :+ ModifyAuxiliaryData(_ => Some(metadata)))
            )
            .flatMap(
              _.finalizeContext(
                protocolParams = config.cardanoProtocolParams.withZeroFees,
                diffHandler = prebalancedLovelaceDiffHandler,
                evaluator = config.plutusScriptEvaluatorForTxBuild,
                validators = Seq.empty
              )
            )
            .map(_.transaction)
            .left
            .map(_.toString)
    }

    /** Parse one `GET /l2/cardano-eutxo/utxos/{address}` entry back into scalus types.
      * Datum-bearing utxos are accepted for display but their datum is not reconstructed — the
      * input reference is what the tx spends.
      */
    private def parseUtxoView(view: L2UtxoView): Either[String, (TransactionInput, Babbage)] =
        for {
            txId <- Try(TransactionHash.fromHex(view.input.transaction_id)).toEither.left
                .map(e => s"bad transaction_id: ${e.getMessage}")
            address <- Try(Address.fromBech32(view.output.address)).toEither.left
                .map(e => s"bad address: ${e.getMessage}")
            shelley <- address match {
                case sa: ShelleyAddress => Right(sa)
                case other              => Left(s"not a Shelley address: $other")
            }
            coin <- view.output.value.coin.toLongOption.toRight(
              s"bad coin: ${view.output.value.coin}"
            )
            assets <- view.output.value.assets.toList
                .traverse { case (policyHex, byAsset) =>
                    for {
                        policy <- Try(ScriptHash.fromHex(policyHex)).toEither.left
                            .map(e => s"bad policy id: ${e.getMessage}")
                        parsed <- byAsset.toList.traverse { case (nameHex, qty) =>
                            qty.toLongOption
                                .toRight(s"bad asset quantity: $qty")
                                .map(q => AssetName.fromHex(nameHex) -> q)
                        }
                    } yield policy -> parsed.toMap
                }
                .map(_.toMap)
        } yield TransactionInput(txId, view.input.index) ->
            Babbage(shelley, Value.assets(assets, Coin(coin)), None, None)

    private def renderUtxo(utxo: (TransactionInput, Babbage)): String =
        s"${utxo._1.transactionId.toHex.take(16)}…#${utxo._1.index}  " +
            Prompts.renderValue(utxo._2.value)

end SubmitL2Transaction

/** The CLI options shared by the demo targets. */
private object DemoOptions {
    val configDirOpt: Opts[Path] =
        Opts.option[String](
          "config-dir",
          "The config directory (head-config/ + private/), default config/demo",
          short = "c"
        ).map(Path.of(_))
            .withDefault(Path.of("config/demo"))

    val headUriOpt: Opts[Uri] =
        Opts.option[String](
          "head-uri",
          "A head peer's HTTP API, default http://localhost:8080",
          short = "u"
        ).mapValidated(s => Uri.fromString(s).leftMap(e => e.message).toValidatedNel)
            .withDefault(Uri.unsafeFromString("http://localhost:8080"))
}
