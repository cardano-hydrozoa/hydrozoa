package hydrozoa.rulebased.evacuator

import cats.effect.{ExitCode, IO}
import cats.syntax.apply.*
import cats.syntax.traverse.*
import cats.syntax.contravariant.*
import com.monovore.decline.{Command, Opts}
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.addExpectedSigners
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.consensus.peer.PeerWallet.peerWalletDecoder
import hydrozoa.multisig.ledger.l1.tx.RawTx
import io.circe.Decoder
import io.circe.parser.decode
import java.nio.file.{Files, Path}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{Change, TransactionBuilder}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.uplc.builtin.ByteString

/** Sweeps unabsorbed deposits out of a dead head's multisig address, consolidating them into one
  * payout.
  *
  * When a head dies without absorbing its deposits, the depositors' funds stay locked at the head's
  * multisig address. The head normally holds a pre-signed post-dated refund transaction for each
  * one, but those live only in the peers' stores — so a generation whose stores were wiped leaves
  * its deposits with no refund path at all.
  *
  * Nothing is lost, though: the refund *instructions* are in each deposit's inline datum, on chain,
  * and the multisig address is a native script over the peers' keys. So given those keys the
  * refunds can be rebuilt from chain state alone. That is what this does.
  *
  * It consolidates rather than paying each deposit separately: a thousand refund transactions of
  * one input each would burn a thousand fees to produce a thousand dust utxos. Batches are chained
  * — each transaction spends the previous one's output alongside its own slice of deposits, and
  * only the last pays out — so however many transactions the size limit forces, the destination
  * receives exactly one utxo.
  */
object RefundSweep {

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(
          Slf4jMsgFormat.humanFormat("hydrozoa.rulebased.evacuator.RefundSweep")
        )

    /** One deposit to sweep, as read from the chain. The inline datum is deliberately not carried:
      * a native-script spend puts no datum in the witness set and there is no script-data hash to
      * compute, so it cannot affect the transaction that gets built.
      */
    final case class Deposit(txHash: String, index: Int, lovelace: Long) derives Decoder

    private val headConfigPathArg: Opts[Path] =
        Opts.argument[String]("head-config.json").map(Path.of(_))

    private val privateConfigPathArg: Opts[Path] =
        Opts.argument[String]("peer-private.json").map(Path.of(_))

    /** The peers' private configs, one per required signer. The multisig demands every head peer
      * plus `coilQuorum` coils, so a short list produces a transaction the ledger rejects — which
      * is why the count is checked against the script before anything is built.
      */
    private val keysOpt: Opts[List[Path]] =
        Opts
            .options[String]("signer", "A peer private.json whose wallet co-signs the sweep")
            .map(_.toList.map(Path.of(_)))

    private val depositsOpt: Opts[Path] =
        Opts.option[String]("deposits", "JSON array of deposits to sweep").map(Path.of(_))

    /** Destination as a payment key hash rather than bech32: the sweep is only ever pointed at an
      * address read out of a deposit datum, which is where it arrives in this form anyway.
      */
    private val toOpt: Opts[String] =
        Opts.option[String]("to-key-hash", "Payment key hash of the payout address (hex)")

    private val batchOpt: Opts[Int] =
        Opts
            .option[Int]("batch", "Deposits per transaction")
            .withDefault(200)

    private val commitOpt: Opts[Boolean] =
        Opts.flag("commit", "Actually submit; without it, build and report only").orFalse

    lazy val command: Command[IO[ExitCode]] =
        Command(
          name = "sweep-refunds",
          header = "Refund unabsorbed deposits from a head's multisig address into one payout utxo"
        )(
          (
            headConfigPathArg,
            privateConfigPathArg,
            keysOpt,
            depositsOpt,
            toOpt,
            batchOpt,
            commitOpt
          ).mapN(run)
        )

    def run(
        headConfigPath: Path,
        privateConfigPath: Path,
        signerPaths: List[Path],
        depositsPath: Path,
        toKeyHash: String,
        batchSize: Int,
        commit: Boolean
    ): IO[ExitCode] =
        for {
            loaded <- NodeConfig.load(headConfigPath, privateConfigPath, None)
            (nodeConfig, backend) = loaded
            exit <- {
                given HeadConfig.Bootstrap.Section = nodeConfig.headConfig
                drive(
                  backend,
                  signerPaths,
                  depositsPath,
                  toKeyHash,
                  batchSize,
                  commit
                )
            }
        } yield exit

    private def drive(
        backend: CardanoBackend[IO],
        signerPaths: List[Path],
        depositsPath: Path,
        toKeyHash: String,
        batchSize: Int,
        commit: Boolean
    )(using config: HeadConfig.Bootstrap.Section): IO[ExitCode] = {

        val script = config.headMultisigScript
        val multisigAddress = config.headMultisigAddress
        val payoutAddress = ShelleyAddress(
          network = config.network,
          payment = ShelleyPaymentPart.Key(AddrKeyHash(ByteString.fromHex(toKeyHash))),
          delegation = Null
        )

        for {
            signers <- signerPaths.traverse(loadSigner)
            // The multisig is satisfied by all head peers plus `coilQuorum` coils, and
            // `numSigners` is exactly that count. Checking it here turns an unsatisfiable
            // witness set — which the ledger only rejects after the whole sweep is built and
            // submitted — into an argument error before any work happens.
            _ <- IO.raiseUnless(signers.size == script.numSigners)(
              RuntimeException(
                s"multisig needs ${script.numSigners} signatures, ${signers.size} supplied"
              )
            )
            _ <- log.info(s"signers: ${signers.size}, as the script requires")

            json <- IO.blocking(Files.readString(depositsPath))
            deposits <- IO.fromEither(
              decode[List[Deposit]](json).left.map(e => RuntimeException(s"deposits: $e"))
            )
            _ <- IO.raiseWhen(deposits.isEmpty)(RuntimeException("no deposits to sweep"))
            total = deposits.map(_.lovelace).sum
            batches = deposits.grouped(batchSize).toList
            _ <- log.info(
              s"${deposits.size} deposits, ${total / 1_000_000.0} ada, " +
                  s"${batches.size} transaction(s) of up to $batchSize"
            )
            // The script is rebuilt from the roster, so log the address it yields: if it does not
            // match the address the deposits sit at, every input is unspendable and the mistake is
            // visible here rather than as a ledger rejection.
            _ <- log.info(s"spending from $multisigAddress")
            _ <- log.info(s"payout to    $payoutAddress")

            params <- backend.fetchLatestParams.flatMap(
              IO.fromEither(_).adaptError(e => RuntimeException(s"protocol params: $e"))
            )

            exit <- sweep(
              batches,
              signers,
              multisigAddress,
              payoutAddress,
              params,
              backend,
              commit
            )
        } yield exit
    }

    private def loadSigner(path: Path): IO[PeerWallet] =
        IO.blocking(Files.readString(path)).flatMap { s =>
            IO.fromEither(
              decode[PeerWallet](s)
                  .orElse(
                    io.circe.parser
                        .parse(s)
                        .flatMap(
                          _.hcursor
                              .downField("ownPeerPrivate")
                              .downField("ownHeadWallet")
                              .as[PeerWallet]
                              .orElse(
                                io.circe.parser
                                    .parse(s)
                                    .flatMap(
                                      _.hcursor
                                          .downField("ownPeerPrivate")
                                          .downField("ownCoilWallet")
                                          .as[PeerWallet]
                                    )
                              )
                        )
                  )
                  .left
                  .map(e => RuntimeException(s"$path: no peer wallet found ($e)"))
            )
        }

    /** Build, sign and optionally submit the chain.
      *
      * Every batch but the last pays back to the multisig address, so the next transaction can
      * spend it with the same script; only the last pays the destination. The carried output is
      * always index 0, and it is known from the transaction just built — no query between steps.
      */
    private def sweep(
        batches: List[List[Deposit]],
        signers: List[PeerWallet],
        multisigAddress: ShelleyAddress,
        payoutAddress: ShelleyAddress,
        params: ProtocolParams,
        backend: CardanoBackend[IO],
        commit: Boolean
    )(using config: HeadConfig.Bootstrap.Section): IO[ExitCode] = {

        val script = config.headMultisigScript
        val last = batches.size - 1

        def step(
            remaining: List[(List[Deposit], Int)],
            carry: Option[Utxo],
            submitted: Int
        ): IO[Int] = remaining match {
            case Nil => IO.pure(submitted)
            case (batch, i) :: rest =>
                val destination = if i == last then payoutAddress else multisigAddress

                val inputs: List[Utxo] =
                    carry.toList ++ batch.map(d =>
                        Utxo(
                          TransactionInput(TransactionHash.fromHex(d.txHash), d.index),
                          TransactionOutput.Babbage(
                            address = multisigAddress,
                            value = Value(Coin(d.lovelace)),
                            datumOption = None,
                            scriptRef = None
                          )
                        )
                    )

                // The script travels by value on the first spend that needs it and by reference
                // after: the builder's steps are not commutative, so attaching first fails to
                // resolve. Same ordering rule the treasury mint follows.
                val spendSteps = inputs.zipWithIndex.map { case (u, n) =>
                    Spend(u, if n == 0 then script.witnessValue else script.witnessAttached)
                }

                val steps = spendSteps ++ List(
                  Send(
                    TransactionOutput.Babbage(
                      address = destination,
                      value = Value(Coin(0L)),
                      datumOption = None,
                      scriptRef = None
                    )
                  )
                )

                val built = for {
                    ctx0 <- TransactionBuilder
                        .build(config.network, steps)
                        .left
                        .map(e => RuntimeException(s"build failed at batch $i: $e"))
                    // Balancing prices the transaction it can see, and the multisig's signatures
                    // are not on it yet — so without declaring them the fee is short by exactly
                    // their bytes and the ledger rejects it as FeeTooSmallUTxO. Only the count
                    // matters here: these are placeholder hashes for sizing, not signers.
                    ctx = ctx0.addExpectedSigners(script.numSigners)
                    balanced <- ctx
                        .balanceContext(
                          diffHandler = Change.changeOutputDiffHandler(
                            _,
                            _,
                            protocolParams = params,
                            changeOutputIdx = 0
                          ),
                          protocolParams = params,
                          evaluator = PlutusScriptEvaluator(
                            config.cardanoInfo,
                            EvaluatorMode.EvaluateAndComputeCost
                          )
                        )
                        .left
                        .map(e => RuntimeException(s"balancing failed at batch $i: $e"))
                } yield balanced.transaction

                IO.fromEither(built).flatMap { unsigned =>
                    val signed = signers.foldLeft(unsigned)((tx, w) => w.signTx(tx))
                    val out = signed.body.value.outputs.head.value
                    val size = signed.toCbor.length
                    for {
                        _ <- log.info(
                          s"batch $i: ${batch.size} deposits" +
                              carry.fold("")(_ => " + carried") +
                              s", out ${out.value.coin.value / 1_000_000.0} ada" +
                              s", fee ${signed.body.value.fee.value / 1_000_000.0} ada" +
                              s", ${size} bytes, ${signed.id}"
                        )
                        _ <- IO.raiseWhen(size > params.maxTxSize)(
                          RuntimeException(
                            s"batch $i is $size bytes, over the ${params.maxTxSize} limit" +
                                " — lower --batch"
                          )
                        )
                        n <-
                            if !commit then step(rest, nextCarry(signed), submitted)
                            else
                                backend
                                    .submitTx(RawTx(signed))
                                    .flatMap(r =>
                                        IO.fromEither(
                                          r.left.map(e =>
                                              RuntimeException(s"submit failed at batch $i: $e")
                                          )
                                        )
                                    ) *> step(rest, nextCarry(signed), submitted + 1)
                    } yield n
                }
        }

        step(batches.zipWithIndex, None, 0).flatMap { n =>
            if commit then log.info(s"submitted $n transactions").as(ExitCode.Success)
            else log.info("not submitting; pass --commit to run it").as(ExitCode.Success)
        }
    }

    /** The consolidated output the next batch spends: always index 0 of what we just built. */
    private def nextCarry(signed: Transaction): Option[Utxo] =
        Some(
          Utxo(
            TransactionInput(signed.id, 0),
            signed.body.value.outputs.head.value
          )
        )
}
