package hydrozoa.rulebased.evacuator

import cats.effect.{ExitCode, IO}
import cats.syntax.apply.*
import cats.syntax.contravariant.*
import com.monovore.decline.{Command, Opts}
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.{addrKeyHash, shelleyAddress}
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.joint.EvacuationMap.evacuationMapEncoder
import hydrozoa.multisig.ledger.l1.tx.RawTx
import io.circe.syntax.*
import java.nio.file.{Files, Path}
import scalus.cardano.address.ShelleyPaymentPart
import scalus.cardano.ledger.{Coin, EvaluatorMode, PlutusScriptEvaluator}
import scalus.cardano.txbuilder.Change

/** Stands up a treasury to evacuate, without needing a head to have produced one.
  *
  * A real evacuation map only exists inside a head that has run and fallen back, so testing an
  * evacuator against one means waiting for an incident and taking whatever shape it happens to hand
  * us. The properties worth measuring — chain depth, throughput, recovery from a rollback — are
  * exactly the ones an incident does not let us choose.
  *
  * What makes this safe is that `Evacuate` is permissionless and fully validated: the worst outcome
  * of a wrong map is a transaction the ledger rejects. Value conservation and membership are
  * checked on chain, so funds cannot be misdirected by a mistake here.
  *
  * Every payout goes back to an address we control, so the ada returns when the evacuation
  * completes and only fees are spent.
  */
object DeploySyntheticTreasury {

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(
          Slf4jMsgFormat.humanFormat("hydrozoa.rulebased.evacuator.DeploySyntheticTreasury")
        )

    private val headConfigPathArg: Opts[Path] =
        Opts.argument[String]("head-config.json").map(Path.of(_))

    private val privateConfigPathArg: Opts[Path] =
        Opts.argument[String]("peer-private.json").map(Path.of(_))

    private val entriesOpt: Opts[Int] =
        Opts
            .option[Int]("entries", "How many payouts the evacuation map should hold")
            .withDefault(500)

    private val perEntryOpt: Opts[Long] =
        Opts
            .option[Long]("lovelace-per-entry", "Ada per payout, in lovelace")
            .withDefault(2_000_000L)

    /** Report only; nothing is submitted unless this is passed. The default is a dry run because
      * this command locks up real funds, and the figures it prints are what decide whether that is
      * affordable.
      */
    private val commitOpt: Opts[Boolean] =
        Opts.flag("commit", "Actually submit; without it, report and stop").orFalse

    lazy val command: Command[IO[ExitCode]] =
        Command(
          name = "deploy-synthetic-treasury",
          header = "Stand up a rule-based treasury holding a generated evacuation map, for testing"
        )(
          (headConfigPathArg, privateConfigPathArg, entriesOpt, perEntryOpt, commitOpt)
              .mapN(run)
        )

    def run(
        headConfigPath: Path,
        privateConfigPath: Path,
        entries: Int,
        lovelacePerEntry: Long,
        commit: Boolean
    ): IO[ExitCode] =
        for {
            loaded <- NodeConfig.load(headConfigPath, privateConfigPath, None)
            (nodeConfig, backend) = loaded
            exit <- {
                given HeadConfig.Bootstrap.Section = nodeConfig.headConfig
                plan(nodeConfig, backend, entries, lovelacePerEntry, commit)
            }
        } yield exit

    private def plan(
        nodeConfig: NodeConfig,
        backend: CardanoBackend[IO],
        entries: Int,
        lovelacePerEntry: Long,
        commit: Boolean
    )(using HeadConfig.Bootstrap.Section): IO[ExitCode] = {
        val payTo = ShelleyPaymentPart.Key(nodeConfig.ownWallet.exportVerificationKey.addrKeyHash)

        SyntheticMap(entries, payTo, nodeConfig.headConfig.network, Coin(lovelacePerEntry)) match {
            case Left(violation) =>
                log.info(s"map does not build: $violation").as(ExitCode.Error)

            case Right(map) =>
                report(map, nodeConfig) *>
                    (if commit then submit(map, nodeConfig, backend)
                     else log.info("dry run; pass --commit to submit").as(ExitCode.Success))
        }
    }

    /** Write the map beside the config, so the evacuator can be pointed at the same preimage the
      * treasury commits to. Without it a run would have to regenerate the map and hope it matches.
      */
    private def writeMap(map: EvacuationMap, to: Path)(using
        CardanoNetwork.Section
    ): IO[Unit] =
        IO.blocking {
            Files.writeString(to, map.asJson.spaces2): Unit
        } *> log.info(s"evacuation map written to $to")

    private def submit(
        map: EvacuationMap,
        nodeConfig: NodeConfig,
        backend: CardanoBackend[IO]
    )(using HeadConfig.Bootstrap.Section): IO[ExitCode] = {
        val wallet = nodeConfig.ownWallet
        val walletAddress =
            wallet.exportVerificationKey.shelleyAddress()(using nodeConfig.headConfig)

        for {
            _ <- writeMap(map, Path.of("synthetic-evacuation-map.json"))(using nodeConfig)
            utxos <- backend
                .utxosAt(walletAddress)
                .flatMap(IO.fromEither(_).adaptError(e => RuntimeException(s"utxo query: $e")))
            _ <- log.info(s"funding from $walletAddress (${utxos.size} utxo(s))")

            built <- IO.fromEither(
              SyntheticTreasuryTx
                  .build(utxos, map, walletAddress)
                  .left
                  .map(e => RuntimeException(s"building the treasury tx failed: $e"))
            )
            params <- backend.fetchLatestParams.flatMap(
              IO.fromEither(_).adaptError(e => RuntimeException(s"protocol params: $e"))
            )
            balanced <- IO.fromEither(
              built
                  .balanceContext(
                    diffHandler = Change.changeOutputDiffHandler(
                      _,
                      _,
                      protocolParams = params,
                      changeOutputIdx = 2
                    ),
                    protocolParams = params,
                    evaluator = PlutusScriptEvaluator(
                      nodeConfig.headConfig.cardanoInfo,
                      EvaluatorMode.EvaluateAndComputeCost
                    )
                  )
                  .left
                  .map(e => RuntimeException(s"balancing failed: $e"))
                  .map(_.transaction)
            )
            signed = wallet.signTx(balanced)
            _ <- log.info(s"submitting ${signed.id}")
            result <- backend.submitTx(RawTx(signed))
            _ <- IO.fromEither(result.left.map(e => RuntimeException(s"submit failed: $e")))
            _ <- log.info(s"submitted: ${signed.id}")
            _ <- log.info("treasury utxo will be output #1 of that transaction once it confirms")
        } yield ExitCode.Success
    }

    /** What the run will cost and how long it will take, before anything is submitted. */
    private def report(
        map: EvacuationMap,
        nodeConfig: NodeConfig
    ): IO[Unit] = {
        val params = nodeConfig.headConfig.cardanoProtocolParams
        val funding = SyntheticMap.fundingRequired(map)
        val batch = BatchPlanner.maxBatchSizeFor(map, params)
        val txs = EvacuationPlan.txCount(map, params)
        val blocks = EvacuationPlan.minimumBlocks(map, params)

        for {
            _ <- log.info(s"evacuation map: ${map.size} entries")
            _ <- log.info(s"commitment:     ${map.kzgCommitment.toHex}")
            _ <- log.info(
              s"treasury must hold ${funding.value / 1_000_000.0} ada " +
                  "(returned to our own address as the evacuation drains it)"
            )
            _ <- log.info(s"plan: $txs transactions of up to $batch payouts each")
            _ <- log.info(
              s"floor: $blocks blocks — the block ex-unit limit admits " +
                  s"${BatchPlanner.txsPerBlockOfSize(batch, map, params)} of these per block"
            )
        } yield ()
    }
}
