package hydrozoa.rulebased.evacuator

import cats.effect.{ExitCode, IO}
import cats.syntax.apply.*
import cats.syntax.contravariant.*
import com.monovore.decline.{Command, Opts}
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.ledger.joint.EvacuationMap
import java.nio.file.Path
import scalus.cardano.address.ShelleyPaymentPart
import scalus.cardano.ledger.Coin

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
                plan(nodeConfig, entries, lovelacePerEntry, commit)
            }
        } yield exit

    private def plan(
        nodeConfig: NodeConfig,
        entries: Int,
        lovelacePerEntry: Long,
        commit: Boolean
    )(using HeadConfig.Bootstrap.Section): IO[ExitCode] = {
        val payTo = ShelleyPaymentPart.Key(nodeConfig.ownWallet.exportVerificationKey.addrKeyHash)

        SyntheticMap(entries, payTo, nodeConfig.headConfig.network, Coin(lovelacePerEntry)) match {
            case Left(violation) =>
                log.info(s"map does not build: $violation").as(ExitCode.Error)

            case Right(map) => report(map, nodeConfig, commit)
        }
    }

    /** What the run will cost and how long it will take, before anything is submitted. */
    private def report(
        map: EvacuationMap,
        nodeConfig: NodeConfig,
        commit: Boolean
    ): IO[ExitCode] = {
        val params = nodeConfig.headConfig.cardanoProtocolParams
        val funding = SyntheticMap.fundingRequired(map)
        val batch = BatchPlanner.maxBatchSize(map.size, params)
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
                  s"${BatchPlanner.txsPerBlock(batch, params)} of these per block"
            )
            _ <-
                if commit then log.info("--commit is not implemented yet; nothing submitted")
                else log.info("dry run; pass --commit to submit")
        } yield ExitCode.Success
    }
}
