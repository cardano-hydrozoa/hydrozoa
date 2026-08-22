package hydrozoa.rulebased.evacuator

import cats.effect.{ExitCode, IO}
import cats.syntax.apply.*
import cats.syntax.contravariant.*
import com.monovore.decline.{Command, Opts}
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.NodeConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.joint.EvacuationMap.evacuationMapDecoder
import hydrozoa.multisig.ledger.joint.{EvacuationMap, evacuationKeyOrdering}
import hydrozoa.multisig.ledger.l1.tx.RawTx
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTx
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedRegimeUtxo, RuleBasedTreasuryUtxo}
import io.circe.parser.decode
import java.nio.file.{Files, Path}
import scalus.cardano.ledger.*

/** Drains a rule-based treasury as fast as the chain will take it.
  *
  * The transaction chain is computable offline: every `Evacuate` spends the treasury and produces
  * the next, so once the outstanding set is known, so is every batch after it. That is what this
  * exploits — it builds and submits back to back rather than polling between transactions, and it
  * never re-reads history it already has.
  *
  * Submitting without pausing is also what protects the claim. While an unbroken chain spending the
  * current treasury sits in the network's mempools there is no unclaimed treasury utxo for anyone
  * else to take; every pause re-opens that window for a block or two.
  */
object RunEvacuator {

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(
          Slf4jMsgFormat.humanFormat("hydrozoa.rulebased.evacuator.RunEvacuator")
        )

    private val headConfigPathArg: Opts[Path] =
        Opts.argument[String]("head-config.json").map(Path.of(_))

    private val privateConfigPathArg: Opts[Path] =
        Opts.argument[String]("peer-private.json").map(Path.of(_))

    private val mapPathOpt: Opts[Path] =
        Opts
            .option[String]("map", "The evacuation map's preimage, as JSON")
            .map(Path.of(_))
            .withDefault(Path.of("synthetic-evacuation-map.json"))

    /** The transaction that opened this regime — for a real head, its fallback. Everything the
      * treasury has paid out since is reachable from it, and nothing before it is relevant.
      */
    private val anchorOpt: Opts[String] =
        Opts.option[String]("anchor", "Tx hash the regime began at (a real head's fallback tx)")

    private val limitOpt: Opts[Option[Int]] =
        Opts
            .option[Int]("max-txs", "Stop after this many transactions (default: drain it)")
            .orNone

    private val commitOpt: Opts[Boolean] =
        Opts.flag("commit", "Actually submit; without it, build and report only").orFalse

    lazy val command: Command[IO[ExitCode]] =
        Command(
          name = "run-evacuator",
          header = "Drain a rule-based treasury, submitting a chain of Evacuate transactions"
        )(
          (headConfigPathArg, privateConfigPathArg, mapPathOpt, anchorOpt, limitOpt, commitOpt)
              .mapN(run)
        )

    def run(
        headConfigPath: Path,
        privateConfigPath: Path,
        mapPath: Path,
        anchor: String,
        maxTxs: Option[Int],
        commit: Boolean
    ): IO[ExitCode] =
        for {
            loaded <- NodeConfig.load(headConfigPath, privateConfigPath, None)
            (nodeConfig, backend) = loaded
            exit <- {
                given HeadConfig.Bootstrap.Section = nodeConfig.headConfig
                drive(nodeConfig, backend, mapPath, TransactionHash.fromHex(anchor), maxTxs, commit)
            }
        } yield exit

    private def drive(
        nodeConfig: NodeConfig,
        backend: CardanoBackend[IO],
        mapPath: Path,
        anchorTx: TransactionHash,
        maxTxs: Option[Int],
        commit: Boolean
    )(using HeadConfig.Bootstrap.Section): IO[ExitCode] = {
        val params = nodeConfig.headConfig.cardanoProtocolParams
        val wallet = nodeConfig.ownWallet
        val walletAddress =
            wallet.exportVerificationKey.shelleyAddress()(using nodeConfig.headConfig)
        val treasuryAddress = nodeConfig.headConfig.ruleBasedTreasuryAddress
        val beacon =
            (
              nodeConfig.headConfig.headMultisigScript.policyId,
              nodeConfig.headConfig.headTokenNames.treasuryTokenName
            )

        for {
            json <- IO.blocking(Files.readString(mapPath))
            map <- IO.fromEither(
              decode[EvacuationMap](json).left.map(e => RuntimeException(s"map: $e"))
            )
            _ <- log.info(s"map: ${map.size} entries, commitment ${map.kzgCommitment.toHex}")

            treasuryUtxos <- backend
                .utxosAt(treasuryAddress, beacon)
                .flatMap(IO.fromEither(_).adaptError(e => RuntimeException(s"treasury: $e")))
            _ <- IO.raiseWhen(treasuryUtxos.isEmpty)(
              RuntimeException(s"no treasury utxo bearing our beacon at $treasuryAddress")
            )
            treasury <- IO.fromEither(
              RuleBasedTreasuryUtxo
                  .parse(Utxo(treasuryUtxos.head._1, treasuryUtxos.head._2))
                  .left
                  .map(e => RuntimeException(s"treasury utxo does not parse: $e"))
            )
            _ <- log.info(s"treasury: ${treasury.utxoId}")

            walletUtxos <- backend
                .utxosAt(walletAddress)
                .flatMap(IO.fromEither(_).adaptError(e => RuntimeException(s"wallet: $e")))

            // Collateral must be a separate utxo from the one paying fees: the builder spends it as
            // a regular input and returns it fee-subtracted, so sharing it with another input
            // would double-spend within one transaction.
            collateral <- IO.fromEither(
              walletUtxos.toList
                  .sortBy { case (_, o) => -o.value.coin.value }
                  .collectFirst(Function.unlift { case (i, o) =>
                      CollateralUtxo.parse(Utxo(i, o)).toOption
                  })
                  .toRight(RuntimeException("no usable collateral utxo in the wallet"))
            )

            // The regime utxo is found the same way the validator finds it: by its HRWT beacon at
            // the head multisig address. Reading it from config would assume a head that ran.
            regimeUtxos <- backend
                .utxosAt(
                  nodeConfig.headConfig.headMultisigAddress,
                  (
                    nodeConfig.headConfig.headMultisigScript.policyId,
                    nodeConfig.headConfig.headTokenNames.regimeWitnessTokenName
                  )
                )
                .flatMap(IO.fromEither(_).adaptError(e => RuntimeException(s"regime: $e")))
            _ <- IO.raiseWhen(regimeUtxos.isEmpty)(RuntimeException("no regime utxo on chain"))
            regime = RuleBasedRegimeUtxo(regimeUtxos.head._1)
            _ <- log.info(s"regime:   ${regime.input}")

            // What is still owed, not what the map originally held: a treasury part-way through an
            // evacuation commits to the residual, so planning against the full preimage builds
            // proofs against a set the validator no longer recognises. Reconstruction replays the
            // Evacuate redeemers already on chain and checks its own answer against the datum's
            // commitment before anything is built on it.
            // The anchor is where the regime began — the transaction that produced the first
            // treasury — so the walk covers every Evacuate since. Anchoring on the current
            // treasury would find no history and silently conclude nothing had been paid out.
            outstanding <- OutstandingSet
                .reconstruct(backend, map, beacon, anchorTx, treasuryCommitment(treasury))
                .flatMap(IO.fromEither(_))
                .map(_.outstanding)
            _ <- log.info(s"outstanding: ${outstanding.size} of ${map.size} entries")

            plan = EvacuationPlan.plan(outstanding, params, maxTxs).toList
            _ <- log.info(
              s"plan: ${plan.size} transactions, ${plan.headOption.fold(0)(_.batchSize)} payouts each"
            )

            exit <-
                if !commit then
                    log.info("not submitting; pass --commit to run it").as(ExitCode.Success)
                else submitChain(plan, treasury, collateral, regime, nodeConfig, backend)
        } yield exit
    }

    /** The commitment the treasury currently advertises — what any reconstruction must reproduce.
      */
    private def treasuryCommitment(
        treasury: RuleBasedTreasuryUtxo
    ): String = treasury.treasuryOutput.datum match {
        case r: hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved =>
            r.evacuationActive.toHex
        case _ => ""
    }

    /** Build and submit each transaction, threading the treasury and collateral the previous one
      * produced. No query between steps: their outrefs are known the moment the parent is built.
      */
    private def submitChain(
        plan: List[EvacuationPlan.Step],
        treasury0: RuleBasedTreasuryUtxo,
        collateral0: CollateralUtxo,
        regime: RuleBasedRegimeUtxo,
        nodeConfig: NodeConfig,
        backend: CardanoBackend[IO]
    )(using HeadConfig.Bootstrap.Section): IO[ExitCode] = {
        val wallet = nodeConfig.ownWallet

        def step(
            remaining: List[EvacuationPlan.Step],
            treasury: RuleBasedTreasuryUtxo,
            collateral: CollateralUtxo,
            submitted: Int
        ): IO[Int] = remaining match {
            case Nil => IO.pure(submitted)
            case s :: rest =>
                val allRemaining =
                    EvacuationMap(s.batch.evacuationMap ++ s.remainingAfter.evacuationMap)
                IO.fromEither(
                  EvacuationTx
                      .Build(
                        inputTreasuryUtxo = treasury,
                        regimeUtxo = regime,
                        evacuateesToTryNext = s.batch,
                        allRemainingEvacuatees = allRemaining,
                        collateralUtxo = collateral
                      )
                      .result(using nodeConfig)
                      .left
                      .map(e => RuntimeException(s"build failed at step ${s.index}: $e"))
                ).flatMap { evac =>
                    val signed = wallet.signTx(evac.tx)
                    for {
                        result <- backend.submitTx(RawTx(signed))
                        _ <- IO.fromEither(
                          result.left
                              .map(e => RuntimeException(s"submit failed at step ${s.index}: $e"))
                        )
                        _ <- log.info(
                          s"step ${s.index}: ${s.batchSize} payouts, ${signed.id}"
                        )
                        // The successor treasury is output 1 (the returned collateral is 0), and
                        // the collateral we just got back funds the next transaction. Both are
                        // known from the transaction we built, so nothing needs re-reading.
                        nextTreasury = evac.treasuryUtxoProduced.copy(
                          utxoId = TransactionInput(signed.id, 1)
                        )
                        nextCollateral <- IO.fromEither(
                          CollateralUtxo
                              .parse(
                                Utxo(
                                  TransactionInput(signed.id, 0),
                                  evac.tx.body.value.outputs.head.value
                                )
                              )
                              .left
                              .map(e => RuntimeException(s"collateral at step ${s.index}: $e"))
                        )
                        n <- step(rest, nextTreasury, nextCollateral, submitted + 1)
                    } yield n
                }
        }

        step(plan, treasury0, collateral0, 0).flatMap { n =>
            log.info(s"submitted $n transactions").as(ExitCode.Success)
        }
    }
}
