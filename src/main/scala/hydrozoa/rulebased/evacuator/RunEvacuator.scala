package hydrozoa.rulebased.evacuator

import cats.effect.{ExitCode, IO}
import cats.syntax.apply.*
import cats.syntax.contravariant.*
import com.monovore.decline.{Command, Opts}
import hydrozoa.app.cli.DemoConfig
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork.cardanoNetworkDecoder
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.ledger.joint.EvacuationMap.evacuationMapDecoder
import hydrozoa.multisig.ledger.joint.{EvacuationMap, evacuationKeyOrdering}
import hydrozoa.multisig.ledger.l1.tx.RawTx
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTx
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedRegimeUtxo, RuleBasedTreasuryUtxo}
import io.circe.parser.decode
import java.nio.file.{Files, Path}
import scala.concurrent.duration.*
import scalus.cardano.address.ShelleyAddress
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
          (
            headConfigPathArg,
            privateConfigPathArg,
            mapPathOpt,
            anchorOpt,
            limitOpt,
            commitOpt
          )
              .mapN(run)
        )

    /** Everything the evacuator needs: the head's public parameters, the evacuation operation
      * settings, and a wallet to pay fees and collateral with.
      *
      * Deliberately avoids [[hydrozoa.config.node.NodeConfig.load]], which refuses any wallet whose
      * verification key is not among the head's configured peers. Evacuation is permissionless —
      * the validator authorises an `Evacuate` on the payouts it makes, not on who signs it — so the
      * party draining a dead head is typically not a peer of it, and must not be required to hold a
      * peer's key to act.
      */
    private final case class EvacuatorConfig(
        headConfig: HeadConfig,
        override val nodeOperationEvacuationConfig: NodeOperationEvacuationConfig,
        wallet: PeerWallet
    ) extends HeadConfig.Bootstrap.Section,
          NodeOperationEvacuationConfig.Section {
        override def headConfigBootstrap: HeadConfig.Bootstrap = headConfig.headConfigBootstrap
    }

    private def loadConfig(
        headConfigPath: Path,
        privateConfigPath: Path
    ): IO[(EvacuatorConfig, CardanoBackend[IO])] =
        for {
            headStr <- IO.blocking(Files.readString(headConfigPath))
            headJson <- IO.fromEither(io.circe.parser.parse(headStr))
            privStr <- IO.blocking(Files.readString(privateConfigPath))
            privJson <- IO.fromEither(io.circe.parser.parse(privStr))

            network <- IO.fromEither(
              headJson.hcursor.get[CardanoNetwork]("cardanoNetwork")(using cardanoNetworkDecoder)
            )
            apiKey <- IO.fromEither(privJson.hcursor.get[String]("blockfrostApiKey"))
            evacuationConfig <- IO.fromEither(
              privJson.hcursor.get[NodeOperationEvacuationConfig]("nodeOperationEvacuationConfig")
            )

            backend <- network match {
                case n: StandardCardanoNetwork =>
                    CardanoBackendBlockfrost(
                      Left(n),
                      apiKey,
                      tracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)
                    )
                case c: CardanoNetwork.Custom =>
                    IO.raiseError(
                      RuntimeException(s"custom network $c needs an explicit Blockfrost URL")
                    )
            }

            headConfig <- HeadConfig
                .fromJson(headStr, backend)
                .foldF(
                  err => IO.raiseError(RuntimeException(s"head config: $err")),
                  IO.pure
                )
            wallet <- DemoConfig.readWallet(privateConfigPath)
        } yield (EvacuatorConfig(headConfig, evacuationConfig, wallet), backend)

    def run(
        headConfigPath: Path,
        privateConfigPath: Path,
        mapPath: Path,
        anchor: String,
        maxTxs: Option[Int],
        commit: Boolean
    ): IO[ExitCode] =
        for {
            loaded <- loadConfig(headConfigPath, privateConfigPath)
            (config, backend) = loaded
            exit <- {
                given HeadConfig.Bootstrap.Section = config.headConfig
                drive(
                  config,
                  backend,
                  mapPath,
                  TransactionHash.fromHex(anchor),
                  maxTxs,
                  commit
                )
            }
        } yield exit

    private def drive(
        config: EvacuatorConfig,
        backend: CardanoBackend[IO],
        mapPath: Path,
        anchorTx: TransactionHash,
        maxTxs: Option[Int],
        commit: Boolean
    )(using HeadConfig.Bootstrap.Section): IO[ExitCode] = {
        val params = config.headConfig.cardanoProtocolParams
        val wallet = config.wallet
        val walletAddress =
            wallet.exportVerificationKey.shelleyAddress()(using config.headConfig)
        val treasuryAddress = config.headConfig.ruleBasedTreasuryAddress
        val beacon =
            (
              config.headConfig.headMultisigScript.policyId,
              config.headConfig.headTokenNames.treasuryTokenName
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
                  config.headConfig.headMultisigAddress,
                  (
                    config.headConfig.headMultisigScript.policyId,
                    config.headConfig.headTokenNames.regimeWitnessTokenName
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

            // Preflight. The chain is funded by ONE utxo, not by a balance, so build the first
            // transaction for real and read its actual fee rather than estimating from protocol
            // parameters — every transaction in the chain is near-identical, so it predicts the
            // rest to well under a percent.
            funded <- plan.headOption match {
                case None => IO.pure(collateral)
                case Some(first) =>
                    for {
                        probe <- IO.fromEither(
                          buildStep(first, treasury, collateral, regime, config)
                        )
                        feePerTx = probe.tx.body.value.fee
                        needed = WalletFunding.required(feePerTx, plan.size)
                        have = collateral.collateralOutput.coin
                        _ <- log.info(
                          f"funding: ${plan.size} txs at ${feePerTx.value / 1e6}%.6f ada each " +
                              f"needs ${needed.value / 1e6}%.6f ada in one utxo; " +
                              f"collateral holds ${have.value / 1e6}%.6f"
                        )
                        c <-
                            if have.value >= needed.value then IO.pure(collateral)
                            else
                                fundChain(
                                  walletUtxos,
                                  walletAddress,
                                  needed,
                                  have,
                                  feePerTx,
                                  config,
                                  backend,
                                  commit
                                )
                    } yield c
            }

            exit <-
                if !commit then
                    log.info("not submitting; pass --commit to run it").as(ExitCode.Success)
                else submitChain(plan, treasury, funded, regime, config, backend)
        } yield exit
    }

    /** Gather the wallet into one collateral utxo.
      *
      * This is not offered as a choice. Pointing an evacuator at a wallet is an instruction to
      * drain the treasury with it, and a wallet that holds the ada but in the wrong shape is a
      * detail of that wallet, not a decision for whoever launched the run — which by then may be
      * the rule-based regime itself, with nobody to ask. The only unrecoverable case is a wallet
      * that does not hold enough ada at all, and that still stops the run.
      */
    private def fundChain(
        walletUtxos: Utxos,
        walletAddress: ShelleyAddress,
        needed: Coin,
        have: Coin,
        feePerTx: Coin,
        config: EvacuatorConfig,
        backend: CardanoBackend[IO],
        commit: Boolean
    ): IO[CollateralUtxo] = {
        val fundable = WalletFunding.fundableTxs(have, feePerTx)
        val shortfall =
            f"collateral holds ${have.value / 1e6}%.6f ada, enough for $fundable of the planned " +
                f"transactions; the chain needs ${needed.value / 1e6}%.6f ada in a SINGLE utxo"
        val selection = WalletFunding.select(walletUtxos, needed)

        for {
            _ <- log.info(shortfall)
            _ <- IO.raiseWhen(selection.ada.value < needed.value)(
              RuntimeException(
                f"the whole wallet holds only ${selection.ada.value / 1e6}%.6f ada across " +
                    s"${selection.utxos.size} utxos — it cannot fund this chain at all"
              )
            )
            _ <- log.info(
              f"consolidating ${selection.utxos.size} utxos into one " +
                  f"(${selection.ada.value / 1e6}%.6f ada" +
                  (if selection.hasTokens then ", tokens to a separate output)" else ")")
            )
            params <- backend.fetchLatestParams.flatMap(
              IO.fromEither(_).adaptError(e => RuntimeException(s"protocol params: $e"))
            )
            tx <- IO.fromEither(
              WalletFunding.consolidationTx(selection, walletAddress, params)(using
                config.headConfig
              )
            )
            signed = config.wallet.signTx(tx)
            collateral <- IO.fromEither(WalletFunding.collateralOf(signed))
            _ <-
                if !commit then log.info(s"consolidation built (not submitted): ${signed.id}")
                else
                    for {
                        _ <- log.info(s"submitting consolidation ${signed.id}")
                        r <- backend.submitTx(RawTx(signed))
                        _ <- IO.fromEither(
                          r.left.map(e => RuntimeException(s"consolidation submit failed: $e"))
                        )
                        // Wait for it before starting the chain. The chain could spend it
                        // unconfirmed — it spends its own unconfirmed outputs throughout — but
                        // this one is the ROOT: if it were dropped, every transaction built on
                        // it dies. One block is cheap against a run measured in half-hours.
                        _ <- awaitUtxo(backend, walletAddress, collateral.input)
                    } yield ()
        } yield collateral
    }

    /** Poll until a utxo we just created is visible on chain, so nothing is built on a transaction
      * that never landed.
      */
    private def awaitUtxo(
        backend: CardanoBackend[IO],
        address: ShelleyAddress,
        input: TransactionInput,
        attempts: Int = 30
    ): IO[Unit] =
        if attempts <= 0 then
            IO.raiseError(RuntimeException(s"consolidation utxo $input never appeared on chain"))
        else
            backend
                .utxosAt(address)
                .flatMap(IO.fromEither(_).adaptError(e => RuntimeException(s"wallet: $e")))
                .flatMap { utxos =>
                    if utxos.contains(input) then log.info(s"consolidation confirmed: $input")
                    else IO.sleep(20.seconds) *> awaitUtxo(backend, address, input, attempts - 1)
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
        config: EvacuatorConfig,
        backend: CardanoBackend[IO]
    ): IO[ExitCode] = {
        val wallet = config.wallet

        def step(
            remaining: List[EvacuationPlan.Step],
            treasury: RuleBasedTreasuryUtxo,
            collateral: CollateralUtxo,
            submitted: Int
        ): IO[Int] = remaining match {
            case Nil => IO.pure(submitted)
            case s :: rest =>
                IO.fromEither(buildStep(s, treasury, collateral, regime, config)).flatMap { evac =>
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

    /** Build one step of the chain. Shared so the preflight prices a REAL transaction — the same
      * one that will be submitted — rather than a reconstruction of it.
      */
    private def buildStep(
        s: EvacuationPlan.Step,
        treasury: RuleBasedTreasuryUtxo,
        collateral: CollateralUtxo,
        regime: RuleBasedRegimeUtxo,
        config: EvacuatorConfig
    ): Either[Throwable, EvacuationTx] =
        EvacuationTx
            .Build(
              inputTreasuryUtxo = treasury,
              regimeUtxo = regime,
              evacuateesToTryNext = s.batch,
              allRemainingEvacuatees =
                  EvacuationMap(s.batch.evacuationMap ++ s.remainingAfter.evacuationMap),
              collateralUtxo = collateral
            )
            .result(using config)
            .left
            .map(e => RuntimeException(s"build failed at step ${s.index}: $e"))
}
