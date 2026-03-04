package hydrozoa.integration.stage1

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.bloxbean.cardano.client.util.HexUtil
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.config.head.initialization.HeadStartTimeGen.HeadStartTimeGen
import hydrozoa.config.head.initialization.{CappedValueGen, InitializationParametersGenTopDown}
import hydrozoa.config.head.multisig.timing.TxTimingGen
import hydrozoa.config.head.network.{CardanoNetwork, StandardCardanoNetwork}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.config.node.operation.liquidation.generateNodeOperationLiquidationConfig
import hydrozoa.config.node.operation.multisig.generateNodeOperationMultisigConfig
import hydrozoa.integration.stage1.Model.CurrentTime.{AfterCompetingFallbackStartTime, BeforeHappyPathExpiration}
import hydrozoa.integration.stage1.Model.{BlockCycle, CurrentTime}
import hydrozoa.integration.stage1.SuiteCardano.*
import hydrozoa.integration.yaci.DevKit
import hydrozoa.integration.yaci.DevKit.DevnetInfo
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.backend.cardano.CardanoBackendBlockfrost.URL
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendMock, MockState, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{BlockWeaver, CardanoLiaison, ConsensusActor, EventSequencer}
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.block.{BlockEffects, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.dapp.tx.{FinalizationTx, SettlementTx}
import hydrozoa.multisig.ledger.event.LedgerEventNumber
import java.util.concurrent.TimeUnit
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.{ModelBasedSuite, ScenarioGen}
import org.scalacheck.{Gen, Prop}
import org.typelevel.log4cats.Logger
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scalus.cardano.address.Network
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.{CardanoInfo, CertState, Coin, EvaluatorMode, PlutusScriptEvaluator, ProtocolParams, SlotConfig, Transaction, TransactionHash, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{Change, TransactionBuilder}
import test.TestPeerName.Alice
import test.{GoodNetwork, PeersNumberSpec, SeedPhrase, TestPeerName, TestPeersSpec}

/** Integration Stage 1 (the simplest).
  *   - Only three real actors are involved: [[JointLedger]], [[ConsensusActor]], and
  *     [[CardanoLiaison]]
  *
  * Notes:
  *   - The absence of the weaver prevents automatic block creation, including timed-out major
  *     blocks.
  */

// TODO: copied from cardano liaison test suite
class BlockWeaverMock extends Actor[IO, BlockWeaver.Request] {
    override def receive: Receive[IO, BlockWeaver.Request] = _ => IO.pure(())
}

enum SuiteCardano:
    case Mock(
        cardanoNetwork: CardanoNetwork
    )
    case Yaci(
        url: CardanoBackendBlockfrost.URL = DevKit.blockfrostApiBaseUri,
        protocolParams: ProtocolParams
    )
    case Public(
        cardanoNetwork: StandardCardanoNetwork,
        blockfrostKey: String
    )

case class Suite(
    suiteCardano: SuiteCardano,
    override val scenarioGen: ScenarioGen[Model.State, Stage1Sut],
    txTimingGen: TxTimingGen,
) extends ModelBasedSuite {

    override type Env = Stage1Env

    case class Stage1Env(
        startTime: java.time.Instant,
        cardanoNetwork: CardanoNetwork,
        genesisUtxo: List[TestPeerName] => Map[TestPeerName, Utxos]
    )

    override type State = Model.State
    override type Sut = Stage1Sut

    override val useTestControl: Boolean = suiteCardano match {
        case Mock(_) => true
        case _       => false
    }

    override def commandGenTweaker: [A] => (g: Gen[A]) => Gen[A] = suiteCardano match {
        case _: SuiteCardano.Mock => [A] => (g: Gen[A]) => g
        // When using Yaci and devnet we don't want to generate short sequences
        case _: SuiteCardano.Yaci   => [A] => (g: Gen[A]) => Gen.resize(200, g)
        case _: SuiteCardano.Public => [A] => (g: Gen[A]) => Gen.resize(200, g)
    }

    override def initEnv: Env = suiteCardano match {
        case SuiteCardano.Mock(network) =>
            Stage1Env(
              startTime = java.time.Instant.now(),
              cardanoNetwork = network,
              genesisUtxo = yaciTestSauceGenesis(network.network)
            )

        case SuiteCardano.Yaci(url, protocolParams) =>
            logger.info("Resetting Yaci...")
            DevKit.reset()

            // TODO: Here we need peer wallets/addresses
            val aliceAddress = ??? // Alice.address(Testnet)
            // logger.info(s"Topping up Alice's address ${aliceAddress.toBech32.get}")

            DevKit.topup(aliceAddress, Coin(20_000_000_000L))

            logger.info("Getting devnet info...")
            val devnetInfo: DevnetInfo = DevKit.devnetInfo()
            logger.debug(s"devnetInfo: $devnetInfo")

            val startTime = java.time.Instant.ofEpochSecond(devnetInfo.startTime)
            val testnet = Network.Testnet
            val cardanoInfo = CardanoInfo(
              protocolParams = protocolParams,
              network = testnet,
              slotConfig = SlotConfig(
                zeroTime = startTime.toEpochMilli,
                zeroSlot = 0,
                slotLength = devnetInfo.slotLength * 1_000L
              )
            )

            val cardanoNetwork: CardanoNetwork.Custom = CardanoNetwork.Custom(cardanoInfo)
            val backend = CardanoBackendBlockfrost.apply_(Right((cardanoNetwork, url)))

            val peerUtxos = backend
                .utxosAt(aliceAddress)
                .unsafeRunSync()
                .fold(err => throw RuntimeException(err), identity)
            val totalValue = Value.combine(peerUtxos.map((_, o) => o.value))
            val foo = CappedValueGen.generateCappedValue(cardanoNetwork)
            val outputValues = Gen
                .tailRecM((totalValue, List.empty: List[Value]))((rest, acc) =>
                    foo(rest, Some(20_000_000L), Some(1000_000_000), None).map(next =>
                        if next == rest
                        then Right(acc :+ next)
                        else Left((rest - next, acc :+ next))
                    )
                )
                .sample
                .get

            val splitTx: Transaction = (for {
                unbalanced <- TransactionBuilder
                    .build(
                      cardanoNetwork.cardanoInfo.network,
                      peerUtxos.map { case (utxoId, output) =>
                          Spend(Utxo(utxoId, output))
                      }.toList ++ outputValues.map(value =>
                          Send(
                            TransactionOutput.Babbage(
                              address = aliceAddress,
                              value = value
                            )
                          )
                      )
                    )
                balanced <- unbalanced.balanceContext(
                  diffHandler = Change.changeOutputDiffHandler(
                    _,
                    _,
                    protocolParams = protocolParams,
                    changeOutputIdx = 0
                  ),
                  protocolParams = protocolParams,
                  evaluator =
                      PlutusScriptEvaluator(cardanoInfo, EvaluatorMode.EvaluateAndComputeCost)
                )
            } yield balanced.transaction)
                .fold(err => throw RuntimeException(err.toString), identity)

            logger.trace(s"splitTx = ${HexUtil.encodeHexString(splitTx.toCbor)}")

            // TODO:
            val splitTxSigned = ???
            // splitTx.attachVKeyWitnesses(List(Alice.wallet.mkVKeyWitness(splitTx)))
            // logger.trace(s"splitTxSigned = ${HexUtil.encodeHexString(splitTxSigned.toCbor)}")

            val ret = backend.submitTx(splitTxSigned).unsafeRunSync()

            logger.trace(s"submission response: $ret")

            Thread.sleep(5_000)

            //
            val splitUpUtxos = backend
                .utxosAt(aliceAddress)
                .unsafeRunSync()
                .fold(err => throw RuntimeException(err), identity)

            Stage1Env(
              startTime = startTime,
              cardanoNetwork = CardanoNetwork.Custom(
                cardanoInfo
              ),
              genesisUtxo = _ => Map(Alice -> splitUpUtxos) // yaciTestSauceGenesis(testnet)
            )

        case SuiteCardano.Public(cardanoNetwork, blockfrostKey) =>

            // TODO: Here we need peer wallets/addresses
            val aliceAddress = ??? // Alice.address(Testnet)
            // logger.info(s"Topping up Alice's address ${aliceAddress.toBech32.get}")

            val backend = CardanoBackendBlockfrost.apply_(Left(cardanoNetwork), blockfrostKey)

            // logger.info(s"Splitting up utxos at Alice's address ${aliceAddress.toBech32.get}")
            //
            // val peerUtxos = backend
            //    .utxosAt(aliceAddress)
            //    .unsafeRunSync()
            //    .fold(err => throw RuntimeException(err), identity)
            //
            // logger.trace(s"existing utxos: $peerUtxos")
            //
            // val totalValue = Value.combine(peerUtxos.map((_, o) => o.value))
            // val foo = CappedValueGen.generateCappedValue(cardanoNetwork)
            // val outputValues = Gen
            //    .tailRecM((totalValue, List.empty: List[Value]))((rest, acc) =>
            //        foo(rest, Some(20_000_000L), Some(1000_000_000), None).map(next =>
            //            if next == rest
            //            then Right(acc :+ next)
            //            else Left((rest - next, acc :+ next))
            //        )
            //    )
            //    .sample
            //    .get
            //
            // val splitTx: Transaction = (for {
            //    unbalanced <- TransactionBuilder
            //        .build(
            //            cardanoNetwork.cardanoInfo.network,
            //            peerUtxos.map { case (utxoId, output) =>
            //                Spend(Utxo(utxoId, output))
            //            }.toList ++ outputValues.map(value =>
            //                Send(
            //                    TransactionOutput.Babbage(
            //                        address = aliceAddress,
            //                        value = value
            //                    )
            //                )
            //            )
            //        )
            //    balanced <- unbalanced.balanceContext(
            //        diffHandler = Change.changeOutputDiffHandler(
            //            _,
            //            _,
            //            protocolParams = cardanoNetwork.protocolParams,
            //            changeOutputIdx = 0
            //        ),
            //        protocolParams = cardanoNetwork.protocolParams,
            //        evaluator =
            //            PlutusScriptEvaluator(cardanoNetwork.cardanoInfo, EvaluatorMode.EvaluateAndComputeCost)
            //    )
            // } yield balanced.transaction)
            //    .fold(err => throw RuntimeException(err.toString), identity)
            //
            // logger.trace(s"splitTx = ${HexUtil.encodeHexString(splitTx.toCbor)}")
            //
            // val splitTxSigned =
            //    splitTx.attachVKeyWitnesses(List(Alice.wallet.mkVKeyWitness(splitTx)))
            //
            // logger.trace(s"splitTxSigned = ${HexUtil.encodeHexString(splitTxSigned.toCbor)}")
            //
            // val ret = backend.submitTx(splitTxSigned).unsafeRunSync()
            //
            // logger.trace(s"submission response: $ret")
            //
            // Thread.sleep(60_000)

            //
            val splitUpUtxos = backend
                .utxosAt(aliceAddress)
                .unsafeRunSync()
                .fold(err => throw RuntimeException(err), identity)

            Stage1Env(
              startTime = java.time.Instant.now(),
              cardanoNetwork = cardanoNetwork,
              genesisUtxo = _ => Map(Alice -> splitUpUtxos) // yaciTestSauceGenesis(testnet)
            )
    }

    val logger: org.slf4j.Logger = Logging.logger("Stage1.Suite")
    val loggerIO: Logger[IO] = Logging.loggerIO("Stage1.Suite")

    // ===================================
    // Initial state handling
    // ===================================

    override def genInitialState(env: Env): Gen[State] = {

        logger.trace(s"env start time: ${env.startTime}")
        logger.trace(s"env genesis utxo: ${env.genesisUtxo.apply(List(Alice))}")

        // One-peer head
        val spec =
            TestPeersSpec.apply(SeedPhrase.Yaci, GoodNetwork.Preprod, PeersNumberSpec.Exact(1))
        val ownTestPeer = Alice

        val generateHeadStartTime: HeadStartTimeGen = slotConfig =>
            Gen.const(env.startTime.quantize(slotConfig))
        val generateCardanoNetwork = Gen.const(env.cardanoNetwork)
        val generateTxTiming = txTimingGen

        for {

            testPeerToUtxos <- Gen.const(env.genesisUtxo(List(ownTestPeer)))

            config <- MultiNodeConfig.generate(spec)(
              generateHeadStartTime = generateHeadStartTime,
              generateTxTiming = generateTxTiming,
              generateInitializationParameters = InitializationParametersGenTopDown.GenWithDeps(
                generateGenesisUtxosL1 =
                    network => Gen.const(testPeerToUtxos.map((k, v) => k.headPeerNumber -> v))
              )
            )

            _ = logger.debug(s"total contingency: ${config.headConfig.fallbackContingency}")
            _ = logger.debug(s"l2 utxos: ${config.headConfig.initialL2Utxos.size}")
            _ = logger.debug(s"l2 total: ${config.headConfig.initialL2Value}")

            peerL1GenesisUtxos = testPeerToUtxos.values.flatten.toMap

            _ = logger.debug(s"peerL1GenesisUtxos: ${peerL1GenesisUtxos}")

            operationalMultisigConfig <- generateNodeOperationMultisigConfig
            operationalLiquidationConfig <- generateNodeOperationLiquidationConfig
        } yield Model
            .State(
              multiNodeConfig = config,
              operationalMultisigConfig = operationalMultisigConfig,
              operationalLiquidationConfig = operationalLiquidationConfig,
              nextLedgerEventNumber = LedgerEventNumber(0),
              currentTime = BeforeHappyPathExpiration(config.headConfig.headStartTime),
              blockCycle = BlockCycle.Done(BlockNumber.zero, BlockVersion.Full.zero),
              competingFallbackStartTime =
                  config.headConfig.txTiming.newFallbackStartTime(config.headConfig.headStartTime),
              activeUtxos = config.headConfig.initialL2Utxos,
              peerUtxosL1 = peerL1GenesisUtxos,
              peerGenesisUtxosL1 = peerL1GenesisUtxos,
              depositEnqueued = List.empty,
              depositsRegistered = List.empty,
              utxoLocked = Set.empty,
              depositSigned = Map.empty,
              depositSubmitted = List.empty,
              depositRejected = List.empty,
            )
            .applyContinuingL1Tx(config.headConfig.initializationTx.tx)
    }

    // ===================================
    // SUT handling
    // ===================================

    // TODO: do we want to run multiple SUTs when using L1 mock?
    override def canStartupNewSut(): Boolean = true

    override def startupSut(state: Model.State): IO[Sut] = {
        val multiNodeConfig = state.multiNodeConfig

        for {
            _ <- loggerIO.info("Creating new SUT")

            // Fast-forward to the current time if TestControl is used
            _ <- IO.whenA(useTestControl)(for {
                _ <- loggerIO.debug("Fast-forward to the current time...")

                // Before creating the actor system, if we are in the TestControl we need
                // to fast-forward to the zero block creation time.
                // Will take almost forever if is run after the actor system is spun up
                _ <- IO.sleep(
                  FiniteDuration(
                    state.currentTime.instant.instant.toEpochMilli,
                    TimeUnit.MILLISECONDS
                  )
                )
                now <- IO.realTimeInstant
                _ <- loggerIO.info(s"Current time: $now")
            } yield ())

            _ <- loggerIO.debug(s"peerKeys: ${multiNodeConfig.headConfig.headPeers.headPeerVKeys}")

            nodeConfig = multiNodeConfig.nodeConfigs(HeadPeerNumber.zero)

            // Actor system
            system <- ActorSystem[IO]("Stage1").allocated.map(_._1)

            // Note: Actor exceptions are logged by the supervision strategy but don't
            // automatically fail tests. To treat them as test failures check that the
            // system was not terminated in the [[shutdownSut]] action.

            // Run cardano L1 backend - a mock or Yaci
            cardanoBackendConfig = suiteCardano match {
                case Mock(_) =>
                    CardanoBackendConfig.Mock(
                      // TODO: remove
                      ownTestPeerName = ???,
                      network = multiNodeConfig.headConfig.cardanoInfo.network,
                      slotConfig = multiNodeConfig.headConfig.cardanoInfo.slotConfig,
                      protocolParams = multiNodeConfig.headConfig.cardanoInfo.protocolParams,
                      genesisUtxos = state.peerGenesisUtxosL1
                    )
                case Yaci(url, _) =>
                    CardanoBackendConfig.Blockfrost(
                      network = Right(
                        (CardanoNetwork.Custom(multiNodeConfig.headConfig.cardanoInfo), url)
                      )
                    )
                case Public(cardanoNetwork, blockfrostKey) =>
                    CardanoBackendConfig.Blockfrost(
                      network = Left(cardanoNetwork),
                      blockfrostKey = blockfrostKey
                    )

            }
            cardanoBackend <- mkCardanoBackend(cardanoBackendConfig)

            // Weaver stub
            blockWeaver <- system.actorOf(new BlockWeaverMock)

            // Cardano liaison
            cardanoLiaison <- system.actorOf(
              CardanoLiaison(nodeConfig, cardanoBackend, CardanoLiaison.Connections(blockWeaver))
            )

            // Event sequencer stub
            eventSequencerStub <- system.actorOf(new Actor[IO, EventSequencer.Request] {
                override def receive: Receive[IO, EventSequencer.Request] = _ => IO.pure(())
            })

            // Consensus actor
            consensusConnections = ConsensusActor.Connections(
              blockWeaver = blockWeaver,
              cardanoLiaison = cardanoLiaison,
              eventSequencer = eventSequencerStub,
              peerLiaisons = List.empty
            )

            consensusActor <- system.actorOf(ConsensusActor(nodeConfig, consensusConnections))

            // Agent actor
            jointLedgerD <- IO.deferred[JointLedger.Handle]
            agent <- system.actorOf(AgentActor(jointLedgerD, consensusActor, cardanoLiaison))

            jointLedgerConnections = JointLedger.Connections(
              consensusActor = agent,
              peerLiaisons = List()
            )

            jointLedger <- system.actorOf(
              JointLedger(
                nodeConfig,
                jointLedgerConnections
              )
            )

            _ <- jointLedgerD.complete(jointLedger)

        } yield Stage1Sut(
          headAddress = multiNodeConfig.headConfig.headMultisigAddress,
          system = system,
          cardanoBackend = cardanoBackend,
          agent = agent
        )
    }

    enum CardanoBackendConfig:
        case Mock(
            ownTestPeerName: TestPeerName,
            network: Network,
            slotConfig: SlotConfig,
            protocolParams: ProtocolParams,
            genesisUtxos: Utxos
        )
        case Blockfrost(
            network: Either[StandardCardanoNetwork, (CardanoNetwork.Custom, URL)],
            blockfrostKey: String = ""
        )

    private def mkCardanoBackend(config: CardanoBackendConfig): IO[CardanoBackend[IO]] =
        config match {
            case mock: CardanoBackendConfig.Mock =>
                for {
                    _ <- IO.pure(())
                    utxos = mock.genesisUtxos
                    mockState = MockState.apply(utxos)
                    cardanoBackend <- CardanoBackendMock.mockIO(
                      initialState = mockState,
                      mkContext = slot =>
                          Context(
                            env = UtxoEnv(
                              slot = slot,
                              params = mock.protocolParams,
                              certState = CertState.empty,
                              network = mock.network
                            ),
                            slotConfig = mock.slotConfig
                          )
                    )
                } yield cardanoBackend

            case CardanoBackendConfig.Blockfrost(network, blockfrostKey) =>
                val expectedProtocolParams = network.fold(
                  _.asInstanceOf[CardanoNetwork].cardanoProtocolParams,
                  _._1.cardanoProtocolParams
                )
                for {
                    _ <- loggerIO.info("Wait a bit for backend being ready...")
                    _ <- IO.sleep(1.second)
                    _ <- loggerIO.info(
                      "Creating Cardano backend and fetching the last epoch parameters to check they match ones in the head config..."
                    )
                    cardanoBackend <- CardanoBackendBlockfrost(
                      network = network,
                      apiKey = blockfrostKey
                    )
                    // Here we use start-up parameters
                    response <- cardanoBackend.getStartupParams
                    check = response
                        .fold(
                          err => throw RuntimeException(s"Cannot obtain protocol parameters: $err"),
                          actualParams => (actualParams == expectedProtocolParams) -> actualParams
                        )
                    _ <- IO.raiseWhen(!check._1)(
                      RuntimeException(
                        "Protocol parameters mismatch: " +
                            s"\nexpected: ${expectedProtocolParams}" +
                            s"\nactual: ${check._2}"
                      )
                    )
                } yield cardanoBackend
        }

    override def shutdownSut(lastState: State, sut: Sut): IO[Prop] = for {

        _ <- loggerIO.info("shutdownSut")

        /** Important: this action should ensure that the actor system was not terminated.
          *
          * Even more important: before terminating, make sure [[waitForIdle]] is called - otherwise
          * you just immediately shutdown the system and will get a false-positive test.
          *
          * Luckily enough, [[waitForIdle]] does exactly what we need in addition to checking the
          * mailboxes it also verifies that the system was not terminated.
          */
        _ <- sut.system.waitForIdle(maxTimeout = 1.second)

        // Next part of the property is to check that expected effects were submitted and are known to the Cardano backend.
        effects <- sut.effectsAcc.get

        expectedEffects: List[(String, TransactionHash)] = mkExpectedEffects(
          lastState.multiNodeConfig.headConfig.initialBlock.initializationTx.tx.id,
          lastState.multiNodeConfig.headConfig.initialBlock.fallbackTx.tx.id,
          effects,
          lastState.currentTime
        )

        _ <- IO.whenA(expectedEffects.nonEmpty)(
          loggerIO.info(s"Utxo set size: ${lastState.activeUtxos.size}") >>
              loggerIO.info("Expected effects:" + expectedEffects.map { case (label, hash) =>
                  s"\n\t- $label: $hash"
              }.mkString)
        )

        // In Yaci transactions may appear a bit slowly
        effectsResults <- {
            def poll(attempt: Int): IO[List[Either[Throwable, Boolean]]] =
                IO.traverse(expectedEffects) { case (_, hash) =>
                    sut.cardanoBackend.isTxKnown(hash)
                }.flatMap { results =>
                    val allKnown = results.forall(_.contains(true))
                    if allKnown || attempt >= 9 then IO.pure(results)
                    else IO.sleep(1.second) >> poll(attempt + 1)
                }
            poll(0)
        }

        // Finally we have to terminate the actor system, otherwise in TestControlownTestPeer
        // this will loop indefinitely.
        _ <- sut.system.terminate()
    } yield {
        val missing = expectedEffects.zip(effectsResults).collect {
            case ((label, txHash), Right(false)) => s"$label tx not found: $txHash"
            case ((label, txHash), Left(err))    => s"error checking $label tx $txHash: $err"
        }
        missing.isEmpty :| s"missing effects: ${missing.mkString(", ")}"
    }

    enum TxLabel:
        case Init, Settlement, Rollout, Finalization, Deinit, Fallback

    /** Compute the list of tx hashes expected to have been submitted to L1, each tagged with its
      * role.
      *
      * The initialization tx and happy-path backbone txs (settlementTx, finalizationTx, rolloutTxs,
      * deinitTx) are always expected.
      *
      * If currentTime is AfterCompetingFallbackStartTime and the last effect is not Final, the
      * competing fallback is also expected. The competing fallback is the one from the last Major
      * effect, or the initialization fallback if no Major effect exists yet.
      */
    private def mkExpectedEffects(
        initTxHash: TransactionHash,
        fallbackTxHash: TransactionHash,
        effects: List[BlockEffects.Unsigned],
        currentTime: CurrentTime
    ): List[(String, TransactionHash)] = {
        val initHash = (TxLabel.Init.toString, initTxHash)

        def payoutSuffix(n: Option[Int]): String = n.fold("")(c => s"($c payouts)")

        val happyPathHashes: List[(String, TransactionHash)] = effects.flatMap {
            case e: BlockEffects.Unsigned.Major =>
                (
                  s"${TxLabel.Settlement}${payoutSuffix(e.settlementTx.payoutCount)}",
                  e.settlementTx.tx.id
                ) ::
                    e.rolloutTxs.map(tx =>
                        (s"${TxLabel.Rollout}(${tx.payoutCount} payouts)", tx.tx.id)
                    )
            case e: BlockEffects.Unsigned.Final =>
                (
                  s"${TxLabel.Finalization}${payoutSuffix(e.finalizationTx.payoutCount)}",
                  e.finalizationTx.tx.id
                ) ::
                    e.rolloutTxs.map(tx =>
                        (s"${TxLabel.Rollout}(${tx.payoutCount} payouts)", tx.tx.id)
                    )
            case _: BlockEffects.Unsigned.Minor   => Nil
            case _: BlockEffects.Unsigned.Initial => Nil
        }

        val fallbackHash: List[(String, TransactionHash)] = currentTime match {
            case AfterCompetingFallbackStartTime(_)
                if !effects.lastOption.exists(_.isInstanceOf[BlockEffects.Unsigned.Final]) =>
                // Last Major's fallback, or the init fallback if no Major block completed yet
                effects
                    .collect { case e: BlockEffects.Unsigned.Major => e.fallbackTx.tx.id }
                    .lastOption
                    .orElse(Some(fallbackTxHash))
                    .map((TxLabel.Fallback.toString, _))
                    .toList
            case _ => Nil
        }

        initHash :: happyPathHashes ++ fallbackHash
    }
}

extension (tx: SettlementTx)
    def payoutCount: Option[Int] = tx match
        case t: SettlementTx.WithPayouts => Some(t.payoutCount)
        case _: SettlementTx.NoPayouts   => None

extension (tx: FinalizationTx)
    def payoutCount: Option[Int] = tx match
        case t: FinalizationTx.WithPayouts => Some(t.payoutCount)
        case _: FinalizationTx.NoPayouts   => None
