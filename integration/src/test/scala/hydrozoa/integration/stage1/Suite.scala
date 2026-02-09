package hydrozoa.integration.stage1

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.applicative.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.config.HeadConfig.OwnPeer
import hydrozoa.config.{HeadConfig, NetworkInfo, RawConfig, StandardCardanoNetwork}
import hydrozoa.integration.stage1.CurrentTime.{AfterCompetingFallbackStartTime, BeforeHappyPathExpiration}
import hydrozoa.integration.stage1.SuiteCardano.*
import hydrozoa.integration.yaci.DevKit
import hydrozoa.integration.yaci.DevKit.DevnetInfo
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant, quantize}
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.backend.cardano.CardanoBackendBlockfrost.URL
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendMock, MockState, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.CardanoLiaisonTest.BlockWeaverMock
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.consensus.{CardanoLiaison, ConsensusActor, EventSequencer}
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.SpentUtxos
import hydrozoa.multisig.ledger.dapp.tx.{TxTiming, minInitTreasuryAda}
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq.Builder
import hydrozoa.rulebased.ledger.dapp.tx.genEquityShares
import hydrozoa.{attachVKeyWitnesses, maxNonPlutusTxFee}
import java.util.concurrent.TimeUnit
import monocle.Focus.focus
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.{CommandGen, ModelBasedSuite}
import org.scalacheck.{Gen, Prop, YetAnotherProperties}
import org.typelevel.log4cats.Logger
import scala.concurrent.duration.{DurationInt, FiniteDuration, HOURS}
import scalus.cardano.address.Network
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.{CardanoInfo, CertState, Coin, ProtocolParams, SlotConfig, TransactionHash, Utxo, Utxos, Value}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import spire.math.UByte
import test.TestPeer.Alice
import test.{TestPeer, minPubkeyAda, sumUtxoValues}

/** Integration Stage 1 (the simplest).
  *   - Only three real actors are involved: [[JointLedger]], [[ConsensusActor]], and
  *     [[CardanoLiaison]]
  *
  * Notes:
  *   - The absence of the weaver prevents automatic block creation, including timed-out major
  *     blocks.
  */
object Stage1Properties extends YetAnotherProperties("Integration Stage 1"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = {
        p.withWorkers(1)
            .withMinSuccessfulTests(100) // 10000
            .withMaxSize(100) // 500
    }

    private val preprod = StandardCardanoNetwork.Preprod

    /** Block propagation.
      *
      * This property checks that block promotion Minor -> Major * works correctly. It uses
      * [[ArbitraryEventsOnly]] which gives L2 event that are invalid. They are not strictly needed
      * for testing block propagation, which must work on empty blocks, but we additionally decided
      * to check block briefs.
      */
    // val _ = property("Block propagation works well on L1 mock") = Suite(
    //  suiteCardano = Mock(Left(preprod)),
    //  mkTxTiming = TxTiming.default,
    //  mkGenesisUtxos = yaciTestSauceGenesis(preprod.scalusNetwork),
    //  commandGen = ArbitraryEventsOnly
    // ).property()

    val _ = property("Block propagation works well on Yaci devkit") = Suite(
      suiteCardano = Yaci(
        protocolParams = DevKit.yaciParams
      ),
      mkTxTiming = TxTiming.yaci,
      mkGenesisUtxos = yaciTestSauceGenesis(preprod.scalusNetwork),
      commandGen = ArbitraryEventsOnly
    ).property()

    // val _ = property("Works well on Yaci DevKit (slower, reproducible)") = ???
    // val _ = property("Works well on Preview (even slower, non-reproducible)") = ???

enum SuiteCardano:
    case Mock(
        cardanoNetwork: Either[StandardCardanoNetwork, NetworkInfo]
    )
    case Yaci(
        url: CardanoBackendBlockfrost.URL = DevKit.blockfrostApiBaseUri,
        protocolParams: ProtocolParams
    )

case class Suite(
    suiteCardano: SuiteCardano,
    override val commandGen: CommandGen[ModelState, Stage1Sut],
    mkTxTiming: SlotConfig => TxTiming,
    mkGenesisUtxos: List[TestPeer] => Map[TestPeer, Utxos],
) extends ModelBasedSuite {

    override type Env = Stage1Env

    case class Stage1Env(
        zeroTimeSec: java.time.Instant,
        cardanoInfo: CardanoInfo
    )

    override type State = ModelState
    override type Sut = Stage1Sut

    override val useTestControl: Boolean = suiteCardano == Mock

    override def commandGenTweaker: [A] => (x$1: Gen[A]) => Gen[A] = suiteCardano match {
        case _: SuiteCardano.Mock => [A] => (g: Gen[A]) => g
        // When using Yaci (and devnet) we don't want to generate short sequences
        case _: SuiteCardano.Yaci => [A] => (g: Gen[A]) => Gen.resize(200, g)
    }

    override def initEnv: Env = suiteCardano match {
        case SuiteCardano.Mock(network) =>
            val zeroTimeSec = java.time.Instant.now()
            Stage1Env(
              zeroTimeSec = zeroTimeSec,
              cardanoInfo = network.toCardanoInfo
            )

        case SuiteCardano.Yaci(url, protocolParams) =>
            logger.info("Resetting Yaci...")
            DevKit.reset()
            val devnetInfo: DevnetInfo = DevKit.devnetInfo()
            logger.debug(s"devnetInfo: $devnetInfo")
            val zeroTimeSec = java.time.Instant.ofEpochSecond(devnetInfo.startTime)
            Stage1Env(
              zeroTimeSec = zeroTimeSec,
              cardanoInfo = CardanoInfo(
                protocolParams = protocolParams,
                network = Network.Testnet,
                slotConfig = SlotConfig(
                  zeroTime = zeroTimeSec.toEpochMilli,
                  zeroSlot = 0,
                  slotLength = devnetInfo.slotLength * 1_000L
                )
              )
            )
    }

    val logger: org.slf4j.Logger = Logging.logger("Stage1.Suite")
    val loggerIO: Logger[IO] = Logging.loggerIO("Stage1.Suite")

    // ===================================
    // SUT handling
    // ===================================

    override def startupSut(state: ModelState): IO[Sut] = {
        val cardanoInfo = state.cardanoInfo

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

            // Create HeadConfig
            ownTestPeer = state.ownTestPeer

            ownPeer = (
              OwnPeer(PeerId(0, 1), ownTestPeer.wallet),
              ownTestPeer.address(cardanoInfo.network),
              state.equityShares.peerShares(UByte(0)).equityShare
            )

            rawConfig = RawConfig(
              ownPeer = ownPeer,
              otherPeers = List.empty,
              receiveTimeout = 10.seconds,
              initializationTxBytes = state.initTxSigned.toCbor,
              initialFallbackTxBytes = state.fallbackTxSigned.toCbor,
              network = Right(
                NetworkInfo(
                  networkId = cardanoInfo.network.networkId,
                  slotConfig = cardanoInfo.slotConfig,
                  protocolParams = cardanoInfo.protocolParams
                )
              ),
              tallyFeeAllowance = Coin.ada(2),
              votingDuration = QuantizedFiniteDuration(
                finiteDuration = 24.hours,
                slotConfig = cardanoInfo.slotConfig
              ),
              txTiming = state.txTiming,
              startTime = state.currentTime.instant,
              resolvedUtxosForInitialization =
                  ResolvedUtxos(Map.from(state.spentUtxos.toList.map(_.toTuple))),
              // Can it be the peer's automated wallet though, at least for now?
              withdrawalFeeWallet = ownTestPeer.wallet,
              pollingPeriod = 5.seconds
            )

            // Parse HeadConfig
            headConfig = HeadConfig.parse(rawConfig) match {
                case Left(err) =>
                    throw RuntimeException(s"error parsing raw config: ${err.explain}")
                case Right(value) => value
            }

            _ <- loggerIO.debug(s"peerKeys: ${headConfig.headParameters.headPeers.peerKeys}")
            _ <- loggerIO.debug(s"ownPeer: ${headConfig.privateNodeSettings.ownPeer}")

            // Actor system
            system <- ActorSystem[IO]("Stage1").allocated.map(_._1)

            // Note: Actor exceptions are logged by the supervision strategy but don't
            // automatically fail tests. To treat them as test failures check that the
            // system was not terminated in the [[shutdownSut]] action.

            // Run cardano L1 backend - a mock or Yaci
            cardanoBackendConfig = suiteCardano match {
                case Mock(_) =>
                    CardanoBackendConfig.Mock(
                      ownTestPeer = ownTestPeer,
                      network = headConfig.cardanoInfo.network,
                      slotConfig = headConfig.cardanoInfo.slotConfig,
                      protocolParams = headConfig.cardanoInfo.protocolParams
                    )
                case Yaci(url, _) =>
                    CardanoBackendConfig.Yaci(
                      url = Right(url),
                      expectedProtocolParams = headConfig.cardanoInfo.protocolParams
                    )
            }
            cardanoBackend <- mkCardanoBackend(cardanoBackendConfig)

            // Weaver stub
            blockWeaver <- system.actorOf(new BlockWeaverMock)

            // Cardano liaison
            liaisonConfig = CardanoLiaison.Config(
              cardanoBackend = cardanoBackend,
              initializationTx = headConfig.initialBlock.effects.initializationTx,
              initializationFallbackTx = headConfig.initialBlock.effects.fallbackTx,
              receiveTimeout = 100.millis,
              slotConfig = cardanoInfo.slotConfig
            )

            liaisonConnections = CardanoLiaison.Connections(blockWeaver)

            cardanoLiaison <- system.actorOf(
              CardanoLiaison.apply(liaisonConfig, liaisonConnections)
            )

            // Event sequencer stub
            eventSequencerStub <- system.actorOf(new Actor[IO, EventSequencer.Request] {
                override def receive: Receive[IO, EventSequencer.Request] = _ => IO.pure(())
            })

            // Consensus actor
            consensusConfig = ConsensusActor.Config(
              peerNumber = headConfig.privateNodeSettings.ownPeer.peerId.peerNum,
              verificationKeys = headConfig.headParameters.headPeers.peerKeys.map((peerId, key) =>
                  peerId.peerNum -> key
              )
            )

            consensusConnections = ConsensusActor.Connections(
              blockWeaver = blockWeaver,
              cardanoLiaison = cardanoLiaison,
              eventSequencer = eventSequencerStub,
              peerLiaisons = List.empty
            )

            consensusActor <- system.actorOf(ConsensusActor(consensusConfig, consensusConnections))

            // Agent actor
            jointLedgerD <- IO.deferred[JointLedger.Handle]
            agent <- system.actorOf(AgentActor(jointLedgerD, consensusActor))

            // Joint ledger
            initialBlock1 = Block.MultiSigned.Initial(
              blockBrief = BlockBrief.Initial(
                header = BlockHeader.Initial(
                  startTime = headConfig.initialBlock.startTime,
                  kzgCommitment = headConfig.initialBlock.initialKzgCommitment
                )
              ),
              effects = headConfig.initialBlock.effects
            )

            jointLedgerConfig = JointLedger.Config(
              initialBlock = initialBlock1,
              peerId = headConfig.privateNodeSettings.ownPeer.peerId,
              wallet = headConfig.privateNodeSettings.ownPeer.wallet,
              tallyFeeAllowance = headConfig.headParameters.fallbackSettings.tallyFeeAllowance,
              tokenNames = headConfig.initialBlock.tokenNames,
              headMultisigScript = headConfig.headParameters.headPeers.headMultisigScript,
              cardanoInfo = cardanoInfo,
              initialBlockTime = headConfig.initialBlock.startTime,
              initialBlockKzg = headConfig.initialBlock.initialKzgCommitment,
              equityShares = headConfig.headParameters.equityShares,
              multisigRegimeUtxo = headConfig.initialBlock.multisigRegimeUtxo,
              votingDuration = headConfig.headParameters.ruleBasedRegimeSettings.votingDuration,
              initialTreasury = headConfig.initialBlock.initialTreasury,
              txTiming = headConfig.headParameters.multisigRegimeSettings.txTiming,
              initialFallbackValidityStart =
                  headConfig.initialBlock.effects.fallbackTx.validityStart
            )

            jointLedgerConnections = JointLedger.Connections(
              consensusActor = agent,
              peerLiaisons = List()
            )

            jointLedger <- system.actorOf(
              JointLedger(
                jointLedgerConfig,
                jointLedgerConnections
              )
            )

            _ <- jointLedgerD.complete(jointLedger)

        } yield Stage1Sut(system, cardanoBackend, agent)
    }

    enum CardanoBackendConfig:
        case Mock(
            ownTestPeer: TestPeer,
            network: Network,
            slotConfig: SlotConfig,
            protocolParams: ProtocolParams
        )
        case Yaci(
            url: Either[CardanoBackendBlockfrost.Network, CardanoBackendBlockfrost.URL],
            expectedProtocolParams: ProtocolParams
        )

    private def mkCardanoBackend(config: CardanoBackendConfig): IO[CardanoBackend[IO]] =
        config match {
            case mock: CardanoBackendConfig.Mock =>
                for {
                    _ <- IO.pure(())
                    // TODO: save utxos in the model state?
                    utxos = mkGenesisUtxos(List(mock.ownTestPeer)).values.flatten.toMap
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

            case CardanoBackendConfig.Yaci(url, expectedProtocolParams) =>
                for {
                    cardanoBackend <- CardanoBackendBlockfrost(url = url)
                    _ <- loggerIO.info("Wait a bit Yaci is ready... (")
                    _ <- IO.sleep(1.second)
                    _ <- loggerIO.info(
                      "Fetching last epoch parameters to check they match ones in the head config... ("
                    )
                    response <- cardanoBackend.latestParams
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

        expectedEffects: List[(TxLabel, TransactionHash)] = mkExpectedEffects(
          lastState.initTxSigned.id,
          lastState.fallbackTxSigned.id,
          effects,
          lastState.currentTime
        )

        _ <- IO.whenA(expectedEffects.nonEmpty)(
          loggerIO.info("Expected effects:" + expectedEffects.map { case (label, hash) =>
              s"\n\t- $label: $hash"
          }.mkString)
        )

        effectsResults <- IO.traverse(expectedEffects) { case (_, hash) =>
            sut.cardanoBackend.isTxKnown(hash)
        }

        // Finally we have to terminate the actor system, otherwise in TestControl
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
    ): List[(TxLabel, TransactionHash)] = {
        val initHash = (TxLabel.Init, initTxHash)

        val happyPathHashes: List[(TxLabel, TransactionHash)] = effects.flatMap {
            case e: BlockEffects.Unsigned.Major =>
                (TxLabel.Settlement, e.settlementTx.tx.id) ::
                    e.rolloutTxs.map(tx => (TxLabel.Rollout, tx.tx.id))
            case e: BlockEffects.Unsigned.Final =>
                (TxLabel.Finalization, e.finalizationTx.tx.id) ::
                    e.rolloutTxs.map(tx => (TxLabel.Rollout, tx.tx.id)) ++
                    e.deinitTx.map(tx => (TxLabel.Deinit, tx.tx.id))
            case _: BlockEffects.Unsigned.Minor   => Nil
            case _: BlockEffects.Unsigned.Initial => Nil
        }

        val fallbackHash: List[(TxLabel, TransactionHash)] = currentTime match {
            case AfterCompetingFallbackStartTime(_)
                if !effects.lastOption.exists(_.isInstanceOf[BlockEffects.Unsigned.Final]) =>
                // Last Major's fallback, or the init fallback if no Major block completed yet
                effects
                    .collect { case e: BlockEffects.Unsigned.Major => e.fallbackTx.tx.id }
                    .lastOption
                    .orElse(Some(fallbackTxHash))
                    .map((TxLabel.Fallback, _))
                    .toList
            case _ => Nil
        }

        initHash :: happyPathHashes ++ fallbackHash
    }

    // TODO: do we want to run multiple SUTs when using L1 mock?
    override def canStartupNewSut(): Boolean = true

    // ===================================
    // Initial state handling
    // ===================================

    override def genInitialState(env: Env): Gen[State] = {
        val cardanoInfo = env.cardanoInfo

        for {
            headStartTime <- Gen.const(env.zeroTimeSec.quantize(cardanoInfo.slotConfig))
            // If we want to use different peers we have to rework test peers' wallets
            // since now those wallets use ordinal from TestPeer - Bob is always peerNumber = 1
            // if it's the only peer in the head. But I don't see what it buys us.
            ownTestPeer = Alice
            peers = NonEmptyList.one(ownTestPeer) // useful to have since many gens want it
            ownPeerIndex = 0

            equityShares <- genEquityShares(NonEmptyList.one(ownTestPeer), cardanoInfo.network)

            seedUtxo <- Gen
                .oneOf(mkGenesisUtxos(List(ownTestPeer))(ownTestPeer))
                .flatMap((i, o) => Utxo(i, o))

            // NB: I don't think funding utxos make sense for this scenario
            spentUtxos = NonEmptyList.one(seedUtxo)

            // Initial treasury must be at least enough for the minAda of the treasury, and no more than the
            // sum of the seed+funding utxos, while leaving enough left for the estimated fee and the minAda of the change
            // output
            initialTreasuryCoin <- Gen
                .choose(
                  minInitTreasuryAda.value,
                  sumUtxoValues(spentUtxos.toList).coin.value
                      - maxNonPlutusTxFee(cardanoInfo.protocolParams).value
                      - minPubkeyAda().value
                )
                .map(Coin(_))
                .label("initial treasury coin")

            initialTreasury = Value(initialTreasuryCoin)

            txTiming = mkTxTiming(cardanoInfo.slotConfig)

            initTxConfig = InitializationTxSeq.Config(
              tallyFeeAllowance = Coin.ada(2),
              votingDuration = FiniteDuration(24, HOURS).quantize(cardanoInfo.slotConfig),
              cardanoInfo = cardanoInfo,
              peerKeys = peers.map(_.wallet.exportVerificationKeyBytes),
              startTime = headStartTime,
              txTiming = txTiming
            )

            initTxArgs =
                InitializationTxSeq.Builder.Args(
                  spentUtxos = SpentUtxos(seedUtxo, List.empty),
                  initialTreasury = initialTreasury,
                  initializationTxChangePP = ownTestPeer.address(cardanoInfo.network).payment,
                )

            headMultisigScript = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))

            initTxSeq <- InitializationTxSeq.Builder
                .build(initTxArgs, initTxConfig)
                .fold(
                  // TODO: this should be unified in the builder
                  err => {
                      err match {
                          case Builder.FallbackPRError(e)       => println(e)
                          case Builder.InitializationTxError(e) => println(e)
                          case Builder.FallbackTxError(e)       => println(e)
                      }
                      Gen.fail
                  },
                  Gen.const
                )

            initTxSigned = attachVKeyWitnesses(
              initTxSeq.initializationTx.tx,
              List(ownTestPeer.wallet.mkVKeyWitness(initTxSeq.initializationTx.tx))
            )
            fallbackTxSigned = attachVKeyWitnesses(
              initTxSeq.fallbackTx.tx,
              List(ownTestPeer.wallet.mkVKeyWitness(initTxSeq.fallbackTx.tx))
            )

        } yield ModelState(
          ownTestPeer = ownTestPeer,
          spentUtxos = spentUtxos,
          txTiming = txTiming,
          equityShares = equityShares,
          initTxSigned = initTxSigned,
          fallbackTxSigned = fallbackTxSigned,
          cardanoInfo = cardanoInfo,
          // Initial (zero) block is "done"
          currentTime = BeforeHappyPathExpiration(headStartTime),
          blockCycle = BlockCycle.Done(BlockNumber.zero, BlockVersion.Full.zero),
          competingFallbackStartTime = txTiming.newFallbackStartTime(headStartTime),
          activeUtxos = Map.empty
        )
    }
}
