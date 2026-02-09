package hydrozoa.integration.stage1

//import cats.data.NonEmptyList
//import cats.effect.IO
//import cats.effect.unsafe.implicits.global
//import cats.syntax.applicative.*
//import com.suprnation.actor.Actor.{Actor, Receive}
//import com.suprnation.actor.ActorSystem
//import com.suprnation.typelevel.actors.syntax.*
//import hydrozoa.config.HeadConfig.OwnPeer
//import hydrozoa.config.{HeadConfig, NetworkInfo, RawConfig, StandardCardanoNetwork}
//import hydrozoa.integration.stage1.CurrentTime.{AfterCompetingFallbackStartTime, BeforeHappyPathExpiration}
//import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
//import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant, quantize}
//import hydrozoa.lib.logging.Logging
//import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState, yaciTestSauceGenesis}
//import hydrozoa.multisig.consensus.CardanoLiaisonTest.BlockWeaverMock
//import hydrozoa.multisig.consensus.peer.PeerId
//import hydrozoa.multisig.consensus.{CardanoLiaison, ConsensusActor, EventSequencer}
//import hydrozoa.multisig.ledger.JointLedger
//import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader, BlockNumber, BlockVersion}
//import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
//import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.SpentUtxos
//import hydrozoa.multisig.ledger.dapp.tx.{TxTiming, minInitTreasuryAda}
//import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq
//import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq.Builder
//import hydrozoa.rulebased.ledger.dapp.tx.genEquityShares
//import hydrozoa.{attachVKeyWitnesses, maxNonPlutusTxFee}
//import java.util.concurrent.TimeUnit
//import org.scalacheck.Prop.propBoolean
//import org.scalacheck.commands.{CommandGen, ModelBasedSuite}
//import org.scalacheck.{Gen, Prop, YetAnotherProperties}
//import org.typelevel.log4cats.Logger
//import scala.concurrent.duration.{DurationInt, FiniteDuration, HOURS}
//import scalus.cardano.ledger.rules.{Context, UtxoEnv}
//import scalus.cardano.ledger.{CardanoInfo, CertState, Coin, TransactionHash, Utxo, Utxos, Value}
//import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
//import spire.math.UByte
//import test.TestPeer.Alice
//import test.{TestPeer, minPubkeyAda, sumUtxoValues}
//
///** Integration Stage 1 (the simplest).
//  *   - Only three real actors are involved: [[JointLedger]], [[ConsensusActor]], and
//  *     [[CardanoLiaison]]
//  *
//  * Notes:
//  *   - The absence of the weaver prevents automatic block creation, including timed-out major
//  *     blocks.
//  */
//object Stage1Properties extends YetAnotherProperties("Integration Stage 1"):
//
//    override def overrideParameters(
//        p: org.scalacheck.Test.Parameters
//    ): org.scalacheck.Test.Parameters = {
//        p.withWorkers(1)
//            .withMinSuccessfulTests(100) // 10000
//            .withMaxSize(100) // 500
//    }
//
//    private val preprod = StandardCardanoNetwork.Preprod
//
//    /** This property uses [[ArbitraryEventsOnly]] which gives L2 events that are invalid. It's
//      * useful to check that block promotion Minor -> Major works correctly.
//      */
//    val _ = property("Block propagation works well on L1 mock") = Suite(
//      useTestControl = true,
//      network = Left(preprod),
//      getGenesisUtxos = yaciTestSauceGenesis(preprod.scalusNetwork),
//      commandGen = ArbitraryEventsOnly
//    ).property()
//
//    // val _ = property("Works well on Yaci DevKit (slower, reproducible)") = ???
//    // val _ = property("Works well on Preview (even slower, non-reproducible)") = ???
//
//case class Suite(
//    override val useTestControl: Boolean,
//    override val commandGen: CommandGen[ModelState, Stage1Sut],
//    network: Either[StandardCardanoNetwork, NetworkInfo],
//    getGenesisUtxos: List[TestPeer] => Map[TestPeer, Utxos],
//) extends ModelBasedSuite {
//
//    override type State = ModelState
//    override type Sut = Stage1Sut
//
//    val logger: Logger[IO] = Logging.loggerIO("Stage1.Suite")
//
//    // ===================================
//    // SUT handling
//    // ===================================
//
//    private val cardanoInfo: CardanoInfo = network.toCardanoInfo
//
//    override def newSut(state: ModelState): IO[Sut] = {
//
//        for {
//            _ <- logger.info("Creating new SUT")
//            ownTestPeer <- IO.pure(state.ownTestPeer)
//
//            ownPeer = (
//              OwnPeer(PeerId(0, 1), ownTestPeer.wallet),
//              ownTestPeer.address(cardanoInfo.network),
//              state.equityShares.peerShares(UByte(0)).equityShare
//            )
//
//            // Create HeadConfig
//            rawConfig = RawConfig(
//              ownPeer = ownPeer,
//              otherPeers = List.empty,
//              receiveTimeout = 10.seconds,
//              initializationTxBytes = state.initTxSigned.toCbor,
//              initialFallbackTxBytes = state.fallbackTxSigned.toCbor,
//              network = network,
//              tallyFeeAllowance = Coin.ada(2),
//              votingDuration = QuantizedFiniteDuration(
//                finiteDuration = 24.hours,
//                slotConfig = cardanoInfo.slotConfig
//              ),
//              txTiming = state.txTiming,
//              startTime = state.currentTime.instant,
//              resolvedUtxosForInitialization =
//                  ResolvedUtxos(Map.from(state.spentUtxos.toList.map(_.toTuple))),
//              // Can it be the peer's automated wallet though, at least for now?
//              withdrawalFeeWallet = ownTestPeer.wallet,
//              pollingPeriod = 5.seconds
//            )
//
//            // Parse HeadConfig to extract initialBlock
//            headConfig = HeadConfig.parse(rawConfig) match {
//                case Left(err) =>
//                    throw RuntimeException(s"error parsing raw config: ${err.explain}")
//                case Right(value) => value
//            }
//
//            _ <- logger.debug(s"peerKeys: ${headConfig.headParameters.headPeers.peerKeys}")
//            _ <- logger.debug(s"ownPeer: ${headConfig.privateNodeSettings.ownPeer}")
//
//            // TODO: make it optional, not needed for Yaci
//            // Fast-forward to the current time
//            now <- IO.realTimeInstant
//            _ <- logger.debug(s"Current time: $now")
//
//            // Before creating the actor system, if we are in the TestControl we need
//            // to fast-forward to the zero block creation time.
//            _ <- IO.sleep(
//              FiniteDuration(state.currentTime.instant.instant.toEpochMilli, TimeUnit.MILLISECONDS)
//            )
//
//            now <- IO.realTimeInstant
//            _ <- logger.info(s"Current time: $now")
//
//            // Actor system
//            system <- ActorSystem[IO]("Stage1").allocated.map(_._1)
//
//            // Note: Actor exceptions are logged by the supervision strategy but don't
//            // automatically fail tests. To treat them as test failures check that the
//            // system was not terminated in the [[shutdownSut]] action.
//
//            // Cardano L1 backend mock
//            // TODO: save utxos in the model state?
//            utxos = getGenesisUtxos(List(ownTestPeer)).values.flatten.toMap
//            mockState = MockState.apply(utxos)
//            cardanoBackend <- CardanoBackendMock.mockIO(
//              initialState = mockState,
//              mkContext = slot =>
//                  Context(
//                    env = UtxoEnv(
//                      slot = slot,
//                      params = headConfig.cardanoInfo.protocolParams,
//                      certState = CertState.empty,
//                      network = headConfig.cardanoInfo.network
//                    ),
//                    slotConfig = headConfig.cardanoInfo.slotConfig
//                  )
//            )
//
//            // Weaver stub
//            blockWeaver <- system.actorOf(new BlockWeaverMock)
//
//            // Cardano liaison
//            liaisonConfig = CardanoLiaison.Config(
//              cardanoBackend = cardanoBackend,
//              initializationTx = headConfig.initialBlock.effects.initializationTx,
//              initializationFallbackTx = headConfig.initialBlock.effects.fallbackTx,
//              receiveTimeout = 100.millis,
//              slotConfig = cardanoInfo.slotConfig
//            )
//
//            liaisonConnections = CardanoLiaison.Connections(blockWeaver)
//
//            cardanoLiaison <- system.actorOf(
//              CardanoLiaison.apply(liaisonConfig, liaisonConnections)
//            )
//
//            // Event sequencer stub
//            eventSequencerStub <- system.actorOf(new Actor[IO, EventSequencer.Request] {
//                override def receive: Receive[IO, EventSequencer.Request] = _ => IO.pure(())
//            })
//
//            // Consensus actor
//            consensusConfig = ConsensusActor.Config(
//              peerNumber = headConfig.privateNodeSettings.ownPeer.peerId.peerNum,
//              verificationKeys = headConfig.headParameters.headPeers.peerKeys.map((peerId, key) =>
//                  peerId.peerNum -> key
//              )
//            )
//
//            consensusConnections = ConsensusActor.Connections(
//              blockWeaver = blockWeaver,
//              cardanoLiaison = cardanoLiaison,
//              eventSequencer = eventSequencerStub,
//              peerLiaisons = List.empty
//            )
//
//            consensusActor <- system.actorOf(ConsensusActor(consensusConfig, consensusConnections))
//
//            // Agent actor
//            jointLedgerD <- IO.deferred[JointLedger.Handle]
//            agent <- system.actorOf(AgentActor(jointLedgerD, consensusActor))
//
//            // Joint ledger
//            initialBlock1 = Block.MultiSigned.Initial(
//              blockBrief = BlockBrief.Initial(
//                header = BlockHeader.Initial(
//                  startTime = headConfig.initialBlock.startTime,
//                  kzgCommitment = headConfig.initialBlock.initialKzgCommitment
//                )
//              ),
//              effects = headConfig.initialBlock.effects
//            )
//
//            jointLedgerConfig = JointLedger.Config(
//              initialBlock = initialBlock1,
//              peerId = headConfig.privateNodeSettings.ownPeer.peerId,
//              wallet = headConfig.privateNodeSettings.ownPeer.wallet,
//              tallyFeeAllowance = headConfig.headParameters.fallbackSettings.tallyFeeAllowance,
//              tokenNames = headConfig.initialBlock.tokenNames,
//              headMultisigScript = headConfig.headParameters.headPeers.headMultisigScript,
//              cardanoInfo = cardanoInfo,
//              initialBlockTime = headConfig.initialBlock.startTime,
//              initialBlockKzg = headConfig.initialBlock.initialKzgCommitment,
//              equityShares = headConfig.headParameters.equityShares,
//              multisigRegimeUtxo = headConfig.initialBlock.multisigRegimeUtxo,
//              votingDuration = headConfig.headParameters.ruleBasedRegimeSettings.votingDuration,
//              initialTreasury = headConfig.initialBlock.initialTreasury,
//              txTiming = headConfig.headParameters.multisigRegimeSettings.txTiming,
//              initialFallbackValidityStart =
//                  headConfig.initialBlock.effects.fallbackTx.validityStart
//            )
//
//            jointLedgerConnections = JointLedger.Connections(
//              consensusActor = agent,
//              peerLiaisons = List()
//            )
//
//            jointLedger <- system.actorOf(
//              JointLedger(
//                jointLedgerConfig,
//                jointLedgerConnections
//              )
//            )
//
//            _ <- jointLedgerD.complete(jointLedger)
//
//        } yield Stage1Sut(system, cardanoBackend, agent)
//    }
//
//    // TODO: split up:  preShutdownCondition / shutdownSut
//    override def shutdownSut(state: State, sut: Sut): IO[Prop] = for {
//
//        _ <- logger.info("shutdownSut")
//
//        /** Important: this action should ensure that the actor system was not terminated.
//          *
//          * Even more important: before terminating, make sure [[waitForIdle]] is called - otherwise
//          * you just immediately shutdown the system and will get a false-positive test.
//          *
//          * Luckily enough, [[waitForIdle]] does exactly what we need in addition to checking the
//          * mailboxes it also verifies that the system was not terminated.
//          */
//        _ <- sut.system.waitForIdle(maxTimeout = 1.second)
//
//        // Next part of the property is to check that expected effects were submitted and are known to the Cardano backend.
//        effects <- sut.effectsAcc.get
//
//        expectedEffects: List[(TxLabel, TransactionHash)] = mkExpectedEffects(
//          state.initTxSigned.id,
//          state.fallbackTxSigned.id,
//          effects,
//          state.currentTime
//        )
//
//        _ <- IO.whenA(expectedEffects.nonEmpty)(
//          logger.info("Expected effects:" + expectedEffects.map { case (label, hash) =>
//              s"\n\t- $label: $hash"
//          }.mkString)
//        )
//
//        effectsResults <- IO.traverse(expectedEffects) { case (_, hash) =>
//            sut.cardanoBackend.isTxKnown(hash)
//        }
//
//        // Finally we have to terminate the actor system, otherwise in TestControl
//        // this will loop indefinitely.
//        _ <- sut.system.terminate()
//
//        // TODO shutdown Yaci? Clean up the public testnet?
//    } yield {
//        val missing = expectedEffects.zip(effectsResults).collect {
//            case ((label, txHash), Right(false)) => s"$label tx not found: $txHash"
//            case ((label, txHash), Left(err))    => s"error checking $label tx $txHash: $err"
//        }
//        missing.isEmpty :| s"missing effects: ${missing.mkString(", ")}"
//    }
//
//    enum TxLabel:
//        case Init, Settlement, Rollout, Finalization, Deinit, Fallback
//
//    /** Compute the list of tx hashes expected to have been submitted to L1, each tagged with its
//      * role.
//      *
//      * The initialization tx and happy-path backbone txs (settlementTx, finalizationTx, rolloutTxs,
//      * deinitTx) are always expected.
//      *
//      * If currentTime is AfterCompetingFallbackStartTime and the last effect is not Final, the
//      * competing fallback is also expected. The competing fallback is the one from the last Major
//      * effect, or the initialization fallback if no Major effect exists yet.
//      */
//    private def mkExpectedEffects(
//        initTxHash: TransactionHash,
//        fallbackTxHash: TransactionHash,
//        effects: List[BlockEffects.Unsigned],
//        currentTime: CurrentTime
//    ): List[(TxLabel, TransactionHash)] = {
//        val initHash = (TxLabel.Init, initTxHash)
//
//        val happyPathHashes: List[(TxLabel, TransactionHash)] = effects.flatMap {
//            case e: BlockEffects.Unsigned.Major =>
//                (TxLabel.Settlement, e.settlementTx.tx.id) ::
//                    e.rolloutTxs.map(tx => (TxLabel.Rollout, tx.tx.id))
//            case e: BlockEffects.Unsigned.Final =>
//                (TxLabel.Finalization, e.finalizationTx.tx.id) ::
//                    e.rolloutTxs.map(tx => (TxLabel.Rollout, tx.tx.id)) ++
//                    e.deinitTx.map(tx => (TxLabel.Deinit, tx.tx.id))
//            case _: BlockEffects.Unsigned.Minor   => Nil
//            case _: BlockEffects.Unsigned.Initial => Nil
//        }
//
//        val fallbackHash: List[(TxLabel, TransactionHash)] = currentTime match {
//            case AfterCompetingFallbackStartTime(_)
//                if !effects.lastOption.exists(_.isInstanceOf[BlockEffects.Unsigned.Final]) =>
//                // Last Major's fallback, or the init fallback if no Major block completed yet
//                effects
//                    .collect { case e: BlockEffects.Unsigned.Major => e.fallbackTx.tx.id }
//                    .lastOption
//                    .orElse(Some(fallbackTxHash))
//                    .map((TxLabel.Fallback, _))
//                    .toList
//            case _ => Nil
//        }
//
//        initHash :: happyPathHashes ++ fallbackHash
//    }
//
//    // TODO: do we want to run multiple SUTs when using L1 mock?
//    override def canCreateNewSut(
//        candidateState: State,
//        inactiveSuts: Iterable[State],
//        runningSuts: Iterable[State]
//    ): Boolean = inactiveSuts.isEmpty && runningSuts.isEmpty
//
//    // ===================================
//    // Initial state handling
//    // ===================================
//
//    override def genInitialState: Gen[State] = {
//
//        for {
//            headStartTime <- Gen
//                .const(realTimeQuantizedInstant(cardanoInfo.slotConfig).unsafeRunSync())
//                .label("Zero block creation time")
//
//            // If we want to use different peers we have to rework test peers' wallets
//            // since now those wallets use ordinal from TestPeer - Bob is always peerNumber = 1
//            // if it's the only peer in the head. But I don't see what it buys us.
//            ownTestPeer = Alice
//            peers = NonEmptyList.one(ownTestPeer) // useful to have since many gens want it
//            ownPeerIndex = 0
//
//            equityShares <- genEquityShares(NonEmptyList.one(ownTestPeer), cardanoInfo.network)
//                .label("Equity shares")
//
//            seedUtxo <- Gen
//                .oneOf(getGenesisUtxos(List(ownTestPeer))(ownTestPeer))
//                .flatMap((i, o) => Utxo(i, o))
//                .label("Seed utxo")
//
//            // NB: I don't think funding utxos make sense for this scenario
//            spentUtxos = NonEmptyList.one(seedUtxo)
//
//            // Initial treasury must be at least enough for the minAda of the treasury, and no more than the
//            // sum of the seed+funding utxos, while leaving enough left for the estimated fee and the minAda of the change
//            // output
//            initialTreasuryCoin <- Gen
//                .choose(
//                  minInitTreasuryAda.value,
//                  sumUtxoValues(spentUtxos.toList).coin.value
//                      - maxNonPlutusTxFee(cardanoInfo.protocolParams).value
//                      - minPubkeyAda().value
//                )
//                .map(Coin(_))
//                .label("initial treasury coin")
//
//            initialTreasury = Value(initialTreasuryCoin)
//
//            txTiming = TxTiming.default(cardanoInfo.slotConfig)
//
//            initTxConfig = InitializationTxSeq.Config(
//              tallyFeeAllowance = Coin.ada(2),
//              votingDuration = FiniteDuration(24, HOURS).quantize(cardanoInfo.slotConfig),
//              cardanoInfo = cardanoInfo,
//              peerKeys = peers.map(_.wallet.exportVerificationKeyBytes),
//              startTime = headStartTime,
//              txTiming = txTiming
//            )
//
//            initTxArgs =
//                InitializationTxSeq.Builder.Args(
//                  spentUtxos = SpentUtxos(seedUtxo, List.empty),
//                  initialTreasury = initialTreasury,
//                  initializationTxChangePP = ownTestPeer.address(cardanoInfo.network).payment,
//                )
//
//            headMultisigScript = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
//
//            initTxSeq <- InitializationTxSeq.Builder
//                .build(initTxArgs, initTxConfig)
//                .fold(
//                  // TODO: this should be unified in the builder
//                  err => {
//                      err match {
//                          case Builder.FallbackPRError(e)       => println(e)
//                          case Builder.InitializationTxError(e) => println(e)
//                          case Builder.FallbackTxError(e)       => println(e)
//                      }
//                      Gen.fail
//                  },
//                  Gen.const
//                )
//
//            initTxSigned = attachVKeyWitnesses(
//              initTxSeq.initializationTx.tx,
//              List(ownTestPeer.wallet.mkVKeyWitness(initTxSeq.initializationTx.tx))
//            )
//            fallbackTxSigned = attachVKeyWitnesses(
//              initTxSeq.fallbackTx.tx,
//              List(ownTestPeer.wallet.mkVKeyWitness(initTxSeq.fallbackTx.tx))
//            )
//
//        } yield ModelState(
//          ownTestPeer = ownTestPeer,
//          spentUtxos = spentUtxos,
//          txTiming = txTiming,
//          equityShares = equityShares,
//          initTxSigned = initTxSigned,
//          fallbackTxSigned = fallbackTxSigned,
//          slotConfig = cardanoInfo.slotConfig,
//          // Initial (zero) block is "done"
//          currentTime = BeforeHappyPathExpiration(headStartTime),
//          blockCycle = BlockCycle.Done(BlockNumber.zero, BlockVersion.Full.zero),
//          competingFallbackStartTime = txTiming.newFallbackStartTime(headStartTime),
//          activeUtxos = Map.empty
//        )
//    }
//}
