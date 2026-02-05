package hydrozoa.integration.stage1

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.applicative.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.config.HeadConfig.OwnPeer
import hydrozoa.config.{HeadConfig, NetworkInfo, RawConfig, StandardCardanoNetwork}
import hydrozoa.integration.stage1.AgentActor.CompleteBlock
import hydrozoa.integration.stage1.BlockCycle.*
import hydrozoa.integration.stage1.CurrentTime.{AfterCompetingFallbackStartTime, BeforeHappyPathExpiration, InSilencePeriod}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant, quantize}
import hydrozoa.lib.cardano.scalus.given_Choose_QuantizedInstant
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.CardanoLiaisonTest.BlockWeaverMock
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.consensus.{CardanoLiaison, ConsensusActor, EventSequencer}
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.block.BlockBrief.{Final, Major, Minor}
import hydrozoa.multisig.ledger.block.{Block, BlockBody, BlockBrief, BlockEffects, BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.SpentUtxos
import hydrozoa.multisig.ledger.dapp.tx.{TxTiming, minInitTreasuryAda}
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq.Builder
import hydrozoa.multisig.ledger.event.LedgerEvent
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.rulebased.ledger.dapp.tx.genEquityShares
import hydrozoa.{Address, L1, UtxoSetL1, attachVKeyWitnesses, maxNonPlutusTxFee}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.ModelBasedSuite
import org.scalacheck.{Arbitrary, Gen, Prop, YetAnotherProperties}
import scala.concurrent.duration.{DurationInt, FiniteDuration, HOURS}
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.{CertState, Coin, SlotConfig, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import spire.math.UByte
import test.Generators.Hydrozoa.ArbitraryInstances.given_Arbitrary_LedgerEvent
import test.TestPeer.Alice
import test.{TestPeer, minPubkeyAda, sumUtxoValues}
import org.slf4j.Logger

/** Integration Stage 1 (the simplest).
  *   - Only three real actors are involved: [[JointLedger]], [[ConsensusActor]], and
  *     [[CardanoLiaison]]
  *
  * Notes:
  *   - The absence of the weaver prevents automatic block creation, including timed-out major
  *     blocks.
  *   - There is a customizable delay before starting every new block. If the delay happens to be
  *     long enough so the latest fallback becomes active, all next commands are NoOp and the
  *     fallback is expected to be submitted. Otherwise, only happy path effects are expected to be
  *     submitted.
  *
  * TODO:
  *   - use different generators, let's say having only invalid events (or we can remove events
  *     completely) allows to verify block promotion
  */
object Stage1Properties extends YetAnotherProperties("Integration Stage 1"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = {
        p.withWorkers(1)
            .withMinSuccessfulTests(10000)
            .withMaxSize(500)
    }

    private val preprod = StandardCardanoNetwork.Preprod

    val _ = property("Works well on L1 mock (fast, reproducible)") = Stage1(
      network = Left(preprod),
      genesisUtxos = yaciTestSauceGenesis(preprod.scalusNetwork)
    ).property()

    // val _ = property("Works well on Yaci DevKit (slower, reproducible)") = ???
    // val _ = property("Works well on Preview (even slower, non-reproducible)") = ???

case class Stage1(
    network: Either[StandardCardanoNetwork, NetworkInfo],
    genesisUtxos: Map[TestPeer, UtxoSetL1]
) extends ModelBasedSuite {

    override type State = ModelState
    override type Sut = Stage1Sut

    // ===================================
    // SUT handling
    // ===================================

    override def newSut(state: ModelState): IO[Sut] = {
        import state.headConfig.*

        for {

            now <- IO.realTimeInstant
            _ <- IO.println(s"Current time: $now")

            // Before creating the actor system, if we are in the TestControl we need
            // to fast-forward to the zero block creation time.
            // TODO: make it optional
            // NB: Using advance for commands can be used here, but I (Ilia) think
            // it s a bit different thing so I decided not to piggyback on commands.
            _ <- IO.sleep(
              FiniteDuration(state.currentTime.instant.instant.toEpochMilli, TimeUnit.MILLISECONDS)
            )

            now <- IO.realTimeInstant
            _ <- IO.println(s"Current time: $now")

            logging <- Logging.create

            // Actor system
            system <- ActorSystem[IO]("Stage1").allocated.map(_._1)

            // Note: Actor exceptions are logged by the supervision strategy but don't
            // automatically fail tests. To treat them as test failures check that the
            // system was not terminated in the [[shutdownSut]] action.

            // Cardano L1 backend mock
            utxos = genesisUtxos.values.flatten.map((k, v) => k.untagged -> v.untagged).toMap
            mockState = MockState.apply(utxos)
            cardanoBackend <- CardanoBackendMock.mockIO(
              initialState = mockState,
              mkContext = slot =>
                  Context(
                    env = UtxoEnv(
                      slot = slot,
                      params = state.headConfig.cardanoInfo.protocolParams,
                      certState = CertState.empty,
                      network = state.headConfig.cardanoInfo.network
                    ),
                    slotConfig = state.headConfig.cardanoInfo.slotConfig
                  )
            )

            // Weaver stub
            blockWeaver <- system.actorOf(new BlockWeaverMock)

            // Cardano liaison
            liaisonConfig = CardanoLiaison.Config(
              cardanoBackend = cardanoBackend,
              initializationTx = initialBlock.effects.initializationTx,
              initializationFallbackTx = initialBlock.effects.fallbackTx,
              receiveTimeout = 100.millis,
              slotConfig = cardanoInfo.slotConfig,
              logging = logging
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
              peerNumber = privateNodeSettings.ownPeer.peerId.peerNum,
              verificationKeys =
                  headParameters.headPeers.peerKeys.map((peerId, key) => peerId.peerNum -> key)
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
                  startTime = initialBlock.startTime,
                  kzgCommitment = initialBlock.initialKzgCommitment
                )
              ),
              effects = initialBlock.effects
            )
            jointLedgerConfig = JointLedger.Config(
              initialBlock = initialBlock1,
              peerId = privateNodeSettings.ownPeer.peerId,
              wallet = privateNodeSettings.ownPeer.wallet,
              tallyFeeAllowance = headParameters.fallbackSettings.tallyFeeAllowance,
              tokenNames = initialBlock.tokenNames,
              headMultisigScript = headParameters.headPeers.headMultisigScript,
              cardanoInfo = cardanoInfo,
              initialBlockTime = initialBlock.startTime,
              initialBlockKzg = initialBlock.initialKzgCommitment,
              equityShares = headParameters.equityShares,
              multisigRegimeUtxo = initialBlock.multisigRegimeUtxo,
              votingDuration = headParameters.ruleBasedRegimeSettings.votingDuration,
              initialTreasury = initialBlock.initialTreasury,
              txTiming = headParameters.multisigRegimeSettings.txTiming,
              initialFallbackValidityStart = initialBlock.effects.fallbackTx.validityStart
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

    // TODO: split up:  preShutdownCondition / shutdownSut
    override def shutdownSut(state: State, sut: Sut): IO[Prop] = for {

        // _ <- IO.println(s"shutdownSut state=${state}")
        _ <- IO.println("shutdownSut")

        /** Important: this action should ensure that the actor system was not terminated.
          *
          * Even more important: before terminating, make sure [[waitForIdle]] is called - otherwise
          * you just immediately shutdown the system and will get a false-positive test.
          *
          * Luckily enough, [[waitForIdle]] does exactly what we need in addition to checking the
          * mailboxes it also verifies that the system was not terminated.
          */
        _ <- sut.system.waitForIdle(maxTimeout = 1.second)

        _ <- IO.println("waitForIdle complete")

        // Next part of the property is to check that expected effects were submitted and are known to the Cardano backend.
        effects <- sut.effectsAcc.get

        // _ <- IO.println(s"shutdownSut effects: ${effects.length}")

        expectedEffects: List[(TxLabel, TransactionHash)] = mkExpectedEffects(
          state.headConfig.initialBlock,
          effects,
          state.currentTime
        )

        _ <- IO.whenA(expectedEffects.nonEmpty)(
          IO.println("\nExpected effects:") >>
              IO.traverse_(expectedEffects) { case (label, hash) =>
                  IO.println(s"\t- $label: $hash")
              }
        )

        effectsResults <- IO.traverse(expectedEffects) { case (_, hash) =>
            sut.cardanoBackend.isTxKnown(hash)
        }

        // Finally we have to terminate the actor system, otherwise in TestControl
        // this will loop indefinitely.
        _ <- sut.system.terminate()

        // TODO shutdown Yaci? Clean up the public testnet?
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
        initialBlock: HeadConfig.InitialBlock,
        effects: List[BlockEffects.Unsigned],
        currentTime: CurrentTime
    ): List[(TxLabel, TransactionHash)] = {
        val initHash = (TxLabel.Init, initialBlock.initializationTx.tx.id)

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
                    .orElse(Some(initialBlock.initialFallbackTx.tx.id))
                    .map((TxLabel.Fallback, _))
                    .toList
            case _ => Nil
        }

        initHash :: happyPathHashes ++ fallbackHash
    }

    // TODO: do we want to run multiple SUTs when using L1 mock?
    override def canCreateNewSut(
        candidateState: State,
        inactiveSuts: Iterable[State],
        runningSuts: Iterable[State]
    ): Boolean = inactiveSuts.isEmpty && runningSuts.isEmpty

    // ===================================
    // Initial state handling
    // ===================================

    override def initialPreCondition(state: ModelState): Boolean = true

    override def genInitialState: Gen[State] = {
        val cardanoInfo = network.toCardanoInfo

        for {
            // TODO: Can this be pure? - Yes, for L1 mock it can be, for Yaci/testnet it cannot
            zeroBlockCreationTime <- Gen
                .const(realTimeQuantizedInstant(cardanoInfo.slotConfig).unsafeRunSync())
                .label("Zero block creation time")

            // _ = println(s"zeroBlockCreationTime: $zeroBlockCreationTime")
            // _ = println(s"zeroBlockCreationTime: ${zeroBlockCreationTime.toSlot}")

            ownTestPeer = Alice
            peers = NonEmptyList.one(ownTestPeer) // useful to have since many gens want it
            ownPeerIndex = 0
            equityShares <- genEquityShares(peers, cardanoInfo.network).label("Equity shares")

            // _ = println(s"equityShares: $equityShares")

            ownPeer = (
              OwnPeer(PeerId(ownPeerIndex, peers.size), ownTestPeer.wallet),
              Address[L1](ownTestPeer.address(cardanoInfo.network)),
              equityShares.peerShares(UByte(ownPeerIndex)).equityShare
            )

            seedUtxo <- Gen
                .oneOf(
                  genesisUtxos(ownTestPeer)
                      .map(u => Utxo(u._1.untagged, u._2.untagged))
                )
                .label("seed utxo")

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

            txTiming = TxTiming.default(cardanoInfo.slotConfig)

            initTxConfig = InitializationTxSeq.Config(
              tallyFeeAllowance = Coin.ada(2),
              votingDuration = FiniteDuration(24, HOURS).quantize(cardanoInfo.slotConfig),
              cardanoInfo = cardanoInfo,
              peerKeys = peers.map(_.wallet.exportVerificationKeyBytes),
              startTime = zeroBlockCreationTime,
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

            rawConfig: RawConfig = RawConfig(
              ownPeer = ownPeer,
              otherPeers = List.empty,
              receiveTimeout = 10.seconds,
              initializationTxBytes = initTxSigned.toCbor,
              initialFallbackTxBytes = fallbackTxSigned.toCbor,
              network = network,
              tallyFeeAllowance = Coin.ada(2),
              votingDuration = QuantizedFiniteDuration(
                finiteDuration = 24.hours,
                slotConfig = cardanoInfo.slotConfig
              ),
              txTiming = txTiming,
              startTime = zeroBlockCreationTime,
              resolvedUtxosForInitialization =
                  ResolvedUtxos(Map.from(spentUtxos.toList.map(_.toTuple))),
              // Can it be the peer's automated wallet though, at least for now?
              withdrawalFeeWallet = ownTestPeer.wallet,
              pollingPeriod = 5.seconds
            )

            // _ = println(rawConfig)
            // _ = println(HexUtil.encodeHexString(rawConfig.initializationTxBytes))

            configParsed = HeadConfig.parse(rawConfig) match {
                case Left(err) =>
                    throw RuntimeException(s"error parsing the config: ${err.explain}")
                case Right(value) => value
            }

            _ = println(
              s"txTiming.newFallbackStartTime(zeroBlockCreationTime)=${txTiming.newFallbackStartTime(zeroBlockCreationTime)}"
            )
            // _ = println(initTxSeq.fallbackTx.validityStart)

        } yield ModelState(
          ownTestPeer = ownTestPeer,
          headConfig = configParsed,
          // Initial (zero) block is "done"
          currentTime = BeforeHappyPathExpiration(zeroBlockCreationTime),
          blockCycle = Done(BlockNumber.zero, BlockVersion.Full.zero),
          //
          competingFallbackStartTime = txTiming.newFallbackStartTime(zeroBlockCreationTime),
          activeUtxos = Map.empty
        )
    }

    // ===================================
    // Commands & command generation
    // ===================================

    enum Delay(d: QuantizedFiniteDuration):
        case EndsBeforeHappyPathExpires(d: QuantizedFiniteDuration) extends Delay(d)
        case EndsInTheSilencePeriod(d: QuantizedFiniteDuration) extends Delay(d)
        case EndsAfterHappyPathExpires(d: QuantizedFiniteDuration) extends Delay(d)

        def duration: QuantizedFiniteDuration = d

    final case class DelayCommand(
        override val id: Int,
        delaySpec: Delay
    ) extends Command {

        override type Result = Unit

        // Declares the virtual-time duration to advance before this command runs.
        // The ModelBasedSuite outer driver handles the actual clock advancement.
        override def delay: FiniteDuration = delaySpec.duration.finiteDuration

        override def run(sut: Stage1Sut): IO[Unit] = for {
            _ <- IO.println(s">> DelayCommand(id=$id, delay=$delay)")
            now <- IO.realTimeInstant
            _ <- IO.println(s"Current time: $now")
        } yield ()

        override def runState(state: ModelState): (Unit, ModelState) = {
            val newBlock = state.blockCycle match {
                case Done(blockNumber, version) =>
                    Ready(blockNumber = blockNumber, prevVersion = version)
                case _ => throw Error.UnexpectedState
            }
            val instant = state.currentTime.instant + delaySpec.duration
            () -> state.copy(
              blockCycle = newBlock,
              currentTime = delaySpec match {
                  case Delay.EndsBeforeHappyPathExpires(_) => BeforeHappyPathExpiration(instant)
                  case Delay.EndsInTheSilencePeriod(_)     => InSilencePeriod(instant)
                  case Delay.EndsAfterHappyPathExpires(_) =>
                      AfterCompetingFallbackStartTime(instant)
              }
            )
        }

        override def preCondition(state: ModelState): Boolean = true
    }

    final case class StartBlockCommand(
        override val id: Int,
        blockNumber: BlockNumber,
        creationTime: QuantizedInstant
    ) extends Command {

        override type Result = Unit

        override def runState(state: ModelState): (Result, ModelState) = {
            state.currentTime match {
                case BeforeHappyPathExpiration(_) =>
                    val newBlock = state.blockCycle match {
                        case Ready(prevBlockNumber, prevVersion)
                            if prevBlockNumber.increment == blockNumber =>
                            BlockCycle.InProgress(
                              blockNumber = blockNumber,
                              creationTime = creationTime,
                              prevVersion = prevVersion
                            )
                        case _ => throw Error.UnexpectedState
                    }
                    () -> state.copy(
                      blockCycle = newBlock
                    )
                // TODO: improve output for those type of exceptions
                case _ => throw Error.UnexpectedState
            }

        }

        override def run(sut: Sut): IO[Result] =
            IO.println(s">> StartBlockCommand(id=$id, blockNumber=$blockNumber)") >>
                (sut.agent ! StartBlock(
                  blockNum = blockNumber,
                  blockCreationTime = creationTime
                ))

        override def preCondition(state: State): Boolean = true
    }

    final case class LedgerEventCommand(
        override val id: Int,
        event: LedgerEvent
    ) extends Command {

        override type Result = Unit

        override def runState(state: ModelState): (Result, ModelState) = {
            () -> state.copy(currentBlockEvents = state.currentBlockEvents :+ event)
        }

        override def run(sut: Sut): IO[Result] =
            IO.println(s">> LedgerEventCommand(id=$id)") >>
                (sut.agent ! event)

        override def preCondition(state: State): Boolean = true
    }

    final case class CompleteBlockCommand(
        override val id: Int,
        blockNumber: BlockNumber,
        isFinal: Boolean
    ) extends Command {

        val logger: Logger = org.slf4j.LoggerFactory.getLogger(CompleteBlockCommand.getClass)

        override type Result = BlockBrief

        override def runState(state: ModelState): (Result, ModelState) = {
            state.blockCycle match {
                case InProgress(_, creationTime, prevVersion) =>
                    val result = mkBlockBrief(
                      state.currentBlockEvents,
                      state.competingFallbackStartTime,
                      state.headConfig.headParameters.multisigRegimeSettings.txTiming,
                      creationTime,
                      prevVersion,
                      isFinal
                    )
                    val newCompetingFallbackStartTime =
                        if result.isInstanceOf[Major]
                        then
                            state.headConfig.headParameters.multisigRegimeSettings.txTiming
                                .newFallbackStartTime(creationTime)
                        else state.competingFallbackStartTime
                    logger.debug(s"newCompetingFallbackStartTime: $newCompetingFallbackStartTime")

                    val newState = state.copy(
                      blockCycle =
                          if isFinal then HeadFinalized else Done(blockNumber, result.blockVersion),
                      currentBlockEvents = List.empty,
                      competingFallbackStartTime = newCompetingFallbackStartTime
                    )
                    result -> newState
                case _ => throw Error.UnexpectedState
            }
        }

        private def mkBlockBrief(
            currentBlockEvents: List[LedgerEvent],
            competingFallbackStartTime: QuantizedInstant,
            txTiming: TxTiming,
            creationTime: QuantizedInstant,
            prevVersion: BlockVersion.Full,
            isFinal: Boolean
        ): BlockBrief = {

            logger.debug(s"creationTime=$creationTime")
            logger.debug(s"competingFallbackStartTime=$competingFallbackStartTime")
            logger.debug(s"txTiming=$txTiming")

            if isFinal then
                Final(
                  header = BlockHeader.Final(
                    blockNum = blockNumber,
                    blockVersion = prevVersion.incrementMajor,
                    startTime = creationTime,
                  ),
                  body = BlockBody.Final(
                    events = currentBlockEvents.map(_.eventId -> ValidityFlag.Invalid),
                    depositsRefunded = List.empty
                  )
                )
            else if txTiming.blockCanStayMinor(creationTime, competingFallbackStartTime)
            then
                Minor(
                  header = BlockHeader.Minor(
                    blockNum = blockNumber,
                    blockVersion = prevVersion.incrementMinor,
                    startTime = creationTime,
                    kzgCommitment = KzgCommitment.empty
                  ),
                  body = BlockBody.Minor(
                    events = currentBlockEvents.map(_.eventId -> ValidityFlag.Invalid),
                    depositsRefunded = List.empty
                  )
                )
            else
                Major(
                  header = BlockHeader.Major(
                    blockNum = blockNumber,
                    blockVersion = prevVersion.incrementMajor,
                    startTime = creationTime,
                    kzgCommitment = KzgCommitment.empty
                  ),
                  body = BlockBody.Major(
                    events = currentBlockEvents.map(_.eventId -> ValidityFlag.Invalid),
                    depositsAbsorbed = List.empty,
                    depositsRefunded = List.empty
                  )
                )
        }

        override def run(sut: Sut): IO[Result] = for {
            _ <- IO.println(
              s">> CompleteBlockCommand(id=$id, blockNumber=$blockNumber, isFinal=$isFinal)"
            )
            block <- IO.pure(
              if isFinal
              then CompleteBlockFinal(None)
              else CompleteBlockRegular(None, Set.empty, false)
            )
            // All sync commands should be timed out since the system may terminate
            d <- (sut.agent ?: CompleteBlock(block, blockNumber)).timeout(5.seconds)
            // Save unsigned block effects
            _ <- sut.effectsAcc.update(_ :+ d.effects.asInstanceOf[BlockEffects.Unsigned])
        } yield d.blockBrief

        override def preCondition(state: State): Boolean = state.blockCycle match {
            case BlockCycle.InProgress(currentBlockNumber, _, _) =>
                blockNumber == currentBlockNumber
            case _ => false
        }

        override def onSuccessCheck(
            expectedResult: BlockBrief,
            _stateBefore: ModelState,
            _stateAfter: ModelState,
            result: BlockBrief
        ): Prop =
            (expectedResult == result) :|
                s"block briefs should be identical: \n\texpected: $expectedResult\n\tgot: $result"
    }

    // This is used to numerate commands that we generate
    // TODO: remove - doesn't make sense for parallel commands
    val cmdCnt: AtomicInteger = AtomicInteger(0)
    def nextCmdCnt: Int = cmdCnt.getAndIncrement()

    override def genCommand(state: State): Gen[Command] = {
        import hydrozoa.integration.stage1.BlockCycle.*

        for {
            cmd <-
                state.currentTime match {
                    case BeforeHappyPathExpiration(qi) =>
                        state.blockCycle match {
                            case Done(blockNumber, _) =>
                                val settlementExpirationTime =
                                    state.headConfig.headParameters.multisigRegimeSettings.txTiming
                                        .currentSettlementExpiringTime(
                                          state.competingFallbackStartTime
                                        )
                                genDelay(
                                  state.currentTime.instant,
                                  settlementExpirationTime,
                                  state.competingFallbackStartTime,
                                  state.headConfig.cardanoInfo.slotConfig
                                )

                            case Ready(blockNumber, _) =>
                                genStartBlock(blockNumber, state.currentTime.instant)

                            case InProgress(blockNumber, _, _) =>
                                Gen.frequency(
                                  1 -> genCompleteBlock(blockNumber),
                                  10 -> genLedgerEvent(state.activeUtxos)
                                )

                            case HeadFinalized => Gen.const(NoOp(nextCmdCnt))
                        }
                    case _ => Gen.const(NoOp(nextCmdCnt))
                }

        } yield cmd
    }

    private def genDelay(
        currentTime: QuantizedInstant,
        settlementExpirationTime: QuantizedInstant,
        competingFallbackStartTime: QuantizedInstant,
        slotConfig: SlotConfig
    ): Gen[DelayCommand] = for {
        delay <- Gen
            .frequency(
              50 -> Gen
                  .choose(currentTime, settlementExpirationTime)
                  .flatMap(d =>
                      DelayCommand(nextCmdCnt, Delay.EndsBeforeHappyPathExpires(d - currentTime))
                  ),
              1 -> Gen
                  .choose(settlementExpirationTime, competingFallbackStartTime)
                  .flatMap(d =>
                      DelayCommand(nextCmdCnt, Delay.EndsInTheSilencePeriod(d - currentTime))
                  ),
              1 ->
                  Gen
                      .choose(
                        competingFallbackStartTime,
                        competingFallbackStartTime + QuantizedFiniteDuration(
                          slotConfig,
                          (competingFallbackStartTime - currentTime).finiteDuration / 10
                        )
                      )
                      .flatMap(d =>
                          DelayCommand(nextCmdCnt, Delay.EndsInTheSilencePeriod(d - currentTime))
                      ),
            )
    } yield delay

    private def genStartBlock(
        prevBlockNumber: BlockNumber,
        currentTime: QuantizedInstant
    ): Gen[StartBlockCommand] = Gen.const(
      StartBlockCommand(
        nextCmdCnt,
        prevBlockNumber.increment,
        currentTime
      )
    )

    private def genCompleteBlock(blockNumber: BlockNumber): Gen[CompleteBlockCommand] = for {
        isFinal <- Gen.frequency(
          1 -> true,
          40 -> false
        )
    } yield CompleteBlockCommand(nextCmdCnt, blockNumber, isFinal)

    private def genLedgerEvent(
        _activeUtxos: Map[TransactionInput, TransactionOutput]
    ): Gen[LedgerEventCommand] = for {
        // TODO: implement properly
        event <- Arbitrary.arbitrary[LedgerEvent]
    } yield LedgerEventCommand(id = nextCmdCnt, event = event)

    enum Error extends Throwable:
        case UnexpectedState
}
