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
import hydrozoa.integration.stage1.CurrentBlock.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant, quantize}
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.CardanoLiaisonTest.BlockWeaverMock
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.consensus.{CardanoLiaison, ConsensusActor, EventSequencer}
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.block.BlockBrief.{Final, Major, Minor}
import hydrozoa.multisig.ledger.block.{Block, BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
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
import scalus.cardano.ledger.{Coin, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import spire.math.UByte
import test.Generators.Hydrozoa.ArbitraryInstances.given_Arbitrary_LedgerEvent
import test.TestPeer.Alice
import test.{TestPeer, minPubkeyAda, sumUtxoValues}

/** Integration Stage 1 (the simplest).
  *   - Only three real actors are involved: [[JointLedger]], [[ConsensusActor]], and
  *     [[CardanoLiaison]]
  */
object Stage1Properties extends YetAnotherProperties("Joint ledger and Cardano liaison (stage 1"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = {
        p.withWorkers(1).withMinSuccessfulTests(100)
    }

    private val preprod = StandardCardanoNetwork.Preprod

    val _ = property("Work well on L1 mock (fast, reproducible)") = Stage1(
      network = Left(preprod),
      genesisUtxos = yaciTestSauceGenesis(preprod.scalusNetwork)
    ).property()

    // val _ = property("Work well on Yaci DevKit (slow, reproducible)") = ???
    // val _ = property("Work well on Preview (slow, non-reproducible)") = ???

case class Stage1(
    network: Either[StandardCardanoNetwork, NetworkInfo],
    genesisUtxos: Map[TestPeer, UtxoSetL1]
) extends ModelBasedSuite {

    override type State = Stage1State
    override type Sut = Stage1Sut

    // ===================================
    // SUT handling
    // ===================================

    override def newSut(state: Stage1State): IO[Sut] = {
        import state.headConfig.*

        for {

            // Actor system
            system <- ActorSystem[IO]("Stage1").allocated.map(_._1)

            // Note: Actor exceptions are logged by the supervision strategy but don't
            // automatically fail tests. To treat them as test failures check that the
            // system was not terminated in the [[shutdownSut]] action.

            // Cardano L1 backend mock
            utxos = genesisUtxos.values.flatten.map((k, v) => k.untagged -> v.untagged).toMap
            mockState = MockState.apply(utxos)
            cardanoBackend <- CardanoBackendMock.mockIO(mockState)

            // Weaver stub
            blockWeaver <- system.actorOf(new BlockWeaverMock)

            // Cardano liaison
            liaisonConfig = CardanoLiaison.Config(
              cardanoBackend = cardanoBackend,
              initializationTx = initialBlock.effects.initializationTx,
              initializationFallbackTx = initialBlock.effects.fallbackTx,
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

        } yield Stage1Sut(system, agent)
    }

    override def shutdownSut(sut: Sut): IO[Prop] = for {

        /** Important: this action should ensure that the actor system was not terminated.
          *
          * Even more important: before terminating, make sure [[waitForIdle]] is called - otherwise
          * you just immediately shutdown the system and will get a false-positive test.
          *
          * Luckily enough, [[waitForIdle]] does exactly what we need in addition to checking the
          * mailboxes it also verifies that the system was not terminated.
          */
        _ <- sut.system.waitForIdle()

        // Next part of the property is to check that all txs are known to the Cardano backend.

        // TODO shutdown Yaci? Clean up the public testnet?
    } yield true // !wasTerminated

    // TODO: do we want to run multiple SUTs when using L1 mock?
    override def canCreateNewSut(
        candidateState: State,
        inactiveSuts: Iterable[State],
        runningSuts: Iterable[State]
    ): Boolean = inactiveSuts.isEmpty && runningSuts.isEmpty

    // ===================================
    // Initial state handling
    // ===================================

    override def initialPreCondition(state: Stage1State): Boolean = true

    override def genInitialState: Gen[State] = {
        val cardanoInfo = network.toCardanoInfo

        for {
            // Can this be pure?
            zeroBlockCreationTime <- Gen
                .const(realTimeQuantizedInstant(cardanoInfo.slotConfig).unsafeRunSync())
                .label("Zero block creation time")

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

            // _ = println(txTiming.newFallbackStartTime(zeroBlockCreationTime))
            // _ = println(initTxSeq.fallbackTx.validityStart)

        } yield Stage1State(
          headConfig = configParsed,
          currentBlock =
              Done(BlockNumber.zero, BlockVersion.Full.zero), // initial (zero) block is finished
          currentBlockEvents = List.empty,
          currentTime = zeroBlockCreationTime,
          competingFallbackStartTime = txTiming.newFallbackStartTime(zeroBlockCreationTime),
          activeUtxos = Map.empty
        )
    }

    // ===================================
    // Command generation
    // ===================================

    final case class StartBlockCommand(
        override val id: Int,
        blockNumber: BlockNumber,
        creationTime: QuantizedInstant
    ) extends Command {

        override type Result = Unit

        override def runState(state: Stage1State): (Result, Stage1State) = {
            val newBlock = state.currentBlock match {
                case CurrentBlock.Done(prevBlockNumber, version)
                    if prevBlockNumber.increment == blockNumber =>
                    CurrentBlock.InProgress(blockNumber, creationTime, version)
                case _ => throw Error.UnexpectedState
            }
            () -> state.copy(
              currentBlock = newBlock,
              currentTime = creationTime
            )
        }

        override def run(sut: Sut): IO[Result] =
            sut.agent ! StartBlock(
              blockNum = blockNumber,
              blockCreationTime = creationTime
            )

        override def preCondition(state: State): Boolean = true
    }

    final case class LedgerEventCommand(
        override val id: Int,
        event: LedgerEvent
    ) extends Command {

        override type Result = Unit

        override def runState(state: Stage1State): (Result, Stage1State) = {
            () -> state.copy(currentBlockEvents = state.currentBlockEvents :+ event)
        }

        override def run(sut: Sut): IO[Result] = sut.agent ! event

        override def preCondition(state: State): Boolean = true
    }

    final case class CompleteBlockCommand(
        override val id: Int,
        blockNumber: BlockNumber,
        isFinal: Boolean
    ) extends Command {

        override type Result = BlockBrief

        override def runState(state: Stage1State): (Result, Stage1State) = {
            state.currentBlock match {
                case InProgress(_, creationTime, prevVersion) =>
                    val result = mkBlockBrief(
                      state.currentBlockEvents,
                      state.competingFallbackStartTime,
                      state.headConfig.headParameters.multisigRegimeSettings.txTiming,
                      creationTime,
                      prevVersion,
                      isFinal
                    )
                    val newState = state.copy(
                      currentBlock =
                          if isFinal then HeadFinalized else Done(blockNumber, result.blockVersion),
                      currentBlockEvents = List.empty,
                      competingFallbackStartTime =
                          if result.isInstanceOf[Major] then
                              state.headConfig.headParameters.multisigRegimeSettings.txTiming
                                  .newFallbackStartTime(creationTime)
                          else state.competingFallbackStartTime
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
            block <- IO.pure(
              if isFinal
              then CompleteBlockFinal(None)
              else CompleteBlockRegular(None, Set.empty, false)
            )
            // All sync commands should be timed out since the system may terminate
            d <- (sut.agent ?: CompleteBlock(block, blockNumber)).timeout(5.seconds)
            // TODO: save effects to the suite, return Result which is common for the SUT and the model
            // _ <- IO.println(s"--------->>> ${d}")
        } yield d.blockBrief

        override def preCondition(state: State): Boolean = state.currentBlock match {
            case CurrentBlock.InProgress(currentBlockNumber, _, _) =>
                blockNumber == currentBlockNumber
            case _ => false
        }

        override def onSuccessCheck(
            expectedResult: BlockBrief,
            _stateBefore: Stage1State,
            _stateAfter: Stage1State,
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
        import hydrozoa.integration.stage1.CurrentBlock.*

        for {
            cmd <- state.currentBlock match {
                case InProgress(blockNumber, _, _) =>
                    Gen.frequency(
                      1 -> genCompleteBlock(blockNumber),
                      10 -> genLedgerEvent(state.activeUtxos)
                    )
                case Done(blockNumber, _) => genStartBlock(blockNumber, state.currentTime)
                case HeadFinalized        => Gen.const(NoOp(nextCmdCnt))
            }
        } yield cmd
    }

    private def genStartBlock(
        prevBlockNumber: BlockNumber,
        currentTime: QuantizedInstant
    ): Gen[StartBlockCommand] = for {
        // During command generation we cannot use IO.realTime
        // Instead we can generate delays that could be fast forwarded in the suitable SUT
        delay <- Gen.choose(10, 86400).flatMap(secs => FiniteDuration.apply(secs, TimeUnit.SECONDS))
    } yield StartBlockCommand(
      nextCmdCnt,
      prevBlockNumber.increment,
      currentTime + delay
    )

    private def genCompleteBlock(blockNumber: BlockNumber): Gen[CompleteBlockCommand] = for {
        isFinal <- Gen.frequency(
          1 -> true,
          20 -> false
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
