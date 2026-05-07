package hydrozoa.integration.stage4

import cats.data.ReaderT
import cats.effect.{Deferred, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.ActorSystem
import com.suprnation.actor.event.Error as ActorError
import hydrozoa.config.head.initialization.{InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.multisig.timing.generateYaciTxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{InitParamsType, generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.integration.stage4.Model.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import cats.effect.IOLocal
import hydrozoa.lib.logging.{Logging, Tracer}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.lib.tracing.ProtocolTracer
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{BlockWeaver, CardanoLiaison, ConsensusActor, EventSequencer, PeerLiaison}
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.eutxol2.{EutxoL2Ledger, toUtxos}
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.joint.JointLedger
import org.scalacheck.commands.{AnyCommand, ModelBasedSuite, ScenarioGen}
import org.scalacheck.{Gen, Prop}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.{CertState, TransactionInput, Utxos}
import test.{SeedPhrase, TestPeers, given}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}

// ===================================
// Stage 4 suite
// ===================================

case class Stage4Suite(label: String = "stage4", nPeers: Int = 2) extends ModelBasedSuite:

    override type Env = Unit
    override type State = ModelState
    override type Sut = Stage4Sut

    private val logger = Logging.loggerIO("Stage4.Suite")

    override def useTestControl: Boolean = true

    override def scenarioGen: ScenarioGen[ModelState, Stage4Sut] = Stage4ScenarioGen

    override def commandGenTweaker: [A] => Gen[A] => Gen[A] = [A] =>
        (g: Gen[A]) => Gen.resize(500, g)

    override def onTestCaseGenerated(
        initialState: ModelState,
        commands: List[AnyCommand[ModelState, Stage4Sut]]
    ): IO[Unit] =
        super.onTestCaseGenerated(initialState, commands) >>
            logger.info(Stage4Runner.renderTable(initialState, commands))

    override def initEnv: Unit = ()

    override def genInitialState(env: Unit): Gen[ModelState] =
        Stage4Suite.genInitialState(nPeers = nPeers)

    override def canStartupNewSut(): Boolean = true

    override def startupSut(state: ModelState): IO[Stage4Sut] =
        val multiNodeConfig = state.params.multiNodeConfig
        val cardanoInfo = multiNodeConfig.headConfig.cardanoInfo
        val peers = multiNodeConfig.nodeConfigs.keys.toSeq.sortBy(p => p: Int)

        // Advance simulated clock to the head's start epoch BEFORE creating the ActorSystem.
        // With TestControl, IO.sleep advances the virtual clock only while no actor fibers
        // exist; once actors are started their ping loops compete with tickOne, so the sleep
        // must come first (same pattern as stage1 Suite).
        val startEpochMs = state.currentModelTimes.values.head.getEpochSecond * 1000L

        for
            _ <- IO.sleep(FiniteDuration(startEpochMs, TimeUnit.MILLISECONDS))
            tracerLocal <- Tracer.makeLocal
            given cats.effect.IOLocal[Tracer] = tracerLocal

            system <- ActorSystem[IO](label).allocated.map(_._1)

            // All peers share one mock L1 backend, starting from the merged pre-init UTxOs.
            // The head initialization tx is submitted by the protocol through normal operation.
            genesisUtxos = state.preinitPeerUtxosL1.values.reduce(_ ++ _)
            cardanoBackend <- CardanoBackendMock.mockIO(
              initialState = MockState(genesisUtxos),
              mkContext = slot =>
                  Context(
                    env = UtxoEnv(
                      slot = slot,
                      params = cardanoInfo.protocolParams,
                      certState = CertState.empty,
                      network = cardanoInfo.network
                    ),
                    slotConfig = cardanoInfo.slotConfig
                  )
            )

            // Each peer gets its own PendingConnections deferred, completed after all actors
            // are started so cross-peer liaisons can be wired.
            pendingConnsMap <- peers
                .traverse { peerNum =>
                    Deferred[IO, MultisigRegimeManager.Connections].map(peerNum -> _)
                }
                .map(_.toMap)

            // Create full actor stack per peer; all actors wait on pendingConnections.
            peerStackMap <- peers
                .traverse { peerNum =>
                    val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)
                    val pending = pendingConnsMap(peerNum)
                    Tracer.scopedCtx("peer" -> s"${peerNum: Int}") {
                        for
                            blockWeaver <- system.actorOf(
                              BlockWeaver(nodeConfig, pending, tracerLocal)
                            )
                            cardanoLiaison <- system.actorOf(
                              CardanoLiaison(nodeConfig, cardanoBackend, pending, tracerLocal)
                            )
                            eventSequencer <- system.actorOf(EventSequencer(nodeConfig, pending))
                            l2Ledger <- EutxoL2Ledger(nodeConfig)
                            jointLedger <- system.actorOf(
                              JointLedger(
                                nodeConfig,
                                pending,
                                l2Ledger,
                                ProtocolTracer.noop,
                                tracerLocal
                              )
                            )
                            consensusActor <- system.actorOf(ConsensusActor(nodeConfig, pending))
                        yield peerNum -> PeerStack(
                          blockWeaver,
                          cardanoLiaison,
                          eventSequencer,
                          jointLedger,
                          consensusActor
                        )
                    }
                }
                .map(_.toMap)

            // Create brief-collecting observers wrapping each peer's ConsensusActor.
            // Injected via Connections so JointLedger is unaware; captures both leader-produced
            // and follower-reproduced blocks (both go through JointLedger.handleBlock).
            blockBriefsMap <- peers
                .traverse { peerNum =>
                    Ref[IO].of(Vector.empty[BlockBrief.Intermediate]).map(peerNum -> _)
                }
                .map(_.toMap)

            observerMap <- peers
                .traverse { peerNum =>
                    system
                        .actorOf(
                          BlockBriefObserver(
                            peerStackMap(peerNum).consensusActor,
                            blockBriefsMap(peerNum)
                          )
                        )
                        .map(peerNum -> _)
                }
                .map(_.toMap)

            // Create one PeerLiaison per (local, remote) pair.
            // peerLiaisonMap(A)(B.id) = the liaison at A directed to B.
            peerLiaisonMap <- peers
                .traverse { peerNum =>
                    val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)
                    val pending = pendingConnsMap(peerNum)
                    peers
                        .filterNot(_ == peerNum)
                        .traverse { remotePeerNum =>
                            val remotePeerId =
                                multiNodeConfig.nodeConfigs(remotePeerNum).ownHeadPeerId
                            system
                                .actorOf(PeerLiaison(nodeConfig, remotePeerId, pending))
                                .map(remotePeerId -> _)
                        }
                        .map(liaisons => peerNum -> liaisons.toMap)
                }
                .map(_.toMap)

            // Complete each peer's deferred with its full wiring.
            // remotePeerLiaisons(B.id) = the liaison at B directed back at A (for A's Connections).
            _ <- peers.traverse { peerNum =>
                val stack = peerStackMap(peerNum)
                val ownPeerId = multiNodeConfig.nodeConfigs(peerNum).ownHeadPeerId
                val localLiaisons = peerLiaisonMap(peerNum).values.toList
                val remoteLiaisons = peers
                    .filterNot(_ == peerNum)
                    .map { remotePeerNum =>
                        val remotePeerId = multiNodeConfig.nodeConfigs(remotePeerNum).ownHeadPeerId
                        remotePeerId -> peerLiaisonMap(remotePeerNum)(ownPeerId)
                    }
                    .toMap
                pendingConnsMap(peerNum)
                    .complete(
                      MultisigRegimeManager.Connections(
                        blockWeaver = stack.blockWeaver,
                        cardanoLiaison = stack.cardanoLiaison,
                        consensusActor = observerMap(peerNum),
                        eventSequencer = stack.eventSequencer,
                        jointLedger = stack.jointLedger,
                        peerLiaisons = localLiaisons,
                        remotePeerLiaisons = remoteLiaisons,
                      )
                    )
                    .void
            }

            sutErrors <- Ref[IO].of(List.empty[String])
            errorDrainer <- system.eventStream.take
                .flatMap {
                    case e: ActorError if e.cause != ActorError.NoCause =>
                        sutErrors.update(_ :+ s"[${e.logSource}] ${e.cause.getMessage}")
                    case _ => IO.unit
                }
                .foreverM
                .start

            submittedRequestIds <- Ref[IO].of(Vector.empty[RequestId])
        yield Stage4Sut(
          system = system,
          cardanoBackend = cardanoBackend,
          peers = peerStackMap.map { case (peerNum, stack) =>
              peerNum -> Stage4PeerHandle(eventSequencer = stack.eventSequencer)
          },
          sutErrors = sutErrors,
          errorDrainer = errorDrainer,
          blockBriefs = blockBriefsMap,
          submittedRequestIds = submittedRequestIds,
          tracerLocal = tracerLocal,
        )

    override def shutdownSut(lastState: ModelState, sut: Stage4Sut): IO[Prop] =
        // BlockWeaver's IO.sleep inside sleepSendWakeup keeps _isIdle=false for the full sleep
        // duration, so waitForIdle would always time out. Terminate directly after a brief settle
        // window that lets any in-flight fault-handling chains flush to eventStream.
        for
            _ <- logger.warn("shutdownSut")
            _ <- sut.system.terminate()
            _ <- logger.warn("shutdownSut: system was terminated")
            _ <- IO.sleep(100.millis) // settle: let the drainer consume any last-cycle errors
            _ <- sut.errorDrainer.cancel
            errors <- sut.sutErrors.get
            analysisProp <- analyzeBlockBriefs(lastState, sut)
        yield
            if errors.nonEmpty then
                Prop.exception(RuntimeException(s"SUT actor errors:\n${errors.mkString("\n")}"))
            else analysisProp

    private def analyzeBlockBriefs(lastState: ModelState, sut: Stage4Sut): IO[Prop] = for
        briefsByPeer <- sut.blockBriefs.toList
            .traverse { case (p, ref) => ref.get.map(p -> _) }
            .map(_.toMap)

        sortedPeers = briefsByPeer.keys.toSeq.sortBy(p => p: Int)
        nPeers = sortedPeers.length
        canonicalBriefs = briefsByPeer(sortedPeers.head)
        submittedIds <- sut.submittedRequestIds.get

        _ <- IO(printBlockTable(canonicalBriefs, sortedPeers, briefsByPeer, nPeers, submittedIds, lastState))
    yield
        propLiveness(submittedIds, canonicalBriefs) &&
            propDepositTiming(lastState.registeredDeposits, canonicalBriefs) &&
            propValidRatio(lastState, canonicalBriefs)

    /** Property: every submitted request id eventually appears in some block — either as an event
      * (Valid or Invalid) or as a deposit (absorbed or refunded). Catches silent message loss
      * (e.g. the historical `Mempool.extractRequestsWhile` bug).
      */
    private def propLiveness(
        submittedIds: Vector[RequestId],
        canonicalBriefs: Vector[BlockBrief.Intermediate]
    ): Prop = {
        val processedIds: Set[RequestId] =
            canonicalBriefs.flatMap(b =>
                b.events.map(_._1) ++ b.depositsAbsorbed ++ b.depositsRefunded
            ).toSet
        val missing = submittedIds.toSet -- processedIds
        Prop(missing.isEmpty) :|
            s"liveness: ${missing.size} submitted reqId(s) never appeared in any block: " +
            s"${missing.toSeq.sortBy(r => (r.peerNum.convert, r.requestNum.convert)).mkString(", ")}"
    }

    /** Property: every absorbed deposit was mature by the time the absorbing block ended,
      * i.e. `brief.endTime >= deposit.absorptionStartTime`. Refund-window check is a TODO — needs
      * `depositAbsorptionEndTime` and the `notInPollResults` legitimate case which we don't track.
      */
    private def propDepositTiming(
        registeredDeposits: Map[RequestId, PendingDeposit],
        canonicalBriefs: Vector[BlockBrief.Intermediate]
    ): Prop = {
        val violations: Vector[String] =
            for
                brief <- canonicalBriefs
                reqId <- brief.depositsAbsorbed.toVector
                deposit <- registeredDeposits.get(reqId).toVector
                if brief.endTime.convert < deposit.absorptionStartTime
            yield s"reqId=$reqId absorbed at brief.endTime=${brief.endTime.convert} but " +
                s"absorptionStartTime=${deposit.absorptionStartTime}"
        Prop(violations.isEmpty) :|
            s"deposit timing: ${violations.size} absorbed-too-early violation(s):\n" +
            violations.mkString("\n")
    }

    /** Property: SUT's valid/total ratio is no greater than the model's, i.e. the SUT is not
      * more permissive than the model. Compared as exact rationals via cross-multiplication.
      */
    private def propValidRatio(
        lastState: ModelState,
        canonicalBriefs: Vector[BlockBrief.Intermediate]
    ): Prop = {
        // Restrict model to L2-tx requests so denominators are comparable to SUT's brief.events
        // (deposits are always Valid in the model and don't appear in events).
        val l2TxReqIds = lastState.modelFlags.keySet -- lastState.registeredDeposits.keySet
        val modelValid = l2TxReqIds.count(lastState.modelFlags(_) == ValidityFlag.Valid).toLong
        val modelTotal = l2TxReqIds.size.toLong
        val sutEvents = canonicalBriefs.flatMap(_.events)
        val sutValid = sutEvents.count(_._2 == ValidityFlag.Valid).toLong
        val sutTotal = sutEvents.size.toLong

        // sutValid/sutTotal <= modelValid/modelTotal  iff  sutValid*modelTotal <= modelValid*sutTotal
        // Trivially holds when either total is 0 (vacuous).
        val holds =
            modelTotal == 0L || sutTotal == 0L ||
                sutValid * modelTotal <= modelValid * sutTotal
        Prop(holds) :|
            s"valid ratio: SUT $sutValid/$sutTotal exceeds model $modelValid/$modelTotal " +
            s"(SUT is more permissive than the model)"
    }

    private def printBlockTable(
        canonicalBriefs: Vector[BlockBrief.Intermediate],
        sortedPeers: Seq[HeadPeerNumber],
        briefsByPeer: Map[HeadPeerNumber, Vector[BlockBrief.Intermediate]],
        nPeers: Int,
        submittedIds: Vector[RequestId],
        lastState: ModelState
    ): Unit = {
        val colWidth = 72
        val divider = s"+${"-" * (colWidth + 2)}+"
        val header = s"| ${"Block".padTo(colWidth, ' ')} |"

        println(divider)
        println(header)
        println(divider)

        canonicalBriefs.foreach { brief =>
            val blockType = brief match {
                case _: BlockBrief.Minor => "Min"; case _: BlockBrief.Major => "Maj"
            }
            val vMaj = brief.blockVersion.major.convert
            val vMin = brief.blockVersion.minor.convert
            val leader = (brief.blockNum: Int) % nPeers
            val evs = brief.events.map { case (reqId, flag) =>
                val f = if flag == ValidityFlag.Valid then "V" else "I"
                s"p${reqId.peerNum.convert}:r${reqId.requestNum.convert}=$f"
            }
            val abs = brief.depositsAbsorbed.map(r =>
                s"abs:p${r.peerNum.convert}:r${r.requestNum.convert}"
            )
            val ref = brief.depositsRefunded.map(r =>
                s"ref:p${r.peerNum.convert}:r${r.requestNum.convert}"
            )
            val events = (evs ++ abs ++ ref).mkString(" ")
            val label =
                s"#${brief.blockNum: Int} $blockType v$vMaj.$vMin lead=p$leader | $events"
            println(s"| ${label.take(colWidth).padTo(colWidth, ' ')} |")
        }

        println(divider)

        // SUT processing order — each block contributes its absorbed deposits, then its events.
        val sutOrder: Vector[RequestId] =
            canonicalBriefs.flatMap(b => b.depositsAbsorbed ++ b.events.map(_._1)).toVector
        val commonPrefixLen =
            submittedIds.zip(sutOrder).takeWhile { case (a, b) => a == b }.length

        val l2TxReqIds = lastState.modelFlags.keySet -- lastState.registeredDeposits.keySet
        val modelValid = l2TxReqIds.count(lastState.modelFlags(_) == ValidityFlag.Valid)
        val modelTotal = l2TxReqIds.size
        val sutEvents = canonicalBriefs.flatMap(_.events)
        val sutValid = sutEvents.count(_._2 == ValidityFlag.Valid)
        val sutTotal = sutEvents.size

        println(
          s"Peers: ${sortedPeers.map(p => s"p${p: Int}=${briefsByPeer(p).length}blks").mkString("  ")}"
        )
        println(
          s"Common prefix: $commonPrefixLen / ${submittedIds.length} (submission order vs SUT block order)"
        )
        println(
          s"Valid/total (L2 txs) — model: $modelValid/$modelTotal  SUT: $sutValid/$sutTotal"
        )
        println(
          "Legend: Min=Minor Maj=Major v=version lead=leader p=peer r=requestNum V=valid I=invalid abs=deposit-absorbed ref=refunded"
        )
    }

// ===================================
// Initial state generator (canonical location; Runner delegates here for @main)
// ===================================

object Stage4Suite:

    def genInitialState(
        nPeers: Int = 2,
        absorptionSlack: FiniteDuration = 60.seconds,
        meanInterArrivalTime: HeadPeerNumber => FiniteDuration = _ => 12.seconds,
    ): Gen[ModelState] =
        val cardanoNetwork = CardanoNetwork.Preprod
        val testPeers = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nPeers)
        val testPeerToUtxos = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)

        // This should be deterministic
        val generateHeadStartTime = ReaderT((tp: TestPeers) =>
            // Date and time (GMT): Thursday, January 1, 2026 at 12:00:00 AM, POSIX seconds
            val anchorTime = 1767225600L
            // 100 day range, seconds
            val range = 86_400 * 100L
            for offset <- Gen.choose(0L, range)
            yield BlockCreationEndTime(
              java.time.Instant.ofEpochSecond(anchorTime + offset).quantize(tp.slotConfig)
            )
        )

        val generateHeadConfigBootstrap_ = generateHeadConfigBootstrap(
          generateHeadParams = generateHeadParameters(generateTxTiming = generateYaciTxTiming),
          generateInitializationParameters = InitParamsType.TopDown(
            InitializationParametersGenTopDown.GenWithDeps(
              generateGenesisUtxosL1 = ReaderT((tp: TestPeers) =>
                  Gen.const(testPeerToUtxos.map((k, v) => k.headPeerNumber -> v))
              )
            )
          )
        )

        val generateHeadConfig_ = generateHeadConfig(
          genHeadConfigBootstrap = generateHeadConfigBootstrap_,
          generateInitialBlock = bootstrap =>
              generateInitialBlock(
                genHeadConfigBootstrap = ReaderT
                    .pure[Gen, TestPeers, hydrozoa.config.head.HeadConfig.Bootstrap](bootstrap),
                generateBlockCreationEndTime = generateHeadStartTime
              )
        )

        for
            config <- MultiNodeConfig.generateWith(testPeers)(
              generateHeadConfig = generateHeadConfig_
            )

            preinitPeerUtxosL1 = testPeerToUtxos.map((k, v) => k.headPeerNumber -> v)

            initTx = config.headConfig.initializationTx.tx
            spentInputs = initTx.body.value.inputs.toSet
            initOutputsList = initTx.body.value.outputs.toList.map(_.value).zipWithIndex

            peers = config.nodeConfigs.keys.toSeq.sortBy(p => p: Int)

            peerUtxosL1 = peers.map { pn =>
                val peerAddr = config.addressOf(pn)
                val survived: Utxos = preinitPeerUtxosL1(pn) -- spentInputs
                val newOutputs: Utxos = initOutputsList
                    .filter((out, _) => out.address.asInstanceOf[ShelleyAddress] == peerAddr)
                    .map((out, ix) => TransactionInput(initTx.id, ix) -> out)
                    .toMap
                pn -> (survived ++ newOutputs)
            }.toMap

            startTime = config.headConfig.initialBlock.endTime.convert
        yield ModelState(
          params = Params(
            config,
            absorptionSlack,
            peers.map(pn => pn -> meanInterArrivalTime(pn)).toMap
          ),
          preinitPeerUtxosL1 = preinitPeerUtxosL1,
          currentModelTimes = peers.map(_ -> startTime).toMap,
          utxosL2Active = config.headConfig.initializationParameters.initialEvacuationMap.toUtxos,
          peerUtxosL1 = peerUtxosL1,
          nextRequestNumbers = peers.map(_ -> RequestNumber(0)).toMap,
          pendingDeposits = peers.map(_ -> Nil).toMap,
          modelFlags = Map.empty,
          registeredDeposits = Map.empty,
        )
