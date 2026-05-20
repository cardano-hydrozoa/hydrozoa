package hydrozoa.integration.stage4

import cats.data.ReaderT
import cats.effect.{Deferred, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.ActorSystem
import com.suprnation.actor.event.Error as ActorError
import com.suprnation.typelevel.actors.syntax.*
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
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.consensus.transport.{PeerWsTransport, RemotePeerProxy}
import hydrozoa.multisig.consensus.{BlockWeaver, CardanoLiaison, ConsensusActor, EventSequencer, PeerLiaison, SlowConsensusActor, StackComposer}
import org.http4s.Uri
import com.comcast.ip4s.{Host, Port, host}
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.eutxol2.{EutxoL2Ledger, toUtxos}
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects}
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

case class Stage4Suite(
    label: String = "stage4",
    nPeers: Int = 2,
    transportMode: TransportMode = TransportMode.Direct,
) extends ModelBasedSuite:

    override type Env = Unit
    override type State = ModelState
    override type Sut = Stage4Sut

    private val logger = Logging.loggerIO("Stage4.Suite")

    /** TestControl is incompatible with real sockets — virtual time doesn't drive the OS
      * scheduler that owns the WS connection. Direct mode keeps virtual time; WS mode runs on
      * the real clock.
      */
    override def useTestControl: Boolean = transportMode match {
        case TransportMode.Direct       => true
        case _: TransportMode.WebSocket => false
    }

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
        Stage4Suite.genInitialState(nPeers = nPeers, useTestControl = useTestControl)

    override def canStartupNewSut(): Boolean = true

    override def startupSut(state: ModelState): IO[Stage4Sut] = {
        val multiNodeConfig = state.params.multiNodeConfig
        val cardanoInfo = multiNodeConfig.headConfig.cardanoInfo
        val peers = multiNodeConfig.nodeConfigs.keys.toSeq.sortBy(p => p: Int)

        // Advance simulated clock to the head's start epoch BEFORE creating the ActorSystem.
        // With TestControl, IO.sleep advances the virtual clock only while no actor fibers
        // exist; once actors are started their ping loops compete with tickOne, so the sleep
        // must come first (same pattern as stage1 Suite).
        //
        // In WS mode (real clock) we skip this: the configured start epoch is potentially
        // years in the future, sleeping would block the test. Block production almost
        // certainly won't fire under WS+real-clock until the model anchors startTime to
        // `Instant.now() + small`. v1 of WS mode validates the transport layer; full property
        // validation under WS is a follow-up.
        val startEpochMs = state.currentModelTime.getEpochSecond * 1000L

        for
            // TestControl branch: jump the virtual clock from 0 to the head's start epoch.
            // Without TestControl this would be a literal multi-decade real sleep, so it must
            // be gated on `useTestControl`. The non-TestControl analogue is the
            // `state.takeoffTime` wait below — `genInitialState` anchors `currentModelTime`
            // at `now + 60s` for that mode, so wall-clock sleeping until the anchor is the
            // right move.
            _ <- IO.whenA(useTestControl)(
              IO.sleep(FiniteDuration(startEpochMs, TimeUnit.MILLISECONDS))
            )

            // Non-TestControl branch: wait until the wall clock reaches `takeoffTime` so the
            // model clock and the SUT wall clock coincide at command 1. Abort if setup
            // overran the budget — better a loud failure than a test that starts with
            // already-violated timing. Same shape as stage 1.
            _ <- state.takeoffTime match {
                case None => IO.unit
                case Some(t) =>
                    IO.realTimeInstant.flatMap { now =>
                        if now.isAfter(t) then
                            IO.raiseError(
                              RuntimeException(
                                s"Stage4 startupSut: initialization took too long " +
                                    s"(takeoff: $t, now: $now)"
                              )
                            )
                        else
                            val sleepMs = t.toEpochMilli - now.toEpochMilli
                            IO.sleep(FiniteDuration(sleepMs, TimeUnit.MILLISECONDS))
                    }
            }

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
                            consensusActor <- system.actorOf(ConsensusActor(nodeConfig, pending, tracerLocal))
                            stackComposer <- system.actorOf(
                              StackComposer(nodeConfig, pending, tracerLocal)
                            )
                            slowConsensusActor <- system.actorOf(
                              SlowConsensusActor(nodeConfig, pending, tracerLocal)
                            )
                        yield peerNum -> PeerStack(
                          blockWeaver,
                          cardanoLiaison,
                          eventSequencer,
                          jointLedger,
                          consensusActor,
                          stackComposer,
                          slowConsensusActor
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
                            peerNum,
                            peerStackMap(peerNum).consensusActor,
                            blockBriefsMap(peerNum)
                          )
                        )
                        .map(peerNum -> _)
                }
                .map(_.toMap)

            // Create stack-collecting observers wrapping each peer's CardanoLiaison.
            // Injected via Connections so SlowConsensusActor is unaware; captures every
            // hard-confirmed stack the slow cycle emits (parallel to BlockBriefObserver
            // on the fast side).
            stacksMap <- peers
                .traverse { peerNum =>
                    Ref[IO].of(Vector.empty[Stack.HardConfirmed]).map(peerNum -> _)
                }
                .map(_.toMap)

            stackObserverMap <- peers
                .traverse { peerNum =>
                    system
                        .actorOf(
                          StackObserver(
                            peerNum,
                            peerStackMap(peerNum).cardanoLiaison,
                            stacksMap(peerNum)
                          )
                        )
                        .map(peerNum -> _)
                }
                .map(_.toMap)

            // Create one local PeerLiaison per (local, remote) pair.
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

            // Build the per-peer remote-liaison map. In Direct mode, this is the actual
            // PeerLiaison handle from the other peer's actor stack. In WebSocket mode, it's
            // a RemotePeerProxy that forwards messages over the local PeerWsTransport.
            //
            // remoteLiaisonsByPeer(A)(B.id) = the handle that A's PeerLiaison(A->B) uses to
            // reach B's PeerLiaison(B->A).
            transportSetup <- transportMode match {
                case TransportMode.Direct =>
                    val remoteLiaisonsByPeer: Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaison.Handle]] =
                        peers.map { peerNum =>
                            val ownPeerId = multiNodeConfig.nodeConfigs(peerNum).ownHeadPeerId
                            peerNum -> peers
                                .filterNot(_ == peerNum)
                                .map { remotePeerNum =>
                                    val remotePeerId =
                                        multiNodeConfig.nodeConfigs(remotePeerNum).ownHeadPeerId
                                    remotePeerId -> peerLiaisonMap(remotePeerNum)(ownPeerId)
                                }
                                .toMap
                        }.toMap
                    IO.pure((remoteLiaisonsByPeer, IO.unit))
                case TransportMode.WebSocket(basePort) =>
                    given CardanoNetwork.Section = multiNodeConfig.headConfig
                    setupWebSocketTransports(
                      multiNodeConfig,
                      peers,
                      basePort,
                      system,
                      peerLiaisonMap,
                    )
            }
            (remoteLiaisonsByPeer, transportCleanup) = transportSetup

            // Complete each peer's deferred with its full wiring.
            _ <- peers.traverse { peerNum =>
                val stack = peerStackMap(peerNum)
                val localLiaisons = peerLiaisonMap(peerNum).values.toList
                pendingConnsMap(peerNum)
                    .complete(
                      MultisigRegimeManager.Connections(
                        blockWeaver = stack.blockWeaver,
                        // Route the slow cycle's Stack.HardConfirmed through StackObserver
                        // (forwards to the real CardanoLiaison) so the test can assert
                        // coverage; mirrors observerMap on consensusActor.
                        cardanoLiaison = stackObserverMap(peerNum),
                        consensusActor = observerMap(peerNum),
                        eventSequencer = stack.eventSequencer,
                        jointLedger = stack.jointLedger,
                        stackComposer = stack.stackComposer,
                        slowConsensusActor = stack.slowConsensusActor,
                        peerLiaisons = localLiaisons,
                        remotePeerLiaisons = remoteLiaisonsByPeer(peerNum),
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

            // Side-fiber per peer that periodically pokes its CardanoLiaison with
            // `Timeout`. cats-actors' `setReceiveTimeout` is unusable under TestControl
            // (1s-virtual-ping + wall-clock check), so we drive polling externally to
            // honor `cardanoLiaisonPollingPeriod` in virtual time. `Temporal.sleep` is
            // virtual-clock-aware, so each tick fires after `period` of virtual time and
            // every peer's CardanoLiaison.runEffects runs at the configured cadence.
            liaisonTickFibers <- peers.traverse { peerNum =>
                val pollingPeriod = multiNodeConfig
                    .nodeConfigs(peerNum)
                    .nodeOperationMultisigConfig
                    .cardanoLiaisonPollingPeriod
                val liaison = peerStackMap(peerNum).cardanoLiaison
                (IO.sleep(pollingPeriod) >> (liaison ! CardanoLiaison.Timeout)).foreverM.start
            }

            submittedRequestIds <- Ref[IO].of(Vector.empty[RequestId])
        yield Stage4Sut(
          system = system,
          cardanoBackend = cardanoBackend,
          peers = peerStackMap.map { case (peerNum, stack) =>
              peerNum -> Stage4PeerHandle(eventSequencer = stack.eventSequencer)
          },
          sutErrors = sutErrors,
          errorDrainer = errorDrainer,
          liaisonTickFibers = liaisonTickFibers.toList,
          blockBriefs = blockBriefsMap,
          stacks = stacksMap,
          submittedRequestIds = submittedRequestIds,
          tracerLocal = tracerLocal,
          transportCleanup = transportCleanup,
        )
    }

    /** WS-mode helper: builds one [[PeerWsTransport]] per peer (each bound to a distinct
      * localhost port), spawns one [[RemotePeerProxy]] per (local, remote) pair, registers each
      * local [[PeerLiaison]] with its peer's transport, and returns:
      *
      *   - the remote-handle map (proxy actors keyed by remote peer id), per peer
      *   - a cleanup IO that releases all transports on shutdown
      */
    private def setupWebSocketTransports(
        multiNodeConfig: MultiNodeConfig,
        peers: Seq[HeadPeerNumber],
        basePort: Int,
        system: ActorSystem[IO],
        peerLiaisonMap: Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaison.Handle]],
    )(using CardanoNetwork.Section): IO[
      (Map[HeadPeerNumber, Map[HeadPeerId, PeerLiaison.Handle]], IO[Unit])
    ] = {
        // Map each peer to a localhost address. Bind on 127.0.0.1 (avoids firewall prompts on
        // dev machines); peers dial each other at the same address+port.
        def addrFor(peerNum: HeadPeerNumber): (Host, Port) =
            (host"127.0.0.1", Port.fromInt(basePort + (peerNum: Int)).get)

        def uriFor(peerNum: HeadPeerNumber): Uri = {
            val (h, p) = addrFor(peerNum)
            Uri.unsafeFromString(s"ws://$h:$p/peer")
        }

        // Allocate transports per peer.
        for {
            transportsAndReleases <- peers.toList.traverse { ownPeerNum =>
                val ownPeerId = multiNodeConfig.nodeConfigs(ownPeerNum).ownHeadPeerId
                val (bindH, bindP) = addrFor(ownPeerNum)
                val remotes: Map[HeadPeerId, Uri] = peers
                    .filterNot(_ == ownPeerNum)
                    .map { rpn =>
                        multiNodeConfig.nodeConfigs(rpn).ownHeadPeerId -> uriFor(rpn)
                    }
                    .toMap
                PeerWsTransport
                    .resource(ownPeerId, bindH, bindP, remotes)
                    .allocated
                    .map(ownPeerNum -> _)
            }
            transports = transportsAndReleases.map { case (pn, (t, _)) => pn -> t }.toMap
            cleanup = transportsAndReleases.map { case (_, (_, r)) => r }.sequence_

            // Register each local liaison so inbound msgs from remote get dispatched correctly.
            // peerLiaisonMap(A)(B.id) is A's local liaison for talking to B. When A's transport
            // receives a msg from B, it dispatches to that liaison.
            _ <- peers.toList.traverse_ { ownPeerNum =>
                val transport = transports(ownPeerNum)
                peerLiaisonMap(ownPeerNum).toList.traverse_ { case (remotePeerId, localLiaison) =>
                    transport.register(remotePeerId, localLiaison)
                }
            }

            // Build the proxy actors: one per (local peer, remote peer) pair.
            remoteHandlesByPeer <- peers.toList
                .traverse { ownPeerNum =>
                    val transport = transports(ownPeerNum)
                    peers
                        .filterNot(_ == ownPeerNum)
                        .toList
                        .traverse { remotePeerNum =>
                            val remotePeerId =
                                multiNodeConfig.nodeConfigs(remotePeerNum).ownHeadPeerId
                            for {
                                proxy <- RemotePeerProxy(remotePeerId, transport)
                                ref <- system.actorOf(proxy)
                            } yield remotePeerId -> ref
                        }
                        .map(ownPeerNum -> _.toMap)
                }
                .map(_.toMap)
        } yield (remoteHandlesByPeer, cleanup)
    }

    override def shutdownSut(lastState: ModelState, sut: Stage4Sut): IO[Prop] =
        for
            _ <- logger.warn("shutdownSut")
            // Drain before terminating: the leader's JointLedger is in `Producing` when the
            // last command arrives, and its `userRequestState.requests` only become a
            // `BlockBrief` when the block is sealed. Sealing happens when `BlockWeaver` decides
            // it's time — driven by either the dead-man fallback push-out timer or a deposit
            // decision wakeup, whichever fires first. Without `waitForIdle` we'd terminate
            // before any of that completes and miss the in-progress block's events in the
            // analysis, causing spurious liveness failures.
            //
            // `waitForIdle` returns when all mailboxes are empty, the child set is stable, and
            // deadLetters are drained — it does NOT require "no scheduled future sleeps".
            // BlockWeaver's `sleepSendWakeup` runs in a separate fiber, so between wakeups the
            // actor's mailbox is empty and `isIdle` is true.
            //
            // `maxTimeout` is virtual under TestControl and elapses fast in real time; the
            // default (30s virtual) is enough to drive a pending fallback or deposit-decision
            // wakeup that seals the in-progress block. Under real-clock backends it would be
            // wall-clock; stage4 currently uses only the mock backend so we keep the default.
            //
            // Order matters: `waitForIdle` BEFORE cancelling liaison tick fibers. Cancelling
            // ticks first stops the leader's CardanoLiaison from observing L1 settlement,
            // which kills the path that drives the in-progress block to confirmation — the
            // leader keeps applying its mempool tail but never gets `BlockConfirmed` because
            // followers (with ticks cancelled too) stop emitting acks. The result was visible
            // in WS real-clock 10-peer runs: ~15% of submitted reqIds never reached any
            // brief because the leader's WIP block at shutdown was discarded by terminate.
            // Letting ticks run during `waitForIdle` keeps the L1-polling-driven seal path
            // alive long enough for the tail to land in confirmed blocks. `waitForIdle` can
            // still return because the periods between ticks are mailbox-empty.
            _ <- sut.system.waitForIdle()

            // Slow-cycle tail drain. `waitForIdle` returns when mailboxes are empty + child
            // set stable + deadLetters drained — but it does NOT wait for scheduled future
            // sleeps. The `PeerLiaison` resend timer (`startResendTimer`) ticks in its own
            // fiber via `IO.sleep(peerLiaisonResendInterval) >> ResendCurrent`; between
            // ticks every actor's mailbox is empty and the system reports idle. Under
            // `TransportMode.WebSocket` that interval is real wall-clock, so when the very
            // last stack is closed only milliseconds before `waitForIdle` is called, its
            // hard-acks have not yet completed the cross-peer round-trip needed to
            // saturate every peer's `SlowConsensusActor`. `terminate()` immediately after
            // `waitForIdle` drops the last stack from `StackObserver`'s record, causing a
            // spurious `propStackCoverage` failure on whatever block(s) sit in that final
            // stack. Sleeping ≥ 2 × `peerLiaisonResendInterval` guarantees at least one
            // full resend cycle fires across every link so the tail acks land. The fast
            // cycle has its own analogous tail-drain (the L1-polling tick path described
            // just above); this is its slow-cycle counterpart.
            slowTailDrain = lastState.params.multiNodeConfig.nodeConfigs.values.head
                .peerLiaisonResendInterval * 2
            _ <- IO.sleep(slowTailDrain)

            _ <- sut.liaisonTickFibers.traverse_(_.cancel)
            _ <- sut.system.terminate()
            _ <- logger.warn("shutdownSut: system was terminated")
            _ <- IO.sleep(100.millis) // settle: let the drainer consume any last-cycle errors
            _ <- sut.errorDrainer.cancel
            // Release any WS transport resources (no-op in Direct mode).
            _ <- sut.transportCleanup
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

        stacksByPeer <- sut.stacks.toList
            .traverse { case (p, ref) => ref.get.map(p -> _) }
            .map(_.toMap)
        canonicalStacks = stacksByPeer(sortedPeers.head)
        _ <- logger.info(
          "hard-confirmed stacks per peer: " +
              sortedPeers
                  .map(p => s"peer${p: Int}=${stacksByPeer.getOrElse(p, Vector.empty).size}")
                  .mkString(", ")
        )

        _ <- IO(
          printBlockTable(
            canonicalBriefs,
            sortedPeers,
            briefsByPeer,
            nPeers,
            submittedIds,
            lastState
          )
        )

        _ <- IO(printStackTable(canonicalStacks, sortedPeers, stacksByPeer, nPeers))

        // Mock backend resolves instantly; one attempt is enough. Kept the (attempts, sleep)
        // knob so a Yaci / Blockfrost-backed stage4 future swap just bumps these.
        effectsLandedProp <- {
            given IOLocal[Tracer] = sut.tracerLocal
            EffectsLanded.propEffectsLanded(
              canonicalStacks,
              sut.cardanoBackend,
              attempts = 1,
              sleep = 0.seconds,
            )
        }
    yield propLiveness(submittedIds, canonicalBriefs) &&
        propDepositTiming(lastState.registeredDeposits, canonicalBriefs) &&
        propValidRatio(lastState, canonicalBriefs) &&
        propStackCoverage(canonicalBriefs, canonicalStacks) &&
        effectsLandedProp

    // TODO: side-channel validity-error tracking + propNoStaleRejections
    //
    // `JointLedger.rejectEvent` records every rejection as `(reqId, ValidityFlag.Invalid)` in
    // the in-progress block, so propLiveness sees the request landed in a brief — it cannot
    // distinguish:
    //   1. Reordering-induced ledger errors (e.g. `BadAllInputsUTxOException`) — legitimate
    //      effect of stage4's leader scheduling, expected at stress.
    //   2. Validity-window expirations (`UserRequestError.BlockOutOfRequestValidityInterval`)
    //      — symptom of the request reaching the leader after `requestValidityEnd`, currently
    //      caused by the per-peer model clock vs SUT virtual clock skew (see comment near
    //      `Generator` validity computation).
    //   3. SUT bugs producing wrong Invalid verdicts — what we actually want to catch.
    //
    // Plan:
    //   - Add `Stage4Sut.rejections: Ref[IO, Vector[(RequestId, UserRequestError | L1/L2 err)]]`
    //     populated from a tracer hook in `JointLedger.rejectEvent` (one entry per rejection,
    //     across all peers).
    //   - Add `propNoStaleRejections`: assert no `BlockOutOfRequestValidityInterval` rejections
    //     occurred. Other rejection types are informational only (printed in the analysis
    //     table) — they are the legitimate reordering signal stage4 wants to exercise.
    //   - When `propNoStaleRejections` fails, the message must include the rejection's
    //     timestamps (`blockCreationStartTime`, `requestValidityStart`, `requestValidityEnd`)
    //     and the reqId, so the diagnostic is self-contained without grepping the log.

    /** Property: every submitted request id eventually appears in some block — either as an event
      * (Valid or Invalid) or as a deposit (absorbed or refunded). Catches silent message loss (e.g.
      * the historical `Mempool.extractRequestsWhile` bug).
      */
    private def propLiveness(
        submittedIds: Vector[RequestId],
        canonicalBriefs: Vector[BlockBrief.Intermediate]
    ): Prop = {
        val processedIds: Set[RequestId] =
            canonicalBriefs
                .flatMap(b => b.events.map(_._1) ++ b.depositsAbsorbed ++ b.depositsRefunded)
                .toSet
        val missing = submittedIds.toSet -- processedIds
        Prop(missing.isEmpty) :|
            s"liveness: ${missing.size} submitted reqId(s) never appeared in any block: " +
            s"${missing.toSeq.sortBy(r => (r.peerNum.convert, r.requestNum.convert)).mkString(", ")}"
    }

    /** Property: every absorbed deposit was mature by the time the absorbing block ended, i.e.
      * `brief.endTime >= deposit.absorptionStartTime`. Refund-window check is a TODO — needs
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

    /** Property: SUT's valid/total ratio is no greater than the model's, i.e. the SUT is not more
      * permissive than the model. Compared as exact rationals via cross-multiplication.
      *
      * Both sides are restricted to L2-tx reqIds (excluding any deposit reqId — deposits go into
      * `depositsAbsorbed` / `depositsRefunded` on the SUT side, but a rejected deposit registration
      * ends up in `events` via `JointLedger.rejectEvent` and would otherwise inflate the SUT total
      * relative to the model.
      */
    private def propValidRatio(
        lastState: ModelState,
        canonicalBriefs: Vector[BlockBrief.Intermediate]
    ): Prop = {
        val depositIds = lastState.registeredDeposits.keySet
        val l2TxReqIds = lastState.modelFlags.keySet -- depositIds
        val modelValid = l2TxReqIds.count(lastState.modelFlags(_) == ValidityFlag.Valid).toLong
        val modelTotal = l2TxReqIds.size.toLong
        val sutL2Events =
            canonicalBriefs.flatMap(_.events).filterNot { case (r, _) => depositIds.contains(r) }
        val sutValid = sutL2Events.count(_._2 == ValidityFlag.Valid).toLong
        val sutTotal = sutL2Events.size.toLong

        // sutValid/sutTotal <= modelValid/modelTotal  iff  sutValid*modelTotal <= modelValid*sutTotal
        // Trivially holds when either total is 0 (vacuous).
        val holds =
            modelTotal == 0L || sutTotal == 0L ||
                sutValid * modelTotal <= modelValid * sutTotal
        Prop(holds) :|
            s"valid ratio: SUT $sutValid/$sutTotal exceeds model $modelValid/$modelTotal " +
            s"(SUT is more permissive than the model)"
    }

    /** Property: the slow cycle made progress and kept up — it hard-confirmed at least one stack
      * and, by shutdown idle, every block the fast cycle produced (observed via
      * [[BlockBriefObserver]]) is contained in some hard-confirmed stack (observed via
      * [[StackObserver]]). Catches a slow side that stalls, never closes a stack, or fails to
      * aggregate hard-acks into [[Stack.HardConfirmed]]. Both observers read the canonical peer.
      */
    private def propStackCoverage(
        canonicalBriefs: Vector[BlockBrief.Intermediate],
        canonicalStacks: Vector[Stack.HardConfirmed]
    ): Prop = {
        // A stack covers the inclusive block range [firstBlockNum, lastBlockNum] from its
        // brief (the stack no longer carries its BlockResults — PR #446 review dropped
        // Stack.Unsigned.results; the brief range is the authoritative span).
        val coveredRanges: Vector[(Int, Int)] =
            canonicalStacks.map { s =>
                val b = s.unsigned.brief
                ((b.firstBlockNum: Int), (b.lastBlockNum: Int))
            }
        def covered(n: BlockNumber): Boolean =
            coveredRanges.exists { case (lo, hi) => lo <= (n: Int) && (n: Int) <= hi }
        val observedBlocks: Set[BlockNumber] = canonicalBriefs.map(_.blockNum).toSet
        val uncovered = observedBlocks.filterNot(covered)
        (Prop(canonicalStacks.nonEmpty) :|
            "stack coverage: slow cycle hard-confirmed no stacks") &&
            (Prop(uncovered.isEmpty) :|
                s"stack coverage: ${uncovered.size} observed block(s) never in any " +
                s"hard-confirmed stack: ${uncovered.toSeq.sorted.mkString(", ")}")
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

        val depositIds = lastState.registeredDeposits.keySet
        val l2TxReqIds = lastState.modelFlags.keySet -- depositIds
        val modelValid = l2TxReqIds.count(lastState.modelFlags(_) == ValidityFlag.Valid)
        val modelTotal = l2TxReqIds.size
        val sutL2Events =
            canonicalBriefs.flatMap(_.events).filterNot { case (r, _) => depositIds.contains(r) }
        val sutValid = sutL2Events.count(_._2 == ValidityFlag.Valid)
        val sutTotal = sutL2Events.size

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

    /** Mirror of [[printBlockTable]] for the slow cycle: one row per hard-confirmed stack on the
      * canonical peer, showing the stack number, covered block range, partition spine (Min/Maj/Fin
      * kinds, in stack order), and the round-2 unlock selection (settlement-at-i, finalization-at-i,
      * or sole-acknowledgment / no unlock). Stack-0 renders as `Init`. Followed by a per-peer
      * stack-count line for cross-peer convergence at a glance.
      */
    private def printStackTable(
        canonicalStacks: Vector[Stack.HardConfirmed],
        sortedPeers: Seq[HeadPeerNumber],
        stacksByPeer: Map[HeadPeerNumber, Vector[Stack.HardConfirmed]],
        nPeers: Int
    ): Unit = {
        val colWidth = 72
        val divider = s"+${"-" * (colWidth + 2)}+"
        val header = s"| ${"Stack".padTo(colWidth, ' ')} |"

        println(divider)
        println(header)
        println(divider)

        canonicalStacks.foreach { stack =>
            val brief = stack.unsigned.brief
            val sNum = brief.stackNum: Int
            val first = brief.firstBlockNum: Int
            val last = brief.lastBlockNum: Int
            val nBlks = last - first + 1
            // Slow-consensus leadership schedule is round-robin by stack number, mirroring
            // fast-consensus block-number round-robin.
            val leader = sNum % nPeers
            val (kindStr, partsStr) = stack.effects match {
                case _: StackEffects.HardConfirmed.Initial =>
                    ("Init", "init+fallback")
                case r: StackEffects.HardConfirmed.Regular =>
                    val parts = r.partitions.toList.map {
                        case _: PartitionEffects.Minor[?] => "Min"
                        case _: PartitionEffects.Major[?] => "Maj"
                        case _: PartitionEffects.Final    => "Fin"
                    }
                    val unlockStr = PartitionEffects.unlock(r.partitions) match {
                        case Some(PartitionEffects.Unlock.Settlement(i))   => s"sttlmnt@$i"
                        case Some(PartitionEffects.Unlock.Finalization(i)) => s"fin@$i"
                        case None                                          => "sole"
                    }
                    ("Reg", s"[${parts.mkString(",")}] u=$unlockStr")
            }
            val blkLabel = if nBlks == 1 then "blk" else "blks"
            val label =
                s"#$sNum [$first..$last] ($nBlks $blkLabel) $kindStr lead=p$leader | $partsStr"
            println(s"| ${label.take(colWidth).padTo(colWidth, ' ')} |")
        }

        println(divider)

        println(
          s"Peers: ${sortedPeers.map(p => s"p${p: Int}=${stacksByPeer(p).length}stk").mkString("  ")}"
        )
        println(
          "Legend: Init=initial stack Reg=regular Min/Maj/Fin=partition kinds " +
              "u=unlock (set=settlement, fin=finalization, sole=no unlock) @i=partition index"
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
        useTestControl: Boolean = true,
    ): Gen[ModelState] =
        val cardanoNetwork = CardanoNetwork.Preprod
        val testPeers = TestPeers.apply(SeedPhrase.Yaci, cardanoNetwork, nPeers)
        val testPeerToUtxos = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)

        // For non-TestControl runs we need the head's initial block end-time anchored at a
        // small wall-clock offset in the future, so `startupSut` can sleep until that anchor
        // and have the model clock and the wall clock coincide at command 1. 60s matches
        // stage 1's budget; if 20-peer setup overruns it the test aborts (see startupSut).
        // Under TestControl we keep the deterministic Jan-1-2026 + 100-day random distribution
        // — `Instant.now()` would defeat seed-based reproducibility.
        val takeoffTime: Option[java.time.Instant] =
            if useTestControl then None
            else Some(java.time.Instant.now().plusSeconds(60))

        val generateHeadStartTime = ReaderT((tp: TestPeers) =>
            takeoffTime match {
                case Some(t) =>
                    Gen.const(BlockCreationEndTime(t.quantize(tp.slotConfig)))
                case None =>
                    // Date and time (GMT): Thursday, January 1, 2026 at 12:00:00 AM, POSIX seconds
                    val anchorTime = 1767225600L
                    // 100 day range, seconds
                    val range = 86_400 * 100L
                    for offset <- Gen.choose(0L, range)
                    yield BlockCreationEndTime(
                      java.time.Instant.ofEpochSecond(anchorTime + offset).quantize(tp.slotConfig)
                    )
            }
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
              generateHeadConfig = generateHeadConfig_,
              // Halve the maximum allowed polling period (the default is
              // `headConfig.maxCardanoLiaisonPollingPeriod`). With a lower upper bound,
              // every peer's CardanoLiaison polls L1 more frequently — peers see new
              // deposits closer in time, which should reduce inter-peer skew that may
              // currently produce mismatched block briefs at major-block consensus.
              generateNodeOperationMultisigConfig = hc =>
                  hydrozoa.config.node.operation.multisig.generateNodeOperationMultisigConfig(
                    hc.maxCardanoLiaisonPollingPeriod / 2
                  )
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
          currentModelTime = startTime,
          takeoffTime = takeoffTime,
          utxosL2Active = config.headConfig.initializationParameters.initialEvacuationMap.toUtxos,
          peerUtxosL1 = peerUtxosL1,
          nextRequestNumbers = peers.map(_ -> RequestNumber(0)).toMap,
          pendingDeposits = peers.map(_ -> Nil).toMap,
          modelFlags = Map.empty,
          registeredDeposits = Map.empty,
        )
