package hydrozoa.integration.harness

import cats.data.ReaderT
import cats.effect.{IO, Ref, Resource}
import cats.implicits.*
import com.comcast.ip4s.{Port, host}
import com.suprnation.actor.event.Error as ActorError
import com.suprnation.actor.{ActorContext, ActorSystem}
import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.config.head.initialization.{InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.multisig.timing.TxTiming.Durations.*
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.config.head.{HeadConfig, InitParamsType, generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.operation.multisig.{RateLimits, generateNodeOperationMultisigConfig}
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant, quantize}
import hydrozoa.lib.logging.{ContraTracer, LogEvent, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.{CardanoBackend as L1Backend, CardanoBackendMock, FirewalledCardanoBackendEvent, MockState, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.{CardanoLiaison, RequestSequencer, UserRequest, UserRequestBody, UserRequestHeader}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerId, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.transport.*
import hydrozoa.multisig.ledger.block.BlockVersion.Major.given_Conversion_Major_Int
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.ledger.l1.tx.{EnrichedTx, SettlementTx}
import hydrozoa.multisig.persistence.rocksdb.RocksDbBackendStore
import hydrozoa.multisig.persistence.{BackendStore, Cf, InMemoryBackendStore, Persistence, PersistenceEvent, PersistenceEventFormat}
import hydrozoa.multisig.server.{HydrozoaHttpEvent, HydrozoaHttpEventFormat, HydrozoaRoutes, HydrozoaServer, SubmissionClient}
import hydrozoa.multisig.{CoilMultisigRegimeManager, CoilMultisigRegimeManagerEventFormat, CoilRegimeManagerEvent, HeadMultisigRegimeManager, HeadMultisigRegimeManagerEventFormat, HeadRegimeManagerEvent, NodeStatus}
import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.concurrent.TimeUnit
import org.http4s.client.Client as Http4sClient
import org.http4s.client.websocket.WSClient
import org.http4s.implicits.*
import org.http4s.jdkhttpclient.JdkWSClient
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.{HttpRoutes, Uri}
import org.scalacheck.{Gen, PropertyM}
import scala.concurrent.duration.{DurationLong, FiniteDuration}
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.{CardanoInfo, CertState, Utxos}
import scalus.uplc.builtin.ByteString
import test.{GenWithTestPeers, TestPeerName, TestPeers, given}

/** Scaffold for a multi-peer head (+ optional coil followers) [[Resource]] backed by a shared
  * mock L1.
 *
  * Test-side concerns (capture observers, signal `Deferred`s, custom per-peer handle types like
  * stage4's `Stage4PeerHandle`) are injected via [[Hooks]] — the harness threads test-provided
  * tracers into each MRM and runs a test-provided `(num, connections) => IO[H]` finalizer once
  * each peer's connections are available.
  */
object MultiPeerHeadHarness:

    // ===================================
    // Public surface
    // ===================================

    /** Wall-clock alignment for the head's initial-block end-time.
      *
      *   - `useTestControl = true`: return `None`. The head-config generator falls back to the
      *     deterministic Jan-1-2026 + 100-day random anchor (see [[generateHeadStartTime]]),
      *     which is stable across TestControl seeds; reading `IO.realTimeInstant` there would
      *     defeat reproducibility.
      *   - `useTestControl = false`: return `Some(now + offset)`, materialised via
      *     [[IO.realTimeInstant]] so no wall-clock read happens at property-construction time.
      *     `offset` gives the scenario room to spawn actors before the initial block's
      *     validity window opens; each test tunes it against its actor-spawn budget.
      */
    def mkTakeoffTime(
        useTestControl: Boolean,
        offset: FiniteDuration,
    ): IO[Option[Instant]] =
        if useTestControl then IO.pure(None)
        else IO.realTimeInstant.map(t => Some(t.plusSeconds(offset.toSeconds)))

    /** Head initial-block-end-time generator anchored on [[mkTakeoffTime]]. When `takeoffTime`
      * is `Some`, quantize it to the peer's slot config. When `None`, generate a random
      * Jan-1-2026 + 100-day offset — deterministic per ScalaCheck seed, safe under TestControl.
      * Feed to `hydrozoa.config.head.initialization.generateInitialBlock`'s
      * `generateBlockCreationEndTime` parameter.
      */
    def generateHeadStartTime(
        takeoffTime: Option[Instant]
    ): GenWithTestPeers[BlockCreationEndTime] =
        ReaderT { (tp: TestPeers) =>
            takeoffTime match {
                case Some(t) => Gen.const(BlockCreationEndTime(t.quantize(tp.slotConfig)))
                case None    =>
                    val anchorTime = 1767225600L // Jan 1 2026 00:00:00 UTC
                    val range      = 86_400 * 100L // 100 days in seconds
                    for offset <- Gen.choose(0L, range)
                    yield BlockCreationEndTime(
                      java.time.Instant
                          .ofEpochSecond(anchorTime + offset)
                          .quantize(tp.slotConfig)
                    )
            }
        }

    /** Static fast [[TxTiming]] so `Action.FallbackToRuleBased` fires within a wall-clock
      * scenario budget (settlement/fallback windows are seconds, not hours). Shared by the
      * dispute-flow tests via [[mkResource]]'s default.
      */
    val fastTxTiming: GenWithTestPeers[TxTiming] = ReaderT { (network: TestPeers) =>
        Gen.const(
          TxTiming(
            minSettlementDuration = MinSettlementDuration(2.seconds.quantize(network.slotConfig)),
            // Init tx window: initEndTime = bcet + minSettlementDuration + inactivityMarginDuration
            // = 32s. Actor bring-up + stack-0 hard-confirmation + CL's first poll all fit inside
            // that or `InitWindowElapsed` fires. Also gates the Minor→Major deadman.
            inactivityMarginDuration =
                InactivityMarginDuration(30.seconds.quantize(network.slotConfig)),
            silenceDuration = SilenceDuration(1.second.quantize(network.slotConfig)),
            depositSubmissionDuration =
                DepositSubmissionDuration(1.second.quantize(network.slotConfig)),
            depositMaturityDuration =
                DepositMaturityDuration(1.second.quantize(network.slotConfig)),
            depositAbsorptionDuration =
                DepositAbsorptionDuration(2.minutes.quantize(network.slotConfig)),
          )
        )
    }

    /** Static fast voting deadline (5s `votingDuration`) so the deadline-gated tally path unblocks
      * within a wall-clock scenario budget — the default generator picks 1h..5d. Shared by the
      * dispute-flow tests via [[mkResource]]'s default.
      */
    val fastDisputeResolutionConfig: GenWithTestPeers[DisputeResolutionConfig] =
        ReaderT { (network: TestPeers) =>
            Gen.const(
              DisputeResolutionConfig(
                votingDuration = QuantizedFiniteDuration(
                  slotConfig = network.slotConfig,
                  finiteDuration = 5.seconds,
                )
              )
            )
        }

    /** Route every harness [[Event]] through the per-cell human formatters into slf4j, so scenario
      * runs are visible in the console/log without touching each MRM's internal tracer. Coil events
      * get a synthetic head-peer label past the head range.
      */
    def humanFormatTracer(nHeadPeers: Int): ContraTracer[IO, Event] =
        ContraTracer[IO, Event] {
            case Event.Head(peerNum, evt) =>
                Slf4jTracer.sink.traceWith(
                  HeadMultisigRegimeManagerEventFormat.humanFormat(peerNum)(evt)
                )
            case Event.Coil(coilNum, evt) =>
                val syntheticLabel = HeadPeerNumber(nHeadPeers + coilNum.convert)
                Slf4jTracer.sink.traceWith(
                  CoilMultisigRegimeManagerEventFormat.humanFormat(syntheticLabel, coilNum)(evt)
                )
        }

    /** A pure predicate over outbound txs deciding whether the [[FirewalledCardanoBackend]] drops
      * them. Composable with `||` / `&&` / unary `!`; lift to the backend's gate with [[toGate]].
      */
    opaque type DropRule = EnrichedTx[?] => Boolean

    object DropRule:
        def apply(p: EnrichedTx[?] => Boolean): DropRule = p

        /** Never drops — the unit for `||`. */
        val never: DropRule = _ => false

        /** Drop the [[SettlementTx]] that would advance the on-chain treasury to `major`. Pinning
          * on-chain below the off-chain view is what triggers `Action.FallbackToRuleBased`.
          */
        def settlementProducingMajor(major: Int): DropRule = {
            case s: SettlementTx => s.majorVersionProduced.convert == major
            case _               => false
        }

        extension (self: DropRule)
            def ||(other: DropRule): DropRule = etx => self(etx) || other(etx)
            def &&(other: DropRule): DropRule = etx => self(etx) && other(etx)
            def unary_! : DropRule            = etx => !self(etx)

            /** Lift into the backend's effectful gate. Rules are pure, so this never suspends. */
            def toGate: EnrichedTx[?] => IO[Boolean] = etx => IO.pure(self(etx))

    /** Shared slf4j sink for a peer's [[FirewalledCardanoBackend]] — logs each drop (warn) and
      * pass (info) tagged with the peer. Compose extra capture tracers onto it with `|+|`.
      */
    def firewallSlf4jSink(peerId: PeerId): ContraTracer[IO, FirewalledCardanoBackendEvent] =
        val peerLabel = peerId match
            case PeerId.Head(n) => s"head-$n"
            case PeerId.Coil(n) => s"coil-$n"
        Slf4jTracer.sink.contramap {
            case FirewalledCardanoBackendEvent.DroppedOutboundTx(hash) =>
                LogEvent
                    .From(Map("peer" -> peerLabel), "FirewalledCardanoBackend")
                    .warn(s"firewall DROPPED tx $hash")
            case FirewalledCardanoBackendEvent.SubmittedTx(hash, result) =>
                LogEvent
                    .From(Map("peer" -> peerLabel), "FirewalledCardanoBackend")
                    .info(s"firewall passed tx $hash result=$result")
        }

    /** The standard head-peer handle for the dispute-flow tests: each head peer exposes its
      * `RequestSequencer.Handle`; coil peers expose `None`.
      */
    val requestSequencerHandle
        : (PeerId, HeadMultisigRegimeManager.Connections) => IO[Option[RequestSequencer.Handle]] = {
        case (PeerId.Head(peerNum), conns) =>
            IO.fromOption(conns.requestSequencer)(
              new NoSuchElementException(
                s"peer $peerNum has no RequestSequencer.Handle in its Connections"
              )
            ).map(Some(_))
        case (_: PeerId.Coil, _) => IO.pure(None)
    }

    /** Submit one minimal empty-payload `TransactionRequest` to `peer`'s `RequestSequencer`,
      * kicking block production past block 1. `JointLedger` marks the empty request `Invalid`, but
      * the block still completes — which is all these scenarios need.
      */
    def submitEmptyTransactionRequest(
        harness: Harness[Option[RequestSequencer.Handle]],
        peer: HeadPeerNumber = HeadPeerNumber(0),
    ): IO[Unit] =
        val multiNodeConfig = harness.multiNodeConfig
        val slotConfig = multiNodeConfig.headConfig.cardanoNetwork.slotConfig
        val body: UserRequestBody.TransactionRequestBody =
            UserRequestBody.TransactionRequestBody(
              l2Payload = ByteString.fromArray(Array.empty[Byte])
            )
        val userVk = multiNodeConfig.nodeConfigs(peer).ownWallet.exportVerificationKey
        for
            now <- IO.realTimeInstant
            header = UserRequestHeader(
              headId = multiNodeConfig.headConfig.headId,
              validityStart = RequestValidityStartTime(
                QuantizedInstant.ofEpochSeconds(slotConfig, now.getEpochSecond - 5L)
              ),
              validityEnd = RequestValidityEndTime(
                QuantizedInstant.ofEpochSeconds(slotConfig, now.getEpochSecond + 300L)
              ),
              bodyHash = body.hash,
            )
            userRequest = UserRequest.TransactionRequest(header, body, userVk)
            sequencer <- IO.fromOption(harness.peers.get(peer).flatMap(_.handle))(
              new NoSuchElementException(s"peer $peer missing in harness")
            )
            _ <- sequencer ?: userRequest
        yield ()

    /** Stand up the head + coil peers for a dispute-flow test: derive pre-init UTxOs and coil
      * wallets from `testPeers`, coil node configs and the TestControl start epoch from
      * `multiNodeConfig`, wire the standard `RequestSequencer.Handle` hook, and apply the caller's
      * `tracer` / `wrapBackend`.
      */
    def disputeHarnessResource(
        label: String,
        transportMode: Transport.Mode,
        multiNodeConfig: MultiNodeConfig,
        testPeers: TestPeers,
        takeoffTime: Option[Instant],
        tracer: ContraTracer[IO, Event],
        wrapBackend: (PeerId, L1Backend[IO]) => L1Backend[IO],
    ): Resource[IO, Harness[Option[RequestSequencer.Handle]]] =
        val preinitPeerUtxosL1 =
            yaciTestSauceGenesis(testPeers.cardanoNetwork.network)(testPeers).map {
                case (name, utxos) => name.headPeerNumber -> utxos
            }
        // Under TestControl the harness jumps virtual time to `startEpochMs` before any actor
        // exists (see PreSystem.testControlPresleep). Anchor to the head's configured initial block
        // end-time so the model clock is coherent with the head config.
        val startEpochMs = multiNodeConfig.headConfig.initialBlock.blockBrief.endTime
            .convert.instant.toEpochMilli
        val coilNodeConfigs = multiNodeConfig.mkCoilNodeConfigs(testPeers.coilWallets)
        resource[Option[RequestSequencer.Handle]](
          Inputs(
            config = Config(
              label = label,
              backendMode = StorageBackend.Mode.InMemory,
              transportMode = transportMode,
            ),
            multiNodeConfig = multiNodeConfig,
            coilNodeConfigs = coilNodeConfigs,
            preinitPeerUtxosL1 = preinitPeerUtxosL1,
            takeoffTime = takeoffTime,
            startEpochMs = startEpochMs,
          ),
          Hooks[Option[RequestSequencer.Handle]](
            tracer = tracer,
            handle = requestSequencerHandle,
            wrapBackend = wrapBackend,
          ),
        )

    /** Shared `PropertyM[IO, Resource[IO, Ctx]]` shell for dispute-flow integration tests: takeoff
      * time, yaci-genesis-pinned MNC, initial block anchored on `takeoffTime`, coil peers in the
      * bootstrap. Pins the 100ms evacuation polling + 500ms/250ms rate-limits both callers need;
      * `buildCtx` owns everything test-specific.
      */
    def mkResource[Ctx](
        transportMode: Transport.Mode,
        testPeers: TestPeers,
        testPeerToUtxos: Map[TestPeerName, Utxos],
        takeoffOffset: FiniteDuration,
        fastTxTiming: GenWithTestPeers[TxTiming] = fastTxTiming,
        disputeResolutionConfig: GenWithTestPeers[DisputeResolutionConfig] =
            fastDisputeResolutionConfig,
        coilPeers: CoilPeers = CoilPeers.empty,
        coilQuorum: Int = 0,
    )(
        buildCtx: (Option[Instant], MultiNodeConfig) => Resource[IO, Ctx]
    ): PropertyM[IO, Resource[IO, Ctx]] =
        for {
            takeoffTime <- PropertyM.run(
              mkTakeoffTime(transportMode.useTestControl, takeoffOffset)
            )
            mnc <- PropertyM.pick[IO, MultiNodeConfig](
              MultiNodeConfig
                  .generateWith(testPeers)(
                    generateHeadConfig = generateHeadConfig(
                      genHeadConfigBootstrap = generateHeadConfigBootstrap(
                        generateHeadParams = generateHeadParameters(
                          generateTxTiming = fastTxTiming,
                          generateDisputeResolutionConfig = disputeResolutionConfig,
                        ).map(_.copy(coilQuorum = coilQuorum)),
                        generateInitializationParameters = InitParamsType.TopDown(
                          InitializationParametersGenTopDown.GenWithDeps(
                            generateGenesisUtxosL1 = ReaderT((_: TestPeers) =>
                                Gen.const(
                                  testPeerToUtxos.map { case (k, v) => k.headPeerNumber -> v }
                                )
                            )
                          )
                        ),
                        coilPeers = coilPeers,
                      ),
                      generateInitialBlock = (bootstrap, funding) =>
                          generateInitialBlock(
                            genHeadConfigBootstrap = ReaderT
                                .pure[Gen, TestPeers, (
                                    HeadConfig.Bootstrap,
                                    hydrozoa.bootstrap.InitializationFunding
                                )]((bootstrap, funding)),
                            generateBlockCreationEndTime = generateHeadStartTime(takeoffTime),
                          ),
                    ),
                    generateNodeOperationEvacuationConfig = w =>
                        Gen.const(
                          NodeOperationEvacuationConfig(
                            evacuationBotPollingPeriod = 100.millis,
                            ruleBasedWallet = w,
                          )
                        ),
                    generateNodeOperationMultisigConfig = hc =>
                        generateNodeOperationMultisigConfig(
                          maxPollingPeriod = hc.maxCardanoLiaisonPollingPeriod / 2,
                          rateLimits = RateLimits(
                            softBlockMinPeriod = 500.millis,
                            hardStackMinPeriod = 250.millis,
                          ),
                        )
                  )
                  .label("MultiNodeConfig")
            )
        } yield buildCtx(takeoffTime, mnc)

    case class Config(
        label: String,
        backendMode: StorageBackend.Mode,
        transportMode: Transport.Mode,
    )

    case class Inputs(
        config: Config,
        multiNodeConfig: MultiNodeConfig,
        coilNodeConfigs: List[NodeConfig],
        preinitPeerUtxosL1: Map[HeadPeerNumber, Utxos],
        takeoffTime: Option[Instant],
        startEpochMs: Long,
    )

    /** Roll-up of every event emitted by the regime managers the harness owns.
      */
    enum Event:
        case Head(peerNum: HeadPeerNumber, event: HeadRegimeManagerEvent)
        case Coil(coilNum: CoilPeerNumber, event: CoilRegimeManagerEvent)

    /** Test-side wiring injected into each MRM. The [[Event]]-typed projects down to Head/Coil-specific tracers
     * and gets contramapped with the regime managers' tracers. `peerHandle` / `coilHandle` run once each MRM's
      * `connectionsDeferred` resolves.
      */
    /** Test-side wiring — one `PeerId`-keyed hook per concern. Callers that only need a head-side
      * handle can use `H = Option[HeadHandle]` and return `None` from the coil branch; likewise
      * `wrapBackend` can pattern-match on `PeerId.Head` / `PeerId.Coil` when the wrap differs
      * (e.g. only head peers get firewalled).
      */
    case class Hooks[H](
        tracer: ContraTracer[IO, Event],
        handle: (PeerId, HeadMultisigRegimeManager.Connections) => IO[H],
        // Wrap the shared mock backend per peer (e.g. FirewalledCardanoBackend). Identity default.
        wrapBackend: (PeerId, L1Backend[IO]) => L1Backend[IO] = (_, b) => b,
    )

    /** Per-head-peer artifacts exposed to callers: resolved connections, persistence backend, the
      * in-process [[SubmissionClient]] (bound to this peer's [[HydrozoaRoutes]] via
      * `Client.fromHttpApp`), and the caller-derived handle.
      */
    case class Peer[H](
        connections: HeadMultisigRegimeManager.Connections,
        backendStore: BackendStore[IO],
        submissionClient: SubmissionClient,
        handle: H,
    )

    case class Coil[H](
        connections: HeadMultisigRegimeManager.Connections,
        backendStore: BackendStore[IO],
        handle: H,
    )

    /** Everything the harness yields. `sutErrors` is appended to by the error drainer (one entry
      * per uncaught actor exception); callers read it post-run.
      */
    case class Harness[H](
        transportMode: Transport.Mode,
        multiNodeConfig: MultiNodeConfig,
        system: ActorSystem[IO],
        cardanoBackend: L1Backend[IO],
        l1Snapshot: IO[Utxos],
        peers: Map[HeadPeerNumber, Peer[H]],
        coils: Map[CoilPeerNumber, Coil[H]],
        sutErrors: Ref[IO, List[String]],
    )

    /** Build a fully-wired multi-peer head + coil followers. The returned resource owns
      * everything; release cancels the CL tick fibers and the error drainer.
      */
    def resource[H](
        inputs: Inputs,
        hooks: Hooks[H],
    ): Resource[IO, Harness[H]] =
        import inputs.*
        import inputs.config.*
        val peers = multiNodeConfig.nodeConfigs.keys.toSeq.sortBy(p => p: Int)
        val log: ContraTracer[IO, Slf4jMsg] =
            Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("Harness"))
        for
            // Handle websocket "take off" timing alignment
            _ <- Resource.eval(
                     PreSystem.align(transportMode.useTestControl, startEpochMs, takeoffTime, log)
                 )

            system                     <- ActorSystem[IO](label)
            backendAndSnapshot         <- Resource.eval(
                                              CardanoBackend.mkMock(
                                                preinitPeerUtxosL1,
                                                multiNodeConfig.headConfig.scriptReferenceUtxos,
                                                multiNodeConfig.headConfig.cardanoInfo,
                                              )
                                          )
            (cardanoBackend, l1Snapshot) = backendAndSnapshot
            transports <- Transport.setup(
                              transportMode,
                              multiNodeConfig,
                              peers,
                              coilNodeConfigs,
                          )
            peerMrms <- peers.toList
                            .traverse(peerNum =>
                                Mrm
                                    .buildPeer(
                                      peerNum,
                                      system,
                                      hooks.wrapBackend(PeerId.Head(peerNum), cardanoBackend),
                                      multiNodeConfig,
                                      backendMode,
                                      transports.headNetworks(peerNum),
                                      hooks.tracer.contramap(Event.Head(peerNum, _)),
                                    )
                                    .map(peerNum -> _)
                            )
                            .map(_.toMap)
            coilMrms <- coilNodeConfigs
                            .traverse { coilConfig =>
                                val coilNum = Transport.coilNumOf(coilConfig)
                                Mrm
                                    .buildCoil(
                                      coilConfig,
                                      coilNum,
                                      system,
                                      hooks.wrapBackend(PeerId.Coil(coilNum), cardanoBackend),
                                      multiNodeConfig,
                                      transports.coilUplinks(coilNum),
                                      hooks.tracer.contramap(Event.Coil(coilNum, _)),
                                    )
                                    .map(coilNum -> _)
                            }
                            .map(_.toMap)
            // WS Phase 2 — bind NodeWsServers and start mesh + coil dialers. Acquired *after*
            // peerMrms/coilMrms so its finalizer (server stop + dialer cancel) runs *before*
            // the MRMs stop their actors and close RocksDB. Otherwise an inbound WS frame can
            // tell an actor whose handler calls Persistence after the column-family handles
            // were freed → use-after-free SIGSEGV in `FailIfCfHasTs`. Direct mode: no-op.
            _ <- transports.bringUpNetwork
            peerConnections <- Resource.eval(
                                   peerMrms.toList
                                       .traverse { case (peerNum, peerMrm) =>
                                           peerMrm.mrm.connectionsDeferred.get.map(peerNum -> _)
                                       }
                                       .map(_.toMap)
                               )
            coilConnections <- Resource.eval(
                                   coilMrms.toList
                                       .traverse { case (coilNum, coilMrm) =>
                                           coilMrm.mrm.connectionsDeferred.get.map(coilNum -> _)
                                       }
                                       .map(_.toMap)
                               )
            sutErrors <- Resource.eval(Ref[IO].of(List.empty[String]))
            _         <- ErrorDrainer.start(system, sutErrors)
            _ <- Ticks.startForHeads(
                     peerConnections,
                     peerNum =>
                         multiNodeConfig
                             .nodeConfigs(peerNum)
                             .nodeOperationMultisigConfig
                             .cardanoLiaisonPollingPeriod,
                 )
            _ <- Ticks.startForCoils(
                     coilConnections,
                     coilNum =>
                         coilMrms(coilNum).config.nodeOperationMultisigConfig
                             .cardanoLiaisonPollingPeriod,
                 )
            peerEntries <- Resource.eval(
                               peerConnections.toList.traverse { case (peerNum, conns) =>
                                   for
                                       submissionClient <- Http.mkPeerSubmissionClient(
                                                               peerNum,
                                                               conns,
                                                               multiNodeConfig,
                                                           )
                                       h <- hooks.handle(PeerId.Head(peerNum), conns)
                                   yield peerNum -> Peer(
                                     connections = conns,
                                     backendStore = peerMrms(peerNum).backendStore,
                                     submissionClient = submissionClient,
                                     handle = h,
                                   )
                               }
                           )
            coilEntries <- Resource.eval(
                               coilConnections.toList.traverse { case (coilNum, conns) =>
                                   hooks.handle(PeerId.Coil(coilNum), conns).map { h =>
                                       coilNum -> Coil(
                                         connections = conns,
                                         backendStore = coilMrms(coilNum).backendStore,
                                         handle = h,
                                       )
                                   }
                               }
                           )
        yield Harness(
          transportMode = transportMode,
          multiNodeConfig = multiNodeConfig,
          system = system,
          cardanoBackend = cardanoBackend,
          l1Snapshot = l1Snapshot,
          peers = peerEntries.toMap,
          coils = coilEntries.toMap,
          sutErrors = sutErrors,
        )

    // ===================================
    // PreSystem — clock alignment before any actor exists
    // ===================================

    object PreSystem:
        /** TestControl jump + WS wall-clock takeoff wait, in order. The TestControl jump must
          * precede `ActorSystem` so actor ping loops don't compete with `tickOne`.
          */
        def align(
            useTestControl: Boolean,
            startEpochMs: Long,
            takeoffTime: Option[Instant],
            log: ContraTracer[IO, Slf4jMsg],
        ): IO[Unit] =
            testControlPresleep(useTestControl, startEpochMs) >>
                websocketTakeoff(takeoffTime, log)

        /** Under TestControl, jump the virtual clock from 0 to the head's start epoch BEFORE any
          * actor exists. No-op when TestControl is off (the configured start epoch is potentially
          * years in the future).
          */
        private def testControlPresleep(useTC: Boolean, startEpochMs: Long): IO[Unit] =
            IO.whenA(useTC)(IO.sleep(FiniteDuration(startEpochMs, TimeUnit.MILLISECONDS)))

        /** Under WS (real-clock) runs, wait until the wall clock reaches `takeoffTime` so the
          * model clock and the SUT wall clock coincide at command 1. Abort if setup overran the
          * budget. Same shape as stage 1. No-op when `takeoffTime` is `None` (TestControl runs).
          */
        private def websocketTakeoff(
            takeoffTime: Option[Instant],
            log: ContraTracer[IO, Slf4jMsg],
        ): IO[Unit] = takeoffTime match
            case None    => IO.unit
            case Some(t) =>
                IO.realTimeInstant.flatMap { now =>
                    if now.isAfter(t) then
                        IO.raiseError(
                          RuntimeException(
                            "PreSystem: initialization took too long " +
                                s"(takeoff: $t, now: $now)"
                          )
                        )
                    else
                        val sleepMs = t.toEpochMilli - now.toEpochMilli
                        val tickMs  = 5_000L
                        val ticks   = sleepMs / tickMs
                        val remMs   = sleepMs % tickMs
                        log.info(s"WS mode: sleeping ${sleepMs / 1000}s until takeoff") >>
                            (0L until ticks).toList.traverse_ { i =>
                                IO.sleep(tickMs.millis) >>
                                    log.info(
                                      s"WS takeoff in ${(sleepMs - (i + 1) * tickMs) / 1000}s"
                                    )
                            } >>
                            IO.sleep(remMs.millis)
                }

    // ===================================
    // CardanoBackend — shared mock L1
    // ===================================

    object CardanoBackend:
        /** Single mock L1 shared by every peer, seeded with the merged pre-init UTxOs plus the
          * globally-deployed script reference UTxOs (treasury + dispute validators). The head
          * initialization tx is submitted by the protocol through normal operation.
          */
        def mkMock(
            preinitPeerUtxosL1: Map[HeadPeerNumber, Utxos],
            scriptReferenceUtxos: hydrozoa.config.ScriptReferenceUtxos,
            cardanoInfo: CardanoInfo,
        ): IO[(L1Backend[IO], IO[Utxos])] =
            val genesisUtxos: Utxos =
                preinitPeerUtxosL1.values.reduce(_ ++ _) ++
                    scriptReferenceUtxos.toList.map(_.toTuple).toMap
            CardanoBackendMock.mockIOWithSnapshot(
              initialState = MockState(genesisUtxos),
              mkContext = slot =>
                  Context(
                    env = UtxoEnv(
                      slot = slot,
                      params = cardanoInfo.protocolParams,
                      certState = CertState.empty,
                      network = cardanoInfo.network,
                    ),
                    slotConfig = cardanoInfo.slotConfig,
                  ),
            )

    // ===================================
    // StorageBackend — per-peer persistence
    // ===================================

    object StorageBackend:
        /** Persistence backend selector. In-memory is fast and isolated; RocksDB exercises on-disk
          * compaction/batching.
          */
        enum Mode:
            case InMemory
            case RocksDb(root: Path = Files.createTempDirectory("hydrozoa-harness-rocksdb-"))

        /** Per-peer backend allocator chosen by [[Mode]]. */
        def openPerPeer(
            peerNum: HeadPeerNumber,
            mode: Mode,
            cfs: List[Cf],
            tracer: ContraTracer[IO, PersistenceEvent],
        ): Resource[IO, BackendStore[IO]] =
            mode match
                case Mode.InMemory => InMemoryBackendStore.open(tracer)
                case Mode.RocksDb(root) =>
                    val dir = root.resolve(s"peer-${peerNum: Int}")
                    Resource.eval(IO.blocking(Files.createDirectories(dir))) >>
                        RocksDbBackendStore.open(dir, cfs, tracer)

    // ===================================
    // Transport — per-mode bring-up
    // ===================================

    object Transport:
        /** Per-peer mesh transport selector. `useTestControl` is an extension on the mode so
          * callers can wire it into their `ModelBasedSuite.useTestControl` override without
          * re-deriving the rule.
          */
        enum Mode:
            case Direct
            case WebSocket

        extension (mode: Mode)
            def useTestControl: Boolean = mode match
                case Mode.Direct    => true
                case Mode.WebSocket => false

        /** Transport-layer state needed to build Regime Managers. Produced by
          * exactly one of [[setupDirect]] / [[setupWebSocket]]; the consumer doesn't need to know
          * which mode it's in.
          *
          * `bringUpNetwork` is a second-phase resource the caller must acquire *after* the MRMs
          * (and their RocksDB backends) are built. Under WS it binds sockets and starts dialers;
         *  under Direct it is a no-op. Acquiring it last makes its finalizer run *before* the MRM
          * finalizers stop their actors and close RocksDB — so no inbound WS message can cause a use-after-free
         * segfault.
          */
        case class Setup(
            headNetworks: Map[HeadPeerNumber, HeadNetwork],
            coilUplinks: Map[CoilPeerNumber, ContextFn[CoilTransport]],
            bringUpNetwork: Resource[IO, Unit],
        )

        /** Per-peer transport bundle: the head-mesh transport and an optional hub-side hub-coil
          * transport (present on hubs).
          */
        case class HeadNetwork(
            peerTransport: PeerTransport,
            hubTransport: Option[HubTransport],
        )

        type ContextArg  = ActorContext[IO, HeadMultisigRegimeManager.Request, Any]
        type ContextFn[T] = ContextArg => T

        def setup(
            mode: Mode,
            multiNodeConfig: MultiNodeConfig,
            peers: Seq[HeadPeerNumber],
            coilNodeConfigs: List[NodeConfig],
        ): Resource[IO, Setup] = mode match
            case Mode.Direct    => setupDirect(multiNodeConfig, peers, coilNodeConfigs)
            case Mode.WebSocket => setupWebSocket(multiNodeConfig, peers, coilNodeConfigs)

        /** This peer's [[HeadPeerId]] — derived from its number + head-set size; `NodeConfig` does
          * not expose a head-specific id.
          */
        def headPeerId(multiNodeConfig: MultiNodeConfig, peerNum: HeadPeerNumber): HeadPeerId =
            HeadPeerId(peerNum, multiNodeConfig.nHeadPeers)

        def coilNumOf(coilConfig: NodeConfig): CoilPeerNumber =
            coilConfig.ownPeerId match
                case PeerId.Coil(n) => n
                case PeerId.Head(_) =>
                    throw new IllegalStateException("coil node config carries a head peer id")

        /** Direct (in-process) bring-up: allocates the head-mesh registry and (if needed) the
          * hub-coil registry, builds every peer's transport bundle, and connects every coil's
          * uplink through the in-process hub-coil router. No `wsClient`, no port discovery, no
          * dialer pass.
          */
        private def setupDirect(
            multiNodeConfig: MultiNodeConfig,
            peers: Seq[HeadPeerNumber],
            coilNodeConfigs: List[NodeConfig],
        ): Resource[IO, Setup] =
            for
                inProcessRegistry <- Resource.eval(InProcessPeerTransport.emptyRegistry)
                hubCoilRegistry <-
                    if coilNodeConfigs.isEmpty then
                        Resource.pure[IO, Option[InProcessHubCoilTransport.Registry]](None)
                    else
                        Resource
                            .eval(InProcessHubCoilTransport.emptyRegistry)
                            .map(Some(_))
                headNetworks <- peers.toList
                                    .traverse(peerNum =>
                                        directHeadNetwork(
                                          peerNum,
                                          multiNodeConfig,
                                          inProcessRegistry,
                                          hubCoilRegistry,
                                        ).map(peerNum -> _)
                                    )
                                    .map(_.toMap)
                coilUplinks <- coilNodeConfigs
                                   .traverse { coilConfig =>
                                       val coilNum = coilNumOf(coilConfig)
                                       val registry = hubCoilRegistry.getOrElse(
                                         throw new IllegalStateException(
                                           "coilNodeConfigs is non-empty but " +
                                               "hubCoilRegistry was not allocated"
                                         )
                                       )
                                       Resource
                                           .eval(InProcessHubCoilTransport.Coil
                                               .create(coilNum, registry))
                                           .map(t =>
                                               coilNum -> ((_: ContextArg) => t: CoilTransport)
                                           )
                                   }
                                   .map(_.toMap)
            yield Setup(headNetworks, coilUplinks, Resource.unit)

        /** WebSocket (real-clock) bring-up: split into a creation phase (this method) and a
          * deferred [[Setup.bringUpNetwork]] phase. The creation phase allocates the shared
          * `JdkWSClient`, each peer's `WsPeerTransport`/`HubWsTransport`, and each coil's
          * `CoilPeerWsTransport` — handles the MRMs need. The bring-up phase, acquired by the
          * harness *after* the MRMs are built, binds every peer's `NodeWsServer` on port 0
          * (OS-assigned ephemeral), then builds the `HeadPeerId -> Uri` map from the bound ports
          * and starts every mesh + coil dialer. Acquiring bring-up last guarantees its
          * finalizers (server stop + dialer cancel) run before the MRMs stop their actors and
          * close RocksDB.
          */
        private def setupWebSocket(
            multiNodeConfig: MultiNodeConfig,
            peers: Seq[HeadPeerNumber],
            coilNodeConfigs: List[NodeConfig],
        ): Resource[IO, Setup] =
            given CardanoNetwork.Section = multiNodeConfig.headConfig
            for
                wsClient <- Resource.eval(JdkWSClient.simple[IO])
                headParts <- peers.toList
                                 .traverse(peerNum =>
                                     wsHeadParts(peerNum, multiNodeConfig, peers)
                                         .map(peerNum -> _)
                                 )
                                 .map(_.toMap)
                coilTransports <- coilNodeConfigs
                                      .traverse { coilConfig =>
                                          val coilNum = coilNumOf(coilConfig)
                                          val cpwtTracer = Slf4jTracer.sink.contramap(
                                            CoilPeerWsTransportEventFormat.humanFormat(coilNum)
                                          )
                                          Resource
                                              .eval(
                                                CoilPeerWsTransport.create(coilNum, cpwtTracer)
                                              )
                                              .map(coilNum -> _)
                                      }
                                      .map(_.toMap)
                headNetworks = headParts.view.mapValues(_.network).toMap
                coilUplinks  = coilTransports.view
                                   .mapValues(t => (_: ContextArg) => t: CoilTransport)
                                   .toMap
                bringUp = wsBringUpNetwork(
                              multiNodeConfig,
                              peers,
                              coilNodeConfigs,
                              wsClient,
                              headParts,
                              coilTransports,
                          )
            yield Setup(headNetworks, coilUplinks, bringUp)

        /** Per-peer parts produced in WS Phase 1: the transport bundle exposed to the MRM, the
          * concrete mesh transport needed by Phase 2's dialer starter, and the route builders
          * Phase 2 binds into a `NodeWsServer`.
          */
        private case class WsHeadParts(
            network: HeadNetwork,
            wsPeerTransport: WsPeerTransport,
            routes: List[WebSocketBuilder2[IO] => HttpRoutes[IO]],
            nwsTracer: ContraTracer[IO, NodeWsServerEvent],
        )

        /** WS Phase 2: bind each peer's `NodeWsServer`, derive its `Uri`, then start every mesh
          * and coil dialer. Returned as a `Resource` so the harness can acquire it *after* the
          * MRMs and ensure LIFO release: dialers + servers stop, then actors stop, then
          * RocksDB closes.
          */
        private def wsBringUpNetwork(
            multiNodeConfig: MultiNodeConfig,
            peers: Seq[HeadPeerNumber],
            coilNodeConfigs: List[NodeConfig],
            wsClient: WSClient[IO],
            headParts: Map[HeadPeerNumber, WsHeadParts],
            coilTransports: Map[CoilPeerNumber, CoilPeerWsTransport],
        ): Resource[IO, Unit] =
            val bindHost = host"127.0.0.1"
            for
                boundPorts <- peers.toList
                                  .traverse { peerNum =>
                                      val parts = headParts(peerNum)
                                      NodeWsServer
                                          .resource(
                                            bindHost,
                                            Port.fromInt(0).get,
                                            parts.routes,
                                            parts.nwsTracer,
                                          )
                                          .map(server => peerNum -> server.address.getPort)
                                  }
                                  .map(_.toMap)
                peerHeadUris = peers.map { p =>
                                   headPeerId(multiNodeConfig, p) -> Uri.unsafeFromString(
                                     s"ws://127.0.0.1:${boundPorts(p)}/head"
                                   )
                               }.toMap
                _ <- peers.toList.traverse_ { peerNum =>
                         val ownId = headPeerId(multiNodeConfig, peerNum)
                         headParts(peerNum).wsPeerTransport
                             .startDialers(wsClient, peerHeadUris - ownId)
                     }
                _ <- coilNodeConfigs.traverse_ { coilConfig =>
                         val coilNum = coilNumOf(coilConfig)
                         val hubNum = coilConfig
                             .coilPeerHub(coilNum)
                             .getOrElse(
                               throw new IllegalStateException(
                                 s"no hub configured for coil peer $coilNum"
                               )
                             )
                         val hubUri = Uri.unsafeFromString(
                           s"ws://127.0.0.1:${boundPorts(hubNum)}/hub"
                         )
                         coilTransports(coilNum).startDialer(wsClient, hubUri)
                     }
            yield ()

        private def directHeadNetwork(
            peerNum: HeadPeerNumber,
            multiNodeConfig: MultiNodeConfig,
            inProcessRegistry: InProcessPeerTransport.Registry,
            hubCoilRegistry: Option[InProcessHubCoilTransport.Registry],
        ): Resource[IO, HeadNetwork] =
            val ownHeadPeerId = headPeerId(multiNodeConfig, peerNum)
            val hubbedCoils   = multiNodeConfig.headConfig.hubbedCoilPeerNums(peerNum)
            for
                peerT <- Resource.eval(
                             InProcessPeerTransport.create(ownHeadPeerId, inProcessRegistry)
                         )
                hubT <-
                    if hubbedCoils.isEmpty then Resource.pure[IO, Option[HubTransport]](None)
                    else
                        hubCoilRegistry match
                            case None =>
                                Resource.eval(
                                  IO.raiseError(
                                    new IllegalStateException(
                                      s"head peer $peerNum hubs coil peers in Direct mode " +
                                          "but no hubCoilRegistry was allocated"
                                    )
                                  )
                                )
                            case Some(reg) =>
                                Resource
                                    .eval(InProcessHubCoilTransport.Hub.create(reg))
                                    .map(h => Some(h: HubTransport))
            yield HeadNetwork(peerT, hubT)

        /** WS Phase 1: allocate the concrete `WsPeerTransport` (+ optional `HubWsTransport`)
          * and the route builders that Phase 2 binds into a `NodeWsServer`. No bind, no dialer
          * start.
          */
        private def wsHeadParts(
            peerNum: HeadPeerNumber,
            multiNodeConfig: MultiNodeConfig,
            peers: Seq[HeadPeerNumber],
        )(using CardanoNetwork.Section): Resource[IO, WsHeadParts] =
            val ownHeadPeerId = headPeerId(multiNodeConfig, peerNum)
            val hubbedCoils   = multiNodeConfig.headConfig.hubbedCoilPeerNums(peerNum)
            val remoteIds: List[HeadPeerId] =
                peers.filterNot(_ == peerNum).map(headPeerId(multiNodeConfig, _)).toList
            val ptTracer =
                Slf4jTracer.sink.contramap(PeerTransportEventFormat.humanFormat(peerNum))
            val nwsTracer =
                Slf4jTracer.sink.contramap(NodeWsServerEventFormat.humanFormat(peerNum))
            val hubTracer =
                Slf4jTracer.sink.contramap(HubWsTransportEventFormat.humanFormat(peerNum))
            for
                peerT <- Resource.eval(
                             WsPeerTransport.create(ownHeadPeerId, remoteIds, ptTracer)
                         )
                hubTConcrete: Option[HubWsTransport] <-
                    if hubbedCoils.isEmpty then
                        Resource.pure[IO, Option[HubWsTransport]](None)
                    else
                        Resource
                            .eval(HubWsTransport.create(hubbedCoils, hubTracer))
                            .map(Some(_))
                meshRoute = (wsb: WebSocketBuilder2[IO]) => peerT.routes(wsb)
                hubRoutes = hubTConcrete.toList.map(h =>
                                (wsb: WebSocketBuilder2[IO]) => h.routes(wsb)
                            )
            yield WsHeadParts(
              network = HeadNetwork(peerT, hubTConcrete.map(h => h: HubTransport)),
              wsPeerTransport = peerT,
              routes = meshRoute :: hubRoutes,
              nwsTracer = nwsTracer,
            )

    // ===================================
    // Mrm — build per-peer HMRM and per-coil CMRM
    // ===================================

    object Mrm:
        case class Peer(
            mrm: HeadMultisigRegimeManager,
            backendStore: BackendStore[IO],
        )

        case class Coil(
            mrm: CoilMultisigRegimeManager,
            backendStore: BackendStore[IO],
            config: NodeConfig,
        )

        def buildPeer(
            peerNum: HeadPeerNumber,
            system: ActorSystem[IO],
            cardanoBackend: L1Backend[IO],
            multiNodeConfig: MultiNodeConfig,
            backendMode: StorageBackend.Mode,
            network: Transport.HeadNetwork,
            callerTracer: ContraTracer[IO, HeadRegimeManagerEvent],
        ): Resource[IO, Peer] =
            val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)
            val slf4jMrm: ContraTracer[IO, HeadRegimeManagerEvent] =
                Slf4jTracer.sink.contramap(
                  HeadMultisigRegimeManagerEventFormat.humanFormat(peerNum)
                )
            val mrmTracer         = slf4jMrm |+| callerTracer
            val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
            val peerFactory: Resource[IO, Transport.ContextFn[PeerTransport]] =
                Resource.pure((_: Transport.ContextArg) => network.peerTransport)
            val hubFactory: Option[Resource[IO, Transport.ContextFn[HubTransport]]] =
                network.hubTransport.map(h =>
                    Resource.pure((_: Transport.ContextArg) => h)
                )
            StorageBackend
                .openPerPeer(
                  peerNum,
                  backendMode,
                  Cf.mkAll(
                    headPeers = multiNodeConfig.headConfig.headPeerNums.toList,
                    coilPeers = multiNodeConfig.headConfig.coilPeers.coilPeerNumbers,
                    hubs = multiNodeConfig.headConfig.coilPeers.hubHeadPeerNumbers,
                  ),
                  persistenceTracer,
                )
                .flatMap { backendStore =>
                    for
                        persistence <- Resource.eval {
                                           given CardanoNetwork.Section = nodeConfig
                                           Persistence.fromBackend(backendStore, persistenceTracer)
                                       }
                        l2Ledger <- Resource.eval(EutxoL2Ledger(nodeConfig))
                        mrm <- HeadMultisigRegimeManager.resource(
                                   nodeConfig,
                                   cardanoBackend,
                                   l2Ledger,
                                   persistence,
                                   mrmTracer,
                                   peerFactory,
                                   hubFactory,
                               )
                        _ <- Resource.eval(system.actorOf(mrm, s"hmrm-$peerNum"))
                    yield Peer(mrm, backendStore)
                }

        def buildCoil(
            coilConfig: NodeConfig,
            coilNum: CoilPeerNumber,
            system: ActorSystem[IO],
            cardanoBackend: L1Backend[IO],
            multiNodeConfig: MultiNodeConfig,
            uplink: Transport.ContextFn[CoilTransport],
            callerTracer: ContraTracer[IO, CoilRegimeManagerEvent],
        ): Resource[IO, Coil] =
            val nHeadPeers = multiNodeConfig.nHeadPeers
            // Synthetic `HeadPeerNumber` label so coil log lines stay distinguishable from head
            // ones in the same run; passes through `CoilMultisigRegimeManagerEventFormat` which
            // still delegates to the head per-actor formatters (per the TODO inside that object).
            val labelNum = HeadPeerNumber(nHeadPeers + coilNum.convert)
            val slf4jMrm: ContraTracer[IO, CoilRegimeManagerEvent] =
                Slf4jTracer.sink.contramap(
                  CoilMultisigRegimeManagerEventFormat.humanFormat(labelNum, coilNum)
                )
            val mrmTracer         = slf4jMrm |+| callerTracer
            val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
            val uplinkFactory: Resource[IO, Transport.ContextFn[CoilTransport]] =
                Resource.pure(uplink)
            InMemoryBackendStore.open(persistenceTracer).flatMap { backendStore =>
                for
                    persistence <- Resource.eval {
                                       given CardanoNetwork.Section = coilConfig
                                       Persistence.fromBackend(backendStore, persistenceTracer)
                                   }
                    l2Ledger <- Resource.eval(EutxoL2Ledger(coilConfig))
                    mrm <- CoilMultisigRegimeManager.resource(
                               coilConfig,
                               cardanoBackend,
                               l2Ledger,
                               persistence,
                               mrmTracer,
                               uplinkFactory,
                           )
                    _ <- Resource.eval(system.actorOf(mrm, s"cmrm-${coilNum.convert}"))
                yield Coil(mrm, backendStore, coilConfig)
            }

    // ===================================
    // Http — per-peer in-memory HydrozoaRoutes + SubmissionClient
    // ===================================

    object Http:
        /** Placeholder authority for the in-process HTTP client. `Client.fromHttpApp` dispatches
          * by path against the wrapped `HttpApp`, so the authority never hits a socket.
          */
        private val InProcBaseUri: Uri = uri"http://harness"

        /** Build a peer's [[HydrozoaRoutes]] and wrap it in an in-memory http4s
          * [[org.http4s.client.Client]] via `Client.fromHttpApp`, then return a
          * [[SubmissionClient]] that signs each request with the peer's own wallet.
          */
        def mkPeerSubmissionClient(
            peerNum: HeadPeerNumber,
            conns: HeadMultisigRegimeManager.Connections,
            multiNodeConfig: MultiNodeConfig,
        ): IO[SubmissionClient] =
            val nodeConfig = multiNodeConfig.nodeConfigs(peerNum)
            val requestSequencer = conns.requestSequencer.getOrElse(
              throw new IllegalStateException(
                s"head peer $peerNum missing RequestSequencer; cannot build SubmissionClient"
              )
            )
            val serverConfig = HydrozoaServer.Config(
              adminUsername = "harness-admin",
              adminPassword = "harness-admin",
            )
            val httpTracer: ContraTracer[IO, HydrozoaHttpEvent] =
                Slf4jTracer.sink.contramap(HydrozoaHttpEventFormat.humanFormat)
            HydrozoaRoutes(
              requestSequencer,
              conns.blockWeaver,
              // The harness runs no head lifecycle, so readiness is a constant Active.
              IO.pure(NodeStatus.Active),
              // No EUTXO L2-query reader in the harness — the SubmissionClient uses the write path.
              None,
              nodeConfig.headConfig,
              serverConfig,
              httpTracer,
            ).map { hydrozoaRoutes =>
                val client: Http4sClient[IO] =
                    Http4sClient.fromHttpApp(hydrozoaRoutes.routes.orNotFound)
                SubmissionClient.http(client, InProcBaseUri, nodeConfig.ownWallet)
            }

    // ===================================
    // ErrorDrainer — captures uncaught actor exceptions into a Ref
    // ===================================

    object ErrorDrainer:
        /** Spawn a fiber that drains `system.eventStream` and appends every uncaught actor
          * exception to `sutErrors`. Cancelled on release.
          */
        def start(
            system: ActorSystem[IO],
            sutErrors: Ref[IO, List[String]],
        ): Resource[IO, Unit] =
            startedFiber(
              system.eventStream.take
                  .flatMap {
                      case e: ActorError if e.cause != ActorError.NoCause =>
                          sutErrors.update(_ :+ s"[${e.logSource}] ${e.cause.getMessage}")
                      case _ => IO.unit
                  }
                  .foreverM
            )

    // ===================================
    // Ticks — per-CardanoLiaison tick fibers
    // ===================================

    object Ticks:
        /** Per-peer fibers that periodically poke each `CardanoLiaison` with
          * `CardanoLiaison.Timeout`. Replaces the broken `setReceiveTimeout`-based polling
          * (cats-actors `setReceiveTimeout` checks via a hardcoded 1s ping AND uses
          * `System.currentTimeMillis()` rather than the F-effect clock — both unusable under
          * TestControl).
          */
        def startForHeads(
            connections: Map[HeadPeerNumber, HeadMultisigRegimeManager.Connections],
            pollingPeriodOf: HeadPeerNumber => FiniteDuration,
        ): Resource[IO, Unit] =
            connections.toList.traverse_ { case (peerNum, conns) =>
                startedFiber(tickLoop(pollingPeriodOf(peerNum), conns))
            }

        def startForCoils(
            connections: Map[CoilPeerNumber, HeadMultisigRegimeManager.Connections],
            pollingPeriodOf: CoilPeerNumber => FiniteDuration,
        ): Resource[IO, Unit] =
            connections.toList.traverse_ { case (coilNum, conns) =>
                startedFiber(tickLoop(pollingPeriodOf(coilNum), conns))
            }

        private def tickLoop(
            pollingPeriod: FiniteDuration,
            conns: HeadMultisigRegimeManager.Connections,
        ): IO[Nothing] =
            (IO.sleep(pollingPeriod) >> (conns.cardanoLiaison ! CardanoLiaison.Timeout)).foreverM

    // ===================================
    // Shared helper
    // ===================================

    /** Long-running fiber managed as a resource: started on acquire, cancelled on release. */
    private def startedFiber(action: IO[Nothing]): Resource[IO, Unit] =
        Resource.make(action.start)(_.cancel).void
