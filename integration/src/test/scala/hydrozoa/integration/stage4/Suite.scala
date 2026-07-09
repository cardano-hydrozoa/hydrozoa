package hydrozoa.integration.stage4

import cats.data.ReaderT
import cats.effect.{IO, Ref, Resource}
import cats.implicits.*
import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.config.head.initialization.{InitializationParametersGenTopDown, generateInitialBlock}
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.Durations.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import test.GenWithTestPeers
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{InitParamsType, generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.integration.harness.MultiPeerHeadHarness.StorageBackend.Mode as BackendMode
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.integration.harness.{
  MultiPeerHeadHarness,
  Plugin
}
import hydrozoa.integration.stage4.EffectsLanded.BlockExpectation
import hydrozoa.integration.stage4.Model.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, info, warn}
import hydrozoa.multisig.backend.cardano.yaciTestSauceGenesis
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId, PeerWallet}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.eutxol2.toUtxos
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects}
import hydrozoa.multisig.persistence.{BackendStore, Cf}
import org.scalacheck.commands.{AnyCommand, ModelBasedSuite, ScenarioGen}
import org.scalacheck.{Gen, Prop, PropertyM}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{TransactionInput, Utxos}
import test.{SeedPhrase, TestPeers, given}

// ===================================
// Stage 4 suite
// ===================================

case class Stage4Suite(
    label: String = "stage4",
    nPeers: Int = 2,
    nCoilPeers: Int = 0,
    nCommands: Int = 10,
    transportMode: TransportMode = TransportMode.Direct,
    backendMode: BackendMode = BackendMode.InMemory,
    absorptionSlack: FiniteDuration = Stage4Suite.ms("stage4.absorptionSlackMs", 60 * 1000),
    takeoffOffset: FiniteDuration = Stage4Suite.ms("stage4.takeoffOffsetMs", 60 * 1000),
) extends ModelBasedSuite:

    override type Env = Unit
    override type State = ModelState
    override type Sut = Stage4Sut

    private val log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("Stage4.Suite"))

    /** TestControl is incompatible with real sockets — virtual time doesn't drive the OS scheduler
      * that owns the WS connection. Direct mode keeps virtual time; WS mode runs on the real clock.
      */
    override def useTestControl: Boolean = transportMode match {
        case TransportMode.Direct    => true
        case TransportMode.WebSocket => false
    }

    override def scenarioGen: ScenarioGen[ModelState, Stage4Sut] = Stage4ScenarioGen

    override def commandGenTweaker: [A] => Gen[A] => Gen[A] = [A] =>
        (g: Gen[A]) => Gen.resize(nCommands, g)

    override def onTestCaseGenerated(
        initialState: ModelState,
        commands: List[AnyCommand[ModelState, Stage4Sut]]
    ): IO[Unit] =
        for
            _ <- super.onTestCaseGenerated(initialState, commands)
            table <- Stage4Runner.renderTable(initialState, commands)(using log)
            _ <- log.info(table)
        yield ()

    override def initEnv: PropertyM[IO, Unit] = PropertyM.run(IO.unit)

    override def genInitialState(env: Unit): PropertyM[IO, ModelState] =
        PropertyM.pick(
          Stage4Suite.genInitialState(
            nPeers = nPeers,
            nCoilPeers = nCoilPeers,
            useTestControl = useTestControl,
            absorptionSlack = absorptionSlack,
            takeoffOffset = takeoffOffset,
          )
        )

    override def canStartupNewSut(): Boolean = true

    // Stage4 owns its capture/signal plugins + handle extraction; the harness
    // (MultiPeerHeadHarness) owns the ActorSystem, mock CardanoBackend, transports, per-peer
    // HMRM with BackendStore + Persistence, per-coil CMRM, connections drain, error drainer,
    // and CL tick fibers. Hooks thread the stage4-side root tracer (`Plugin.tracerOf(...)`)
    // into each MRM.
    override def sutResource(state: ModelState): Resource[IO, Stage4Sut] =
        val multiNodeConfig = state.params.multiNodeConfig
        val peers           = multiNodeConfig.nodeConfigs.keys.toSeq.sortBy(p => p: Int)
        val coilConfigs     = state.params.coilNodeConfigs
        val startEpochMs    = state.currentModelTime.getEpochSecond * 1000L
        val coilNums        = coilConfigs.map(MultiPeerHeadHarness.Transport.coilNumOf)

        for
            // Captures (writer arms over Refs)
            perPeer   <- Resource.eval(Stage4Plugins.perPeerCaptures(peers))
            perCoil   <- Resource.eval(Stage4Plugins.perCoilCaptures(coilNums))
            landedTxs <- Resource.eval(Stage4Plugins.effectsLandedCapture)

            // Target Deferreds — armed in beforeFinalize to gate the signal predicates below.
            fastSettlementTarget <- Resource.eval(IO.deferred[Set[RequestId]])
            slowCoverageTarget   <- Resource.eval(IO.deferred[Set[Int]])
            effectsLandedTarget  <- Resource.eval(IO.deferred[List[BlockExpectation]])

            // Signals (predicate arms over Deferred[T])
            fastSettlementSignal  <- Resource.eval(
                                       Stage4Plugins
                                           .fastSettlementSignal(perPeer, fastSettlementTarget)
                                     )
            slowCoverageSignal    <- Resource.eval(
                                       Stage4Plugins
                                           .slowCoverageSignal(perPeer, slowCoverageTarget)
                                     )
            effectsLandedSignal   <- Resource.eval(
                                       Stage4Plugins
                                           .effectsLandedSignal(landedTxs, effectsLandedTarget)
                                     )
            fallbackEnteredSignal <- Resource.eval(Stage4Plugins.fallbackEnteredSignal)

            // SUT-command-fed Ref (not a plugin — written by commands, not tracer arms)
            submittedRequestIds <- Resource.eval(Ref[IO].of(Vector.empty[RequestId]))

            hooks = MultiPeerHeadHarness.Hooks[Option[Stage4PeerHandle]](
                      tracer = Plugin.tracerOf(
                        perPeer,
                        perCoil,
                        landedTxs,
                        fastSettlementSignal,
                        slowCoverageSignal,
                        effectsLandedSignal,
                        fallbackEnteredSignal,
                      ),
                      handle = {
                          case (PeerId.Head(peerNum), conns) =>
                              IO.pure(
                                Some(
                                  Stage4PeerHandle(
                                    conns.requestSequencer.getOrElse(
                                      sys.error(s"head peer $peerNum missing RequestSequencer")
                                    )
                                  )
                                )
                              )
                          case (_: PeerId.Coil, _) => IO.pure(None)
                      },
                    )
            harness <- MultiPeerHeadHarness.resource(
                           MultiPeerHeadHarness.Inputs(
                             config = MultiPeerHeadHarness
                                 .Config(label, backendMode, transportMode),
                             multiNodeConfig = multiNodeConfig,
                             coilNodeConfigs = coilConfigs,
                             preinitPeerUtxosL1 = state.preinitPeerUtxosL1,
                             takeoffTime = state.takeoffTime,
                             startEpochMs = startEpochMs,
                           ),
                           hooks,
                       )
        yield Stage4Sut(
          static = Stage4SutStatic(
            system = harness.system,
            cardanoBackend = harness.cardanoBackend,
            // Head peers always yield `Some` (see the `Hooks.handle` above); crash loudly if the
            // invariant is violated so downstream doesn't silently drop peers.
            peers = harness.peers.map { case (n, p) =>
                n -> p.handle.getOrElse(
                  sys.error(s"head peer $n missing Stage4PeerHandle after harness bring-up")
                )
            },
            backendStores = harness.peers.map { case (n, p) => n -> p.backendStore },
            log = Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("Stage4.Sut")),
          ),
          mutable = Stage4SutMutable(
            sutErrors = harness.sutErrors,
            perPeer = perPeer,
            perCoil = perCoil,
            submittedRequestIds = submittedRequestIds,
            fastSettlementSignal = fastSettlementSignal,
            slowCoverageSignal = slowCoverageSignal,
            fastSettlementTarget = fastSettlementTarget,
            slowCoverageTarget = slowCoverageTarget,
            landedTxs = landedTxs,
            effectsLandedSignal = effectsLandedSignal,
            effectsLandedTarget = effectsLandedTarget,
            fallbackEnteredSignal = fallbackEnteredSignal,
          ),
        )


    override def beforeFinalize(lastState: ModelState, sut: Stage4Sut): IO[Prop] = {
        // Race the happy-path drain against the fallback-entered signal. If any peer's CL
        // successfully submits a `FallbackToRuleBased`, abandon the analysis and fail with
        // a clear message — the properties below (liveness / coverage / effects-landed) don't
        // model rule-based; waiting on them after fallback would spin the CL polling loop
        // until the outer test timeout, accumulating the InitWindowElapsed warn flood.
        val happyPathProp: IO[Prop] = for
            _ <- log.warn("beforeFinalize")
            submitted <- sut.mutable.submittedRequestIds.get
            // Arm the fast-cycle drain: publish the final submitted set so the JL predicate arm
            // knows the target. The signal fires only after this is set, preventing mid-run
            // firing against a partial submittedRequestIds snapshot.
            _ <- sut.mutable.fastSettlementTarget.complete(submitted.toSet)
            // One-time coverage check: if all IDs already landed before we armed the target,
            // fire the signal ourselves (no new brief will arrive to trigger the predicate).
            allBriefs <- sut.mutable.perPeer.state.values.toList
                             .traverse(_.blockBriefs.get)
                             .map(_.flatten)
            seen       = allBriefs
                             .flatMap(br =>
                                 br.events.map(_._1) ++ br.depositsAbsorbed ++ br.depositsRefunded
                             )
                             .toSet
            _ <- IO.whenA(submitted.forall(seen.contains))(
                     sut.mutable.fastSettlementSignal.complete(())
                 )
            _ <- IO.whenA(submitted.nonEmpty)(sut.mutable.fastSettlementSignal.await)
            // Arm the slow-cycle drain: freeze the block nums that must be covered. Done after
            // the fast drain so any blocks produced during that wait are included in the target.
            blockNums <- sut.mutable.perPeer.state.values.toList
                             .traverse(_.blockBriefs.get)
                             .map(_.flatten.map(b => (b.blockNum: Int)).toSet)
            _ <- sut.mutable.slowCoverageTarget.complete(blockNums)
            // One-time coverage check across ALL peers — matching the predicate condition so a
            // spurious signal fire can't race ahead of any peer's stacks update.
            allPeersStacks <- sut.mutable.perPeer.state.values.toList.traverse(_.stacks.get)
            allCovered      = blockNums.isEmpty ||
                                  allPeersStacks.forall { peerStacks =>
                                      blockNums.forall { bn =>
                                          peerStacks.exists { s =>
                                              (s.brief.firstBlockNum: Int) <= bn &&
                                              bn <= (s.brief.lastBlockNum: Int)
                                          }
                                      }
                                  }
            _ <- IO.whenA(allCovered)(sut.mutable.slowCoverageSignal.complete(()))
            _ <- IO.whenA(blockNums.nonEmpty)(sut.mutable.slowCoverageSignal.await)
            // Arm the effects-landed drain: now that the slow cycle has reached agreement on
            // every block, derive the backbone expectations from the canonical peer's stacks and
            // wait for the TxSubmitting predicate to observe enough hashes to satisfy them. Gap
            // between slow signal and this one = the StackComposer rate-limit delay.
            canonicalStacksForTarget <- sut.mutable.perPeer.state.toList
                                            .traverse { case (p, c) => c.stacks.get.map(p -> _) }
                                            .map { byPeer =>
                                                val sorted = byPeer.toMap.toSeq.sortBy(_._1: Int)
                                                sorted.headOption.map(_._2).getOrElse(Vector.empty)
                                            }
            expectations = EffectsLanded.expectations(canonicalStacksForTarget)
            _ <- sut.mutable.effectsLandedTarget.complete(expectations)
            // One-time check: if every relevant expectation is already satisfied by the hashes
            // we've observed so far, fire the signal ourselves (no new TxSubmitting will arrive).
            landedNow <- sut.mutable.landedTxs.state.get
            _ <- IO.whenA(EffectsLanded.isComplete(landedNow, expectations))(
                     sut.mutable.effectsLandedSignal.complete(())
                 )
            _ <- IO.whenA(expectations.nonEmpty)(sut.mutable.effectsLandedSignal.await)
            errors <- sut.mutable.sutErrors.get
            // Snapshot every capture-written Ref ONCE here, after all drain signals have fired,
            // and reuse this single frozen view for the whole analysis. Re-reading these Refs
            // separately (here and again inside analyzeBlockBriefs) races the capture arms under
            // WS (real clock, no TestControl tick-drain): cross-peer quantities could then be
            // derived from inconsistent snapshots. Freezing once removes that window by
            // construction.
            briefsByPeer <- sut.mutable.perPeer.state.toList
                                .traverse { case (p, c) => c.blockBriefs.get.map(p -> _) }
                                .map(_.toMap)
            stacksByPeer <- sut.mutable.perPeer.state.toList
                                .traverse { case (p, c) => c.stacks.get.map(p -> _) }
                                .map(_.toMap)
            coilStacksByCoil <- sut.mutable.perCoil.state.toList
                                    .traverse { case (c, cap) => cap.stacks.get.map(c -> _) }
                                    .map(_.toMap)
            submittedIds <- sut.mutable.submittedRequestIds.get
            sortedPeers = stacksByPeer.keys.toSeq.sortBy(p => p: Int)
            // propEffectsLanded must check exactly what effectsLandedSignal confirmed landed, so it
            // uses the stacks frozen when effectsLandedTarget was armed (above), not the post-signal
            // snapshot — which could include a trailing stack whose txs have not landed yet.
            analysisProp <- analyzeBlockBriefs(
                              lastState,
                              sut,
                              briefsByPeer,
                              stacksByPeer,
                              coilStacksByCoil,
                              submittedIds,
                              canonicalStacksForTarget,
                            )
            persistenceProp <- analyzePersistence(sut, stacksByPeer, sortedPeers)
            props = analysisProp && persistenceProp
        yield
            if errors.nonEmpty then
                Prop.exception(RuntimeException(s"SUT actor errors:\n${errors.mkString("\n")}"))
            else props

        IO.race(sut.mutable.fallbackEnteredSignal.await, happyPathProp).map {
            case Left(txId) =>
                Prop.exception(
                  RuntimeException(
                    s"scenario entered rule-based fallback (txId=$txId) — outside the modeled" +
                        " happy-path regime; widen stage4 timings or cap the inter-arrival tail"
                  )
                )
            case Right(prop) => prop
        }
    }

    private def analyzeBlockBriefs(
        lastState: ModelState,
        sut: Stage4Sut,
        briefsByPeer: Map[HeadPeerNumber, Vector[BlockBrief.Intermediate]],
        stacksByPeer: Map[HeadPeerNumber, Vector[Stack.HardConfirmed]],
        coilStacksByCoil: Map[CoilPeerNumber, Vector[Stack.HardConfirmed]],
        submittedIds: Vector[RequestId],
        canonicalStacksForEffects: Vector[Stack.HardConfirmed],
    ): IO[Prop] = {
        val sortedPeers = briefsByPeer.keys.toSeq.sortBy(p => p: Int)
        val nPeers = sortedPeers.length
        val canonicalBriefs = briefsByPeer(sortedPeers.head)
        val canonicalStacks = stacksByPeer(sortedPeers.head)
        for
            _ <- log.info(
              "hard-confirmed stacks per peer: " +
                  sortedPeers
                      .map(p => s"peer${p: Int}=${stacksByPeer.getOrElse(p, Vector.empty).size}")
                      .mkString(", ")
            )

            _ <- IO.whenA(coilStacksByCoil.nonEmpty)(
              log.info(
                "coil hard-confirmed stacks: " +
                    coilStacksByCoil.toList
                        .sortBy((c, _) => c.convert)
                        .map((c, ss) => s"coil${c.convert}=${ss.size}")
                        .mkString(", ")
              )
            )

            _ <- PrettyPrinters.traceBlockTable(
              canonicalBriefs,
              sortedPeers,
              briefsByPeer,
              nPeers,
              submittedIds,
              lastState,
            )

            _ <- PrettyPrinters.traceStackTable(canonicalStacks, sortedPeers, stacksByPeer, nPeers)

            // Checks the stacks frozen when the effects-landed target was armed — consistent with
            // the signal. Under virtual time (Direct) the backend resolves instantly, so one
            // attempt suffices. Under WS (real clock) `effectsLandedSignal` confirms only that the
            // TxSubmitting sink *observed* each hash; the backend's `isTxKnown` can still lag that
            // observation by a submission round-trip, so poll to absorb the gap.
            effectsLandedProp <- EffectsLanded.propEffectsLanded(
              canonicalStacksForEffects,
              sut.static.cardanoBackend,
              log,
              attempts = if useTestControl then 1 else 20,
              sleep = if useTestControl then 0.seconds else 1.second,
            )

            targetBlockNums <- sut.mutable.slowCoverageTarget.get

        yield propLiveness(submittedIds, canonicalBriefs) &&
            propDepositTiming(lastState.registeredDeposits, canonicalBriefs) &&
            propValidRatio(lastState, canonicalBriefs) &&
            propStackCoverage(targetBlockNums, canonicalStacks) &&
            propCoilParticipation(coilStacksByCoil, canonicalStacks) &&
            effectsLandedProp
    }

    /** Post-scenario verification of the §6 producer-side persistence writes:
      *   - **SC** (`StackComposer`) writes `Treasury` once and one `EvacuationMap` per
      *     **committed** block (each major + each last-of-partition SEC minor) at every own
      *     hard-ack stack-close (#21).
      *   - **SCA** (`SlowConsensusActor`) writes `HardConfirmation` at every hard-confirmation
      *     (#22).
      *   - **JL** (`JointLedger`) writes `BlockResult` + the `DepositMap` snapshot + its own
      *     `Block` (leader) / `SoftAck` lane entries at each own soft ack; **FCA**
      *     (`FastConsensusActor`) writes `SoftConfirmation` at each soft-confirmation; **SC** also
      *     writes its own `Stack` (leader) / `HardAck` lane entries at stack-close.
      *   - **RequestSequencer** writes the assigned request to the `Request` lane (CR1);
      *     **PeerLiaison** writes each inbound *remote* lane entry it receives (CR8).
      *
      * For each peer that observed at least one `Stack.HardConfirmed` during the scenario, this
      * property asserts:
      *   1. `Cf.HardConfirmation` holds an entry per hard-confirmed stack (= the captured stacks
      *      count, since both the observer and the persistence write happen on hard-confirmation).
      *   2. `Cf.Treasury` holds its single snapshot blob (always exactly 1 — overwritten per
      *      close).
      *   3. `Cf.EvacuationMap` holds one entry per committed block (major / SEC minor) of every
      *      Regular stack the peer hard-confirmed — the only maps that back an on-chain commitment,
      *      counted from each stack's partitions (mirroring `StackComposer.committedBlockNums`).
      *      `Initial` (stack 0) contributes nothing since `bootstrapInitialStack` doesn't go
      *      through the close paths.
      *   4. The fast side wrote: `Cf.BlockResult` / `Cf.SoftConfirmation` non-empty and
      *      `Cf.DepositMap` a singleton — a peer that hard-confirmed necessarily produced and
      *      soft-confirmed blocks first (sanity lower bounds, not exact counts).
      *   5. The satellite lanes are non-empty: `Cf.SoftAck` (every block) and `Cf.HardAck` (every
      *      confirmed stack), and `Cf.Request` (own assignments + inbound). The `Block` / `Stack`
      *      spine lanes get both own (leader) and inbound (follower) writes but are per-peer
      *      variable, so they are logged but not asserted.
      *
      * Skip a peer entirely if it never reached a hard-confirmation (the typical `nPeers < 3`
      * cold-start scenarios). The property only fires once at least one hard-confirmation actually
      * happened, which is exactly where the writes should have landed.
      */
    private def analyzePersistence(
        sut: Stage4Sut,
        stacksByPeer: Map[HeadPeerNumber, Vector[Stack.HardConfirmed]],
        sortedPeers: Seq[HeadPeerNumber]
    ): IO[Prop] = {
        sortedPeers
            .traverse { peerNum =>
                val backend = sut.static.backendStores(peerNum)
                val captured = stacksByPeer.getOrElse(peerNum, Vector.empty)
                val expectedStacks = captured.size
                // Only the blocks that back an on-chain KZG commitment get an `EvacuationMap`
                // entry — each major (the settlement's `nextKzg`) and each last-of-partition SEC
                // minor — mirroring `StackComposer.committedBlockNums` /
                // `StackEffectsBuilder.mkEffectsRegular`. So per Regular stack the count is, over
                // its partitions: Major → 1 (the major) + 1 iff it carries a trailing-minor SEC;
                // Minor → 1 (its mandatory SEC minor); Final → 0 (drains the map, commits nothing).
                // `Initial` (stack 0) goes through `bootstrapInitialStack`, never the close paths,
                // so it contributes nothing.
                val expectedEvac = captured.map { s =>
                    s.effects match {
                        case _: StackEffects.HardConfirmed.Initial => 0
                        case r: StackEffects.HardConfirmed.Regular =>
                            r.partitions.toList.map {
                                case m: PartitionEffects.Major[?] =>
                                    if m.sec.isDefined then 2 else 1
                                case _: PartitionEffects.Minor[?] => 1
                                case _: PartitionEffects.Final    => 0
                            }.sum
                    }
                }.sum
                for {
                    hardConfirmations <- countEntries(backend, Cf.HardConfirmation)
                    treasuries <- countEntries(backend, Cf.Treasury)
                    evacuationMaps <- countEntries(backend, Cf.EvacuationMap)
                    blockResults <- countEntries(backend, Cf.BlockResult)
                    softConfirmations <- countEntries(backend, Cf.SoftConfirmation)
                    depositMaps <- countEntries(backend, Cf.DepositMap)
                    // Satellites are split one CF per author (§7.1); count across every head peer's
                    // own-author CF (each peer holds all peers' entries — own + inbound).
                    softAcks <- countAcross(backend, sortedPeers.toList.map(Cf.SoftAck(_)))
                    hardAcks <- countAcross(
                      backend,
                      sortedPeers.toList.map(p => Cf.HardAck(PeerId.Head(p)))
                    )
                    requests <- countAcross(backend, sortedPeers.toList.map(Cf.Request(_)))
                    _ <- log.info(
                      s"peer${peerNum: Int} persistence: expectedHardConf=$expectedStacks " +
                          s"hardConfirmations=$hardConfirmations treasuries=$treasuries " +
                          s"evacuationMaps=$evacuationMaps (expected=$expectedEvac) " +
                          s"blockResults=$blockResults softConfirmations=$softConfirmations " +
                          s"depositMaps=$depositMaps softAcks=$softAcks hardAcks=$hardAcks " +
                          s"requests=$requests"
                    )
                } yield {
                    if expectedStacks == 0 then Prop.passed
                    else {
                        val hardOk = hardConfirmations == expectedStacks
                        val treasuryOk = treasuries == 1
                        val evacOk = evacuationMaps == expectedEvac
                        // Fast-side producer writes: a peer that hard-confirmed has necessarily
                        // produced blocks (JL's `BlockResult`) and soft-confirmed them (FCA's
                        // `SoftConfirmation`), and the deposits snapshot is a singleton.
                        val fastOk =
                            blockResults >= 1 && softConfirmations >= 1 && depositMaps == 1
                        // Lane writes (own + inbound): every peer soft-acks every block (JL) and
                        // hard-acks each stack it confirmed (SC); requests flow into the Request
                        // lane (RequestSequencer own + PeerLiaison inbound, CR1/CR8). All non-empty.
                        val laneOk = softAcks >= 1 && hardAcks >= 1 && requests >= 1
                        Prop(hardOk && treasuryOk && evacOk && fastOk && laneOk).label(
                          s"peer${peerNum: Int}: " +
                              s"hardConfirmations=$hardConfirmations expected=$expectedStacks, " +
                              s"treasuries=$treasuries (expected 1), " +
                              s"evacuationMaps=$evacuationMaps (expected $expectedEvac), " +
                              s"blockResults=$blockResults softConfirmations=$softConfirmations " +
                              s"depositMaps=$depositMaps softAcks=$softAcks hardAcks=$hardAcks " +
                              s"requests=$requests (fast >=1/>=1/==1, lanes >=1/>=1/>=1)"
                        )
                    }
                }
            }
            .map(_.foldLeft(Prop.passed)(_ && _))
    }

    private def countEntries(
        backend: BackendStore[IO],
        cf: Cf
    ): IO[Int] =
        backend.cursor(cf, Array.emptyByteArray).use { c =>
            def loop(n: Int): IO[Int] =
                c.next.flatMap {
                    case None    => IO.pure(n)
                    case Some(_) => loop(n + 1)
                }
            loop(0)
        }

    /** Sum entry counts across a set of (per-author) CFs — e.g. every head peer's `SoftAck` CF, now
      * that satellites are split one CF per author (§7.1).
      */
    private def countAcross(backend: BackendStore[IO], cfs: List[Cf]): IO[Int] =
        cfs.traverse(countEntries(backend, _)).map(_.sum)

    /** With `coilQuorum` ≥ 1, no stack hard-confirms without the coil peers' acks, so
      * [[propStackCoverage]] already proves coil participation on the head side. This additionally
      * checks the relay back: each coil peer follower itself hard-confirms stacks (it received the
      * head acks + its own echo), and never hard-confirms a stack the canonical head didn't. No-op
      * for a pure-head run.
      */
    private def propCoilParticipation(
        coilStacksByCoil: Map[CoilPeerNumber, Vector[Stack.HardConfirmed]],
        canonicalStacks: Vector[Stack.HardConfirmed]
    ): Prop =
        if coilStacksByCoil.isEmpty then Prop.proved
        else
            val headStackNums = canonicalStacks.map(_.brief.stackNum).toSet
            coilStacksByCoil.toList
                .map { (coilNum, coilStacks) =>
                    val coilStackNums = coilStacks.map(_.brief.stackNum).toSet
                    (Prop(coilStacks.nonEmpty) :|
                        s"coil ${coilNum.convert} hard-confirmed no stacks") &&
                    (Prop(coilStackNums.subsetOf(headStackNums)) :|
                        s"coil ${coilNum.convert} hard-confirmed stacks absent from the head: " +
                        s"${(coilStackNums -- headStackNums).map(_.convert)}")
                }
                .foldLeft(Prop.proved)(_ && _)

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
            "(SUT is more permissive than the model)"
    }

    /** Property: the slow cycle hard-confirmed at least one stack, and every block in the frozen
      * `slowCoverageTarget` lies in some hard-confirmed stack on the canonical peer. Using the
      * frozen target — the contract the shutdown waited on — rather than a fresh brief re-read
      * avoids drift if the leader produces another block between the signal and the snapshot.
      */
    private def propStackCoverage(
        targetBlockNums: Set[Int],
        canonicalStacks: Vector[Stack.HardConfirmed]
    ): Prop = {
        // A stack covers the inclusive block range [firstBlockNum, lastBlockNum] from its
        // brief. The Initial stack carries a synthetic zero-range brief ([0..0]), so it
        // contributes no real block coverage.
        val coveredRanges: Vector[(Int, Int)] =
            canonicalStacks.map { s =>
                val b = s.brief
                ((b.firstBlockNum: Int), (b.lastBlockNum: Int))
            }
        def covered(n: Int): Boolean =
            coveredRanges.exists { case (lo, hi) => lo <= n && n <= hi }
        val uncovered = targetBlockNums.filterNot(covered)
        (Prop(canonicalStacks.nonEmpty) :|
            "stack coverage: slow cycle hard-confirmed no stacks") &&
        (Prop(uncovered.isEmpty) :|
            s"stack coverage: ${uncovered.size} observed block(s) never in any " +
            s"hard-confirmed stack: ${uncovered.toSeq.sorted.mkString(", ")}")
    }

    /** Trace-side ASCII tables used by [[analyzeBlockBriefs]] to render the captured fast-cycle
      * brief stream and slow-cycle stack stream. Pure formatting on frozen snapshots; closes over
      * the suite's `log` tracer so emit lines for free.
      */
    private object PrettyPrinters:
      def traceBlockTable(
        canonicalBriefs: Vector[BlockBrief.Intermediate],
        sortedPeers: Seq[HeadPeerNumber],
        briefsByPeer: Map[HeadPeerNumber, Vector[BlockBrief.Intermediate]],
        nPeers: Int,
        submittedIds: Vector[RequestId],
        lastState: ModelState,
    ): IO[Unit] = {
        val colWidth = 72
        val divider = s"+${"-" * (colWidth + 2)}+"
        val header = s"| ${"Block".padTo(colWidth, ' ')} |"

        val rows = canonicalBriefs.map { brief =>
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
            s"| ${label.take(colWidth).padTo(colWidth, ' ')} |"
        }

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

        val peersLine =
            s"Peers: ${sortedPeers.map(p => s"p${p: Int}=${briefsByPeer(p).length}blks").mkString("  ")}"
        val prefixLine =
            s"Common prefix: $commonPrefixLen / ${submittedIds.length} (submission order vs SUT block order)"
        val ratioLine =
            s"Valid/total (L2 txs) — model: $modelValid/$modelTotal  SUT: $sutValid/$sutTotal"
        val legend =
            "Legend: Min=Minor Maj=Major v=version lead=leader p=peer r=requestNum V=valid I=invalid abs=deposit-absorbed ref=refunded"

        val text = (divider :: header :: divider :: rows.toList ++
            (divider :: peersLine :: prefixLine :: ratioLine :: legend :: Nil))
            .mkString("\n", "\n", "")
        log.info(text)
    }

      /** Mirror of [[traceBlockTable]] for the slow cycle: one row per hard-confirmed stack on
        * the canonical peer, showing the stack number, covered block range, partition spine
        * (Min/Maj/Fin kinds, in stack order), and the round-2 unlock selection (settlement-at-i,
        * finalization-at-i, or sole-acknowledgment / no unlock). Stack-0 renders as `Init`.
        * Followed by a per-peer stack-count line for cross-peer convergence at a glance.
        */
      def traceStackTable(
        canonicalStacks: Vector[Stack.HardConfirmed],
        sortedPeers: Seq[HeadPeerNumber],
        stacksByPeer: Map[HeadPeerNumber, Vector[Stack.HardConfirmed]],
        nPeers: Int,
    ): IO[Unit] = {
        val colWidth = 72
        val divider = s"+${"-" * (colWidth + 2)}+"
        val header = s"| ${"Stack".padTo(colWidth, ' ')} |"

        val rows = canonicalStacks.map { stack =>
            val brief = stack.brief
            val sNum = brief.stackNum: Int
            val first = brief.firstBlockNum: Int
            val last = brief.lastBlockNum: Int
            val nBlks = last - first + 1
            // Slow-consensus leadership schedule is round-robin by stack number, mirroring
            // fast-consensus block-number round-robin.
            val leader = sNum % nPeers
            val label = stack.effects match {
                case _: StackEffects.HardConfirmed.Initial =>
                    s"#$sNum Init lead=p$leader | init+fallback"
                case r: StackEffects.HardConfirmed.Regular =>
                    val blkLabel = if nBlks == 1 then "blk" else "blks"
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
                    s"#$sNum [$first..$last] ($nBlks $blkLabel) Reg lead=p$leader | " +
                        s"[${parts.mkString(",")}] u=$unlockStr"
            }
            s"| ${label.take(colWidth).padTo(colWidth, ' ')} |"
        }

        val peersLine =
            s"Peers: ${sortedPeers.map(p => s"p${p: Int}=${stacksByPeer(p).length}stk").mkString("  ")}"
        val legend =
            "Legend: Init=initial stack Reg=regular Min/Maj/Fin=partition kinds " +
                "u=unlock (set=settlement, fin=finalization, sole=no unlock) @i=partition index"

        val text = (divider :: header :: divider :: rows.toList ++
            (divider :: peersLine :: legend :: Nil))
            .mkString("\n", "\n", "")
        log.info(text)
    }

// ===================================
// Initial state generator (canonical location; Runner delegates here for @main)
// ===================================

object Stage4Suite:

    // Tuning overrides read from -D system properties (see Stage4WsTune in Runner.scala).
    // Only affects WS runs; TestControl runs use virtual time so wall-clock is insensitive.
    def ms(key: String, default: Long): FiniteDuration =
        FiniteDuration(Option(System.getProperty(key)).map(_.toLong).getOrElse(default), "milliseconds")

    def genInitialState(
        nPeers: Int = 2,
        nCoilPeers: Int = 0,
        absorptionSlack: FiniteDuration = ms("stage4.absorptionSlackMs", 60 * 1000),
        takeoffOffset: FiniteDuration = ms("stage4.takeoffOffsetMs", 60 * 1000),
        meanInterArrivalTime: HeadPeerNumber => FiniteDuration =
            _ => ms("stage4.meanInterArrivalMs", 12 * 1000),
        useTestControl: Boolean = true,
    ): Gen[ModelState] =
        val cardanoNetwork = CardanoNetwork.Preprod
        // TestPeers provisions head + coil wallets from the same seed under stable ordinals.
        // Coil peers are hubbed by head 0 (single-hub topology); their vkeys land in the head
        // bootstrap so the threshold script requires `coilQuorum` of them, and
        // `mkCoilNodeConfigs` (below) derives each coil's own node config from the MNC.
        val testPeers = TestPeers(SeedPhrase.Yaci, cardanoNetwork, nPeers, nCoilPeers)
        val testPeerToUtxos = yaciTestSauceGenesis(cardanoNetwork.network)(testPeers)
        val coilWallets: List[PeerWallet] = testPeers.coilWallets
        val coilPeers: CoilPeers = testPeers.coilPeersConfig(hub = HeadPeerNumber(0))

        // Non-TestControl runs anchor the initial block's end-time to a wall-clock offset in
        // the future so `sutResource` can sleep until that anchor and have the model clock
        // and the wall clock coincide at command 1. 60s matches stage 1's budget; if 20-peer
        // setup overruns it the test aborts (see sutResource). Under TestControl the head-config
        // generator falls back to the deterministic Jan-1-2026 + 100-day random distribution
        // — reading the wall clock there would defeat seed-based reproducibility.
        //
        // TODO: `genInitialState` returns a pure `Gen[ModelState]` so we can't thread
        // [[MultiPeerHeadHarness.mkTakeoffTime]] (which returns `IO[Option[Instant]]`) here
        // without lifting the whole model construction into an IO/PropertyM. Callers under
        // TestControl are unaffected because the wall-clock branch is skipped anyway.
        val takeoffTime: Option[java.time.Instant] =
            if useTestControl then None
            else Some(java.time.Instant.now().plusMillis(takeoffOffset.toMillis))

        val generateHeadStartTime = MultiPeerHeadHarness.generateHeadStartTime(takeoffTime)

        // Tuned TxTiming: yaci defaults, but each field can be overridden via -D for bisecting.
        val generateTunedTxTiming: GenWithTestPeers[TxTiming] = ReaderT { network =>
            val sc = network.slotConfig
            Gen.const(
              TxTiming(
                MinSettlementDuration(ms("stage4.minSettlementMs", 10 * 60 * 1000).quantize(sc)),
                InactivityMarginDuration(ms("stage4.inactivityMarginMs", 60 * 1000).quantize(sc)),
                SilenceDuration(ms("stage4.silenceMs", 10 * 60 * 1000).quantize(sc)),
                DepositSubmissionDuration(ms("stage4.depositSubmissionMs", 1000).quantize(sc)),
                DepositMaturityDuration(ms("stage4.depositMaturityMs", 1000).quantize(sc)),
                DepositAbsorptionDuration(ms("stage4.depositAbsorptionMs", 2 * 60 * 1000).quantize(sc)),
              )
            )
        }

        val generateHeadConfigBootstrap_ = generateHeadConfigBootstrap(
          generateHeadParams = generateHeadParameters(generateTxTiming = generateTunedTxTiming)
              .map(_.copy(coilQuorum = nCoilPeers)),
          generateInitializationParameters = InitParamsType.TopDown(
            InitializationParametersGenTopDown.GenWithDeps(
              generateGenesisUtxosL1 = ReaderT((_: TestPeers) =>
                  Gen.const(testPeerToUtxos.map((k, v) => k.headPeerNumber -> v))
              )
            )
          ),
          coilPeers = coilPeers
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
                    maxPollingPeriod = hc.maxCardanoLiaisonPollingPeriod / 2,
                    // Narrow both periods to fit the WS test wall-clock budget (see
                    // docs/rate-limiter.md). The 20s production `softBlockMinPeriod` paces every
                    // soft-confirmed block on the real clock under WS, stretching each scenario to
                    // tens of minutes and delaying settlement submission past the effects-landed
                    // poll budget; 5s keeps block production (and the model command-batching that
                    // tracks it) brisk while preserving the throttle's cross-block batching intent.
                    // `hardStackMinPeriod`'s 3-minute production value would likewise starve
                    // CardanoLiaison of PushResults within the budget; 2s preserves batching too.
                    rateLimits = hydrozoa.config.node.operation.multisig.RateLimits(
                      softBlockMinPeriod = ms("stage4.softBlockMinPeriodMs", 5000),
                      hardStackMinPeriod = ms("stage4.hardStackMinPeriodMs", 2000)
                    )
                  )
            )

            preinitPeerUtxosL1 = testPeerToUtxos.map((k, v) => k.headPeerNumber -> v)
            coilNodeConfigs = config.mkCoilNodeConfigs(coilWallets)

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

            startTime = config.headConfig.initialBlock.blockBrief.endTime.convert
        yield ModelState(
          params = Params(
            config,
            absorptionSlack,
            peers.map(pn => pn -> meanInterArrivalTime(pn)).toMap,
            coilNodeConfigs = coilNodeConfigs
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
