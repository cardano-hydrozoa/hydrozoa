package hydrozoa.integration.rbr.property

import cats.effect.*
import cats.effect.implicits.parallelForGenSpawn
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.suprnation.actor.ActorSystem
import hydrozoa.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.config.node.operation.evacuation.{NodeOperationEvacuationConfig, NodeOperationEvacuationConfigGen}
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId
import hydrozoa.integration.rbr.model.petri.net.RBRPlaceId.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.classification.Histogram
import hydrozoa.lib.logging.{ContraTracer, Slf4jTracer}
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.peer.HeadPeerWallet
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment
import hydrozoa.rulebased.{DisputeActor, DisputeActorEvent, DisputeActorEventFormat, EvacuationActor, EvacuationActorEvent, EvacuationActorEventFormat, RuleBasedRegimeManager}
import org.scalacheck.util.Pretty
import org.scalacheck.{Arbitrary, Gen, Properties, PropertyM}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import scalus.cardano.ledger.*
import test.TestPeersSpec

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/*
CURRENT STATUS:
- The test only tests "happy path" behavior, where every peer votes for the same commitment
- We start with a mocked fallback tx
- We go through dispute + resolution + evacuation successfully
- Full classification of the utxo state matches
- Next steps (in order):
  - Instead of starting the actors individually, start them with the rule-based regime manager
  - Add in a cardano backend proxy to drop transactions from certain peers and regain some determinism ("rig the races")
  - Vote for different commitments
  - start with an actual fallback
  - add deinit
  - Domain-based logging (rather than stringly typed)
  - Model based testing of intermediary states according to full classification
 */
object EvacuationPropertyTest extends Properties("RBR Evacuation Property"):

    given ppIDU: (InitialDisputeUtxos => Pretty) = _ =>
        Pretty(_ => s"InitialDisputeUtxos (too long to print)")

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters =
        p.withMinSuccessfulTests(3)

    // These might need to be tuned. Basically we don't want to end up in a spin loop.
    //  TODO: How can we detect this in the actors themselves?
    val votingDuration: FiniteDuration = 5.seconds
    val evacuationDuration: FiniteDuration = 10.minute
    // After the terminal "no more evacuations" signal fires, keep the actors running for this
    // buffer to catch any post-completion crashes (e.g. building an EvacuationTx with an empty
    // map; treating any such error as Recoverable is what keeps the actors alive for rollbacks).
    val postCompletionBuffer: FiniteDuration = 30.seconds
    val actorRunDuration: FiniteDuration =
        votingDuration + evacuationDuration + postCompletionBuffer

    // 100ms polling period so actors poll on every ~1s cats-actors ping loop tick
    val fastEvacConfig: NodeOperationEvacuationConfigGen =
        (wallet: HeadPeerWallet) => Gen.const(NodeOperationEvacuationConfig(100.millis, wallet))

    import MultiNodeConfig.*

    val _ = property("evacuation resolves via vote: treasury present, no votes remain") =
        run(
          scenario(mkAction = (sec, sigs) =>
              RuleBasedRegimeManager.DisputeAction.Vote(sec = sec, signatures = sigs)
          ),
          PropertyM.pick(
            MultiNodeConfig
                .generate(TestPeersSpec.default)(
                  generateNodeOperationEvacuationConfig = fastEvacConfig
                )
                .label("MultiNodeConfig")
          )
        )

    lazy val _ = property("evacuation resolves via abstain: treasury present, no votes remain") =
        run(
          scenario(mkAction = (_, _) => RuleBasedRegimeManager.DisputeAction.Abstain),
          PropertyM.pick(
            MultiNodeConfig
                .generate(TestPeersSpec.default)(
                  generateNodeOperationEvacuationConfig = fastEvacConfig
                )
                .label("MultiNodeConfig")
          )
        )

    /** Shared happy-path scenario: synthesize the post-fallback UTxO set, spawn the
      * [[DisputeActor]] + [[EvacuationActor]] pair per peer (with the [[DisputeAction]] under test),
      * and assert the terminal UTxO classification.
      *
      * Both Vote and Abstain produce the same terminal state — the default vote utxo already
      * commits to `evacMap.kzgCommitment`, so peers either join the default's vote or step aside
      * and let it carry the tally; in either case the resolution evacuates against `evacMap`.
      *
      * monadicIO (real time): `setReceiveTimeout` in cats-actors uses
      * `System.currentTimeMillis()`, which is NOT controlled by TestControl. Real wall-clock time
      * is required for actors to poll. `fastEvacConfig` overrides the default 1–10 min polling
      * period so actors poll every ~1s.
      */
    private def scenario(
        mkAction: (
            StandaloneEvacuationCommitment.Onchain,
            List[BlockHeader.Minor.HeaderSignature]
        ) => RuleBasedRegimeManager.DisputeAction
    ): MultiNodeConfigTestM[Boolean] =
        for
            env <- ask
            fallbackTxId <- pick[TransactionHash](
              Arbitrary.arbitrary[TransactionHash].label("FallbackTx id")
            )
            mockFallback: Transaction = new Transaction(
              body = KeepRaw(TransactionBody(TaggedSortedSet.empty, IndexedSeq.empty, Coin.zero)),
              witnessSetRaw = KeepRaw(TransactionWitnessSet.empty),
              isValid = true,
              auxiliaryData = None
            ) {
                override lazy val id = fallbackTxId
            }

            // Real wall-clock time is already in the valid Cardano era (post-2020).
            // No clock warmup is needed, unlike when using TestControl.
            now = QuantizedInstant.ofEpochSeconds(env.slotConfig, 20000000000L)

            nEvacs <- pick(Gen.choose(1, 1000).label("nEvacs"))

            // Generate the synthetic post-fallback UTxO set.
            initialUtxos <- pick[InitialDisputeUtxos](
              InitialDisputeUtxos
                  .gen(fallbackTxId, now, votingDuration, nEvacs)(using env)
                  .label("initial dispute utxos")
            )

            // block header: all peers vote for the same commitment (happy path).
            blockHeader = StandaloneEvacuationCommitment.Onchain(
              blockNum = BigInt(1),
              startTime = now.toPosixTime,
              versionMajor = BigInt(1),
              versionMinor = BigInt(1),
              commitment = initialUtxos.kzgCommitment
            )

            // All peers co-sign the block header (a voted block requires all signatures)
            signatures = env.multisignHeader(blockHeader).toList

            action = mkAction(blockHeader, signatures)

            backendAndSnapshot <- lift(
              CardanoBackendMock.mockIOWithSnapshot(
                MockState(
                  ledgerState = State(initialUtxos.allUtxos(using env)),
                  currentSlot = now.toSlot,
                  knownTxs = Set(fallbackTxId),
                  submittedTxs = List((Map.empty, mockFallback))
                ),
                mkContext = _ =>
                    // Needed so that the headConfig's network, slot config, etc. is used.
                    // TODO: This should probably be factored out into a helper in CardanoBackedMock
                    //   and used by default.
                    Context(
                      fee = Coin.zero,
                      env = UtxoEnv.apply(
                        now.toSlot.slot,
                        env.headConfig.cardanoProtocolParams,
                        certState = CertState.empty,
                        env.headConfig.network
                      ),
                      slotConfig = env.headConfig.slotConfig,
                      evaluatorMode = EvaluateAndComputeCost
                    )
              )
            )

            (sharedBackend, utxoSnapshot) = backendAndSnapshot

            // Fires when any peer logs that no evacuations remain.
            evacuatedSignal <- lift(IO.deferred[Unit])

            peerNum = env.nodeConfigs.head._2.ownHeadPeerNum
            
            // Signal tracer that fires when any peer finishes evacuating.
            evacuationSignalTap: ContraTracer[IO, EvacuationActorEvent] = ContraTracer.emit {
                case EvacuationActorEvent.NoMoreEvacuations => evacuatedSignal.complete(()).void
                case _                                      => IO.unit
            }

            baseEvacTracer: ContraTracer[IO, EvacuationActorEvent] =
                Slf4jTracer.sink.contramap(EvacuationActorEventFormat.humanFormat(peerNum))

            baseDisputeTracer: ContraTracer[IO, DisputeActorEvent] =
                Slf4jTracer.sink.contramap(DisputeActorEventFormat.humanFormat(peerNum))

            terminalUtxos <- lift {
                val peerBots: List[IO[Unit]] =
                    env.nodePrivateConfigs.toList.map { (peerId, _) =>
                        val peerEvacTracer =
                            Slf4jTracer.sink.contramap(
                              EvacuationActorEventFormat.humanFormat(
                                env.nodeConfigs(peerId).ownHeadPeerNum
                              )
                            ) |+| evacuationSignalTap
                        val peerDisputeTracer =
                            Slf4jTracer.sink.contramap(
                              DisputeActorEventFormat.humanFormat(
                                env.nodeConfigs(peerId).ownHeadPeerNum
                              )
                            )
                        actorsFor(
                          peerId = peerId,
                          action = action,
                          sharedBackend = sharedBackend,
                          candidateEvacMaps = Map(
                            initialUtxos.evacuationMap.kzgCommitment ->
                                initialUtxos.evacuationMap
                          ),
                          fallbackTxHash = fallbackTxId,
                          disputeTracer = peerDisputeTracer,
                          evacuationTracer = peerEvacTracer,
                        )(using env.nodeConfigs(peerId))
                    }

                val infoTracer = Slf4jTracer.sink.contramap(
                  EvacuationActorEventFormat.humanFormat(peerNum)
                )

                IO.race(
                  evacuatedSignal.get
                      >> infoTracer.traceWith(EvacuationActorEvent.EvacTxSubmitted)
                      >> IO.sleep(postCompletionBuffer),
                  peerBots.parSequence
                ).timeoutTo(
                  actorRunDuration,
                  IO.raiseError(
                    RuntimeException(s"Dispute/evacuation phase timed out after $actorRunDuration")
                  )
                ) >> utxoSnapshot
            }

            classification <- lift(
              IO.fromEither(
                Histogram
                    .empty(RBRClassifier(using env))
                    .addAll(terminalUtxos.map { case (i, o) => Utxo(i, o) })
                    .toEither
                    .left
                    .map(errs => RuntimeException(errs.toList.mkString("\n")))
              )
            )

            nPeers = env.headConfig.nHeadPeers.convert

            // TODO: these buckets probably need refinement. TBD
            expectedBuckets: Map[RBRPlaceId, Int] = Map(
              TreasuryRefPlaceId -> 1,
              DisputeRefPlaceId -> 1,
              EvacuationOutputPlaceId -> nEvacs,
              ResolvedTreasuryPlaceId -> 1,
              CollateralPlaceId -> nPeers,
              AmbientPlaceId -> env.nHeadPeers
            )

            _ <- assertWith(
              classification.classified == expectedBuckets,
              s"Histogram mismatch:\n" +
                  s"  expected: ${expectedBuckets.toList.map((k, v) => (k.toString, v)).sorted.mkString("\n")}\n" +
                  s"  actual:   $classification"
            )
        yield true

    /** Spawn a [[DisputeActor]] + [[EvacuationActor]] pair for one peer inside its own
      * [[ActorSystem]] and block until the system terminates. Until a stage4-style harness can
      * drive a head through hard-confirmation + fallback into the rule-based regime, this test
      * bypasses [[RuleBasedRegimeManager]] and feeds the synthetic post-fallback inputs straight
      * to the actors.
      */
    private def actorsFor(
        peerId: Int,
        action: RuleBasedRegimeManager.DisputeAction,
        sharedBackend: CardanoBackend[IO],
        candidateEvacMaps: Map[KzgCommitment, EvacuationMap],
        fallbackTxHash: TransactionHash,
        disputeTracer: ContraTracer[IO, DisputeActorEvent],
        evacuationTracer: ContraTracer[IO, EvacuationActorEvent],
    )(using config: RuleBasedRegimeManager.Config): IO[Unit] =
        ActorSystem[IO](s"RBR actors for peer $peerId").use { system =>
            for {
                _ <- system.actorOf(
                  DisputeActor(
                    action = action,
                    cardanoBackend = sharedBackend,
                    tracer = disputeTracer
                  )
                )
                _ <- system.actorOf(
                  EvacuationActor(
                    candidateEvacMaps = candidateEvacMaps,
                    cardanoBackend = sharedBackend,
                    fallbackTxHash = fallbackTxHash,
                    tracer = evacuationTracer
                  )
                )
                _ <- system.waitForTermination
            } yield ()
        }
