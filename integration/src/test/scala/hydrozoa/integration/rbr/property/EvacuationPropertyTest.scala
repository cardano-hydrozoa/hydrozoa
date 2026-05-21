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
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.{ContraTracer, LogEvent, Tracer}
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.peer.HeadPeerWallet
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.rulebased.{DisputeActor, EvacuationActor}
import org.scalacheck.util.Pretty
import org.scalacheck.{Arbitrary, Gen, Properties, PropertyM}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.{CertState, Coin, KeepRaw, TaggedSortedSet, Transaction, TransactionBody, TransactionHash, TransactionWitnessSet, Utxo}
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import test.TestPeersSpec

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/*
CURRENT STATUS:
- The test only tests "happy path" behavior, where every peer votes for the same commitment
- We start with a mocked fallback tx
- We go through dispute + resolution successfully
- We go through evacuation up until the final recursive evacuation.
  - EvacuationTx builder cannot balance in some cases because the minAda in the output treasury is too small
    (say, 25 ADA vs the necessary 29).
    - This causes the balancer to increases the treasury's ada, which breaks the value invariant
        treasuryInput = treasuryOutput + evacuatedTotal
    - This then causes a plutus script error, which crashes the evacuation actor (it is unrecoverable)
  - The solution is _probably_ to increase the amount that the resolution transaction reserves for the treasury.
- Next steps (in order):
  - ensure full classification matches
  - Instead of starting the actors individually, start them with the rule-based regime manager
  - Add in a cardano backend proxy to drop transactions from certain peers and regain some determinism (rig the races)
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
//            .withInitialSeed(org.scalacheck.rng.Seed.fromBase64("4vIY0T61oMWbCS-0R8WCW6qUpqczPAZmtj97v75k0pL=").get)

    // These might need to be tuned. Basically we don't want to end up in a spin loop.
    //  TODO: How can we detect this in the actors themselves?
    val votingDuration: FiniteDuration = 5.seconds
    val evacuationDuration: FiniteDuration = 10.minute
    val actorRunDuration: FiniteDuration = votingDuration + evacuationDuration

    // 100ms polling period so actors poll on every ~1s cats-actors ping loop tick
    val fastEvacConfig: NodeOperationEvacuationConfigGen =
        (wallet: HeadPeerWallet) => Gen.const(NodeOperationEvacuationConfig(100.millis, wallet))

    import MultiNodeConfig.*

    val _ = property("evacuation resolves: treasury present, no votes remain") =
        // monadicIO (real time): setReceiveTimeout in cats-actors uses System.currentTimeMillis(),
        // which is NOT controlled by TestControl. Real wall-clock time is required for actors to poll.
        // fastEvacConfig overrides the default 1–10 min polling period so actors poll every ~1s.
        run(
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
//              nEvacs <- pick(Gen.const(50).label("nEvacs"))

              // Generate the synthetic post-fallback UTxO set.
              initialUtxos <- pick[InitialDisputeUtxos](
                InitialDisputeUtxos
                    .gen(fallbackTxId, now, votingDuration, nEvacs)(using env)
                    .label("initial dispute utxos")
              )

              // block header: all peers vote for the same commitment (happy path).
              blockHeader = BlockHeader.Minor.Onchain(
                blockNum = BigInt(1),
                startTime = now.toPosixTime,
                versionMajor = BigInt(1),
                versionMinor = BigInt(1),
                commitment = initialUtxos.kzgCommitment
              )

              // All peers co-sign the block header (a voted block requires all signatures)
              signatures = env.multisignHeader(blockHeader).toList

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

              // Fires when any peer logs "Treasury is Resolved" (Phase 1 termination).
              // Fires when any peer logs "Evacuation TX submitted" (Phase 2 termination).
              resolvedSignal <- lift(IO.deferred[Unit])
              evacuatedSignal <- lift(IO.deferred[Unit])

              // Single shared tracer that fans out to both signal completions.
              tracer <- lift {
                  // TODO: domain-specific event types should be used here instead of brittle strings
                  val signalTracer: Tracer = ContraTracer.emit { (ev: LogEvent) =>
                      if ev.msg == "Treasury is Resolved" then resolvedSignal.complete(()).void
                      else if ev.msg == "No more evacuations to be done. Staying alive in case of rollbacks"
                      then evacuatedSignal.complete(()).void
                      else IO.unit
                  }
                  Tracer.makeLocal.flatMap(local => local.update(_ |+| signalTracer).as(local))
              }

              terminalUtxos <- lift {
                  // Phase 1 — Dispute: run all peer dispute actors until "Treasury is Resolved".
                  val disputeSystems: List[IO[Unit]] =
                      env.nodePrivateConfigs.toList.map { (peerId, _) =>
                          ActorSystem[IO](s"Dispute System for peer $peerId").use { system =>
                              val disputeBot: IO[Unit] = system
                                  .actorOf(
                                    DisputeActor(
                                      blockHeader = blockHeader,
                                      cardanoBackend = sharedBackend,
                                      signatures = signatures,
                                      tracerLocal = tracer
                                    )(using env.nodeConfigs(peerId))
                                  )
                                  >> system.waitForTermination

                              IO.race(
                                resolvedSignal.get >> Tracer.info("!!! RESOLVED !!!")(using tracer),
                                disputeBot
                              ).void
                          }
                      }

                  // Phase 2 — Evacuation: run all peer evacuation actors until one submits the TX.
                  val evacuationSystems: List[IO[Unit]] =
                      env.nodePrivateConfigs.toList.map { (peerId, _) =>
                          ActorSystem[IO](s"Evacuation System for peer $peerId").use { system =>
                              val evacuationBot: IO[Unit] = system.actorOf(
                                EvacuationActor(
                                  thisNodeEvacuates = initialUtxos.evacuationMap,
                                  cardanoBackend = sharedBackend,
                                  evacuationMapAtFallback = initialUtxos.evacuationMap,
                                  fallbackTxHash = fallbackTxId,
                                  tracerLocal = tracer
                                )(using env.nodeConfigs(peerId))
                              ) >> system.waitForTermination

                              IO.race(
                                evacuatedSignal.get >> Tracer.info("!!! EVACUATION FINISHED !!!")(
                                  using tracer
                                ),
                                evacuationBot
                              ).void
                          }
                      }

                  // Run Phase 1, then Phase 2, then snapshot the terminal UTxO set.
                  // timeoutTo handles the "silent retry" case where actors never die and never signal.
                  disputeSystems.parSequence.timeoutTo(
                    actorRunDuration,
                    IO.raiseError(
                      RuntimeException(s"Dispute phase timed out after $actorRunDuration")
                    )
                  ) >>
                      evacuationSystems.parSequence.timeoutTo(
                        actorRunDuration,
                        IO.raiseError(
                          RuntimeException(s"Evacuation phase timed out after $actorRunDuration")
                        )
                      ) >>
                      utxoSnapshot
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
                CollateralPlaceId -> env.nHeadPeers,
                EvacuationOutputPlaceId -> nEvacs,
                ResolvedTreasuryPlaceId -> 1,
                AmbientPlaceId -> env.nHeadPeers
              )

              _ <- assertWith(
                classification.classified == expectedBuckets,
                s"Histogram mismatch:\n" +
                    s"  expected: ${expectedBuckets.toList.map((k, v) => (k.toString, v)).sorted.mkString("\n")}\n" +
                    s"  actual:   $classification"
              )
          yield true,
          PropertyM.pick(
            MultiNodeConfig
                .generate(TestPeersSpec.default)(
                  generateNodeOperationEvacuationConfig = fastEvacConfig
                )
                .label("MultiNodeConfig")
          )
        )
