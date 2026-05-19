package hydrozoa.integration.rbr.property

import cats.effect.*
import cats.effect.implicits.parallelForGenSpawn
import cats.effect.unsafe.implicits.global
import com.suprnation.actor.ActorSystem
import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.{ContraTracer, LogEvent, Tracer}
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.rulebased.DisputeActor
import hydrozoa.config.node.operation.evacuation.{NodeOperationEvacuationConfig, NodeOperationEvacuationConfigGen}
import hydrozoa.multisig.consensus.peer.HeadPeerWallet
import org.scalacheck.{Arbitrary, Gen, Properties, PropertyM}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.{CertState, Coin, TransactionHash}
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import test.TestPeersSpec
import scala.concurrent.duration.DurationInt

object DisputePropertyTest extends Properties("RBR Dispute Property"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters =
        p.withMinSuccessfulTests(3)

    val votingDuration   = 5.seconds
    val postVotingSlack  = 20.seconds
    val actorRunDuration = votingDuration + postVotingSlack

    // 100ms polling period so actors poll on every ~1s cats-actors ping loop tick
    val fastEvacConfig: NodeOperationEvacuationConfigGen =
        (wallet: HeadPeerWallet) => Gen.const(NodeOperationEvacuationConfig(100.millis, wallet))

    import MultiNodeConfig.*

    val _ = property("dispute resolves: treasury resolved, no votes remain") =
        // monadicIO (real time): setReceiveTimeout in cats-actors uses System.currentTimeMillis(),
        // which is NOT controlled by TestControl. Real wall-clock time is required for actors to poll.
        // fastEvacConfig overrides the default 1–10 min polling period so actors poll every ~1s.
        run(
          for
              env <- ask
              fallbackTxId <- pick[TransactionHash](
                Arbitrary.arbitrary[TransactionHash]
              )

              // Real wall-clock time is already in the valid Cardano era (post-2020).
              // No clock warmup is needed, unlike when using TestControl.
              now <- lift(realTimeQuantizedInstant(env.headConfig.slotConfig))

              // Generate the [SYNTHETIC] post-fallback UTxO set
              initialUtxos <- pick[InitialDisputeUtxos](
                InitialDisputeUtxos.gen(fallbackTxId, now, votingDuration)(using env)
              )

              // [SYNTHETIC] block header: all peers vote for the same commitment (happy path).
              // The commitment matches the default vote so that maxVote always picks the same winner.
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
                    currentSlot = now.toSlot
                  ),
                  mkContext = _ =>
                      // Needed so that the headConfig's network, slot config, etc. is used.
                      // TODO: make this an extension method on MultiNodeConfig?
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

              // Fires when any peer's actor logs "Treasury is Resolved", shutting down all systems early.
              // actorRunDuration remains as a timeout fallback in case the signal is never emitted.
              resolvedSignal <- lift(IO.deferred[Unit])

              tracer <- lift {
                  val signalTracer: Tracer = ContraTracer.emit { (ev: LogEvent) =>
                      if ev.msg == "Treasury is Resolved" then resolvedSignal.complete(()).void
                      else IO.unit
                  }
                  Tracer.makeLocal.flatMap(local => local.update(_ |+| signalTracer).as(local))
              }

              // Plan:
              //   - Create a stage-1 style time machine for two different peer systems which get executed concurrently
              //   - Run them side-by-side until the Resolved utxo appears
              //   - Stop both actor systems
              //   - Inspect the utxo state from the cardano backend
              terminalUtxos <- lift {
                  val peerSystems: List[IO[Unit]] =
                      env.nodePrivateConfigs.toList.map { (peerId, _) =>
                          ActorSystem[IO](s"Dispute System for peer $peerId").use { system =>
                              system.actorOf(
                                DisputeActor(
                                  blockHeader = blockHeader,
                                  cardanoBackend = sharedBackend,
                                  signatures = signatures,
                                  tracerLocal = tracer
                                )(using env.nodeConfigs(peerId))
                              ).void >> IO.race(IO.sleep(actorRunDuration), resolvedSignal.get).void
                          }
                      }

                  // Run all peer systems in parallel. Each exits when the resolved signal fires
                  // (any peer observed "Treasury is Resolved") or actorRunDuration elapses, whichever is first.
                  // Then snapshot the full terminal UTxO set from the shared backend.
                  peerSystems.parSequence >> utxoSnapshot
              }

              classification <- lift(IO.fromEither(
                DisputeAbstraction.classify(terminalUtxos)(using env)
                    .left.map(e => RuntimeException(e.getMessage))
              ))

              _ <- assertWith(
                classification.votes.isEmpty,
                s"No vote UTxOs should remain after resolution, " +
                    s"but found ${classification.votes.size}"
              )
              _ <- assertWith(
                classification.treasury.isDefined,
                "Exactly 1 resolved treasury UTxO should exist after resolution, but found none"
              )
          yield true,
          PropertyM.pick(MultiNodeConfig.generate(TestPeersSpec.default)(
            generateNodeOperationEvacuationConfig = fastEvacConfig
          ))
        )
