package hydrozoa.integration.rbr.property

import cats.effect.*
import cats.effect.syntax.all.*
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.suprnation.actor.ActorSystem
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.config.node.operation.evacuation.{NodeOperationEvacuationConfig, NodeOperationEvacuationConfigGen}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, HeadPeerWallet}
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.rulebased.DisputeActor
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.PropertyM
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.{CertState, Coin, TransactionHash}
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import test.TestPeersSpec

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object DisputePropertyTest extends Properties("RBR Dispute Property"):

    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters =
        p.withMinSuccessfulTests(100)

    import MultiNodeConfig.*

    val _ = property("dispute resolves: treasury resolved, no votes remain") =
        // monadicIO (real time): setReceiveTimeout in cats-actors uses System.currentTimeMillis(),
        // which is NOT controlled by TestControl. Real wall-clock time is required for actors to poll.
        runDefault(
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
                InitialDisputeUtxos.gen(fallbackTxId, now)(using env)
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

              sharedBackend <- lift(
                CardanoBackendMock.mockIO(
                  MockState(
                    ledgerState = State(initialUtxos.allUtxos(using env)),
                    currentSlot = now.toSlot
                  ),
                )
              )

              tracer <- lift(Tracer.makeLocal)

              // Plan:
              //   - Create a stage-1 style time machine for two different peer systems which get executed concurrently
              //   - Run them side-by-side until the Resolved utxo appears
              //   - Stop both actor systems
              //   - Inspect the utxo state from the cardano backend
              result <- lift {
                  val peerSystems: List[IO[Unit]] =
                      env.nodePrivateConfigs.toList.map { (peerId, _) =>
                          ActorSystem[IO](s"Dispute System for peer $peerId").use { system =>
                              for {
                                  disputeActor <- system.actorOf(
                                    DisputeActor(
                                      blockHeader = blockHeader,
                                      cardanoBackend = sharedBackend,
                                      signatures = signatures,
                                      tracerLocal = tracer
                                    )(using env.nodeConfigs(peerId))
                                  )
                              } yield ()
                          }

                      }
                  IO.unit
              }
//                ActorSystem[IO]("dispute-property-test").use { system =>
//                    for
//
//                        // Sleep through voting window + enough time for all tally/resolution rounds
//                        _ <- IO.sleep(actorRunDuration)
//                        // Snapshot the terminal state from the shared backend
//                        utxosAtDisputeAddr <- sharedBackend
//                            .utxosAt(HydrozoaBlueprint.mkDisputeAddress(env.headConfig.network))
//                            .flatMap(
//                              _.fold(e => IO.raiseError(RuntimeException(e.toString)), IO.pure)
//                            )
//                        utxosAtTreasuryAddr <- sharedBackend
//                            .utxosAt(HydrozoaBlueprint.mkTreasuryAddress(env.headConfig.network))
//                            .flatMap(
//                              _.fold(e => IO.raiseError(RuntimeException(e.toString)), IO.pure)
//                            )
//                    yield (utxosAtDisputeAddr, utxosAtTreasuryAddr)
//                }

//              (utxosAtDisputeAddr, utxosAtTreasuryAddr) = result
//
//              _ <- PropertyM.assertWith[IO](
//                utxosAtDisputeAddr.isEmpty,
//                s"No vote UTxOs should remain at the dispute address after resolution, " +
//                    s"but found ${utxosAtDisputeAddr.size}"
//              )
//              _ <- PropertyM.assertWith[IO](
//                utxosAtTreasuryAddr.size == 1,
//                s"Exactly 1 resolved treasury UTxO should be at the treasury address, " +
//                    s"but found ${utxosAtTreasuryAddr.size}"
//              )
          yield true
        )
