package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.joint.{EvacuationMap, JointLedger}
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import hydrozoa.multisig.persistence.codec.TreasuryFixture
import hydrozoa.multisig.persistence.{ArrivalStamp, InMemoryBackendStore, LaneKey, LaneValue, Persistence, StoreKey}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** Records the stack-0 [[SlowConsensusActor.StackHandoff]] a bootstrapping [[StackComposer]] emits,
  * completing `gotHandoff` so the test can await it (cold) or assert its absence (recovered).
  */
private final case class HandoffProbe(
    gotHandoff: Deferred[IO, SlowConsensusActor.StackHandoff]
) extends Actor[IO, SlowConsensusActor.Request]:
    override def receive: Receive[IO, SlowConsensusActor.Request] = {
        case h: SlowConsensusActor.StackHandoff => gotHandoff.complete(h).void
        case _                                  => IO.unit
    }

/** A no-op actor used to fill [[StackComposer.Connections]] slots the boot path never messages. */
private final case class NoopSink[R]() extends Actor[IO, R]:
    override def receive: Receive[IO, R] = { case _ => IO.unit }

/** R3a boot-wiring test: [[StackComposer]]'s `PreStart` must bootstrap stack 0 on a cold store and
  * **skip** that bootstrap on a non-empty one (recovering its `State` instead). The recover logic
  * itself is unit-tested in `RecoverSeamsTest`; here we boot the actor against a seeded store and
  * observe the one externally-visible difference — whether a stack-0 `StackHandoff` reaches
  * `SlowConsensusActor`.
  */
class StackComposerRecoveryTest extends AnyFunSuite:

    private val config: NodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))
    private val headConfig: HeadConfig = config.headConfig
    private given CardanoNetwork.Section = config
    private val stamp: ArrivalStamp = ArrivalStamp(generation = 0, monotonicNanos = 1L)

    /** This node's head peer number (the fixture config is always a head node). */
    private val ownNum: HeadPeerNumber = config.ownPeerId match {
        case PeerId.Head(n) => n
        case PeerId.Coil(_) => fail("fixture config must be a head node")
    }

    test("StackComposer bootstraps stack 0 on a cold store") {
        bootWith(_ => IO.unit) { gotHandoff =>
            gotHandoff.get
                .timeout(3.seconds)
                .map(h => assert(h.unsigned.brief.stackNum == StackNumber.zero))
        }
    }

    test("StackComposer skips the stack-0 bootstrap on a non-empty store (recovers instead)") {
        bootWith(seedRecoverable) { gotHandoff =>
            // No handoff must be produced; let the booted actor settle, then assert silence.
            IO.sleep(1.second) >> gotHandoff.tryGet.map(o => assert(o.isEmpty))
        }
    }

    /** Seed exactly what `StackComposer.State.recover` reads for a peer that hard-acked stack 1
      * (last block 3): the own hard-ack, the stack brief, the treasury, and the evacuation map.
      */
    private def seedRecoverable(p: Persistence[IO]): IO[Unit] =
        val own = ownNum
        for {
            sb <- stackBrief(stack = 1, firstBlock = 0, lastBlock = 3)
            _ <- p.put(LaneKey.HardAck(own, HardAckNumber(0)))(
              LaneValue(stamp, hardAck(peer = own.convert, ackNum = 0, stack = 1))
            )
            _ <- p.put(LaneKey.Stack(StackNumber(1)))(LaneValue(stamp, sb))
            _ <- p.put(StoreKey.Treasury)(TreasuryFixture.sampleTreasury)
            _ <- p.put(StoreKey.EvacuationMap(BlockNumber(3)))(EvacuationMap.empty)
        } yield ()

    private def bootWith[A](
        seed: Persistence[IO] => IO[Unit]
    )(check: Deferred[IO, SlowConsensusActor.StackHandoff] => IO[A]): A =
        InMemoryBackendStore.open
            .use(backend =>
                ActorSystem[IO]("sc-recovery").use(system =>
                    for {
                        persistence <- Persistence.fromBackend(backend)
                        _ <- seed(persistence)
                        gotHandoff <- Deferred[IO, SlowConsensusActor.StackHandoff]
                        probe <- system.actorOf(HandoffProbe(gotHandoff))
                        jlSink <- system.actorOf(NoopSink[JointLedger.Requests.Request]())
                        fcaSink <- system.actorOf(NoopSink[FastConsensusActor.Request]())
                        _ <- system.actorOf(
                          StackComposer(
                            config,
                            StackComposer.Connections(
                              jointLedger = jlSink,
                              fastConsensusActor = fcaSink,
                              slowConsensusActor = probe,
                              headPeerLiaisons = List()
                            ),
                            ContraTracer.nullTracer[IO, StackComposerEvent],
                            persistence
                          )
                        )
                        r <- check(gotHandoff)
                    } yield r
                )
            )
            .unsafeRunSync()

    // ---- fixtures (mirror RecoverSeamsTest) ----

    private def stackBrief(stack: Int, firstBlock: Int, lastBlock: Int): IO[StackBrief] =
        realTimeQuantizedInstant(headConfig.slotConfig).map { now =>
            StackBrief(
              stackNum = StackNumber(stack),
              firstBlockNum = BlockNumber(firstBlock),
              lastBlockNum = BlockNumber(lastBlock),
              creationEndTime = StackCreationEndTime(now)
            )
        }

    private def hardAck(peer: Int, ackNum: Int, stack: Int): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Head(HeadPeerNumber(peer)), HardAckNumber(ackNum)),
          stackNum = StackNumber(stack),
          payload = HardAck.Round2Payload.Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
        )
