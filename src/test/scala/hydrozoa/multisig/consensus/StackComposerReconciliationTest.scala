package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.implicits.*
import cats.syntax.contravariant.*
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.StackTimes.StackCreationEndTime
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.{ContraTracer, Slf4jTracer}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import hydrozoa.multisig.metrics.PeerMetrics
import hydrozoa.multisig.persistence.{Cf, InMemoryBackendStore, Persistence, PersistenceEventFormat}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** The single-flight gate is opened by exactly one `Stack.HardConfirmed` message — no timeout, no
  * retry, no re-derivation — so one lost delivery stops the head permanently, while composition
  * completes, both peers log the confirmation, and durable state stays consistent. Only a restart
  * recovers it, and only because `State.recover` re-derives the gate from the persisted marker.
  *
  * `StackComposer.reconcileHardConfirmedFromPersistence` does that re-derivation without the
  * restart. These tests drive the composer with the message never delivered and assert the gate
  * opens anyway — and, as the control, that it stays shut when persistence genuinely has not
  * advanced.
  */
class StackComposerReconciliationTest extends AnyFunSuite:

    private val config: NodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))
    private val headConfig: HeadConfig = config.headConfig
    private given HeadConfig.Bootstrap.Section = config

    test("the gate opens from persistence when the HardConfirmed message never arrives") {
        val events = run(seedHardConfirmationFor = Some(StackNumber.zero))
        assert(
          events.exists(_.isInstanceOf[StackComposerEvent.HardConfirmationReconciled]),
          s"expected the gate to be reconciled from persistence; saw: $events"
        )
    }

    // Control. Without it this suite could not tell "reconciliation works" from "the event fires
    // whenever the composer waits", which is the failure mode that matters -- reconciling off a
    // marker that has NOT advanced would open the gate on a stack that is not confirmed.
    test("the gate stays shut when the persisted marker has not advanced") {
        val events = run(seedHardConfirmationFor = None)
        assert(
          !events.exists(_.isInstanceOf[StackComposerEvent.HardConfirmationReconciled]),
          s"gate was reconciled with nothing persisted; saw: $events"
        )
    }

    /** Boot a cold composer (which bootstraps stack 0 and closes the gate behind it), optionally
      * seed the hard-confirmation marker, then drive `tryProgress` with inbound briefs. The
      * `Stack.HardConfirmed` message is never sent.
      */
    private def run(seedHardConfirmationFor: Option[StackNumber]): List[StackComposerEvent] =
        val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(persistenceTracer)
            .use(backend =>
                ActorSystem[IO]("sc-reconcile").use(system =>
                    for {
                        persistence <- Persistence.fromBackend(backend, persistenceTracer)
                        seen <- Ref.of[IO, List[StackComposerEvent]](Nil)
                        jlSink <- system.actorOf(NoopSink[JointLedger.Requests.Request]())
                        fcaSink <- system.actorOf(NoopSink[FastConsensusActor.Request]())
                        scaSink <- system.actorOf(NoopSink[SlowConsensusActor.Request]())
                        composer <- system.actorOf(
                          StackComposer(
                            config,
                            StackComposer.Connections(
                              jointLedger = jlSink,
                              fastConsensusActor = fcaSink,
                              slowConsensusActor = scaSink,
                              headPeerLiaisons = List()
                            ),
                            ContraTracer[IO, StackComposerEvent](e => seen.update(e :: _)),
                            persistence,
                            PeerMetrics.create(0L, Vector.empty),
                            // The production threshold is 2 minutes; collapse it so the test does
                            // not have to wait it out.
                            hardConfirmReconcileAfter = 1.milli
                          )
                        )
                        _ <- IO.sleep(
                          500.millis
                        ) // let PreStart bootstrap stack 0 and close the gate
                        // `recoverHardConfirmed` reads only the LAST KEY of Cf.HardConfirmation, so
                        // a raw key with an empty value is a faithful stand-in for a real
                        // multisigned effects payload and keeps this test off the codec.
                        _ <- seedHardConfirmationFor.traverse_(sn =>
                            backend
                                .put(Cf.HardConfirmation, stackKeyBytes(sn), Array.emptyByteArray)
                        )
                        // `tryProgress` runs on every inbound message and is the only clock the
                        // reconciliation has. The first pass only arms the wait, so drive twice.
                        _ <- driveOnce(composer, 1)
                        _ <- IO.sleep(50.millis)
                        _ <- driveOnce(composer, 2)
                        _ <- IO.sleep(500.millis)
                        events <- seen.get
                    } yield events
                )
            )
            .unsafeRunSync()

    /** Spine-shaped key: a 4-byte big-endian stack number. Spelled out here because the production
      * encoder is `private[persistence]`.
      */
    private def stackKeyBytes(sn: StackNumber): Array[Byte] =
        java.nio.ByteBuffer.allocate(4).putInt(sn: Int).array()

    private def driveOnce(composer: StackComposer.Handle, stack: Int): IO[Unit] =
        realTimeQuantizedInstant(headConfig.slotConfig).flatMap { now =>
            composer ! StackBrief(
              stackNum = StackNumber(stack),
              firstBlockNum = BlockNumber(0),
              lastBlockNum = BlockNumber(0),
              creationEndTime = StackCreationEndTime(now)
            )
        }
