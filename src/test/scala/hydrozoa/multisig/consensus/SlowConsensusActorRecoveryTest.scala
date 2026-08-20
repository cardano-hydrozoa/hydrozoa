package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import cats.syntax.contravariant.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorSystem
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.logging.{ContraTracer, Slf4jTracer}
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.metrics.PeerMetrics
import hydrozoa.multisig.persistence.{Cf, InMemoryBackendStore, Persistence, PersistenceEventFormat, StoreKey}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** A no-op actor for the [[SlowConsensusActor.Connections]] slots this test never exercises. */
private final case class SinkActor[R]() extends Actor[IO, R]:
    override def receive: Receive[IO, R] = { case _ => IO.unit }

/** [[SlowConsensusActor]] must read its own base — the hard-confirmed high-water — in its own
  * `PreStart`, like every other actor with base state.
  *
  * Without it `State.lastConfirmed` stays `None`, so the surplus guard in `handleRemoteHardAck` is
  * inert and a remote ack for an ALREADY hard-confirmed stack is buffered as an "orphan" instead of
  * being dropped. Those orphans are only ever cleared at cell creation, which never recurs for a
  * confirmed stack — and `ReplayActor` re-sends the hard-ack journals from key `0` on every boot,
  * so a restart retained every remote ack the head had ever produced.
  */
class SlowConsensusActorRecoveryTest extends AnyFunSuite:

    private val config: NodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))
    private given CardanoNetwork.Section = config

    /** A remote peer's ack — `peer` must not be this node, or the own-echo branch swallows it
      * before the surplus guard is reached.
      */
    private def remoteAck(peer: Int, stack: Int): HardAck =
        HardAck(
          ackId = HardAckId(PeerId.Head(HeadPeerNumber(peer)), HardAckNumber(0)),
          stackNum = StackNumber(stack),
          payload = HardAck.Round2Payload.Regular(TxSignature(IArray.from(Array.fill[Byte](64)(0))))
        )

    /** Boot an SCA against a store whose hard-confirmed high-water is `confirmed`, hand it a remote
      * ack for `ackStack`, and report the events it traced.
      */
    private def eventsFor(confirmed: Int, ackStack: Int): Vector[SlowConsensusActorEvent] =
        val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(persistenceTracer)
            .use(backend =>
                ActorSystem[IO]("sca-recovery-test").use(system =>
                    for {
                        persistence <- Persistence.fromBackend(backend, persistenceTracer)
                        // `Markers.recoverHardConfirmed` reads only the KEY, so a dummy value byte
                        // is enough (the typed value's leaf txs have no public constructors).
                        _ <- persistence.backend.put(
                          Cf.HardConfirmation,
                          StoreKey.HardConfirmation(StackNumber(confirmed)).encode,
                          Array[Byte](0)
                        )
                        seen <- Ref.of[IO, Vector[SlowConsensusActorEvent]](Vector.empty)
                        tracer = ContraTracer[IO, SlowConsensusActorEvent](e => seen.update(_ :+ e))
                        sc <- system.actorOf(SinkActor[StackComposer.Request]())
                        cl <- system.actorOf(SinkActor[CardanoLiaison.Request]())
                        sca <- system.actorOf(
                          SlowConsensusActor(
                            config,
                            SlowConsensusActor.Connections(sc, cl, Nil),
                            tracer,
                            persistence,
                            PeerMetrics.create(0L, Vector.empty)
                          )
                        )
                        _ <- sca ! remoteAck(peer = 1, stack = ackStack)
                        _ <- IO.sleep(1.second) // let PreStart and the ack drain
                        evs <- seen.get
                    } yield evs
                )
            )
            .unsafeRunSync()

    test("SlowConsensusActor drops a remote ack for an already hard-confirmed stack") {
        // Store says stacks up to 3 are hard-confirmed; an ack for stack 1 is a late surplus.
        val evs = eventsFor(confirmed = 3, ackStack = 1)
        assert(
          evs.exists {
              case _: SlowConsensusActorEvent.SurplusHardAckIgnored => true
              case _                                                => false
          },
          s"expected the ack to be dropped as surplus, got: $evs"
        )
    }

    test("SlowConsensusActor still buffers a remote ack ABOVE the hard-confirmed high-water") {
        // The other side of the guard: an ack for a stack this peer has not confirmed is a
        // genuinely-early orphan and must be kept, not dropped.
        val evs = eventsFor(confirmed = 3, ackStack = 5)
        assert(
          !evs.exists {
              case _: SlowConsensusActorEvent.SurplusHardAckIgnored => true
              case _                                                => false
          },
          s"an ack above the high-water must not be dropped as surplus, got: $evs"
        )
    }
