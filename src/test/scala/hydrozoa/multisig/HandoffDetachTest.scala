package hydrozoa.multisig

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.NoSendActorRef
import com.suprnation.actor.event.Debug
import com.suprnation.actor.{DeadLetter, Envelope, EnvelopeWithDeferred}
import hydrozoa.lib.actor.HydrozoaActorSystem
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.HeadMultisigRegimeManager.{Actors, HandoffToRuleBased}
import hydrozoa.multisig.consensus.ack.{HardAckNumber, HubHardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.Mesh
import hydrozoa.multisig.consensus.liaison.{BatchNumber, LiaisonProtocol}
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.consensus.transport.{InProcessPeerTransport, PeerTransport}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.metrics.PeerMetrics
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** At the handoff to the rule-based regime, a regime manager unregisters its liaisons from their
  * transports before it stops them.
  *
  * The remote peers hand off on their own, when each observes the fallback on L1, so until then
  * they keep pulling. A liaison still registered when it stops has those pulls delivered to a
  * stopped actor — dead letters. The stub manager here is the shared base with one liaison on an
  * in-process mesh transport; the control case skips the unregister and shows the dead letter it
  * prevents.
  */
class HandoffDetachTest extends AnyFunSuite {

    private val nPeers: PositiveInt = PositiveInt.unsafeApply(2)
    private val own = HeadPeerId(HeadPeerNumber(0), nPeers)
    private val remote = HeadPeerId(HeadPeerNumber(1), nPeers)

    private val pull: LiaisonProtocol.MeshEmitted = Mesh.Get(
      batchNum = BatchNumber.zero,
      block = BlockNumber(1),
      stack = StackNumber(1),
      request = RequestNumber.zero,
      requestCeiling = RequestNumber.zero,
      softAck = SoftAckNumber.zero,
      headHardAck = HardAckNumber.zero,
      hubHardAck = HubHardAckNumber.zero
    )

    private final class Liaison extends Actor[IO, LiaisonProtocol.MeshEmitted] {
        override def receive: Receive[IO, LiaisonProtocol.MeshEmitted] =
            PartialFunction.fromFunction(_ => IO.unit)
    }

    /** The shared base with one mesh liaison, registered on `transport` for [[remote]]. */
    private final class StubManager(
        transport: PeerTransport,
        detach: Boolean,
        log: Ref[IO, Vector[String]],
        events: Ref[IO, Vector[HeadRegimeManagerEvent]],
    ) extends MultisigRegimeManagerBase[HeadRegimeManagerEvent] {

        override protected val tracer: ContraTracer[IO, HeadRegimeManagerEvent] =
            ContraTracer[IO, HeadRegimeManagerEvent](e => events.update(_ :+ e))

        override protected lazy val tracers: MrmTracers = MrmTracers.fromRoot(tracer)

        override protected val metrics: PeerMetrics = PeerMetrics.create(0L, Vector.empty)

        private val liaison = Ref.unsafe[IO, Option[NoSendActorRef[IO]]](None)

        override protected def preStartLocal: IO[Unit] =
            for {
                l <- context.actorOf(new Liaison)
                _ <- transport.register(remote, l)
                _ <- IO.whenA(detach)(
                  detachAtHandoff(
                    submissions
                        .admit("closed")(IO.pure("open"))
                        .flatMap(gate => log.update(_ :+ s"unregister (submissions $gate)")) >>
                        transport.unregister(remote)
                  )
                )
                _ <- watchChildren(l -> Actors.PeerLiaisonHeadToHead)
                _ <- liaison.set(Some(l))
                _ <- tracer.traceWith(LifecycleEvent.WatchingActors)
            } yield ()

        override protected def onHandoffToRuleBased: IO[Unit] =
            liaison
                .getAndSet(None)
                .flatMap(_.traverse_(l => log.update(_ :+ "stop") >> context.stop(l)))
    }

    /** Hand off, then have the remote pull as it would before it hands off itself. The steps the
      * manager took, and the pulls the system dead-lettered.
      */
    private def handOff(detach: Boolean): (Vector[String], Int) = {
        val run = for {
            deadLetters <- Ref[IO].of(0)
            steps <- HydrozoaActorSystem(
              s"handoff-detach-$detach",
              {
                  case Debug(_, _, dl: DeadLetter[?]) if isPull(dl) =>
                      deadLetters.update(_ + 1)
                  case _ => IO.unit
              }
            )
                .use { actors =>
                    for {
                        registry <- InProcessPeerTransport.emptyRegistry
                        ownT <- InProcessPeerTransport.create(own, registry)
                        remoteT <- InProcessPeerTransport.create(remote, registry)
                        log <- Ref[IO].of(Vector.empty[String])
                        events <- Ref[IO].of(Vector.empty[HeadRegimeManagerEvent])
                        manager <- actors.actorOf(new StubManager(ownT, detach, log, events))
                        // The control provokes its dead letter on purpose; don't count it as one
                        // lost while the system ran, in CI's summary. This test counts it itself.
                        _ <- IO.unlessA(detach)(actors.expectDeadLetters(manager))
                        _ <- await(events)(_.contains(LifecycleEvent.WatchingActors))
                        _ <- remoteT.send(own, pull)
                        _ <- manager ! HandoffToRuleBased
                        _ <- await(events)(_.exists {
                            case LifecycleEvent.TerminatedActor(_, Some(_)) => true
                            case _                                          => false
                        })
                        // A repeated handoff, as a second L1 observation fires one: no second
                        // unregister, no second stop.
                        _ <- manager ! HandoffToRuleBased
                        _ <- remoteT.send(own, pull)
                        // Dead letters are published asynchronously; give one time to surface.
                        _ <- IO.sleep(1.second)
                        steps <- log.get
                    } yield steps
                }
            n <- deadLetters.get
        } yield (steps, n)
        run.timeout(60.seconds).unsafeRunSync()
    }

    /** Whether a dead letter is the remote's pull. Only those are counted: stopping an actor can
      * also dead-letter cats-actors' own self-`Ping`, which says nothing about the transport.
      */
    private def isPull(dl: DeadLetter[?]): Boolean =
        (dl.message match {
            case e: Envelope[?, ?]             => e.message
            case e: EnvelopeWithDeferred[?, ?] => e.envelope.message
            case m                             => m
        }).isInstanceOf[Mesh.Get]

    private def await[A](seen: Ref[IO, Vector[A]])(p: Vector[A] => Boolean): IO[Unit] = {
        def go: IO[Unit] =
            seen.get.flatMap(v => if p(v) then IO.unit else IO.sleep(20.millis) >> go)
        go.timeoutTo(10.seconds, IO.raiseError(new AssertionError("the manager never got there")))
    }

    test("the handoff unregisters the liaisons after closing submissions and before stopping") {
        val (steps, deadLetters) = handOff(detach = true)
        val _ = assert(steps == Vector("unregister (submissions closed)", "stop"))
        assert(deadLetters == 0, "a pull after the handoff reached the stopped liaison")
    }

    test("control: without the unregister, a pull after the handoff is a dead letter") {
        val (steps, deadLetters) = handOff(detach = false)
        val _ = assert(steps == Vector("stop"))
        assert(deadLetters == 1)
    }
}
