package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.contravariant.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.actor.{HydrozoaActorSystem, Quiesce}
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.NoopActor
import hydrozoa.multisig.consensus.liaison.BatchMessages.OwnHardAck
import hydrozoa.multisig.consensus.liaison.{LiaisonProtocol, PeerLiaisonEventFormat, PeerLiaisonHubToCoil}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.persistence.{InMemoryBackendStore, Persistence, PersistenceEventFormat}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** A liaison re-sends its outstanding pull on a timer. Once quiesced it re-sends nothing, so it can
  * be stopped with no `ResendCurrent` of its own in flight.
  */
class LiaisonQuiesceTest extends AnyFunSuite {

    private val env: MultiNodeConfig =
        MultiNodeConfig
            .generateWithCoil(nCoil = 1, quorum = 1)
            .pureApply(Gen.Parameters.default, Seed(0L))

    private val hubNum = HeadPeerNumber(0)
    private val coilNum = CoilPeerNumber(0)
    private val hubConfig: NodeConfig = env.nodeConfigs(hubNum)

    private given CardanoNetwork.Section = hubConfig

    private class Recorder(seen: Ref[IO, Vector[LiaisonProtocol.CoilLiaisonMessage]])
        extends Actor[IO, LiaisonProtocol.CoilLiaisonMessage] {
        override def receive: Receive[IO, LiaisonProtocol.CoilLiaisonMessage] =
            PartialFunction.fromFunction(r => seen.update(_ :+ r))
    }

    /** Start a hub→coil liaison, quiesce it at once if `quiesce`, and return the pulls it sent the
      * coil over a little more than one resend interval.
      */
    private def pullsSent(quiesce: Boolean): Vector[OwnHardAck.Get] = {
        val persistenceTracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(persistenceTracer)
            .use { backend =>
                Persistence.fromBackend(backend, persistenceTracer).flatMap { persistence =>
                    HydrozoaActorSystem.withoutRoot("liaison-quiesce-test").use { system =>
                        for {
                            seen <- Ref[IO].of(Vector.empty[LiaisonProtocol.CoilLiaisonMessage])
                            remote <- system.actorOf(new Recorder(seen))
                            slow <- system.actorOf(NoopActor[SlowConsensusActor.Request])
                            sequencer <- system.actorOf(NoopActor[CoilAckSequencer.Request])
                            liaison <- system.actorOf(
                              PeerLiaisonHubToCoil(
                                hubConfig,
                                coilNum,
                                PeerLiaisonHubToCoil.Connections(slow, sequencer, remote),
                                Slf4jTracer.sink.contramap(
                                  PeerLiaisonEventFormat
                                      .humanFormat(PeerId.Head(hubNum), PeerId.Coil(coilNum))
                                ),
                                persistence,
                                _ => IO.pure(CoilStartPoint.CatchUp)
                              )
                            )
                            _ <- IO.whenA(quiesce)(liaison ! Quiesce)
                            _ <- IO.sleep(hubConfig.peerLiaisonResendInterval + 2.seconds)
                            out <- seen.get
                        } yield out.collect { case g: OwnHardAck.Get => g }
                    }
                }
            }
            .unsafeRunSync()
    }

    test("a quiesced liaison re-sends nothing") {
        val gets = pullsSent(quiesce = true)
        assert(gets.size == 1, s"expected only the pull sent at start, got $gets")
    }

    test("control: a liaison that is not quiesced re-sends its pull") {
        val gets = pullsSent(quiesce = false)
        assert(gets.size >= 2, s"expected the pull sent at start and a re-send, got $gets")
    }
}
