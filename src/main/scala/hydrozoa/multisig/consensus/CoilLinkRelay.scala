package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.CoilLinkRelay.*
import hydrozoa.multisig.consensus.ack.{HardAck, RelayedMsg, RelayedMsgNumber, SoftAck}
import org.typelevel.log4cats.Logger

/** The hub-side relay that fans the **whole population's** multiplexed consensus stream down to a
  * hub's coil peers (§8 "Hub→coil link lane encoding" of `design/coil-network.md`).
  *
  * Three of the hub's actors tee here: its `FastConsensusActor` every soft-ack (fast side), its
  * `SlowConsensusActor` every hard-ack (slow side; head peer and coil peer alike), and its
  * `BlockWeaver` every user request. Each is stamped with a monotonic hub-local
  * [[RelayedMsgNumber]] and fanned, wrapped as a [[RelayedMsg]], onto the coil-ward liaisons'
  * contiguous `relayedMsg` lane. The sequence number is transport ordering only — the embedded
  * signed artifact still carries its own author, so each coil peer verifies it and **routes by type
  * + author** (`Soft`→FCA, `Hard`→SCA, `Req`→BlockWeaver — the de-mux into the same per-author lane
  * structure a head peer keeps).
  *
  * In-memory only for now (durable resume index deferred to coil-persistence), exactly like
  * [[CoilAckSequencer]].
  */
trait CoilLinkRelay(
    config: Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | CoilLinkRelay.Connections
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[CoilLinkRelay.Connections]](None)
    private val nextSeq = Ref.unsafe[IO, RelayedMsgNumber](RelayedMsgNumber.zero)

    private given logger: Logger[IO] = Logging.loggerIO(s"CoilLinkRelay.${config.ownPeerLabel}")

    private def getConnections: IO[Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error("Coil-link relay is missing its connections to other actors.")
          )
        )(IO.pure)
    } yield conn

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            x.get.flatMap(c =>
                connections.set(Some(Connections(coilPeerLiaisons = c.coilPeerLiaisons)))
            )
        case x: CoilLinkRelay.Connections => connections.set(Some(x))
    }

    override def preStart: IO[Unit] = context.self ! CoilLinkRelay.PreStart

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] = req match {
        case CoilLinkRelay.PreStart     => initializeConnections
        case ack: SoftAck               => relay(seq => RelayedMsg.Soft(seq, ack))
        case ack: HardAck               => relay(seq => RelayedMsg.Hard(seq, ack))
        case request: UserRequestWithId => relay(seq => RelayedMsg.Req(seq, request))
    }

    /** Stamp the next sequence number and fan the wrapped ack to the coil-ward liaisons. */
    private def relay(wrap: RelayedMsgNumber => RelayedMsg): IO[Unit] =
        for {
            conn <- getConnections
            seq <- nextSeq.getAndUpdate(_.increment)
            relayed = wrap(seq)
            _ <- logger.debug(
              s"relaying ack as seq $seq"
            ) >> (conn.coilPeerLiaisons ! relayed).parallel
        } yield ()
}

object CoilLinkRelay {
    def apply(
        config: Config,
        pendingConnections: MultisigRegimeManager.PendingConnections
    ): IO[CoilLinkRelay] =
        IO(new CoilLinkRelay(config, pendingConnections) {})

    type Config = OwnPeerPublic.Section

    final case class Connections(coilPeerLiaisons: List[PeerLiaison.Handle])

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | SoftAck | HardAck | UserRequestWithId

    case object PreStart
}
