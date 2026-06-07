package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.RequestSequencer.*
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import org.typelevel.log4cats.Logger

/** The first actor responsible for processing events from end-users, as received by the
  * [[HydrozoaServer]]. Only one request sequencer is running per node, specifically to handle
  * _only_ the events that will be tagged with this Peer's [[HeadPeerNumber]] and sequential
  * [[RequestId]]s.
  *
  * The messages are subsequently passed to the [[BlockWeaver]] and [[PeerLiaisonHeadToHead]]s.
  */
trait RequestSequencer(
    config: Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | RequestSequencer.Connections
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[RequestSequencer.Connections]](None)
    private val state = State()

    private given logger: Logger[IO] = Logging.loggerIO(s"RequestSequencer.${config.ownPeerLabel}")

    private def getConnections: IO[Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error(
              "Request sequencer is missing its connections to other actors."
            )
          )
        )(IO.pure)
    } yield conn

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                _connections <- x.get
                _ <- connections.set(
                  Some(
                    Connections(
                      blockWeaver = _connections.blockWeaver,
                      headPeerLiaisons = _connections.headPeerLiaisons,
                      coilRelay = _connections.coilRelay
                    )
                  )
                )
            } yield ()
        case x: RequestSequencer.Connections => connections.set(Some(x))
    }

    override def preStart: IO[Unit] = context.self ! RequestSequencer.PreStart

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] = req match {
        case RequestSequencer.PreStart => preStartLocal
        case req: UserRequest.Sync =>
            req.request.handleSync(
              req,
              (userRequest: UserRequest) =>
                  for {
                      conn <- getConnections
                      newNum <- state.nextLedgerEventNum()
                      // The user-request surface is head-only, so the author is always a head peer.
                      ownHeadPeerNum <- config.ownPeerId match {
                          case PeerId.Head(n) => IO.pure(n)
                          case PeerId.Coil(_) =>
                              IO.raiseError(
                                new IllegalStateException(
                                  "RequestSequencer runs only on a head peer"
                                )
                              )
                      }
                      newId = RequestId(ownHeadPeerNum, newNum)
                      newRequestWithId = UserRequestWithId(
                        userRequest = userRequest,
                        requestId = newId
                      )
                      _ <- logger.debug(
                        s"Assigned request ID (${newId.peerNum}:${newId.requestNum})"
                      )
                      _ <- req.dResponse.complete(newId)
                      _ <- conn.blockWeaver ! newRequestWithId
                      // To the head-peer mesh, and (on a hub) to CoilRelay so its coil peers get the
                      // request content they need to reproduce block bodies.
                      _ <- (conn.headPeerLiaisons ! newRequestWithId).parallel
                      _ <- conn.coilRelay.traverse_(_ ! newRequestWithId)
                  } yield newId
            )
    }

    private def preStartLocal: IO[Unit] = initializeConnections

    private final class State {
        private val nLedgerEvent = Ref.unsafe[IO, RequestNumber](RequestNumber(0))

        def nextLedgerEventNum(): IO[RequestNumber] =
            nLedgerEvent.getAndUpdate(_.increment)
    }
}

/** Request sequencer receives local submissions of users' requests (via an http server), assigns
  * ledger event ids and emits them sequentially into the consensus system.
  */
object RequestSequencer {
    def apply(
        config: Config,
        pendingConnections: MultisigRegimeManager.PendingConnections
    ): IO[RequestSequencer] =
        IO(new RequestSequencer(config, pendingConnections) {})

    type Config = OwnPeerPublic.Section

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        headPeerLiaisons: List[liaison.PeerLiaisonHeadToHead.Handle],
        /** A hub's coil relay (§8.3): this peer's own requests are sent here so its coil peers get
          * the request content. `None` off a hub.
          */
        coilRelay: Option[CoilRelay.Handle] = None
    )

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | UserRequest.Sync

    case object PreStart
}
