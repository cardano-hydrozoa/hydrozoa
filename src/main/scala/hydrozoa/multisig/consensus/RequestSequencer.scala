package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager
import hydrozoa.multisig.consensus.RequestSequencer.*
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.persistence.{JournalKey, JournalValue, Markers, Persistence, WriteBatch}

/** The first actor responsible for processing events from end-users, as received by the
  * [[HydrozoaServer]]. Only one request sequencer is running per node, specifically to handle
  * _only_ the events that will be tagged with this Peer's [[HeadPeerNumber]] and sequential
  * [[RequestId]]s.
  *
  * The messages are subsequently passed to the [[BlockWeaver]] and [[PeerLiaisonHeadToHead]]s.
  */
trait RequestSequencer(
    config: Config,
    pendingConnections: HeadMultisigRegimeManager.PendingConnections | RequestSequencer.Connections,
    tracer: ContraTracer[IO, EventSequencerEvent],
    persistence: Persistence[IO]
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[RequestSequencer.Connections]](None)
    private val state = State()

    /** `config` is a `CardanoNetwork.Section`; expose it as a given so the typed `Request`-lane
      * `WriteBatch.put` (the CR1 persist) picks it up.
      */
    private given CardanoNetwork.Section = config

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
        case x: HeadMultisigRegimeManager.PendingConnections =>
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
                      newNum <- state.nextRequestNum()
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
                      _ <- tracer.traceWith(
                        EventSequencerEvent.RequestIdAssigned(newId.peerNum, newId.requestNum)
                      )
                      // CR1: persist the assigned request to the Request lane BEFORE telling the
                      // user the id (the id is durable before it is observable; §4 CR1/CR4).
                      stamp <- persistence.arrivalStamp
                      _ <- persistence.write(
                        WriteBatch.start
                            .put(JournalKey.Request(ownHeadPeerNum, newNum))(
                              JournalValue(stamp, newRequestWithId)
                            )
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

    private def preStartLocal: IO[Unit] =
        for {
            _ <- initializeConnections
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
            // R3: continue the request counter from `max(own Request) + 1` (CR3, no re-issue);
            // empty store -> RequestNumber(0), the same cold value.
            next <- Markers.recoverNextRequestNumber(persistence.backend, ownHeadPeerNum)
            _ <- state.seedNextRequestNum(next)
        } yield ()

    private final class State {
        private val nextRequestNumRef = Ref.unsafe[IO, RequestNumber](RequestNumber(0))

        def nextRequestNum(): IO[RequestNumber] =
            nextRequestNumRef.getAndUpdate(_.increment)

        /** Seed the next-to-assign request number on recovery (R3). */
        def seedNextRequestNum(next: RequestNumber): IO[Unit] =
            nextRequestNumRef.set(next)
    }
}

/** Request sequencer receives local submissions of users' requests (via an http server), assigns
  * ledger event ids and emits them sequentially into the consensus system.
  */
object RequestSequencer {
    def apply(
        config: Config,
        pendingConnections: HeadMultisigRegimeManager.PendingConnections,
        tracer: ContraTracer[IO, EventSequencerEvent],
        persistence: Persistence[IO]
    ): IO[RequestSequencer] =
        IO(new RequestSequencer(config, pendingConnections, tracer, persistence) {})

    // `& CardanoNetwork.Section`: the Request-lane codec (UserRequestWithId) is Section-dependent;
    // the full configs passed in satisfy it.
    type Config = OwnPeerPublic.Section & CardanoNetwork.Section

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        headPeerLiaisons: List[liaison.PeerLiaisonHeadToHead.Handle],
        /** A hub's coil relay (§5.4) [doc-ref]: this peer's own requests are sent here so its coil
          * peers get the request content. `None` off a hub.
          */
        coilRelay: Option[CoilRelay.Handle] = None
    )

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | UserRequest.Sync

    case object PreStart
}
