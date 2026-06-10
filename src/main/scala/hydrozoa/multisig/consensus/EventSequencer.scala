package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.EventSequencer.*
import hydrozoa.multisig.consensus.PeerLiaison.Handle
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.persistence.{LaneKey, LaneValue, Persistence, WriteBatch}

/** The first actor responsible for processing events from end-users, as received by the
  * [[HydrozoaServer]]. Only one event sequencer is running per node, specifically to handle _only_
  * the events that will be tagged with this Peer's [[HeadPeerNumber]] and sequential
  * [[RequestId]]s.
  *
  * The messages are subsequently passed to the [[BlockWeaver]] and [[PeerLiaison]]s.
  *
  * TODO: rename to RequestSequencer (and EventSequencer companion object accordingly).
  */
trait EventSequencer(
    config: Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | EventSequencer.Connections,
    tracer: ContraTracer[IO, EventSequencerEvent],
    persistence: Persistence[IO]
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[EventSequencer.Connections]](None)
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
              "Event sequencer is missing its connections to other actors."
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
                      peerLiaisons = _connections.peerLiaisons
                    )
                  )
                )
            } yield ()
        case x: EventSequencer.Connections => connections.set(Some(x))
    }

    override def preStart: IO[Unit] = context.self ! EventSequencer.PreStart

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] = req match {
        case EventSequencer.PreStart => preStartLocal
        case req: UserRequest.Sync =>
            req.request.handleSync(
              req,
              (userRequest: UserRequest) =>
                  for {
                      conn <- getConnections
                      newNum <- state.nextLedgerEventNum()
                      newId = RequestId(config.ownHeadPeerId.peerNum, newNum)
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
                            .put(LaneKey.Request(config.ownHeadPeerNum, newNum))(
                              LaneValue(stamp, newRequestWithId)
                            )
                      )
                      _ <- req.dResponse.complete(newId)
                      _ <- conn.blockWeaver ! newRequestWithId
                      _ <- (conn.peerLiaisons ! newRequestWithId).parallel
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

/** Event sequencer receives local submissions of users' requests (via an http server), assigns
  * ledger event ids and emits them sequentially into the consensus system.
  */
object EventSequencer {
    def apply(
        config: Config,
        pendingConnections: MultisigRegimeManager.PendingConnections,
        tracer: ContraTracer[IO, EventSequencerEvent],
        persistence: Persistence[IO]
    ): IO[EventSequencer] =
        IO(new EventSequencer(config, pendingConnections, tracer, persistence) {})

    // `& CardanoNetwork.Section`: the Request-lane codec (UserRequestWithId) is Section-dependent;
    // the full configs passed in satisfy it.
    type Config = OwnHeadPeerPublic.Section & CardanoNetwork.Section

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        peerLiaisons: List[PeerLiaison.Handle]
    )

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | UserRequest.Sync

    case object PreStart
}
