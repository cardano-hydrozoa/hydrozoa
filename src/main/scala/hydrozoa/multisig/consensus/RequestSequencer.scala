package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.block.BlockConfig
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager
import hydrozoa.multisig.consensus.RequestSequencer.*
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.l1.tx.DepositL1Screening
import hydrozoa.multisig.ledger.l2.L2Screener
import hydrozoa.multisig.metrics.{PeerMetrics, RejectionKind}
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
    l2Screener: L2Screener[IO],
    tracer: ContraTracer[IO, EventSequencerEvent],
    persistence: Persistence[IO],
    metrics: PeerMetrics
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[RequestSequencer.Connections]](None)
    private val state = State()

    // The user-request surface is head-only, so the author is always a head peer.
    private val ownHeadPeerNum: HeadPeerNumber = config.ownPeerId match {
        case PeerId.Head(n) => n
        case PeerId.Coil(_) =>
            throw new IllegalStateException("RequestSequencer runs only on a head peer")
    }

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
        case RequestSequencer.PreStart  => preStartLocal
        case hw: SoftConfirmedHighWater =>
            // Advance this peer's own confirmed request high-water (merge by max); backpressure
            // reads it. A block that carries none of this peer's requests leaves it unchanged.
            hw.highWater.get(ownHeadPeerNum).traverse_(state.advanceConfirmedHighWater)
        case req: UserRequest.Sync =>
            req.request.handleSync(
              req,
              (userRequest: UserRequest) => {
                  // Screening (docs/spec/l2-isomorphism.md): decide whether this request is worth a
                  // RequestId. On a No, reject before assigning one — no id, no CR1 persist, no
                  // consensus fan-out. A transaction goes straight to the ledger (it
                  // self-authenticates through its own witnesses); a deposit first runs Hydrozoa's
                  // L1 screening (the l2Payload pin check + the accept-by gate), then the ledger's
                  // value checks.
                  val screened: IO[Either[String, Unit]] = userRequest.body match {
                      case UserRequestBody.TransactionRequestBody(l2Payload) =>
                          l2Screener.screenTx(l2Payload).value.map(_.left.map(_.message))
                      case UserRequestBody.DepositRequestBody(l1Payload, l2Payload) =>
                          realTimeQuantizedInstant(config.slotConfig).flatMap { now =>
                              DepositL1Screening.screen(l1Payload, l2Payload, now)(
                                config
                              ) match {
                                  case Left(e) => IO.pure(Left(e.toString))
                                  case Right(screenDeposit) =>
                                      l2Screener
                                          .screenDeposit(screenDeposit)
                                          .value
                                          .map(_.left.map(_.message))
                              }
                          }
                  }
                  screened.flatMap {
                      case Left(reason) =>
                          IO(metrics.onLocalRejected(RejectionKind.Screening)) *>
                              IO.pure(Left(UserRequest.Rejected(reason)))
                      case Right(()) =>
                          // Backpressure: refuse to author more than one block's worth of requests
                          // beyond this peer's own confirmed high-water, so the mesh mempool cannot
                          // exceed maxRequestsPerBlock * nHeadPeers (docs/spec/fast-consensus.md).
                          state.tryNextRequestNum(config.maxRequestsPerBlock).flatMap {
                              case None =>
                                  IO(metrics.onLocalRejected(RejectionKind.Backpressure)) *>
                                      IO.pure(
                                        Left(
                                          UserRequest.Rejected(
                                            "too many unconfirmed requests (backpressure);" +
                                                " retry shortly."
                                          )
                                        )
                                      )
                              case Some(newNum) =>
                                  val newId = RequestId(ownHeadPeerNum, newNum)
                                  val newRequestWithId = UserRequestWithId(
                                    userRequest = userRequest,
                                    requestId = newId
                                  )
                                  for {
                                      conn <- getConnections
                                      _ <- tracer.traceWith(
                                        EventSequencerEvent
                                            .RequestIdAssigned(newId.peerNum, newId.requestNum)
                                      )
                                      _ <- IO(metrics.onLocalAccepted())
                                      // CR1: persist the assigned request to the Request lane BEFORE
                                      // telling the user the id (durable before observable; CR1/CR4).
                                      stamp <- persistence.arrivalStamp
                                      _ <- persistence.write(
                                        WriteBatch.start
                                            .put(JournalKey.Request(ownHeadPeerNum, newNum))(
                                              JournalValue(stamp, newRequestWithId)
                                            )
                                      )
                                      _ <- conn.blockWeaver ! newRequestWithId
                                      // To the head-peer mesh, and (on a hub) to CoilRelay so its coil
                                      // peers get the content they need to reproduce block bodies.
                                      _ <- (conn.headPeerLiaisons ! newRequestWithId).parallel
                                      _ <- conn.coilRelay.traverse_(_ ! newRequestWithId)
                                  } yield Right(newId)
                          }
                  }
              }
            )
    }

    private def preStartLocal: IO[Unit] =
        for {
            _ <- initializeConnections
            // R3: continue the request counter from `max(own Request) + 1` (CR3, no re-issue);
            // empty store -> RequestNumber(0), the same cold value.
            next <- Markers.recoverNextRequestNumber(persistence.backend, ownHeadPeerNum)
            _ <- state.seedNextRequestNum(next)
            // Seed the confirmed high-water from the highest already-assigned own request (next - 1).
            // Treating assigned-as-confirmed opens the backpressure window optimistically after a
            // restart; the next real soft-confirmation re-tightens it.
            _ <- state.seedConfirmedHighWater(next.previousOrZero)
        } yield ()

    private final class State {
        private val nextRequestNumRef = Ref.unsafe[IO, RequestNumber](RequestNumber(0))
        private val ownConfirmedHighWaterRef = Ref.unsafe[IO, RequestNumber](RequestNumber.zero)

        /** Assign the next request number, but only while it stays within `maxAhead` of this peer's
          * own confirmed high-water (backpressure). Returns None when that window is full, leaving
          * the counter untouched.
          */
        def tryNextRequestNum(maxAhead: Int): IO[Option[RequestNumber]] =
            ownConfirmedHighWaterRef.get.flatMap { confirmed =>
                val confirmedLong: Long = confirmed
                val ceiling = RequestNumber(confirmedLong + maxAhead)
                nextRequestNumRef.modify { cur =>
                    if Ordering[RequestNumber].lteq(cur, ceiling) then (cur.increment, Some(cur))
                    else (cur, None)
                }
            }

        /** Merge a confirmed high-water for this peer (by max). */
        def advanceConfirmedHighWater(confirmed: RequestNumber): IO[Unit] =
            ownConfirmedHighWaterRef.update(cur => Ordering[RequestNumber].max(cur, confirmed))

        /** Seed the next-to-assign request number on recovery (R3). */
        def seedNextRequestNum(next: RequestNumber): IO[Unit] =
            nextRequestNumRef.set(next)

        /** Seed the confirmed high-water on recovery. */
        def seedConfirmedHighWater(confirmed: RequestNumber): IO[Unit] =
            ownConfirmedHighWaterRef.set(confirmed)
    }
}

/** Request sequencer receives local submissions of users' requests (via an http server), assigns
  * ledger event ids and emits them sequentially into the consensus system.
  */
object RequestSequencer {
    def apply(
        config: Config,
        pendingConnections: HeadMultisigRegimeManager.PendingConnections,
        l2Screener: L2Screener[IO],
        tracer: ContraTracer[IO, EventSequencerEvent],
        persistence: Persistence[IO],
        metrics: PeerMetrics
    ): IO[RequestSequencer] =
        IO(
          new RequestSequencer(
            config,
            pendingConnections,
            l2Screener,
            tracer,
            persistence,
            metrics
          ) {}
        )

    // `& CardanoNetwork.Section`: the Request-lane codec (UserRequestWithId) is Section-dependent;
    // the full configs passed in satisfy it.
    // The sections beyond OwnPeerPublic/CardanoNetwork are what deposit L1 screening needs to
    // parse and time-gate a deposit tx (DepositL1Screening.Config).
    type Config = OwnPeerPublic.Section & CardanoNetwork.Section & HeadPeers.Section &
        InitialBlock.Section & TxTiming.Section & InitializationParameters.Section &
        FallbackContingency.Section & BlockConfig.Section

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        headPeerLiaisons: List[liaison.PeerLiaisonHeadToHead.Handle],
        /** A hub's coil relay (§5.4) [doc-ref]: this peer's own requests are sent here so its coil
          * peers get the request content. `None` off a hub.
          */
        coilRelay: Option[CoilRelay.Handle] = None
    )

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | UserRequest.Sync | SoftConfirmedHighWater

    case object PreStart
}
