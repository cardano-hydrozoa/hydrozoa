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
    private val state = State()

    private given env: Env = Env(config, l2Screener, tracer, persistence, metrics)

    // The user-request surface is head-only, so the author is always a head peer.
    private val ownHeadPeerNum: HeadPeerNumber =
        config.ownPeerId.expectHead("RequestSequencer runs only on a head peer")

    private def initializeConnections: IO[Env.Connected] = {
        val connections: IO[RequestSequencer.Connections] =
            HeadMultisigRegimeManager.resolveConnections(pendingConnections)(c =>
                Connections(
                  blockWeaver = c.blockWeaver,
                  headPeerLiaisons = c.headPeerLiaisons,
                  coilRelay = c.coilRelay
                )
            )
        connections.map(env.connected)
    }

    override def preStart: IO[Unit] = context.self ! RequestSequencer.PreStart

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction {
        case RequestSequencer.PreStart =>
            for {
                // Suspends on the start barrier, so connections are in place before any real
                // message is processed.
                given Env.Connected <- initializeConnections
                _ <- preStartLocal
                _ <- context.become(PartialFunction.fromFunction(receiveConnected))
            } yield ()
        case x =>
            IO.raiseError(RuntimeException(s"Unexpected message received before PreStart: $x"))
    }

    private def receiveConnected(req: Request)(using Env.Connected): IO[Unit] = req match {
        case RequestSequencer.PreStart =>
            IO.raiseError(RuntimeException("Unexpected duplicate PreStart"))
        case hw: SoftConfirmedHighWater =>
            // Advance this peer's own confirmed request high-water (merge by max); backpressure
            // reads it. A block that carries none of this peer's requests leaves it unchanged. The
            // advance frees headroom, so refresh the reported window.
            hw.highWater.get(ownHeadPeerNum).traverse_(state.advanceConfirmedHighWater) >>
                reportBackpressure(config.backpressureCoefficient * config.maxRequestsPerBlock)
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
                          // Backpressure: refuse to author more than `backpressureCoefficient`
                          // blocks' worth of requests beyond this peer's own confirmed high-water
                          // before shedding load. The mesh pull ceiling scales by the same coefficient
                          // (PeerLiaisonHeadToHead), so followers can always pull what a leader packs —
                          // including its prioritized own requests (docs/spec/fast-consensus.md).
                          val window =
                              config.backpressureCoefficient * config.maxRequestsPerBlock
                          state.tryNextRequestNum(window).flatMap {
                              case None =>
                                  IO(metrics.onLocalRejected(RejectionKind.Backpressure)) *>
                                      reportBackpressure(window) *>
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
                                  val conn = summon[Env.Connected].connections
                                  for {
                                      _ <- tracer.traceWith(
                                        EventSequencerEvent
                                            .RequestIdAssigned(newId.peerNum, newId.requestNum)
                                      )
                                      _ <- IO(metrics.onLocalAccepted())
                                      _ <- reportBackpressure(window)
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
            // R3: continue the request counter from `max(own Request) + 1` (CR3, no re-issue);
            // empty store -> RequestNumber(0), the same cold value.
            next <- Markers.recoverNextRequestNumber(persistence.backend, ownHeadPeerNum)
            _ <- state.seedNextRequestNum(next)
            // Seed the confirmed high-water from the highest already-assigned own request (next - 1).
            // Treating assigned-as-confirmed opens the backpressure window optimistically after a
            // restart; the next real soft-confirmation re-tightens it.
            _ <- state.seedConfirmedHighWater(next.previousOrZero)
            _ <- reportBackpressure(config.backpressureCoefficient * config.maxRequestsPerBlock)
        } yield ()

    /** Publish the current backpressure headroom (space left in the
      * `backpressureCoefficient * maxRequestsPerBlock` window) to [[metrics]] — see
      * `docs/spec/peer-stats-endpoint.md`.
      */
    private def reportBackpressure(window: Int): IO[Unit] =
        state.backpressureHeadroom(window).flatMap(h => IO(metrics.onSequencerHeadroom(h)))

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

        /** Requests admittable right now before backpressure trips, for window `maxAhead` (`ceiling -
          * next + 1`, floored at 0).
          */
        def backpressureHeadroom(maxAhead: Int): IO[Long] =
            for {
                confirmed <- ownConfirmedHighWaterRef.get
                next <- nextRequestNumRef.get
            } yield math.max(0L, ((confirmed: Long) + maxAhead) - (next: Long) + 1L)

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

    private final case class Env(
        config: Config,
        l2Screener: L2Screener[IO],
        tracer: ContraTracer[IO, EventSequencerEvent],
        persistence: Persistence[IO],
        metrics: PeerMetrics
    ) extends CardanoNetwork.Section {
        def cardanoNetwork: CardanoNetwork = config.cardanoNetwork
        def connected(connections: Connections): Env.Connected = Env.Connected(this, connections)
    }

    private object Env {

        final class Connected(env: Env, val connections: Connections)
            extends CardanoNetwork.Section {
            export env.*
        }
    }
}
