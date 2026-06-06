package hydrozoa.multisig.consensus.liaison

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, HardAckWithId, HubHardAckNumber, SoftAck, SoftAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.{OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol.*
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.consensus.{BlockWeaver, FastConsensusActor, SlowConsensusActor, StackComposer, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import org.typelevel.log4cats.Logger

/** A coil peer's single liaison toward its hub head peer (§8 of `design/coil-network.md`).
  *
  * Asymmetric: it **pulls the full population** from the hub (block + stack spines, per-head-peer
  * request / soft-ack / head-hard-ack lanes, per-hub coil-hard-ack lanes) and **serves only its own
  * hard-ack** to the hub. Built by composition — a [[Puller]] over the inbound population lanes and
  * a [[Server]] over the single outbound own-hard-ack lane — with no fat shared base and no on-wire
  * author check (each lane is keyed by its author, and signatures are verified end-to-end in the
  * local consensus actors).
  */
abstract class PeerLiaisonCoilToHub(
    config: PeerLiaisonCoilToHub.Config,
    hubHead: HeadPeerId,
    pendingConnections: MultisigRegimeManager.PendingConnections | PeerLiaisonCoilToHub.Connections
) extends Actor[IO, LiaisonProtocol.CoilToHubRequest] {
    import PeerLiaisonCoilToHub.Config

    /** Resolve connections — projected from the shared regime `Connections` (the hub's `HubToCoil`
      * handle from `remoteHubLiaison`) or supplied directly.
      */
    private def resolveConnections: IO[PeerLiaisonCoilToHub.Connections] =
        pendingConnections match {
            case shared: MultisigRegimeManager.PendingConnections =>
                shared.get.flatMap(s =>
                    s.remoteHubLiaison.fold(
                      IO.raiseError(
                        java.lang.Error("Coil→hub liaison requires a hub liaison handle.")
                      )
                    )(hub =>
                        IO.pure(
                          PeerLiaisonCoilToHub.Connections(
                            blockWeaver = s.blockWeaver,
                            consensusActor = s.consensusActor,
                            stackComposer = s.stackComposer,
                            slowConsensusActor = s.slowConsensusActor,
                            remoteHub = hub
                          )
                        )
                    )
                )
            case own: PeerLiaisonCoilToHub.Connections => IO.pure(own)
        }

    private given logger: Logger[IO] =
        Logging.loggerIO(s"PeerLiaison.${config.ownPeerLabel}->${hubHead.peerNum.convert}")

    private val headPeerNums: List[HeadPeerNumber] = config.headPeerNums.toList
    private val hubNums: List[HeadPeerNumber] = config.coilPeers.map(_.hub).distinct

    // ---- Lanes ----------------------------------------------------------------------------------
    // Inbound population (pulled from the hub): block + stack spines are contiguous (the hub relays
    // every item in order; block 0 / stack 0 are out-of-band bootstrap so the first is 1). The
    // per-author / per-hub families are one contiguous Lane each.
    private val blockLane =
        Lane.contiguous[BlockBrief.Next, BlockNumber](_.blockNum, BlockNumber(1), _.increment)
    private val stackLane =
        Lane.contiguous[StackBrief, StackNumber](_.stackNum, StackNumber(1), _.increment)
    private val requestLanes: Map[HeadPeerNumber, Lane[UserRequestWithId, RequestNumber]] =
        headPeerNums.map { h =>
            h -> Lane.contiguous[UserRequestWithId, RequestNumber](
              _.requestId.requestNum,
              RequestNumber.zero,
              _.increment,
              config.peerLiaisonMaxRequestsPerBatch
            )
        }.toMap
    private val softAckLanes: Map[HeadPeerNumber, Lane[SoftAck, SoftAckNumber]] =
        headPeerNums.map { h =>
            h -> Lane.contiguous[SoftAck, SoftAckNumber](
              _.ackNum,
              SoftAckNumber.zero.increment,
              _.increment
            )
        }.toMap
    private val headHardAckLanes: Map[HeadPeerNumber, Lane[HardAck, HardAckNumber]] =
        headPeerNums.map { h =>
            h -> Lane.contiguous[HardAck, HardAckNumber](
              _.hardAckNum,
              HardAckNumber.zero,
              _.increment
            )
        }.toMap
    private val coilHardAckLanes: Map[HeadPeerNumber, Lane[HardAckWithId, HubHardAckNumber]] =
        hubNums.map { h =>
            h -> Lane.contiguous[HardAckWithId, HubHardAckNumber](
              _.seqNum,
              HubHardAckNumber.zero,
              _.increment
            )
        }.toMap

    // Outbound: this coil peer's own hard-ack, served to the hub.
    private val ownHardAckLane =
        Lane.contiguous[HardAck, HardAckNumber](_.hardAckNum, HardAckNumber.zero, _.increment)

    // ---- Connections ----------------------------------------------------------------------------
    private val connections =
        Ref.unsafe[IO, Option[PeerLiaisonCoilToHub.Connections]](None)

    private def getConnections: IO[PeerLiaisonCoilToHub.Connections] =
        connections.get.flatMap(
          _.fold(IO.raiseError(java.lang.Error("Coil→hub liaison missing its connections.")))(
            IO.pure
          )
        )

    // ---- Pull half (population) -----------------------------------------------------------------
    private val initialGet: Population.Get = Population.Get(
      batchNum = BatchNumber.zero,
      block = BlockNumber(1),
      stack = StackNumber(1),
      requests = headPeerNums.map(_ -> RequestNumber.zero).toMap,
      softAcks = headPeerNums.map(_ -> SoftAckNumber.zero.increment).toMap,
      headHardAcks = headPeerNums.map(_ -> HardAckNumber.zero).toMap,
      coilHardAcks = hubNums.map(_ -> HubHardAckNumber.zero).toMap
    )

    private def buildGet(batchNum: BatchNumber): IO[Population.Get] =
        for {
            b <- blockLane.cursor
            s <- stackLane.cursor
            r <- requestLanes.toList.traverse { case (h, l) => l.cursor.map(h -> _) }.map(_.toMap)
            sa <- softAckLanes.toList.traverse { case (h, l) => l.cursor.map(h -> _) }.map(_.toMap)
            hh <- headHardAckLanes.toList
                .traverse { case (h, l) => l.cursor.map(h -> _) }
                .map(_.toMap)
            ch <- coilHardAckLanes.toList
                .traverse { case (h, l) => l.cursor.map(h -> _) }
                .map(_.toMap)
        } yield Population.Get(batchNum, b, s, r, sa, hh, ch)

    /** Verify every lane against its cursor; iff all match, advance them all (atomic). */
    private def accept(pop: Population.New): IO[Either[String, Unit]] = {
        def check[T, N](lane: Lane[T, N], items: List[T]): IO[Either[String, IO[Unit]]] =
            lane.cursor.map(c =>
                lane.verify(items, c) match {
                    case Right(next) => Right(lane.advanceTo(next))
                    case Left(m)     => Left(m.toString)
                }
            )
        val checks: List[IO[Either[String, IO[Unit]]]] =
            check(blockLane, pop.block.toList) ::
                check(stackLane, pop.stack.toList) ::
                requestLanes.toList.map { case (h, l) =>
                    check(l, pop.requests.getOrElse(h, Nil))
                } :::
                softAckLanes.toList.map { case (h, l) =>
                    check(l, pop.softAcks.getOrElse(h, None).toList)
                } :::
                headHardAckLanes.toList.map { case (h, l) =>
                    check(l, pop.headHardAcks.getOrElse(h, None).toList)
                } :::
                coilHardAckLanes.toList.map { case (h, l) =>
                    check(l, pop.coilHardAcks.getOrElse(h, None).toList)
                }
        checks.sequence.flatMap { results =>
            val (lefts, advances) = results.partitionMap(identity)
            lefts.headOption match {
                case Some(reason) => IO.pure(Left(reason))
                case None         => advances.sequence_.as(Right(()))
            }
        }
    }

    /** Route a verified population reply to the local consensus actors. */
    private def dispatch(pop: Population.New): IO[Unit] =
        getConnections.flatMap { conn =>
            for {
                _ <- pop.block.traverse_(conn.blockWeaver ! _)
                _ <- pop.stack.traverse_(conn.stackComposer ! _)
                _ <- pop.requests.values.toList.flatten.traverse_(conn.blockWeaver ! _)
                _ <- pop.softAcks.values.toList.flatten.traverse_(conn.consensusActor ! _)
                _ <- pop.headHardAcks.values.toList.flatten.traverse_(conn.slowConsensusActor ! _)
                _ <- pop.coilHardAcks.values.toList.flatten.traverse_(hc =>
                    conn.slowConsensusActor ! hc.ack
                )
            } yield ()
        }

    private val puller = new Puller[Population.Get, Population.New](
      initialGet = initialGet,
      buildGet = buildGet,
      accept = accept,
      dispatch = dispatch,
      getBatchNum = _.batchNum,
      newBatchNum = _.batchNum
    )(g => getConnections.flatMap(_.remoteHub ! g))

    // ---- Serve half (own hard-ack) --------------------------------------------------------------
    private def serve(get: OwnHardAck.Get): IO[BatchLink.Served[OwnHardAck.New]] =
        ownHardAckLane.reply(get.hardAck).map {
            case Lane.OutOfBounds => BatchLink.Served.OutOfBounds
            case Lane.Items(Nil)  => BatchLink.Served.Empty
            case Lane.Items(items) =>
                BatchLink.Served.Reply(OwnHardAck.New(get.batchNum, items.headOption))
        }

    private val server = new Server[OwnHardAck.Get, OwnHardAck.New](serve)(n =>
        getConnections.flatMap(_.remoteHub ! n)
    )

    // ---- Actor shell ----------------------------------------------------------------------------
    override def preStart: IO[Unit] = context.self ! PreStart

    override def receive: Receive[IO, CoilToHubRequest] =
        PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: CoilToHubRequest): IO[Unit] = req match {
        case PreStart            => preStartLocal
        case ResendCurrent       => puller.resend
        case pop: Population.New => puller.handleReply(pop)
        case get: OwnHardAck.Get => server.handleGet(get)
        case ack: HardAck        => ownHardAckLane.append(ack) >> server.afterAppend
    }

    private def preStartLocal: IO[Unit] =
        for {
            c <- resolveConnections
            _ <- connections.set(Some(c))
            _ <- logger.info(s"starting, hub ${hubHead.peerNum.convert}")
            _ <- puller.start
            _ <- startResendTimer
        } yield ()

    private def startResendTimer: IO[Unit] =
        (IO.sleep(
          config.peerLiaisonResendInterval
        ) >> (context.self ! ResendCurrent)).foreverM.start.void
}

object PeerLiaisonCoilToHub {
    def apply(
        config: Config,
        hubHead: HeadPeerId,
        pendingConnections: MultisigRegimeManager.PendingConnections | Connections
    ): IO[PeerLiaisonCoilToHub] =
        IO(new PeerLiaisonCoilToHub(config, hubHead, pendingConnections) {})

    type Config =
        OwnPeerPublic.Section & NodeOperationMultisigConfig.Section & HeadConfig.Bootstrap.Section

    type Handle = ActorRef[IO, LiaisonProtocol.CoilToHubRequest]

    /** The local actors a verified population reply routes to, plus the send path to the hub's
      * counterpart liaison.
      */
    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        consensusActor: FastConsensusActor.Handle,
        stackComposer: StackComposer.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        remoteHub: LiaisonProtocol.HubToCoilHandle
    )
}
