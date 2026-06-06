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
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{CoilAckSequencer, SlowConsensusActor, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import org.typelevel.log4cats.Logger

/** A hub head peer's liaison toward one coil peer it serves (§8 of `design/coil-network.md`) — the
  * mirror of [[PeerLiaisonCoilToHub]].
  *
  * Asymmetric: it **serves the full population** down to the coil peer (block + stack spines,
  * per-head-peer request / soft-ack / head-hard-ack lanes, per-hub coil-hard-ack lanes — all outbox
  * lanes appended by `CoilRelay` / the hub's producers) and **pulls the coil peer's own hard-ack**,
  * which it routes to the hub's [[SlowConsensusActor]] (for quorum) and to [[CoilAckSequencer]]
  * (which re-sequences it onto the hub's `HubHardAckLane`).
  *
  * Built by composition: a [[Server]] over the population outbox lanes + a [[Puller]] over the one
  * inbound own-hard-ack lane. No fat base; appended artifacts are routed to the right per-author /
  * per-hub lane by their embedded author.
  */
abstract class PeerLiaisonHubToCoil(
    config: PeerLiaisonHubToCoil.Config,
    coil: CoilPeerNumber,
    pendingConnections: MultisigRegimeManager.PendingConnections | PeerLiaisonHubToCoil.Connections
) extends Actor[IO, LiaisonProtocol.HubToCoilRequest] {

    /** Resolve connections — projected from the shared regime `Connections` (remote coil handle
      * from the in-process `remoteCoilLiaisons` map) or supplied directly.
      */
    private def resolveConnections: IO[PeerLiaisonHubToCoil.Connections] =
        pendingConnections match {
            case shared: MultisigRegimeManager.PendingConnections =>
                shared.get.flatMap(s =>
                    s.coilAckSequencer.fold(
                      IO.raiseError(
                        java.lang.Error("Hub→coil liaison requires a CoilAckSequencer.")
                      )
                    )(seq =>
                        IO.pure(
                          PeerLiaisonHubToCoil.Connections(
                            slowConsensusActor = s.slowConsensusActor,
                            coilAckSequencer = seq,
                            remoteCoil = s.remoteCoilLiaisons(coil)
                          )
                        )
                    )
                )
            case own: PeerLiaisonHubToCoil.Connections => IO.pure(own)
        }

    private given logger: Logger[IO] =
        Logging.loggerIO(s"PeerLiaison.${config.ownPeerLabel}->c${coil.convert}")

    private val headPeerNums: List[HeadPeerNumber] = config.headPeerNums.toList
    private val hubNums: List[HeadPeerNumber] = config.coilPeers.map(_.hub).distinct

    // ---- Outbox lanes (the population we serve to the coil peer) ---------------------------------
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

    // ---- Inbound lane (the coil peer's own hard-ack we pull) ------------------------------------
    private val ownHardAckLane =
        Lane.contiguous[HardAck, HardAckNumber](_.hardAckNum, HardAckNumber.zero, _.increment)

    // ---- Connections ----------------------------------------------------------------------------
    private val connections = Ref.unsafe[IO, Option[PeerLiaisonHubToCoil.Connections]](None)

    private def getConnections: IO[PeerLiaisonHubToCoil.Connections] =
        connections.get.flatMap(
          _.fold(IO.raiseError(java.lang.Error("Hub→coil liaison missing its connections.")))(
            IO.pure
          )
        )

    // ---- Serve half (population) ----------------------------------------------------------------
    private def serve(get: Population.Get): IO[BatchLink.Served[Population.New]] =
        for {
            blockR <- blockLane.reply(get.block)
            stackR <- stackLane.reply(get.stack)
            reqR <- requestLanes.toList.traverse { case (h, l) =>
                l.reply(get.requests(h)).map(h -> _)
            }
            saR <- softAckLanes.toList.traverse { case (h, l) =>
                l.reply(get.softAcks(h)).map(h -> _)
            }
            hhR <- headHardAckLanes.toList.traverse { case (h, l) =>
                l.reply(get.headHardAcks(h)).map(h -> _)
            }
            chR <- coilHardAckLanes.toList.traverse { case (h, l) =>
                l.reply(get.coilHardAcks(h)).map(h -> _)
            }
        } yield {
            val all = blockR :: stackR :: (reqR ::: saR ::: hhR ::: chR).map(_._2)
            if all.contains(Lane.OutOfBounds) then BatchLink.Served.OutOfBounds
            else {
                def items[T](r: Lane.Reply[T]): List[T] = r match
                    case Lane.Items(xs)   => xs
                    case Lane.OutOfBounds => Nil
                val block = items(blockR).headOption
                val stack = items(stackR).headOption
                val requests = reqR.map { case (h, r) => h -> items(r) }.toMap
                val softAcks = saR.map { case (h, r) => h -> items(r).headOption }.toMap
                val headHardAcks = hhR.map { case (h, r) => h -> items(r).headOption }.toMap
                val coilHardAcks = chR.map { case (h, r) => h -> items(r).headOption }.toMap
                val anyContent =
                    block.nonEmpty || stack.nonEmpty || requests.values.exists(_.nonEmpty) ||
                        softAcks.values.exists(_.nonEmpty) || headHardAcks.values.exists(
                          _.nonEmpty
                        ) ||
                        coilHardAcks.values.exists(_.nonEmpty)
                if !anyContent then BatchLink.Served.Empty
                else
                    BatchLink.Served.Reply(
                      Population.New(
                        get.batchNum,
                        block,
                        stack,
                        requests,
                        softAcks,
                        headHardAcks,
                        coilHardAcks
                      )
                    )
            }
        }

    private val server = new Server[Population.Get, Population.New](serve)(n =>
        getConnections.flatMap(_.remoteCoil ! n)
    )

    /** Route an artifact relayed to this hub onto its outbox lane, keyed by embedded author. */
    private def appendArtifact(
        artifact: BlockBrief.Next | StackBrief | UserRequestWithId | SoftAck | HardAck |
            HardAckWithId
    ): IO[Unit] = artifact match {
        case b: BlockBrief.Next => blockLane.append(b)
        case s: StackBrief      => stackLane.append(s)
        case r: UserRequestWithId =>
            requestLanes.get(r.requestId.peerNum).traverse_(_.append(r))
        case sa: SoftAck => softAckLanes.get(sa.peerNum).traverse_(_.append(sa))
        case ha: HardAck =>
            ha.peerId match {
                case PeerId.Head(n) => headHardAckLanes.get(n).traverse_(_.append(ha))
                case PeerId.Coil(_) =>
                    IO.raiseError(
                      new IllegalStateException(
                        s"raw coil hard-ack appended to hub outbox: ${ha.peerId}"
                      )
                    )
            }
        case hub: HardAckWithId => coilHardAckLanes.get(hub.hubPeer).traverse_(_.append(hub))
    }

    // ---- Pull half (the coil peer's own hard-ack) -----------------------------------------------
    private val initialGet: OwnHardAck.Get = OwnHardAck.Get(BatchNumber.zero, HardAckNumber.zero)

    private def buildGet(batchNum: BatchNumber): IO[OwnHardAck.Get] =
        ownHardAckLane.cursor.map(OwnHardAck.Get(batchNum, _))

    private def accept(own: OwnHardAck.New): IO[Either[String, Unit]] =
        ownHardAckLane.cursor
            .map(c =>
                ownHardAckLane.verify(own.hardAck.toList, c) match {
                    case Right(next) => Right(ownHardAckLane.advanceTo(next))
                    case Left(m)     => Left(m.toString)
                }
            )
            .flatMap {
                case Right(advance) => advance.as(Right(()))
                case Left(reason)   => IO.pure(Left(reason))
            }

    /** The coil peer's own hard-ack → the hub's quorum (`SlowConsensusActor`) and
      * `CoilAckSequencer` (which re-sequences it onto this hub's `HubHardAckLane`).
      */
    private def dispatch(own: OwnHardAck.New): IO[Unit] =
        getConnections.flatMap { conn =>
            own.hardAck.traverse_(ack =>
                (conn.slowConsensusActor ! ack) >> (conn.coilAckSequencer ! ack)
            )
        }

    private val puller = new Puller[OwnHardAck.Get, OwnHardAck.New](
      initialGet = initialGet,
      buildGet = buildGet,
      accept = accept,
      dispatch = dispatch,
      getBatchNum = _.batchNum,
      newBatchNum = _.batchNum
    )(g => getConnections.flatMap(_.remoteCoil ! g))

    // ---- Actor shell ----------------------------------------------------------------------------
    override def preStart: IO[Unit] = context.self ! PreStart

    override def receive: Receive[IO, HubToCoilRequest] =
        PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: HubToCoilRequest): IO[Unit] = req match {
        case PreStart            => preStartLocal
        case ResendCurrent       => puller.resend
        case get: Population.Get => server.handleGet(get)
        case own: OwnHardAck.New => puller.handleReply(own)
        case artifact @ (_: BlockBrief.Next | _: StackBrief | _: UserRequestWithId | _: SoftAck |
            _: HardAck | _: HardAckWithId) =>
            appendArtifact(artifact) >> server.afterAppend
    }

    private def preStartLocal: IO[Unit] =
        for {
            c <- resolveConnections
            _ <- connections.set(Some(c))
            _ <- logger.info(s"starting, coil peer c${coil.convert}")
            _ <- puller.start
            _ <- startResendTimer
        } yield ()

    private def startResendTimer: IO[Unit] =
        (IO.sleep(
          config.peerLiaisonResendInterval
        ) >> (context.self ! ResendCurrent)).foreverM.start.void
}

object PeerLiaisonHubToCoil {
    def apply(
        config: Config,
        coil: CoilPeerNumber,
        pendingConnections: MultisigRegimeManager.PendingConnections | Connections
    ): IO[PeerLiaisonHubToCoil] =
        IO(new PeerLiaisonHubToCoil(config, coil, pendingConnections) {})

    type Config =
        OwnPeerPublic.Section & NodeOperationMultisigConfig.Section & HeadConfig.Bootstrap.Section

    type Handle = ActorRef[IO, LiaisonProtocol.HubToCoilRequest]

    /** The hub's quorum + relay-sequencer for the coil peer's inbound hard-ack, plus the send path
      * to the coil peer's counterpart liaison.
      */
    final case class Connections(
        slowConsensusActor: SlowConsensusActor.Handle,
        coilAckSequencer: CoilAckSequencer.Handle,
        remoteCoil: LiaisonProtocol.CoilToHubHandle
    )
}
