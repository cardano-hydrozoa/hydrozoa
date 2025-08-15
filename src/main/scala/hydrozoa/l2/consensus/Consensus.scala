package hydrozoa.l2.consensus

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.Req
import hydrozoa.l2.consensus.network.actor.{ConsensusActor, ConsensusActorFactory}
import hydrozoa.l2.consensus.network.transport.{
    AnyMsg,
    HeadPeerNetworkTransport,
    HeadPeerNetworkTransportWS
}
import hydrozoa.node.TestPeer
import ox.Ox
import ox.channels.{Actor, ActorRef, Channel, Source}
import ox.flow.Flow

import scala.collection.mutable

/** Manages consensus dynamic actors and handles incoming messages.
  */
trait ConsensusDispatcher:
    //
    def setTransport(transport: ActorRef[HeadPeerNetworkTransport]): Unit
    def setConsensusActorFactory(consensusActorFactory: ConsensusActorFactory): Unit
    def setOwnActor(ownActor: ActorRef[ConsensusDispatcher]): Unit
    //
    def dispatchMessage(anyMsg: AnyMsg): Unit
    def spawnActorProactively(from: TestPeer, seq: Long, req: Req): Source[req.resultType]
    def dropActor(origin: (TestPeer, Long)): Unit
    def run()(using Ox): Unit
end ConsensusDispatcher

class DefaultConsensusDispatcher extends ConsensusDispatcher:

    private val log = Logger(getClass)
    private val actors: mutable.Map[(TestPeer, Long), ActorRef[ConsensusActor]] = mutable.Map.empty
    // TODO: rename
    private val spawnActorReactivelyIn: Channel[ConsensusActor] = Channel.rendezvous
    private val spawnActorReactivelyOut: Channel[ActorRef[ConsensusActor]] = Channel.rendezvous
    private var transport: ActorRef[HeadPeerNetworkTransport] = _
    private var ownActor: ActorRef[ConsensusDispatcher] = _
    private var consensusActorFactory: ConsensusActorFactory = _

    override def setTransport(transport: ActorRef[HeadPeerNetworkTransport]): Unit =
        this.transport = transport

    override def setOwnActor(ownActor: ActorRef[ConsensusDispatcher]): Unit =
        this.ownActor = ownActor

    override def setConsensusActorFactory(consensusActorFactory: ConsensusActorFactory): Unit =
        this.consensusActorFactory = consensusActorFactory

    def dispatchMessage(msg: AnyMsg): Unit =
        log.info(s"Dispatching incoming message: $msg")

        val origin = msg.origin
        actors.get(origin) match
            case Some(actor) =>
                log.info(s"Actor was found for origin: $origin")
                msg.asReqOrAck match
                    case Left(originPeer, originSeq, req) =>
                        // FIXME: add check whether init has not been called
                        val acks = actor.ask(act => act.init(req.asInstanceOf[act.ReqType]))
                        log.info(s"Replying with acks: $acks")
                        acks.foreach(ack =>
                            transport.tell(_.broadcastAck(originPeer, originSeq)(ack))
                        )
                    case Right(_, _, originPeer, originSeq, ack) =>
                        val mbAck = actor.ask(act => act.deliver(ack.asInstanceOf[act.AckType]))
                        log.info(s"Replying with mbAck: $mbAck")
                        mbAck.foreach(ack =>
                            transport.tell(_.broadcastAck(originPeer, originSeq)(ack))
                        )
            case None =>
                log.info(s"Actor was NOT found for origin: $origin")
                val dropMyself = () => ownActor.tell(_.dropActor(origin))
                // TODO: First reaction: here would be nice to check, that if at least one
                //  block-related consensus actor is already here we must indicate an
                //  erroneous condition: under normal operation there should be exactly
                //  one block in work.
                //  After some thinking: no, we should not, since if the next leader got all
                //  confirmations and start working on the next block theoretically we can
                //  get a message about the next block before we finish with the previous one.
                //  This situation should be definitely tested in simulation.
                val mbNewActor = msg.asReqOrAck match
                    case Left(originPeer, originSeq, req) =>
                        val (newActor, acks) = consensusActorFactory.spawnByReq(req, dropMyself)
                        acks.foreach(ack =>
                            transport.tell(_.broadcastAck(originPeer, originSeq)(ack))
                        )
                        Some(newActor)
                    case Right(_, _, originPeer, originSeq, ack) =>
                        val mbActor -> mbAck = consensusActorFactory.spawnByAck(ack, dropMyself)
                        mbAck.foreach(ack =>
                            transport.tell(_.broadcastAck(originPeer, originSeq)(ack))
                        )
                        mbActor
                mbNewActor match
                    case Some(newActor) =>
                        spawnActorReactivelyIn.send(newActor)
                        val newActorRef = spawnActorReactivelyOut.receive()
                        log.info(s"Adding new actor for $origin")
                        actors.put(origin, newActorRef)
                    case None => ()

    override def spawnActorProactively(
        from: TestPeer,
        seq: Long,
        req: Req
    ): Source[req.resultType] =
        val origin = (from, seq)
        log.info("Spawning actor proactively...")
        val dropMyself = () => ownActor.tell(_.dropActor(origin))
        val (newActor, acks) = consensusActorFactory.spawnByReq(req, dropMyself)
        log.trace(s"Deinit acks = $acks")
        spawnActorReactivelyIn.send(newActor)
        val newActorRef = spawnActorReactivelyOut.receive()
        actors.put(origin, newActorRef)
        transport.tell(_.broadcastReq(Some(seq))(req))
        acks.foreach(ack => transport.tell(_.broadcastAck(from, seq)(ack)))
        log.info("Getting result source...")
        val source: Source[req.resultType] = newActorRef.ask(act => act.result(using req))
        source

    override def dropActor(origin: (TestPeer, Long)): Unit =
        log.info(s"Dropping actor for origin $origin")
        val ret = actors.remove(origin)
        assert(ret.isDefined)

    def run()(using Ox): Unit =
        log.info("running actor spawner...")
        Flow.fromSource(spawnActorReactivelyIn)
            .runForeach(actor =>
                log.info(s"spanning new actor: ${actor.getClass}")
                val actorRef = Actor.create(actor)
                spawnActorReactivelyOut.send(actorRef)
            )

object DefaultConsensusDispatcher:
    def apply() = new DefaultConsensusDispatcher
