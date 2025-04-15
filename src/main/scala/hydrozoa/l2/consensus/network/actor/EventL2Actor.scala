package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.{AckUnit, Req, ReqEventL2}
import hydrozoa.node.state.NodeState
import ox.channels.{ActorRef, Channel, Source}
import ox.{forkDiscard, supervised}

private class EventL2Actor(
    stateActor: ActorRef[NodeState]
) extends ConsensusActor:

    private val log = Logger(getClass)

    override type ReqType = ReqEventL2
    override type AckType = AckUnit

    override def deliver(_ack: AckType): Option[AckUnit] = None

    override def init(req: ReqType): AckType =
        log.info(s"init req: $req")
        stateActor.tell(_.head.openPhase(_.poolEventL2(req.eventL2)))
        resultChannel.send(())
        AckUnit

    private val resultChannel: Channel[Unit] = Channel.buffered(1)
//    private def resultChannel(using req: ReqType): Channel[req.resultType] = Channel.rendezvous

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end EventL2Actor
