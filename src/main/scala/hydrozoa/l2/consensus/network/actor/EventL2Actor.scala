package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.VerificationKeyBytes
import hydrozoa.l2.consensus.network.{AckUnit, Req, ReqEventL2}
import hydrozoa.node.state.{NodeState, WalletId}
import ox.channels.{ActorRef, Channel, Source}
import ox.{forkDiscard, supervised}

import scala.collection.mutable

/** FIXME: Remove acknowledgements completely.
  * @param stateActor
  * @param dropMyself
  */
private class EventL2Actor(
    stateActor: ActorRef[NodeState],
    dropMyself: () => Unit
) extends ConsensusActor:

    private val log = Logger(getClass)

    override type ReqType = ReqEventL2
    override type AckType = AckUnit

    private val acks: mutable.Buffer[AckUnit] = mutable.Buffer.empty

    private def tryMakeResult(): Unit =
        log.trace("tryMakeResult")
        val headPeersNum = stateActor.ask(_.head.openPhase(_.headPeers.size))
        log.debug(s"headPeersNum: $headPeersNum")
        if acks.size == headPeersNum
        then
            resultChannel.send(())
            dropMyself()

    override def deliver(ack: AckType): Option[AckType] =
        log.trace(s"deliver ack: $ack")
        acks.append(ack)
        tryMakeResult()
        None

    override def init(req: ReqType): Seq[AckType] =
        log.info(s"init req: $req")
        stateActor.tell(_.head.openPhase(_.poolEventL2(req.eventL2)))
        this.req = req
        deliver(AckUnit)
        Seq(AckUnit)

    private val resultChannel: Channel[Unit] = Channel.buffered(1)
//    private def resultChannel(using req: ReqType): Channel[req.resultType] = Channel.rendezvous

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end EventL2Actor
