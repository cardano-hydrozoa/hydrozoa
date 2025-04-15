package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.{Ack, AckMinor, AckVerKey, Req, ReqMinor, ReqVerKey}
import hydrozoa.node.state.{NodeState, WalletId}
import hydrozoa.{VerificationKeyBytes, Wallet}
import ox.channels.{ActorRef, Channel, Source}

import scala.collection.mutable

private class MinorBlockConfirmationActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet]
) extends ConsensusActor:

    private val log = Logger(getClass)

    override type ReqType = ReqMinor
    override type AckType = AckMinor

    private val acks: mutable.Map[WalletId, AckMinor] = mutable.Map.empty

    private def tryMakeResult(): Unit =
        log.trace("tryMakeResult")
        val headPeers = stateActor.ask(_.head.openPhase(_.headPeers))
        if acks.keySet == headPeers
        then
            ???
//            val result = acks.toMap
//            log.trace(s"Actor is done with value: $result")
//            stateActor.tell(_.saveKnownPeersVKeys(result))
//            resultChannel.send(result)

    override def deliver(ack: AckType): Option[AckType] =
        log.trace(s"deliver ack: $ack")
        acks.put(ack.peer, ack)
        tryMakeResult()
        None

    override def init(req: ReqType): AckType =
        log.trace(s"init req: $req")
        val (me, signature) =
            walletActor.ask(w => (w.getWalletId, "signature_stub"))
        val ownAck: AckType = AckMinor(me, signature, false) // FIXME: how do we decide whether we want to wrap up the head?
        deliver(ownAck)
        ownAck

    private val resultChannel: Channel[Map[WalletId, VerificationKeyBytes]] = Channel.rendezvous
//    private def resultChannel(using req: ReqType): Channel[req.resultType] = Channel.rendezvous

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end MinorBlockConfirmationActor
