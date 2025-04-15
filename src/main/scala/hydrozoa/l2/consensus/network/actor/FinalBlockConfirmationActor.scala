package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.l1.multisig.tx.finalization.FinalizationTxBuilder
import hydrozoa.l2.consensus.network.{
    Ack,
    AckFinal,
    AckFinal2,
    AckMajor,
    AckMajor2,
    Req,
    ReqFinal,
    ReqMajor,
    ReqVerKey
}
import hydrozoa.node.state.{NodeState, WalletId}
import hydrozoa.{VerificationKeyBytes, Wallet}
import ox.channels.{ActorRef, Channel, Source}

import scala.collection.mutable

private class FinalBlockConfirmationActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet],
    finalizationTxBuilder: FinalizationTxBuilder
) extends ConsensusActor:

    private val log = Logger(getClass)

    override type ReqType = ReqFinal
    override type AckType = AckFinal | AckFinal2

    private val acks: mutable.Map[WalletId, AckFinal] = mutable.Map.empty
    private val acks2: mutable.Map[WalletId, AckFinal2] = mutable.Map.empty

    private def tryMakeResult(): Unit =
        log.trace("tryMakeResult")
        val headPeers = stateActor.ask(_.head.openPhase(_.headPeers))
        (acks.keySet == headPeers, acks2.keySet == headPeers) match
            case (true, true) =>
                //            val result = acks.toMap
                //            log.trace(s"Actor is done with value: $result")
                //            stateActor.tell(_.saveKnownPeersVKeys(result))
                //            resultChannel.send(result)
                ()
            case _ =>

    override def deliver(ack: AckType): Option[AckType] =
        log.trace(s"deliver ack: $ack")
        val mbAck = ack match
            case ack: AckFinal =>
                acks.put(ack.peer, ack)
                val headPeers = stateActor.ask(_.head.openPhase(_.headPeers))
                if acks.keySet == headPeers then
                    // FIXME: check all acks are valid?
                    val (me, finalizationTxKeyWitness) =
                        walletActor.ask(w => (w.getWalletId, w.createTxKeyWitness(???)))
                    val ownAck2 = AckFinal2(me, finalizationTxKeyWitness)
                    Some(ownAck2)
                else None
            case ack2: AckFinal2 =>
                acks2.put(ack2.peer, ack2)
                None

        tryMakeResult()
        mbAck

    override def init(req: ReqType): AckFinal =
        log.trace(s"init req: $req")
        val (me) = walletActor.ask(w => (w.getWalletId))
        val ownAck = AckFinal(me, Seq.empty)
        deliver(ownAck)
        ownAck

    private val resultChannel: Channel[Map[WalletId, VerificationKeyBytes]] = Channel.rendezvous
//    private def resultChannel(using req: ReqType): Channel[req.resultType] = Channel.rendezvous

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end FinalBlockConfirmationActor
