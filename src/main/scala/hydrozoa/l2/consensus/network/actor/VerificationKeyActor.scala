package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.{Ack, AckVerKey, Req, ReqVerKey}
import hydrozoa.node.state.{NodeState, WalletId}
import hydrozoa.{VerificationKeyBytes, Wallet}
import ox.channels.{ActorRef, Channel, Source}

import scala.collection.mutable

private class VerificationKeyActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet]
) extends ConsensusActor:

    private val log = Logger(getClass)

    override type ReqType = ReqVerKey
    override type AckType = AckVerKey

    private val acks: mutable.Map[WalletId, VerificationKeyBytes] = mutable.Map.empty

    private def tryMakeResult(): Unit =
        log.trace("tryMakeResult")
        // NB: known peers cannot change, otherwise
        //  expectedPeers should be calculated upfront
        val expectedPeers: Set[WalletId] =
            stateActor.ask(_.getKnownPeers) + walletActor.ask(_.getWalletId)
        if acks.keySet == expectedPeers
        then
            val result = acks.toMap
            log.trace(s"Actor is done with value: $result")
            stateActor.tell(_.saveKnownPeersVKeys(result))
            resultChannel.send(result)

    override def deliver(ack: AckVerKey): Unit =
        log.trace(s"deliver ack: $ack")
        acks.put(ack.peer, ack.verKey)
        tryMakeResult()

    override def init(req: ReqVerKey): AckVerKey =
        log.trace(s"init req: $req")
        val (me, key) =
            walletActor.ask(w => (w.getWalletId, w.exportVerificationKeyBytes))
        val ownAck: AckType = AckVerKey(me, key)
        deliver(ownAck)
        ownAck

    private val resultChannel: Channel[Map[WalletId, VerificationKeyBytes]] = Channel.rendezvous
//    private def resultChannel(using req: ReqType): Channel[req.resultType] = Channel.rendezvous

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end VerificationKeyActor
