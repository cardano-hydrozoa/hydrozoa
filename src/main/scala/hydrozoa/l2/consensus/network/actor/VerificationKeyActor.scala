package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.{AckVerKey, Req, ReqVerKey}
import hydrozoa.node.state.{NodeState, WalletId}
import hydrozoa.{VerificationKeyBytes, Wallet}
import ox.channels.{ActorRef, Channel, Source}

import scala.collection.mutable

private class VerificationKeyActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet],
    dropMyself: () => Unit
) extends ConsensusActor:

    override type ReqType = ReqVerKey
    override type AckType = AckVerKey
    private val log = Logger(getClass)
    private val acks: mutable.Map[WalletId, VerificationKeyBytes] = mutable.Map.empty
    private val resultChannel: Channel[Map[WalletId, VerificationKeyBytes]] = Channel.buffered(1)

    override def init(req: ReqType): Seq[AckType] =
        log.trace(s"init req: $req")
        val (me, key) =
            walletActor.ask(w => (w.getWalletId, w.exportVerificationKeyBytes))
        val ownAck = AckVerKey(me, key)
        @annotation.unused
        val _ = deliver(ownAck)
        Seq(ownAck)

    override def deliver(ack: AckType): Option[AckType] =
        log.trace(s"deliver ack: $ack")
        @annotation.unused
        val _ = acks.put(ack.peer, ack.verKey)
        tryMakeResult()
        None

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
            dropMyself()
//    private def resultChannel(using req: ReqType): Channel[req.resultType] = Channel.rendezvous

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end VerificationKeyActor
