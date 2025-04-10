package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.{Ack, AckVerKey, Req, ReqVerKey}
import hydrozoa.node.TestPeer
import hydrozoa.node.state.{NodeState, WalletId}
import hydrozoa.{VerificationKeyBytes, Wallet}
import ox.channels.{ActorRef, Channel, Source}

import scala.collection.mutable

class VerificationKeyActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet]
) extends ConsensusActor:

    private val log = Logger(getClass)

    type ReqType = ReqVerKey
    type AckType = AckVerKey
    type Result = Set[VerificationKeyBytes]

    val acks: mutable.Map[TestPeer, VerificationKeyBytes] = mutable.Map.empty

    def tryMakeResult(): Unit =
        log.info("tryMakeResult")
        val expectedPeers: Set[WalletId] =
            stateActor.ask(_.getKnownPeers) + walletActor.ask(_.getWalletId)
        log.info(s"expectedPeers: ${expectedPeers.map(_.name)}")
        log.info(s"acks.keySet: ${acks.keySet.map(_.toString)}")
        log.info(s"acks.keySet: ${acks.keySet.map(_.toString) ==  expectedPeers.map(_.name)}")

        if acks.keySet.map(_.toString) ==  expectedPeers.map(_.name)
        then
            // make and return the resultChannel
            log.warn("make and return the resultChannel!")
            resultChannel.send(acks.values.toSet)
        log.info("leaving tryMakeResult")

    def deliver(ack: Ack): Unit =
        log.info(s"deliver ack: $ack")
        val ackVerKey = ack.asInstanceOf[AckType]
        acks.put(ackVerKey.peer, ackVerKey.verKey)
        tryMakeResult()

    def init(req: Req): AckVerKey =
        log.info(s"init req: $req")
        val (me, key) =
            walletActor.ask(w => (TestPeer.valueOf(w.getName), w.exportVerificationKeyBytes))
        val ownAck: AckType = AckVerKey(me, key)
        log.info(s"me: $me, key: $key")
        deliver(ownAck)
        ownAck

    private val resultChannel: Channel[Result] = Channel.rendezvous

    val result: Source[Result] = resultChannel

end VerificationKeyActor
