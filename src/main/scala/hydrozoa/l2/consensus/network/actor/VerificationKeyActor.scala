package hydrozoa.l2.consensus.network.actor

import hydrozoa.VerificationKeyBytes
import hydrozoa.l2.consensus.network.{AckVerKey, ReqVerKey}
import hydrozoa.node.TestPeer
import ox.channels.{Channel, Source}

import scala.collection.mutable
import scala.concurrent.Future

class VerificationKeyActor extends ConsensusActor:

    type ReqType = ReqVerKey
    type AckType = AckVerKey
    type Result = Set[VerificationKeyBytes]

    val acks: mutable.Map[TestPeer, VerificationKeyBytes] = mutable.Map.empty

    def tryMakeResult(): Unit =
        val headPeers: Set[TestPeer] = ???
        if acks.keySet == headPeers
            // make and return the resultChannel
        then resultChannel.send(acks.values.toSet)

    def deliver(ack: AckType): Unit =
        acks.put(ack.peer, ack.verKey)
        tryMakeResult()

    def init(req: ReqType): AckVerKey =
        // here actual work to build won ack happens
        val ownAck: AckType = ???
        deliver(ownAck)
        ownAck

    private val resultChannel: Channel[Result] = Channel.rendezvous

    val result: Source[Result] = resultChannel

end VerificationKeyActor

type ActorId = (TestPeer, Long)

object VerificationKeyActor:
    def createProactively(actorId: ActorId, reqVerKey: ReqVerKey) = ???
    def createReactively(actorId: ActorId, msg: ReqVerKey | AckVerKey) = ???
