package hydrozoa.l2.consensus.network.transport

import hydrozoa.l2.consensus.ConsensusDispatcher
import hydrozoa.l2.consensus.network.actor.ConsensusActorFactory
import hydrozoa.l2.consensus.network.{Req, ReqVerKey}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.{Alice, Bob, Carol}
import ox.channels.*
import ox.{Ox, forkDiscard, supervised}

class SimTransportSpec extends munit.ScalaCheckSuite {

    test("number of unique channels is N * (N - 1)") {
        val peers = List(Alice, Bob, Carol)
        val n = peers.size
        val network = SimNetwork.apply(peers)
        val chanKeys = network.chanKeys
        println(chanKeys)
        assert(chanKeys.size == (n * (n - 1)))
    }

    test("send msg") {
        val peers = List(Alice, Bob)
        val network = SimNetwork.apply(peers)
        val alice = SimTransport.apply(network, Alice)
        val bob = SimTransport.apply(network, Bob)

        val sink = Channel.rendezvous[AnyMsg]

        supervised {
            forkDiscard {
                bob.setDispatcher(Actor.create(new SinkDispatcher(sink)))
                bob.run()
            }
            alice.broadcastReq(None)(ReqVerKey())

            val msg = sink.receive()
            assert(msg.origin == (Alice, 1))
            assert(msg.asMsg.isInstanceOf[ReqVerKey])
        }
    }

}

// TODO: review ConsensusDispatcher trait
class SinkDispatcher(sink: Sink[AnyMsg]) extends ConsensusDispatcher:
    //
    override def setTransport(transport: ActorRef[HeadPeerNetworkTransportWS]): Unit = ???
    override def setConsensusActorFactory(consensusActorFactory: ConsensusActorFactory): Unit = ???
    override def setOwnActor(ownActor: ActorRef[ConsensusDispatcher]): Unit = ???
    //
    override def dispatchMessage(anyMsg: AnyMsg): Unit =
        println(s"msg = $anyMsg")
        sink.send(anyMsg)

    override def spawnActorProactively(
        from: TestPeer,
        seq: Long,
        req: Req
    ): Source[req.resultType] = ???
    override def dropActor(origin: (TestPeer, Long)): Unit = ???
    override def run()(using Ox): Unit = ???
