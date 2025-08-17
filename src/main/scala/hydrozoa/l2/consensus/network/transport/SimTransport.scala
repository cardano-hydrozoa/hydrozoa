package hydrozoa.l2.consensus.network.transport

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.ConsensusDispatcher
import hydrozoa.l2.consensus.network.{Ack, Req}
import hydrozoa.node.TestPeer
import ox.channels.{ActorRef, Channel, Source}
import ox.flow.Flow
import ox.{Ox, forkDiscard, never, supervised}

class SimNetwork(peers: List[TestPeer]):
    val chans: Map[(TestPeer, TestPeer), Channel[AnyMsg]] =
        val pairs = for
            sender <- peers
            receiver <- peers.filter(_ != sender)
        yield (sender, receiver)
        pairs.zip(pairs.map(_ => Channel.unlimited)).toMap

    def knownPeers: Set[TestPeer] = peers.toSet

    def chanKeys: Iterable[(TestPeer, TestPeer)] = chans.keys

    def mkInbox(recipient: TestPeer)(using Ox): Source[AnyMsg] =
        val inbox = Channel.unlimited[AnyMsg]
        val flows = chans.filter((k, _) => k._2 == recipient).values.map(Flow.fromSource(_))
        flows.foreach(f =>
            forkDiscard {
                f.runPipeToSink(inbox, false)
            }
        )
        inbox

    def send(msg: AnyMsg, sender: TestPeer): Unit =
        chans.filter((k, _) => k._1 == sender).values.foreach(_.send(msg))

object SimNetwork:
    def apply(peers: List[TestPeer]) = new SimNetwork(peers)

class SimTransport(simNetwork: SimNetwork, ownPeer: TestPeer) extends HeadPeerNetworkTransport:

    private val log = Logger(getClass)

    private var counter: Long = 0

    private var dispatcher: ActorRef[ConsensusDispatcher] = _

    override def setDispatcher(dispatcher: ActorRef[ConsensusDispatcher]): Unit =
        this.dispatcher = dispatcher

    override def run(): Unit = supervised {
        // Inbox reader
        forkDiscard {
            log.info("Running incoming reader...")
            val incoming = simNetwork.mkInbox(ownPeer)
            Flow.fromSource(incoming)
                .runForeach(anyMsg =>
                    log.info(s"Handling incoming msg: $anyMsg")
                    val (from, seq) = anyMsg.origin
                    dispatcher.tell(_.dispatchMessage(anyMsg))
                )
        }
        // Wait forever
        never
    }

    override def broadcastReq(seq: Option[Long] = None)(req: Req): Long =
        val next: Long = seq.getOrElse(nextSeq)
        val aux = ReqAux(ownPeer, next)
        val anyMsg = AnyMsg(req, aux)
        simNetwork.send(anyMsg, ownPeer)
        next

    override def broadcastAck(replyTo: TestPeer, replyToSeq: Long)(ack: Ack): Long =
        val next = nextSeq
        val aux = AckAux(ownPeer, next, replyTo, replyToSeq)
        val anyMsg = AnyMsg(ack, aux)
        simNetwork.send(anyMsg, ownPeer)
        next

    override def nextSeq: Long =
        val ret = counter + 1
        counter = ret
        ret

object SimTransport:
    def apply(simNetwork: SimNetwork, ownPeer: TestPeer): HeadPeerNetworkTransport =
        new SimTransport(simNetwork, ownPeer)
