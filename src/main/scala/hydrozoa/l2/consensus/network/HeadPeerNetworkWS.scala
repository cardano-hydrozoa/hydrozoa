package hydrozoa.l2.consensus.network

import com.typesafe.scalalogging.Logger
import hydrozoa.l1.multisig.tx.PostDatedRefundTx
import hydrozoa.l2.consensus.network.transport.{HeadPeerNetworkTransportWS, IncomingDispatcher}
import hydrozoa.node.TestPeer
import hydrozoa.node.state.WalletId
import hydrozoa.{TxId, VerificationKeyBytes}
import ox.channels.ActorRef

class HeadPeerNetworkWS(
    ownPeer: TestPeer,
    knownPeers: Set[WalletId],
    transport: HeadPeerNetworkTransportWS
) extends HeadPeerNetwork:

    private var dispatcher: ActorRef[IncomingDispatcher] = _

    def setDispatcher(dispatcher: ActorRef[IncomingDispatcher]): Unit =
        this.dispatcher = dispatcher

    private val log = Logger(getClass)

    private def requireHeadPeersAreKnown(headPeers: Set[WalletId]): Unit = {
        val headPeersNames = headPeers.map(_.name)
        val knownPeersNames = knownPeers.map(_.name)
        require(headPeersNames.subsetOf(knownPeersNames), "All peers should be known")
    }

    override def reqVerificationKeys(): Map[WalletId, VerificationKeyBytes] =
        log.info(s"reqVerificationKeys")
        val seq = transport.nextSeq
        val req = ReqVerKey()
        dispatcher.ask(_.spawnActorProactively(ownPeer, seq, req)).receive()

    override def reqInit(req: ReqInit): TxId =
        log.info(s"reqInit: $req")
        requireHeadPeersAreKnown(req.otherHeadPeers)
        val seq = transport.nextSeq
        dispatcher.ask(_.spawnActorProactively(ownPeer, seq, req)).receive()

    override def reqRefundLater(req: ReqRefundLater): PostDatedRefundTx =
        log.info(s"reqRefundLater: $req")
        val seq = transport.nextSeq
        dispatcher.ask(_.spawnActorProactively(ownPeer, seq, req)).receive()

    override def reqEventL2(req: ReqEventL2): Unit =
        log.info(s"reqEventL2: $req")
        val seq = transport.nextSeq
        dispatcher.ask(_.spawnActorProactively(ownPeer, seq, req)).receive()

    override def reqMinor(req: ReqMinor): Unit =
        log.info(s"ReqMinor for block: $req.block")
        val seq = transport.nextSeq
        val ret = dispatcher.ask(_.spawnActorProactively(ownPeer, seq, req)).receive()
        log.info(s"reqMinor done")
    
    override def reqMajor(req: ReqMajor): Unit =
        log.info(s"ReqMajor for block: $req.block")
        val seq = transport.nextSeq
        dispatcher.ask(_.spawnActorProactively(ownPeer, seq, req)).receive()

    override def reqFinal(req: ReqFinal): Unit =
        log.info(s"ReqFinal for block: $req.block")
        val seq = transport.nextSeq
        dispatcher.ask(_.spawnActorProactively(ownPeer, seq, req)).receive()
