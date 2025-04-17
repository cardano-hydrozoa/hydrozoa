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

    private var dispatcherRef: ActorRef[IncomingDispatcher] = _

    def setDispatcherActorRef(dispatcherRef: ActorRef[IncomingDispatcher]): Unit =
        this.dispatcherRef = dispatcherRef

    private val log = Logger(getClass)

    private def requireHeadPeersAreKnown(headPeers: Set[WalletId]): Unit = {
        val headPeersNames = headPeers.map(_.name)
        val knownPeersNames = knownPeers.map(_.name)
        require(headPeersNames.subsetOf(knownPeersNames), "All peers should be known")
    }

    override def reqVerificationKeys(): Map[WalletId, VerificationKeyBytes] =
        val seq = transport.nextSeq
        val req = ReqVerKey()
        val sendReq = transport.broadcastReq(Some(seq))
        val sendAck = transport.broadcastAck(ownPeer, seq)

        dispatcherRef.ask(_.spawnActorProactively(ownPeer, seq, req, sendReq, sendAck))

    override def reqInit(req: ReqInit): TxId =
        requireHeadPeersAreKnown(req.otherHeadPeers)

        val seq = transport.nextSeq
        val sendReq = transport.broadcastReq(Some(seq))
        val sendAck = transport.broadcastAck(ownPeer, seq)

        dispatcherRef.ask(_.spawnActorProactively(ownPeer, seq, req, sendReq, sendAck))

    override def reqRefundLater(req: ReqRefundLater): PostDatedRefundTx =
        val seq = transport.nextSeq
        val sendReq = transport.broadcastReq(Some(seq))
        val sendAck = transport.broadcastAck(ownPeer, seq)

        dispatcherRef.ask(_.spawnActorProactively(ownPeer, seq, req, sendReq, sendAck))

    override def reqEventL2(req: ReqEventL2): Unit =
        val seq = transport.nextSeq
        val sendReq = transport.broadcastReq(Some(seq))
        val sendAck = transport.broadcastAck(ownPeer, seq)

        dispatcherRef.ask(_.spawnActorProactively(ownPeer, seq, req, sendReq, sendAck))

    override def reqMinor(req: ReqMinor): Unit =
        log.info(s"ReqMinor for block: $req.block")

        val seq = transport.nextSeq
        val sendReq = transport.broadcastReq(Some(seq))
        val sendAck = transport.broadcastAck(ownPeer, seq)

        dispatcherRef.ask(_.spawnActorProactively(ownPeer, seq, req, sendReq, sendAck))

    override def reqMajor(req: ReqMajor): Unit =
        log.info(s"ReqMajor for block: $req.block")

        val seq = transport.nextSeq
        val sendReq = transport.broadcastReq(Some(seq))
        val sendAck = transport.broadcastAck(ownPeer, seq)

        dispatcherRef.ask(_.spawnActorProactively(ownPeer, seq, req, sendReq, sendAck))

    override def reqFinal(req: ReqFinal): Unit =
        log.info(s"ReqFinal for block: $req.block")

        val seq = transport.nextSeq
        val sendReq = transport.broadcastReq(Some(seq))
        val sendAck = transport.broadcastAck(ownPeer, seq)

        dispatcherRef.ask(_.spawnActorProactively(ownPeer, seq, req, sendReq, sendAck))
