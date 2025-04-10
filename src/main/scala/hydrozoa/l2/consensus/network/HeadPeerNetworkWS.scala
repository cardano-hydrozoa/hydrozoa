package hydrozoa.l2.consensus.network

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.block.Block
import hydrozoa.l2.consensus.network.transport.{HeadPeerNetworkTransportWS, IncomingDispatcher}
import hydrozoa.l2.ledger.UtxosSet
import hydrozoa.node.TestPeer
import hydrozoa.node.state.WalletId
import hydrozoa.{TxKeyWitness, VerificationKeyBytes}
import ox.channels.ActorRef
import ox.{either, timeout}

import scala.collection.mutable
import scala.concurrent.duration
import scala.concurrent.duration.DurationInt

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
        val sendReq = transport.broadcastMessage(Some(seq))
        val sendAck = transport.broadcastMessage(ownPeer, seq)

        dispatcherRef.ask(_.spawnActorProactively(ownPeer, seq, req, sendReq, sendAck))

    override def reqInit(req: ReqInit): Unit =
        requireHeadPeersAreKnown(req.otherHeadPeers)

        val seq = transport.nextSeq
        val sendReq = transport.broadcastMessage(Some(seq))
        val sendAck = transport.broadcastMessage(ownPeer, seq)

        dispatcherRef.ask(_.spawnActorProactively(ownPeer, seq, req, sendReq, sendAck))

    override def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness] = ???
    override def reqMinor(block: Block): Set[AckMinor] = ???
    override def reqMajor(block: Block, utxosWithdrawn: UtxosSet): Set[AckMajorCombined] = ???
    override def reqFinal(block: Block, utxosWithdrawn: UtxosSet): Set[AckFinalCombined] = ???
