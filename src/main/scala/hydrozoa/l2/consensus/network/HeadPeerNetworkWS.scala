package hydrozoa.l2.consensus.network

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.block.Block
import hydrozoa.l2.consensus.network.transport.HeadPeerNetworkTransportWS
import hydrozoa.l2.ledger.UtxosSet
import hydrozoa.node.TestPeer
import hydrozoa.node.state.WalletId
import hydrozoa.{TxKeyWitness, VerificationKeyBytes}
import ox.{either, timeout}

import scala.collection.mutable
import scala.concurrent.duration
import scala.concurrent.duration.DurationInt

class HeadPeerNetworkWS(
    ownPeer: TestPeer,
    knownPeers: Set[WalletId],
    transport: HeadPeerNetworkTransportWS
) extends HeadPeerNetwork:

    private val log = Logger(getClass)

    private def requireHeadPeersAreKnown(headPeers: Set[WalletId]): Unit = {
        val headPeersNames = headPeers.map(_.name)
        val knownPeersNames = knownPeers.map(_.name)
        require(headPeersNames.subsetOf(knownPeersNames), "All peers should be known")
    }

    override def reqVerificationKeys(peers: Set[WalletId]): Set[VerificationKeyBytes] =

        requireHeadPeersAreKnown(peers)

        val source = transport.broadcastAndCollect(ReqVerKey())

        def handleResponses: Set[VerificationKeyBytes] = {
            val responses: mutable.Map[TestPeer, VerificationKeyBytes] = mutable.Map.empty

            while responses.size < peers.size do
                val next = source.receive()
                responses.put(next.peer, next.verKey)

            responses.values.toSet
        }

        either.catching(timeout(10.second)(handleResponses)) match
            case Left(throwable) =>
                log.error(s"Timeout while collecting verification keys: $throwable")
                throw IllegalStateException(throwable)
            case Right(responses) => responses

    override def announceOwnVerificationKey(key: VerificationKeyBytes): Unit =
        // TODO: this is why announcing is unwieldy
        //  * include into Req
        //  * introudce additional type Ann(ounce) (like Req but don't need an Ack)
        transport.broadcastMessage(ownPeer, 0)(
          AckVerKey(ownPeer, key)
        )

    override def reqInit(peers: Set[WalletId], req: ReqInit): Set[TxKeyWitness] =
        requireHeadPeersAreKnown(peers)

        val source = transport.broadcastAndCollect(req)

        def handleResponses: Set[TxKeyWitness] = {
            val responses: mutable.Map[TestPeer, TxKeyWitness] = mutable.Map.empty

            while responses.size < peers.size do
                val next = source.receive()
                responses.put(next.peer, next.signature)

            responses.values.toSet
        }

        either.catching(timeout(10.second)(handleResponses)) match
            case Left(throwable) =>
                log.error(s"Timeout while collecting initialization acknowledges: $throwable")
                throw IllegalStateException(throwable)
            case Right(responses) => responses

    override def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness] = ???
    override def reqMinor(block: Block): Set[AckMinor] = ???
    override def reqMajor(block: Block, utxosWithdrawn: UtxosSet): Set[AckMajorCombined] = ???
    override def reqFinal(block: Block, utxosWithdrawn: UtxosSet): Set[AckFinalCombined] = ???
