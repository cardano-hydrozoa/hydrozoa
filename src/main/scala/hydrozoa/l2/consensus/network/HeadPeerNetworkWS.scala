package hydrozoa.l2.consensus.network

import hydrozoa.infra.Piper
import hydrozoa.infra.decodeHex
import hydrozoa.l2.block.Block
import hydrozoa.l2.ledger.UtxosSet
import hydrozoa.node.TestPeer
import hydrozoa.node.state.WalletId
import hydrozoa.{TxKeyWitness, VerificationKeyBytes, Wallet}

import scala.collection.mutable

class HeadPeerNetworkWS(
    transport: HeadPeerNetworkTransportWS
) extends HeadPeerNetwork:

    override def reqVerificationKeys(peers: Set[WalletId]): Set[VerificationKeyBytes] =

        val responses: mutable.Map[WalletId, VerificationKeyBytes] = mutable.Map.empty

        def handleResponse(msg: String) =
            val parts = msg.split("|")
            val walletId = WalletId(parts(0))
            val verificationKey = parts(1) |> decodeHex |> VerificationKeyBytes.applyI
            responses.put(walletId, verificationKey)
            // if responses.size == peers.size then return responses.values.toSet

        transport.broadcastMessage("send me your VK please")
        ???

    private def requireHeadPeersAreKnown(headPeers: Set[WalletId]): Unit = ???
    override def reqInit(headPeers: Set[WalletId], req: ReqInit): Set[TxKeyWitness] = ???
    private def getOtherPeersWallets: Set[Wallet] = ???
    override def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness] = ???
    override def reqMinor(block: Block): Set[AckMinor] = ???
    override def reqMajor(block: Block, utxosWithdrawn: UtxosSet): Set[AckMajorCombined] = ???
    override def reqFinal(block: Block, utxosWithdrawn: UtxosSet): Set[AckFinalCombined] = ???
