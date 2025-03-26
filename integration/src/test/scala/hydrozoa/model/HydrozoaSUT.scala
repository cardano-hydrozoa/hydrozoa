package hydrozoa.model

import com.bloxbean.cardano.client.api.model.ProtocolParams
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.mkWallet
import hydrozoa.node.server.{InitializeError, Node}
import hydrozoa.node.state.{NodeState, WalletId}
import hydrozoa.{TxId, TxIx, mkHydrozoaNode}

/** Hydrozoa peers' network facade.
  */
trait HydrozoaSUT:
    def initializeHead(
        otherHeadPeers: Set[WalletId],
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): (Either[InitializeError, TxId], NodeStateInspector)

case class OneNodeHydrozoaSUT(
    node: Node
) extends HydrozoaSUT:
    override def initializeHead(
        otherHeadPeers: Set[WalletId],
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): (Either[InitializeError, TxId], NodeStateInspector) =
        val ret = node.initializeHead(otherHeadPeers, ada, txId, txIx)
        (ret, node.nodeStateReader)

object OneNodeHydrozoaSUT:
    def apply(
        ownPeer: TestPeer,
        knownPeers: Set[TestPeer],
        pp: ProtocolParams,
        useYaci: Boolean = false
    ): OneNodeHydrozoaSUT =
        new OneNodeHydrozoaSUT(
          mkHydrozoaNode(
            ownPeerWallet = mkWallet(ownPeer),
            knownPeers = knownPeers.map(mkWallet),
            useL1Mock = !useYaci,
            pp = Some(pp)
          )._2
        )

type NodeStateInspector = NodeState
