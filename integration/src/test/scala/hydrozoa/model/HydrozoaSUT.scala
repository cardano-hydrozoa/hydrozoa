package hydrozoa.model

import com.bloxbean.cardano.client.api.model.ProtocolParams
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.mkPeer
import hydrozoa.node.server.{InitializeError, Node}
import hydrozoa.node.state.{NodeState, PeerInfo}
import hydrozoa.{TxId, TxIx, mkHydrozoaNode}

/** Hydrozoa peers' network facade.
  */
trait HydrozoaSUT:
    def initializeHead(
        otherHeadPeers: Set[PeerInfo],
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): (Either[InitializeError, TxId], NodeStateInspector)

case class OneNodeHydrozoaSUT(
    node: Node
) extends HydrozoaSUT:
    override def initializeHead(
        otherHeadPeers: Set[PeerInfo],
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): (Either[InitializeError, TxId], NodeStateInspector) =
        val ret = node.initializeHead(otherHeadPeers, ada, txId, txIx)
        (ret, node.nodeStateReader)

object OneNodeHydrozoaSUT:
    def apply(
        ownPeer: TestPeer,
        knownPeers: Seq[TestPeer],
        pp: ProtocolParams
    ): OneNodeHydrozoaSUT =
        new OneNodeHydrozoaSUT(
          mkHydrozoaNode(
            ownPeer = mkPeer(ownPeer),
            knownPeers = knownPeers.map(mkPeer),
            useL1Mock = true,
            pp = Some(pp)
          )._2
        )

type NodeStateInspector = NodeState
