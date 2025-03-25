package hydrozoa.model

import com.bloxbean.cardano.client.api.model.ProtocolParams
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.mkPeer
import hydrozoa.node.server.{InitializeError, Node}
import hydrozoa.{TxId, TxIx, mkHydrozoaNode}

/** Hydrozoa peers' network facade.
  */
trait HydrozoaSUT:
    def initializeHead(
        initializer: TestPeer,
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): Either[InitializeError, TxId]

case class OneNodeHydrozoaSUT(
    node: Node
) extends HydrozoaSUT:
    override def initializeHead(
        _initializer: TestPeer,
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): Either[InitializeError, TxId] = node.initializeHead(???, ada, txId, txIx)

object OneNodeHydrozoaSUT:
    def apply(
        ownPeer: TestPeer,
        knownPeers: Seq[TestPeer],
        pp: ProtocolParams
    ): OneNodeHydrozoaSUT =
        println("--------------------> new SUT")
        new OneNodeHydrozoaSUT(
          mkHydrozoaNode(
            ownPeer = mkPeer(ownPeer),
            knownPeers = knownPeers.map(mkPeer),
            useL1Mock = true,
            pp = Some(pp)
          )._2
        )
