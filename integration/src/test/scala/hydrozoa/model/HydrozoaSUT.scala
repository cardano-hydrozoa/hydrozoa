package hydrozoa.model

import com.bloxbean.cardano.client.api.model.ProtocolParams
import hydrozoa.l2.ledger.simple.UtxosSet
import hydrozoa.l2.ledger.{L2Transaction, L2Withdrawal}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.mkWallet
import hydrozoa.node.rest.SubmitRequestL2.{Transaction, Withdrawal}
import hydrozoa.node.server.*
import hydrozoa.node.state.{BlockRecord, NodeState, WalletId}
import hydrozoa.{TxId, TxIx, mkSimpleHydrozoaNode}

/** Hydrozoa peers' network facade.
  */
trait HydrozoaSUT:
    def initializeHead(
        otherHeadPeers: Set[WalletId],
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): (Either[InitializationError, TxId], NodeStateInspector)

    def deposit(
        depositRequest: DepositRequest
    ): (Either[DepositError, DepositResponse], NodeStateInspector)

    def produceBlock(
        nextBlockFinal: Boolean
    ): (Either[String, (BlockRecord, UtxosSet, UtxosSet)], NodeStateInspector)

    def submitL2(
        event: L2Transaction | L2Withdrawal
    ): (Either[String, TxId], NodeStateInspector)

    def shutdownSut(): Unit

class OneNodeHydrozoaSUT(
    node: Node
) extends HydrozoaSUT:
    override def initializeHead(
        otherHeadPeers: Set[WalletId],
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): (Either[InitializationError, TxId], NodeStateInspector) =
        ???
        //val ret = node.initializeHead(otherHeadPeers, ada, txId, txIx)
        // (ret, node.nodeStateReader)
        //(ret, ???)

    override def deposit(
        depositRequest: DepositRequest
    ): (Either[DepositError, DepositResponse], NodeStateInspector) =
        val ret = node.deposit(depositRequest)
        // (ret, node.nodeStateReader)
        (ret, ???)

    override def produceBlock(
        nextBlockFinal: Boolean
    ): (Either[String, (BlockRecord, UtxosSet, UtxosSet)], NodeStateInspector) =
        val ret = node.handleNextBlock(nextBlockFinal)
        // (ret, node.nodeStateReader)
        (ret, ???)

    override def submitL2(
        event: L2Transaction | L2Withdrawal
    ): (Either[InitializationError, TxId], NodeStateInspector) =
        val request = event match
            case tx: L2Transaction => Transaction(tx)
            case wd: L2Withdrawal  => Withdrawal(wd)
        val ret = node.submitL2(request)
        // (ret, node.nodeStateReader)
        (ret, ???)

    override def shutdownSut(): Unit = ()

object OneNodeHydrozoaSUT:
    def apply(
        ownPeer: TestPeer,
        knownPeers: Set[TestPeer],
        pp: ProtocolParams,
        useYaci: Boolean = false
    ): OneNodeHydrozoaSUT =
        new OneNodeHydrozoaSUT(
          mkSimpleHydrozoaNode(
            ownPeerWallet = mkWallet(ownPeer),
            knownPeers = knownPeers.map(mkWallet),
            useL1Mock = !useYaci,
            pp = Some(pp)
          )._2
        )

type NodeStateInspector = NodeState
