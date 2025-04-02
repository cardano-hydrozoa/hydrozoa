package hydrozoa.model

import com.bloxbean.cardano.client.api.model.ProtocolParams
import hydrozoa.l2.ledger.{SimpleTransaction, SimpleWithdrawal, UtxosSet}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.mkWallet
import hydrozoa.node.rest.SubmitRequestL2.{Transaction, Withdrawal}
import hydrozoa.node.server.*
import hydrozoa.node.state.{BlockRecord, NodeState, WalletId}
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

    def deposit(
        depositRequest: DepositRequest
    ): (Either[DepositError, DepositResponse], NodeStateInspector)

    def produceBlock(
        nextBlockFinal: Boolean
    ): (Either[String, (BlockRecord, UtxosSet, UtxosSet)], NodeStateInspector)

    def submitL2(
        event: SimpleTransaction | SimpleWithdrawal
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
    ): (Either[InitializeError, TxId], NodeStateInspector) =
        val ret = node.initializeHead(otherHeadPeers, ada, txId, txIx)
        (ret, node.nodeStateReader)

    override def deposit(
        depositRequest: DepositRequest
    ): (Either[DepositError, DepositResponse], NodeStateInspector) =
        val ret = node.deposit(depositRequest)
        (ret, node.nodeStateReader)

    override def produceBlock(
        nextBlockFinal: Boolean
    ): (Either[String, (BlockRecord, UtxosSet, UtxosSet)], NodeStateInspector) =
        val ret = node.handleNextBlock(nextBlockFinal)
        (ret, node.nodeStateReader)

    override def submitL2(
        event: SimpleTransaction | SimpleWithdrawal
    ): (Either[InitializeError, TxId], NodeStateInspector) =
        val request = event match
            case tx: SimpleTransaction => Transaction(tx)
            case wd: SimpleWithdrawal  => Withdrawal(wd)
        val ret = node.submitL2(request)
        (ret, node.nodeStateReader)

    override def shutdownSut(): Unit = ()

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
