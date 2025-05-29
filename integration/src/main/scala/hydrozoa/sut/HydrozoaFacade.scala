package hydrozoa.sut

import hydrozoa.*
import hydrozoa.l2.block.Block
import hydrozoa.l2.ledger.{L2Genesis, L2Transaction, L2Withdrawal}
import hydrozoa.node.TestPeer
import hydrozoa.node.server.{DepositError, DepositRequest, DepositResponse, InitializationError}
import hydrozoa.node.state.{BlockRecord, WalletId}

/** Hydrozoa peers' network SUT facade.
  */
trait HydrozoaFacade:
    def initializeHead(
        initiator: TestPeer,
        otherHeadPeers: Set[WalletId],
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): Either[InitializationError, TxId]

    def deposit(
        depositor: TestPeer,
        r: DepositRequest
    ): Either[DepositError, DepositResponse]

    def awaitTxL1(txId: TxId): Option[TxL1]
    
    def submitL2(tx: L2Transaction | L2Withdrawal): Either[String, TxId]

    def stateL2(): List[(UtxoId[L2], Output[L2])]

    def produceBlock(
        nextBlockFinal: Boolean
    ): Either[String, (BlockRecord, Option[(TxId, L2Genesis)])]

    def shutdownSut(): Unit
