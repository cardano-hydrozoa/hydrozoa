package hydrozoa.multisig.ledger.multi.trivial

import hydrozoa.multisig.ledger.l1.real.LedgerL1
import hydrozoa.multisig.ledger.l2.real.LedgerL2

object BlockLedger {
}

abstract final case class BlockLedger() {
    sealed trait Transaction

    sealed trait ConsensusTransaction extends Transaction

    final case class CompleteBlock () extends ConsensusTransaction

    val actualLedgerL2: LedgerL2
    val virtualLedgerL1: LedgerL1
    val withdrawnUtxos: List[Unit]
    val addedUtxos: List[Unit]

    sealed trait UserTransaction extends Transaction

    final case class InternalTxL2(tx: LedgerL2.InnerTx) extends UserTransaction

    final case class WithdrawalTxL2(tx: LedgerL2.WithdrawalTx) extends UserTransaction

    final case class DepositTxL1(tx: virtualLedgerL1.DepositTx) extends UserTransaction

    final case class InitializationTxL1(tx: virtualLedgerL1.InitializationTx) extends ConsensusTransaction

    final case class FinalizationTxL1(tx: virtualLedgerL1.FinalizationTx) extends ConsensusTransaction


}
