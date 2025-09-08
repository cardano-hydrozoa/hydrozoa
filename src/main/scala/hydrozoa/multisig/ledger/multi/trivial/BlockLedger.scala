package hydrozoa.multisig.ledger.multi.trivial

import hydrozoa.multisig.ledger.l1.real.LedgerL1
import hydrozoa.multisig.ledger.l2.real.LedgerL2

object BlockLedger {
    sealed trait Transaction
    sealed trait ConsensusTransaction extends Transaction
    sealed trait UserTransaction extends Transaction
    
    final case class InternalTxL2(tx: LedgerL2.InnerTx)
    
    final case class WithdrawalTxL2(tx: LedgerL2.WithdrawalTx)
    
    final case class DepositTxL1(tx: LedgerL1.DepositTx)
}

final case class BlockLedger(private val actualLedgerL2: LedgerL2, private val virtualLedgerL1: LedgerL1) {
    
}
