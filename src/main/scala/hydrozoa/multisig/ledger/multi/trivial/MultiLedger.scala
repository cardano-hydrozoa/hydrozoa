package hydrozoa.multisig.ledger.multi.trivial

import hydrozoa.multisig.ledger.l1.trivial.LedgerL1
import hydrozoa.multisig.ledger.l2.trivial.LedgerL2

object MultiLedger {
  sealed trait LedgerEvent

  final case class TransactionL2(tx: LedgerL2.Transaction)

  final case class WithdrawalL2(tx: LedgerL2.Withdrawal)

  final case class DepositL1(tx: LedgerL1.DepositTx)

  sealed trait LedgerEventOutcome
  sealed trait LedgerEventSuccess
  sealed trait LedgerEventFailure

  final case class TransactionL2Success() extends LedgerEventSuccess
  final case class TransactionL2Failure() extends LedgerEventFailure

  final case class WithdrawalL2Success() extends LedgerEventSuccess
  final case class WithdrawalL2Failure() extends LedgerEventFailure

  final case class DepositL1Success(postDatedRefund: LedgerL1.RefundTx) extends LedgerEventSuccess
  final case class DepositL1Failure() extends LedgerEventFailure
}