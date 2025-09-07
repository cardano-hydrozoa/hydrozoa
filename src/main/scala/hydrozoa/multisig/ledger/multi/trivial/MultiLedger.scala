package hydrozoa.multisig.ledger.multi.trivial

sealed trait LedgerEvent

case class TransactionL2() extends LedgerEvent
case class WithdrawalL2() extends LedgerEvent
case class DepositL1() extends LedgerEvent

// Not sure exactly how to define these and how/where to map block contents to them
sealed trait LedgerEventOutcome

case class LedgerEventSuccess()
case class LedgerEventSuccessWithEffect()
case class LedgerEventFailure()

sealed trait LedgerCallback

case class DepositCallbackL1() extends LedgerCallback

class MultiLedger {

}
