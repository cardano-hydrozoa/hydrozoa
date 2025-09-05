package hydrozoa.multisig.ledger.multi.trivial

sealed trait LedgerEvent

case class TransactionL2() extends LedgerEvent
case class WithdrawalL2() extends LedgerEvent
case class DepositL1() extends LedgerEvent

sealed trait LedgerCallback

case class DepositCallbackL1() extends LedgerCallback

class MultiLedger {

}
