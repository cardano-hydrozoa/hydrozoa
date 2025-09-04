package hydrozoa.multisig.ledger.multi.trivial

sealed trait MultiLedgerEvent

case class TransactionL2() extends MultiLedgerEvent
case class WithdrawalL2() extends MultiLedgerEvent
case class DepositL1() extends MultiLedgerEvent

class MultiLedger {

}
