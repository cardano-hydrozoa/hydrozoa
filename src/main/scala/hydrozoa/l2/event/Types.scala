package hydrozoa.l2.event

import hydrozoa.l2.ledger.{SimpleTransaction, SimpleWithdrawal}

sealed trait L2Event

trait L2Genesis extends L2Event

trait L2NonGenesis extends L2Event

case class L2Transaction(simpleTransaction: SimpleTransaction) extends L2NonGenesis

case class L2Withdrawal(simpleWithdrawal: SimpleWithdrawal) extends L2NonGenesis
