package hydrozoa.l2.event

import hydrozoa.l2.ledger.{SimpleGenesis, SimpleTransaction, SimpleWithdrawal}

sealed trait L2Event

sealed trait L2GenesisEvent(simpleGenesis: SimpleGenesis) extends L2Event

sealed trait L2NonGenesisEvent extends L2Event

sealed case class L2TransactionEvent(simpleTransaction: SimpleTransaction) extends L2NonGenesisEvent

sealed case class L2WithdrawalEvent(simpleWithdrawal: SimpleWithdrawal) extends L2NonGenesisEvent
