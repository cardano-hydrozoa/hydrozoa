package hydrozoa.l2.event

import hydrozoa.l2.ledger.{SimpleGenesis, SimpleTransaction, SimpleWithdrawal}
import hydrozoa.{PosixTime, TxId}

sealed abstract class L2Event(timeReceived: PosixTime, eventId: TxId)

sealed case class L2GenesisEvent(
    timeReceived: PosixTime,
    eventId: TxId,
    simpleGenesis: SimpleGenesis
) extends L2Event(timeReceived, eventId)

sealed abstract class L2NonGenesisEvent(timeReceived: PosixTime, eventId: TxId)
    extends L2Event(timeReceived, eventId) {
    def getEventId(): TxId = eventId
}

sealed case class L2TransactionEvent(
    timeReceived: PosixTime,
    eventId: TxId,
    simpleTransaction: SimpleTransaction
) extends L2NonGenesisEvent(timeReceived, eventId)

sealed case class L2WithdrawalEvent(
    timeReceived: PosixTime,
    eventId: TxId,
    simpleWithdrawal: SimpleWithdrawal
) extends L2NonGenesisEvent(timeReceived, eventId)
