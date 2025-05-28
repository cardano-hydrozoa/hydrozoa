package hydrozoa.l2.ledger

import hydrozoa.TxId
import hydrozoa.l2.ledger.L2EventLabel.{
    L2EventGenesisLabel,
    L2EventTransactionLabel,
    L2EventWithdrawalLabel
}

sealed trait L2Event:
    def getEventId: TxId

enum L2EventLabel derives CanEqual:
    case L2EventGenesisLabel
    case L2EventTransactionLabel
    case L2EventWithdrawalLabel

def l2EventLabel(e: L2Event): L2EventLabel =
    e match
        // case _: L2EventGenesis     => L2EventGenesisLabel
        case _: L2EventTransaction => L2EventTransactionLabel
        case _: L2EventWithdrawal  => L2EventWithdrawalLabel

final case class L2EventTransaction(
    eventId: TxId,
    transaction: L2Transaction
) extends L2Event:
    override def getEventId: TxId = eventId

final case class L2EventWithdrawal(
    eventId: TxId,
    withdrawal: L2Withdrawal
) extends L2Event:
    override def getEventId: TxId = eventId

// TODO: stop using event's for consensus first
//final case class L2EventGenesis(
//    eventId: TxId,
//    genesis: L2Genesis
//) extends L2Event:
//    override def getEventId: TxId = eventId

def mkTransactionEvent(tx: L2Transaction): L2EventTransaction =
    val txId = calculateTxHash(tx)
    L2EventTransaction(txId, tx)

def mkWithdrawalEvent(withdrawal: L2Withdrawal): L2EventWithdrawal =
    val txId = calculateWithdrawalHash(withdrawal)
    L2EventWithdrawal(txId, withdrawal)
