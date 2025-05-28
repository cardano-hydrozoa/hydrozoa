package hydrozoa.l2.ledger

import hydrozoa.infra.{Piper, plutusAddressAsL2}
import hydrozoa.l1.multisig.state.{DepositUtxos, depositDatum}
import hydrozoa.l2.ledger.L2EventLabel.{L2EventTransactionLabel, L2EventWithdrawalLabel}
import hydrozoa.{AddressBechL1, AddressBechL2, L2, Output, OutputL2, TxId, UtxoIdL2}

sealed trait L2Event:
    def getEventId: TxId

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

//final case class GenesisEventL2(eventId: TxId, genesis: L2Genesis)

enum L2EventLabel derives CanEqual:
    case L2EventTransactionLabel
    case L2EventWithdrawalLabel

def l2EventLabel(e: L2Event): L2EventLabel =
    e match
        case _: L2EventTransaction => L2EventTransactionLabel
        case _: L2EventWithdrawal  => L2EventWithdrawalLabel

case class L2Genesis(
    outputs: List[OutputL2]
) derives CanEqual:
    def volume(): Long = outputs.map(_.coins).sum.toLong

object L2Genesis:
    def apply(ds: DepositUtxos): L2Genesis =
        L2Genesis(
          ds.unTag.utxoMap.values
              .map(o =>
                  val datum = depositDatum(o) match
                      case Some(datum) => datum
                      case None =>
                          throw RuntimeException("deposit UTxO doesn't contain a proper datum")
                  Output.apply(datum.address |> plutusAddressAsL2, o.coins)
              )
              .toList
        )

// FIXME: implement network translation (?)
def liftAddress(l: AddressBechL1): AddressBechL2 = l.asL2

case class L2Transaction(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[UtxoIdL2],
    outputs: List[OutputL2]
):
    def volume(): Long = outputs.map(_.coins).sum.toLong

case class L2Withdrawal(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[UtxoIdL2]
)
