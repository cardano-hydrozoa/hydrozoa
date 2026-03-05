package hydrozoa.multisig.ledger.l2

// see: https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events

import cats.syntax.all.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.LedgerEventId
import scalus.cardano.ledger.{Coin, TransactionInput, Value}

/** This trait is the Ledger Event type between hydrozoa and a black-box [[L2Ledger]]. See
  * [[hydrozoa.multisig.ledger.event.UserEvent]] for the type representing events between end-users
  * and hydrozoa
  */
sealed trait L2LedgerEvent

// TODO: We probably want to name these "commands" instead, but I'm
// going to wait for a spec change before doing so.
object L2LedgerEvent {

    /** An L2Event, as forwarded to black-box L2 ledger. It can only be constructed with respect to
      * a user-submitted event and a JointLedger.Producing state.
      */
    final case class L2Event(
        eventId: LedgerEventId,
        blockNumber: BlockNumber,
        blockCreationStartTime: QuantizedInstant,
        l2Payload: Array[Byte]
    ) extends L2LedgerEvent

    final case class DepositEventRegistration(
        eventId: LedgerEventId,
        blockNumber: BlockNumber,
        blockCreationStartTime: QuantizedInstant,
        depositUtxoId: TransactionInput,
        depositFee: Coin,
        depositL2Value: Value,
        l2Payload: Array[Byte],
    ) extends L2LedgerEvent

    final case class DepositEventDecisions(
        blockNumber: BlockNumber,
        absorbedDeposits: Vector[LedgerEventId],
        rejectedDeposits: Vector[LedgerEventId]
    ) extends L2LedgerEvent
}
