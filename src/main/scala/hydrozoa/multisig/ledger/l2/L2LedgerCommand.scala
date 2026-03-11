package hydrozoa.multisig.ledger.l2

// see: https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events

import cats.syntax.all.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestId
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Coin, DatumOption, TransactionInput, Value}
import scalus.cardano.onchain.plutus.v3.PosixTime
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.Data

// Temporary type to represent the "Destination" concept. If we want full flexibility, we'd need to
// allow for the destination to accomodate both hashed and inline datums
type Destination = (Address, Option[Data])

/** This trait is the Ledger Event type between hydrozoa and a black-box [[L2Ledger]]. See
  * [[hydrozoa.multisig.ledger.event.UserRequest]] for the type representing events between end-users
  * and hydrozoa
  */
sealed trait L2LedgerCommand

// TODO: We probably want to name these "commands" instead, but I'm
// going to wait for a spec change before doing so.
object L2LedgerCommand {

    /** An L2Event, as forwarded to black-box L2 ledger. It can only be constructed with respect to
      * a user-submitted event and a JointLedger.Producing state.
      */
    final case class ApplyTransactionRequest(
                                                requestId: RequestId,
                                                blockNumber: BlockNumber,
                                                blockCreationStartTime: QuantizedInstant,
                                                verifierKey : VerificationKey,
                                                l2Payload: Array[Byte]
    ) extends L2LedgerCommand

    final case class RegisterDepositRequest(
                                               requestId: RequestId,
                                               blockNumber: BlockNumber,
                                               blockCreationStartTime: PosixTime,
                                               depositUtxoId: TransactionInput,
                                               depositFee: Coin,
                                               depositL2Value: Value,
                                               refundDestination : Destination,
                                               verifierKey: VerificationKey,
                                               l2Payload: Array[Byte],
    ) extends L2LedgerCommand

    final case class ApplyDepositDecisions(
        blockNumber: BlockNumber,
        blockCreationEndTime: PosixTime,
        absorbedDeposits: Vector[RequestId],
        rejectedDeposits: Vector[RequestId]
    ) extends L2LedgerCommand
}
