package hydrozoa.multisig.ledger.l2

// see: https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events

import cats.syntax.all.*
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l2
import io.bullet.borer.derivation.CompactMapBasedCodecs.derived
import io.bullet.borer.{Cbor, Decoder, Encoder}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Coin, TransactionInput, Value}
import scalus.cardano.onchain.plutus.v3.PosixTime
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.{ByteString, Data}

// Temporary type to represent the "Destination" concept. If we want full flexibility, we'd need to
// allow for the destination to accommodate both hashed and inline datums
case class Destination(address: Address, datum: Option[Data]) {
    def toHex: String = ByteString.fromArray(Cbor.encode(this).toByteArray).toHex
}

given io.bullet.borer.Encoder[Destination] = Encoder.derived
given io.bullet.borer.Decoder[Destination] = Decoder.derived

/** This trait is the Ledger Event type between hydrozoa and a black-box [[L2Ledger]]. See
  * [[hydrozoa.multisig.ledger.event.UserRequest]] for the type representing events between
  * end-users and hydrozoa
  */
sealed trait L2LedgerCommand

// TODO: We probably want to name these "commands" instead, but I'm
// going to wait for a spec change before doing so.
object L2LedgerCommand {
    sealed trait Real extends L2LedgerCommand
    sealed trait Proxy extends L2LedgerCommand

    final case class RegisterDepositRequest(
        requestId: RequestId,
        userVKey: VerificationKey,
        blockNumber: BlockNumber,
        blockCreationStartTime: PosixTime,
        depositId: TransactionInput,
        depositFee: Coin,
        depositL2Value: Value,
        refundDestination: Destination,
        l2Payload: ByteString
    ) extends L2LedgerCommand.Real

    final case class ApplyDepositDecisions(
        blockNumber: BlockNumber,
        blockCreationEndTime: PosixTime,
        absorbedDeposits: List[RequestId],
        rejectedDeposits: List[RequestId]
    ) extends L2LedgerCommand.Real

    /** An L2Event, as forwarded to black-box L2 ledger. It can only be constructed with respect to
      * a user-submitted event and a JointLedger.Producing state.
      */
    final case class ApplyTransaction(
        requestId: RequestId,
        userVKey: VerificationKey,
        blockNumber: BlockNumber,
        blockCreationStartTime: PosixTime,
        l2Payload: ByteString
    ) extends L2LedgerCommand.Real

    final case class ProxyBlockConfirmation(
        blockNumber: BlockNumber,
        refundTxs: Vector[(RequestId, Tx.Serialized)]
    ) extends L2LedgerCommand.Proxy

    final case class ProxyRequestError(
        requestId: RequestId,
        message: String
    ) extends L2LedgerCommand.Proxy

}
