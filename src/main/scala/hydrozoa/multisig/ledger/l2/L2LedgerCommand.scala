package hydrozoa.multisig.ledger.l2

// see: https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events

import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l2
import io.bullet.borer.derivation.CompactMapBasedCodecs.derived
import io.bullet.borer.{Cbor, Decoder, Encoder}
import io.circe.*
import io.circe.syntax.*
import scala.util.Try
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Coin, TransactionInput, Value}
import scalus.cardano.onchain.plutus.v3.PosixTime
import scalus.uplc.builtin.{ByteString, Data}
import scodec.bits.ByteVector

// Temporary type to represent the "Destination" concept. If we want full flexibility, we'd need to
// allow for the destination to accommodate both hashed and inline datums
case class Destination(address: Address, datum: Option[Data]) {
    def toHex: String = ByteString.fromArray(Cbor.encode(this).toByteArray).toHex
}

object Destination {
    given io.bullet.borer.Encoder[Destination] = Encoder.derived
    given io.bullet.borer.Decoder[Destination] = Decoder.derived

    given destinationEncoder: io.circe.Encoder[Destination] =
        (dest: Destination) => {
            val addressBech32 = dest.address match {
                case s: scalus.cardano.address.ShelleyAddress => s.toBech32.get
                case other                                    => other.toString
            }
            io.circe.Json.obj(
              "address" -> io.circe.Json.fromString(addressBech32),
              "datum" -> io.circe.Json.Null
            )
        }
    given destinationDecoder: io.circe.Decoder[Destination] =
        io.circe.Decoder.decodeString.emap(hexStr =>
            for {
                bytes <- ByteVector
                    .fromHex(hexStr)
                    .map(_.toArray)
                    .toRight(s"Invalid hex string: $hexStr")
                dest <- Try(Cbor.decode(bytes).to[Destination].value).toEither.left.map(e =>
                    s"Could not cbor-decode the bytes: ${e}"
                )
            } yield dest
        )
}

/** This trait is the Ledger Event type between hydrozoa and a black-box [[L2Ledger]]. See
  * [[hydrozoa.multisig.ledger.event.UserRequest]] for the type representing events between
  * end-users and hydrozoa
  */
sealed trait L2LedgerCommand

object L2LedgerCommand {

    export RegisterDeposit.given
    export ApplyDepositDecisions.given
    export ApplyTransaction.given

    final case class RegisterDeposit(
        requestId: RequestId,
        blockNumber: BlockNumber,
        blockCreationStartTime: PosixTime,
        depositId: TransactionInput,
        depositFee: Coin,
        depositL2Value: Value,
        refundDestination: Destination,
        l2Payload: ByteString
    ) extends L2LedgerCommand

    object RegisterDeposit {

        // A RegisterDeposit is a ScreenDeposit plus the consensus-assigned fields, and the wire
        // form keeps that relationship: the deposit-reference fields are encoded by the
        // ScreenDeposit encoder (the screening endpoint's request body,
        // hydrozoa.multisig.ledger.remote.RemoteL2Screener), so the two bodies cannot drift.
        given depositRegistrationEncoder: io.circe.Encoder[L2LedgerCommand.RegisterDeposit] = {
            import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{valueEncoder as _, valueDecoder as _, coinDecoder as _, coinEncoder as _}
            import hydrozoa.multisig.ledger.remote.RemoteL2Screener.screenDepositEncoder
            import RequestId.i64.given // L2-ledger / SugarRush wire form (i64), not the default object
            (r: L2LedgerCommand.RegisterDeposit) =>
                L2Screener
                    .ScreenDeposit(
                      depositId = r.depositId,
                      depositFee = r.depositFee,
                      depositL2Value = r.depositL2Value,
                      refundDestination = r.refundDestination,
                      l2Payload = r.l2Payload
                    )
                    .asJson
                    .deepMerge(
                      io.circe.Json.obj(
                        "requestId" -> r.requestId.asJson,
                        "blockNumber" -> r.blockNumber.asJson,
                        "blockCreationStartTime" -> r.blockCreationStartTime.asJson
                      )
                    )
        }

        // FIXME (from Peter, after relocating from `RemoteL2LedgerCodecs`):
        // How does this round-trip? Derived decoder with custom encoder??
        given depositRegistrationDecoder: io.circe.Decoder[L2LedgerCommand.RegisterDeposit] = {
            import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{valueEncoder as _, valueDecoder as _, coinDecoder as _, coinEncoder as _, given}
            import hydrozoa.multisig.ledger.remote.RemoteL2LedgerCodecs.{given_Decoder_Value, given_Decoder_Coin}
            import RequestId.i64.given // L2-ledger / SugarRush wire form (i64), not the default object
            io.circe.generic.semiauto.deriveDecoder
        }
    }

    final case class ApplyDepositDecisions(
        blockNumber: BlockNumber,
        blockCreationEndTime: PosixTime,
        absorbedDeposits: List[RequestId],
        rejectedDeposits: List[RequestId]
    ) extends L2LedgerCommand

    object ApplyDepositDecisions {
        given Codec[L2LedgerCommand.ApplyDepositDecisions] = {
            import RequestId.i64.given // L2-ledger / SugarRush wire form (i64), not the default object
            io.circe.generic.semiauto.deriveCodec
        }
    }

    /** An L2Event, as forwarded to black-box L2 ledger. It can only be constructed with respect to
      * a user-submitted event and a JointLedger.Producing state.
      */
    final case class ApplyTransaction(
        requestId: RequestId,
        blockNumber: BlockNumber,
        blockCreationStartTime: PosixTime,
        l2Payload: ByteString
    ) extends L2LedgerCommand

    object ApplyTransaction {
        import RequestId.i64.given // L2-ledger / SugarRush wire form (i64), not the default object
        import BlockNumber.given
        import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given

        given applyTransactionEncoder: io.circe.Encoder[L2LedgerCommand.ApplyTransaction] =
            (r: L2LedgerCommand.ApplyTransaction) =>
                io.circe.Json.obj(
                  "requestId" -> r.requestId.asJson,
                  "blockNumber" -> r.blockNumber.asJson,
                  "blockCreationStartTime" -> r.blockCreationStartTime.asJson,
                  "l2Payload" -> summon[io.circe.Encoder[ByteString]].apply(r.l2Payload)
                )

        given applyTransactionDecoder: io.circe.Decoder[L2LedgerCommand.ApplyTransaction] =
            io.circe.Decoder.instance(c =>
                for {
                    rid <- c.downField("requestId").as[RequestId]
                    bn <- c.downField("blockNumber").as[BlockNumber]
                    t <- c.downField("blockCreationStartTime").as[PosixTime]
                    p <- c.downField("l2Payload").as[ByteString]
                } yield L2LedgerCommand.ApplyTransaction(rid, bn, t, p)
            )
    }

}
