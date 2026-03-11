package hydrozoa.multisig.server

import hydrozoa.lib.cardano.cip116
import hydrozoa.multisig.consensus.EventSequencer.{DepositRequest, L2TxRequest}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.{LedgerEventId, LedgerEventNumber}
import hydrozoa.multisig.server.ApiResponse.{Error, EventSubmitted}
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import scalus.cardano.ledger.*

/** JSON encoders and decoders for API types */
object JsonCodecs {

    import cip116.JsonCodecs.CIP0116.Conway.given

    // LedgerEventNumber codec
    given ledgerEventNumberEncoder: Encoder[LedgerEventNumber] =
        Encoder.encodeInt.contramap(_.convert)

    given ledgerEventNumberDecoder: Decoder[LedgerEventNumber] =
        Decoder.decodeInt.map(LedgerEventNumber.apply)

    // HeadPeerNumber codec
    given headPeerNumberEncoder: Encoder[HeadPeerNumber] =
        Encoder.encodeInt.contramap(_.convert)

    given headPeerNumberDecoder: Decoder[HeadPeerNumber] =
        Decoder.decodeInt.map(HeadPeerNumber.apply)

    // LedgerEventId codec
    given ledgerEventIdEncoder: Encoder[LedgerEventId] = (eventId: LedgerEventId) =>
        io.circe.Json.obj(
          "peerNum" -> eventId.peerNum.asJson,
          "eventNum" -> eventId.eventNum.asJson
        )

    given ledgerEventIdDecoder: Decoder[LedgerEventId] = c =>
        for {
            peerNum <- c.downField("peerNum").as[HeadPeerNumber]
            eventNum <- c.downField("eventNum").as[LedgerEventNumber]
        } yield LedgerEventId(peerNum, eventNum)

    // Request types
    given l2TxRequestDecoder: Decoder[L2TxRequest] = deriveDecoder[L2TxRequest]

    given l2TxRequestEncoder: Encoder[L2TxRequest] = deriveEncoder[L2TxRequest]

    given depositRequestDecoder: Decoder[DepositRequest] = deriveDecoder[DepositRequest]

    given depositRequestEncoder: Encoder[DepositRequest] = deriveEncoder[DepositRequest]

    // Response types
    given eventSubmittedEncoder: Encoder[EventSubmitted] = deriveEncoder[EventSubmitted]

    given eventSubmittedDecoder: Decoder[EventSubmitted] = deriveDecoder[EventSubmitted]

    given errorEncoder: Encoder[Error] = deriveEncoder[Error]

    given errorDecoder: Decoder[Error] = deriveDecoder[Error]
}
