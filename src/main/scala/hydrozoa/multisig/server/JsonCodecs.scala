package hydrozoa.multisig.server

import hydrozoa.multisig.consensus.EventSequencer.{DepositRequest, L2TxRequest}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.{LedgerEventId, LedgerEventNumber}
import hydrozoa.multisig.server.ApiResponse.{Error, EventSubmitted}
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import scalus.cardano.ledger.{Coin, Value}
import scodec.bits.ByteVector

/** JSON encoders and decoders for API types */
object JsonCodecs {

    // Encode/decode byte arrays as hex strings
    implicit val byteArrayEncoder: Encoder[Array[Byte]] =
        Encoder.encodeString.contramap(bytes => ByteVector(bytes).toHex)

    implicit val byteArrayDecoder: Decoder[Array[Byte]] =
        Decoder.decodeString.emap { hexStr =>
            ByteVector
                .fromHex(hexStr)
                .map(_.toArray)
                .toRight(s"Invalid hex string: $hexStr")
        }

    // LedgerEventNumber codec
    implicit val ledgerEventNumberEncoder: Encoder[LedgerEventNumber] =
        Encoder.encodeInt.contramap(_.convert)

    implicit val ledgerEventNumberDecoder: Decoder[LedgerEventNumber] =
        Decoder.decodeInt.map(LedgerEventNumber.apply)

    // HeadPeerNumber codec
    implicit val headPeerNumberEncoder: Encoder[HeadPeerNumber] =
        Encoder.encodeInt.contramap(_.convert)

    implicit val headPeerNumberDecoder: Decoder[HeadPeerNumber] =
        Decoder.decodeInt.map(HeadPeerNumber.apply)

    // Coin codec
    implicit val coinEncoder: Encoder[Coin] = Encoder.encodeLong.contramap(_.value)
    implicit val coinDecoder: Decoder[Coin] = Decoder.decodeLong.map(Coin.apply)

    // Value codec (simplified - only handles lovelace for now)
    // TODO: Add full multi-asset support if needed
    implicit val valueEncoder: Encoder[Value] = (value: Value) =>
        io.circe.Json.obj(
          "lovelace" -> value.coin.asJson
        )

    implicit val valueDecoder: Decoder[Value] = c =>
        for {
            lovelace <- c.downField("lovelace").as[Coin]
        } yield Value(lovelace)

    // LedgerEventId codec
    implicit val ledgerEventIdEncoder: Encoder[LedgerEventId] = (eventId: LedgerEventId) =>
        io.circe.Json.obj(
          "peerNum" -> eventId.peerNum.asJson,
          "eventNum" -> eventId.eventNum.asJson
        )

    implicit val ledgerEventIdDecoder: Decoder[LedgerEventId] = c =>
        for {
            peerNum <- c.downField("peerNum").as[HeadPeerNumber]
            eventNum <- c.downField("eventNum").as[LedgerEventNumber]
        } yield LedgerEventId(peerNum, eventNum)

    // Request types
    implicit val l2TxRequestDecoder: Decoder[L2TxRequest] = deriveDecoder[L2TxRequest]
    implicit val l2TxRequestEncoder: Encoder[L2TxRequest] = deriveEncoder[L2TxRequest]

    implicit val depositRequestDecoder: Decoder[DepositRequest] = deriveDecoder[DepositRequest]
    implicit val depositRequestEncoder: Encoder[DepositRequest] = deriveEncoder[DepositRequest]

    // Response types
    implicit val eventSubmittedEncoder: Encoder[EventSubmitted] = deriveEncoder[EventSubmitted]
    implicit val eventSubmittedDecoder: Decoder[EventSubmitted] = deriveDecoder[EventSubmitted]

    implicit val errorEncoder: Encoder[Error] = deriveEncoder[Error]
    implicit val errorDecoder: Decoder[Error] = deriveDecoder[Error]
}
