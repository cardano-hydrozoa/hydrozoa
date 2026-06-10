package hydrozoa.multisig.persistence.codec

import hydrozoa.multisig.ledger.l2.Destination
import io.bullet.borer.Cbor
import io.circe.{Decoder, Encoder, Json}
import scala.util.Try
import scalus.uplc.builtin.ByteString

/** Persistence-layer Circe codec for [[Destination]] — encodes as CBOR-hex round-trip.
  *
  * The codec in `Destination`'s companion is asymmetric (encoder produces bech32 JSON; decoder
  * reads CBOR-hex), so we declare a fresh, symmetric pair here. Encoder uses the borer
  * `Cbor.encode` (the type already has `given io.bullet.borer.Encoder[Destination]`); decoder goes
  * the other way.
  */
object DestinationCodec:

    given destinationEncoder: Encoder[Destination] = Encoder.instance { dest =>
        Json.fromString(ByteString.fromArray(Cbor.encode(dest).toByteArray).toHex)
    }

    given destinationDecoder: Decoder[Destination] = Decoder.decodeString.emap(hexStr =>
        Try {
            val bytes = ByteString.fromHex(hexStr).bytes
            Cbor.decode(bytes).to[Destination].value
        }.toEither.left.map(e => s"Failed to decode Destination from CBOR-hex: ${e.getMessage}")
    )
