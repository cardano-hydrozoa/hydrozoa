package hydrozoa.config.head.network

import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.CardanoInfo

/** Shape coverage for the [[CardanoNetwork]] JSON codec.
  *
  * The three standard networks encode as bare strings and round-trip. A `Custom` network encodes as
  * `{ "custom": <CardanoInfo>, "protocolMagic": <Long> }`; this pins that `protocolMagic` is
  * written as the magic (a number). A prior encoder bug wrote the protocol-params object under that
  * key instead, dropping the magic entirely.
  *
  * Note: a full `Custom` decode is not asserted here because it additionally requires the
  * `CardanoInfo` / `ProtocolParams` codec to round-trip, which is a separate open issue (the scalus
  * blockfrost ReadWriter is write/read asymmetric) — see docs/local/integration/phases.md.
  */
class CardanoNetworkCodecTest extends AnyFunSuite:

    test("standard networks round-trip through JSON") {
        List(CardanoNetwork.Mainnet, CardanoNetwork.Preprod, CardanoNetwork.Preview).foreach { n =>
            assert(n.asJson.as[CardanoNetwork] == Right(n), s"round-trip failed for $n")
        }
    }

    test("a Custom network encodes protocolMagic as the magic number") {
        val magic =
            42L // distinct from every standard magic (mainnet 764824073, preprod 1, preview 2)
        val custom: CardanoNetwork = CardanoNetwork.Custom(CardanoInfo.preview, magic)
        val encoded = custom.asJson
        assert(
          encoded.hcursor.downField("protocolMagic").as[Long] == Right(magic),
          s"protocolMagic must encode as the Long magic, not the params object; got ${encoded.noSpaces}"
        )
    }

end CardanoNetworkCodecTest
