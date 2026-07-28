package hydrozoa.config.head.network

import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.given
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, ProtocolParams, ProtocolVersion}

/** Shape and round-trip coverage for the [[CardanoNetwork]] JSON codec.
  *
  * The three standard networks encode as bare strings. A `Custom` network encodes as
  * `{ "custom": <CardanoInfo>, "protocolMagic": <Long> }` and must round-trip in full, which relies
  * on the structural `ProtocolParams` / `CardanoInfo` codec in
  * [[hydrozoa.lib.cardano.scalus.codecs.json.Codecs]] preserving every field exactly.
  */
class CardanoNetworkCodecTest extends AnyFunSuite:

    // Real protocol params (with tiny ExUnitPrices) make these strong round-trip fixtures: they
    // catch any precision loss a Double-routed codec would introduce.
    private val infos = List(CardanoInfo.mainnet, CardanoInfo.preprod, CardanoInfo.preview)

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

    test("ProtocolParams round-trips through JSON") {
        infos.foreach { info =>
            val pp = info.protocolParams
            assert(
              pp.asJson.as[ProtocolParams] == Right(pp),
              s"ProtocolParams round-trip failed for ${info.network}"
            )
        }
    }

    test("a Custom network round-trips through JSON in full") {
        infos.foreach { info =>
            val custom: CardanoNetwork = CardanoNetwork.Custom(info, 42L)
            assert(
              custom.asJson.as[CardanoNetwork] == Right(custom),
              s"Custom round-trip failed for ${info.network}"
            )
        }
    }

    test("a malformed ProtocolVersion decodes to a Left, not a thrown exception") {
        // ProtocolVersion requires major >= 1; a derived decoder would let that require throw. The
        // total decoder must surface it as a DecodingFailure (a Left) instead.
        val badVersion = Json.obj("major" -> Json.fromInt(0), "minor" -> Json.fromInt(0))
        assert(
          badVersion.as[ProtocolVersion].isLeft,
          s"expected a Left for major=0, got ${badVersion.as[ProtocolVersion]}"
        )
    }

end CardanoNetworkCodecTest
