package hydrozoa.multisig.persistence.codec

import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{coinDecoder, coinEncoder}
import hydrozoa.multisig.ledger.l1.utxo.{Equity, MultisigTreasuryUtxo}
import hydrozoa.multisig.persistence.codec.HydrozoaLocalCodecs.given
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Coin
import scalus.uplc.builtin.ByteString

/** Round-trip tests for the Hydrozoa local-type persistence codecs. */
class HydrozoaLocalCodecsTest extends AnyFunSuite:

    test("MultisigTreasuryUtxo.Datum round-trips through Circe JSON") {
        val datum = MultisigTreasuryUtxo.Datum(
          commit = ByteString.fromArray(Array.fill[Byte](48)(0xab.toByte)),
          versionMajor = BigInt(7)
        )
        val json = datum.asJson.noSpaces
        val back = decode[MultisigTreasuryUtxo.Datum](json)
        assert(back == Right(datum), s"decoded $back from $json")
    }

    test("Equity round-trips through Circe JSON for valid (non-negative) amounts") {
        val equity = Equity(Coin(1_000_000L)).getOrElse(fail("expected Some(Equity)"))
        val json = equity.asJson.noSpaces
        val back = decode[Equity](json)
        assert(back == Right(equity), s"decoded $back from $json")
    }

    test("Equity decode rejects negative amounts via DecodingFailure") {
        // Equity smart-constructor rejects negative values; the codec surfaces that as a
        // decoding failure when the on-disk JSON is corrupt.
        val negativeCoin = Coin(-1L)
        val json = negativeCoin.asJson.noSpaces
        val back = decode[Equity](json)
        assert(
          back.isLeft && back.left.exists(_.getMessage.contains("non-negative")),
          s"expected non-negative failure, got $back"
        )
    }
