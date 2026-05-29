package hydrozoa.multisig.persistence.codec

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.l1.utxo.MultisigTreasuryUtxo
import hydrozoa.multisig.persistence.codec.TreasuryCodec.given
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite

/** Round-trip test for [[MultisigTreasuryUtxo]]'s persistence codec. */
class TreasuryCodecTest extends AnyFunSuite:

    given CardanoNetwork.Section = CardanoNetwork.Preview

    test("MultisigTreasuryUtxo round-trips through the persistence Circe codec") {
        val treasury = TreasuryFixture.sampleTreasury
        val json = treasury.asJson.noSpaces
        val back = decode[MultisigTreasuryUtxo](json)
        assert(back == Right(treasury), s"decoded $back from $json")
    }
