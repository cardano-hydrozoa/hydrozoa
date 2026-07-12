package hydrozoa.bootstrap

import hydrozoa.config.head.network.CardanoNetwork
import io.circe.parser
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.Value

/** Pins the JSON field names the bootstrap tooling reads. A successful decode proves the field
  * names (verificationKey / webSocketAddress / hubHeadPeerNumber / headPeers / coilPeers) match the
  * [[Bootstrap.Membership]] decoder, and that a [[Bootstrap.BootstrapConfig]] round-trips.
  */
class BootstrapMembershipTest extends AnyFunSuite {

    test("Membership (roster) decodes the documented shape (incl. distributed hubs)") {
        val json =
            """{
              |  "headPeers": [
              |    { "verificationKey": "0000000000000000000000000000000000000000000000000000000000000000", "webSocketAddress": "ws://head-0:4001" },
              |    { "verificationKey": "1111111111111111111111111111111111111111111111111111111111111111", "webSocketAddress": "ws://head-1:4001" }
              |  ],
              |  "coilPeers": [
              |    { "verificationKey": "2222222222222222222222222222222222222222222222222222222222222222", "hubHeadPeerNumber": 0 },
              |    { "verificationKey": "3333333333333333333333333333333333333333333333333333333333333333", "hubHeadPeerNumber": 1 }
              |  ]
              |}""".stripMargin

        val membership =
            parser
                .decode[Bootstrap.Membership](json)
                .fold(e => fail(s"decode failed: $e"), identity)

        assert(
          membership.headPeers.size == 2 &&
              membership.coilPeers.size == 2 &&
              membership.headPeers.head.webSocketAddress.renderString == "ws://head-0:4001"
        )
    }

    test("Membership decoding ignores the _comment helper key") {
        val json =
            """{
              |  "_comment": ["this key documents the file and must be ignored by the parser"],
              |  "headPeers": [
              |    { "verificationKey": "0000000000000000000000000000000000000000000000000000000000000000", "webSocketAddress": "ws://head-0:4001" }
              |  ],
              |  "coilPeers": []
              |}""".stripMargin

        val membership =
            parser
                .decode[Bootstrap.Membership](json)
                .fold(e => fail(s"decode failed: $e"), identity)

        assert(membership.headPeers.size == 1 && membership.coilPeers.isEmpty)
    }

    test("BootstrapConfig decodes a full bootstrap.json (network + script refs + L2 state)") {
        val json =
            """{
              |  "cardanoNetwork": "preview",
              |  "headPeers": [
              |    { "verificationKey": "85f2378d9af901be0b6d096f8e4b5573274f9bb1d5bfadb86ecac6e51504bbad", "webSocketAddress": "ws://head-0:4001" }
              |  ],
              |  "coilPeers": [],
              |  "coilQuorum": 1,
              |  "scriptReferenceUtxos": {
              |    "rulebasedTreasuryScriptInput": "d17362c69150ccf4cf0974ee4223f5f8c84b9171d74cd210aa042860a1a32ecd#0",
              |    "disputeResolutionScriptInput": "83ec7059e234a2d21b059ae81da478a2be994916675c7cede4617a8267a8e1ff#0"
              |  },
              |  "initialL2State": []
              |}""".stripMargin

        val config =
            parser
                .decode[Bootstrap.BootstrapConfig](json)
                .fold(e => fail(s"decode failed: $e"), identity)

        assert(
          config.cardanoNetwork == CardanoNetwork.Preview &&
              config.headPeers.size == 1 &&
              config.coilQuorum == 1 &&
              config.initialL2State.isEmpty
        )
    }

    test("BootstrapConfig rejects a Custom (non-Blockfrost) network") {
        val json =
            """{
              |  "cardanoNetwork": "custom",
              |  "headPeers": [],
              |  "coilPeers": [],
              |  "coilQuorum": 0,
              |  "scriptReferenceUtxos": {
              |    "rulebasedTreasuryScriptInput": "d17362c69150ccf4cf0974ee4223f5f8c84b9171d74cd210aa042860a1a32ecd#0",
              |    "disputeResolutionScriptInput": "83ec7059e234a2d21b059ae81da478a2be994916675c7cede4617a8267a8e1ff#0"
              |  },
              |  "initialL2State": []
              |}""".stripMargin

        assert(parser.decode[Bootstrap.BootstrapConfig](json).isLeft)
    }

    test("L2Output round-trips through its CIP-0116 JSON") {
        val address =
            Address.fromBech32("addr_test1vrhh0xnmqlh5jpys4cqrj3vteje70r0swakm7q2w8nmcp3sh5wdk4")
        val output = Bootstrap.L2Output(address, Value.ada(5L))
        val decoded =
            output.asJson.as[Bootstrap.L2Output].fold(e => fail(s"decode failed: $e"), identity)
        assert(decoded.value == output.value)
    }
}
