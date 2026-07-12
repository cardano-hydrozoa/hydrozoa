package hydrozoa.bootstrap

import hydrozoa.config.head.network.CardanoNetwork
import io.circe.parser
import org.scalatest.funsuite.AnyFunSuite

/** Pins the JSON field names BuildHeadConfig reads, so rosters written by [[GenerateKeyPair]] keep
  * decoding. A successful decode proves every field name (verificationKey / webSocketAddress /
  * hubHeadPeerNumber / coilQuorum / headPeers / coilPeers) matches the [[Bootstrap.Membership]]
  * decoder.
  */
class BootstrapMembershipTest extends AnyFunSuite {

    test("Membership decodes the documented peers.json shape (incl. distributed hubs)") {
        val json =
            """{
              |  "headPeers": [
              |    { "verificationKey": "0000000000000000000000000000000000000000000000000000000000000000", "webSocketAddress": "ws://head-0:4001" },
              |    { "verificationKey": "1111111111111111111111111111111111111111111111111111111111111111", "webSocketAddress": "ws://head-1:4001" }
              |  ],
              |  "coilPeers": [
              |    { "verificationKey": "2222222222222222222222222222222222222222222222222222222222222222", "hubHeadPeerNumber": 0 },
              |    { "verificationKey": "3333333333333333333333333333333333333333333333333333333333333333", "hubHeadPeerNumber": 1 }
              |  ],
              |  "coilQuorum": 1
              |}""".stripMargin

        val membership =
            parser
                .decode[Bootstrap.Membership](json)
                .fold(e => fail(s"decode failed: $e"), identity)

        assert(
          membership.headPeers.size == 2 &&
              membership.coilPeers.size == 2 &&
              membership.coilQuorum == 1 &&
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
              |  "coilPeers": [],
              |  "coilQuorum": 0
              |}""".stripMargin

        val membership =
            parser
                .decode[Bootstrap.Membership](json)
                .fold(e => fail(s"decode failed: $e"), identity)

        assert(membership.headPeers.size == 1 && membership.coilPeers.isEmpty)
    }

    test("BootstrapConfig decodes a full bootstrap.json (network + script refs + evacuation map)") {
        // The evacuation-map decoder needs the L1 network; the custom decoder must read
        // `cardanoNetwork` first and supply it, so this exercises that ordering.
        val json =
            """{
              |  "cardanoNetwork": "preview",
              |  "headPeers": [
              |    { "verificationKey": "85f2378d9af901be0b6d096f8e4b5573274f9bb1d5bfadb86ecac6e51504bbad", "webSocketAddress": "ws://head-0:4001" }
              |  ],
              |  "coilPeers": [],
              |  "coilQuorum": 0,
              |  "scriptReferenceUtxos": {
              |    "rulebasedTreasuryScriptInput": "d17362c69150ccf4cf0974ee4223f5f8c84b9171d74cd210aa042860a1a32ecd#0",
              |    "disputeResolutionScriptInput": "83ec7059e234a2d21b059ae81da478a2be994916675c7cede4617a8267a8e1ff#0"
              |  },
              |  "initialEvacuationMap": {
              |    "ef779a7b07ef490490ae0039458bccb3e78df0776dbf014e3cf780c600000000": "a200581d60ef779a7b07ef490490ae0039458bccb3e78df0776dbf014e3cf780c6011a00989680"
              |  }
              |}""".stripMargin

        val config =
            parser
                .decode[Bootstrap.BootstrapConfig](json)
                .fold(e => fail(s"decode failed: $e"), identity)

        assert(
          config.cardanoNetwork == CardanoNetwork.Preview &&
              config.headPeers.size == 1 &&
              config.initialEvacuationMap.evacuationMap.size == 1
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
              |  "initialEvacuationMap": {}
              |}""".stripMargin

        assert(parser.decode[Bootstrap.BootstrapConfig](json).isLeft)
    }
}
