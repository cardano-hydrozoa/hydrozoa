package hydrozoa.bootstrap

import hydrozoa.config.head.multisig.fallback.FallbackContingency.mkFallbackContingencyWithDefaults
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import io.circe.syntax.*
import io.circe.{Json, parser}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Coin, Value}

/** Pins the JSON field names the bootstrap tooling reads. A successful decode proves the field
  * names (verificationKey / webSocketAddress / hubHeadPeerNumber / headPeers / coilPeers) match the
  * [[Bootstrap.Membership]] decoder, and that [[Bootstrap.BootstrapDefaults]] and
  * [[Bootstrap.BootstrapConfig]] round-trip through their codecs.
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

    test("BootstrapDefaults round-trips through JSON (head params + per-peer equity)") {
        val network = CardanoNetwork.Preview
        val defaults = mkPreviewDefaults(coilQuorum = 2)
        given CardanoNetwork.Section = network
        val decoded =
            defaults.asJson
                .as[Bootstrap.BootstrapDefaults]
                .fold(e => fail(s"decode failed: $e"), identity)
        assert(
          decoded.cardanoNetwork == network &&
              decoded.headParams.coilQuorum == 2 &&
              decoded.initialEquityContributions.size == 2 &&
              decoded.blockZeroStartTime.isEmpty
        )
    }

    test(
      "BootstrapConfig decodes a full bootstrap.json (network, head params, script refs, equity)"
    ) {
        val config =
            previewBootstrapJson
                .as[Bootstrap.BootstrapConfig]
                .fold(e => fail(s"decode failed: $e"), identity)
        assert(
          config.cardanoNetwork == CardanoNetwork.Preview &&
              config.headParams.coilQuorum == 2 &&
              config.headPeers.size == 1 &&
              config.initialEquityContributions.get(HeadPeerNumber(0)).contains(Coin.ada(100)) &&
              config.initialL2State.isEmpty &&
              config.blockZeroStartTime.isEmpty
        )
    }

    test("BootstrapConfig rejects a Custom (non-Blockfrost) network") {
        val customJson =
            previewBootstrapJson.mapObject(_.add("cardanoNetwork", Json.fromString("custom")))
        assert(customJson.as[Bootstrap.BootstrapConfig].isLeft)
    }

    test("L2Output round-trips through its CIP-0116 JSON") {
        val address =
            Address.fromBech32("addr_test1vrhh0xnmqlh5jpys4cqrj3vteje70r0swakm7q2w8nmcp3sh5wdk4")
        val output = Bootstrap.L2Output(address, Value.ada(5L))
        val decoded =
            output.asJson.as[Bootstrap.L2Output].fold(e => fail(s"decode failed: $e"), identity)
        assert(decoded.value == output.value)
    }

    /** The demo defaults [[InitBootstrapFiles]] writes: preview head parameters, head peer 0
      * funding all equity, no pinned block-zero timing.
      */
    private def mkPreviewDefaults(coilQuorum: Int): Bootstrap.BootstrapDefaults = {
        val network = CardanoNetwork.Preview
        val headParams = Bootstrap.BootstrapHeadParams(
          txTiming = TxTiming.demo(network.slotConfig),
          fallbackContingency = network.mkFallbackContingencyWithDefaults(Coin.ada(3), Coin.ada(3)),
          disputeResolutionConfig = DisputeResolutionConfig.default(network.slotConfig),
          settlementConfig = SettlementConfig(PositiveInt.unsafeApply(100)),
          coilQuorum = coilQuorum
        )
        Bootstrap.BootstrapDefaults(
          cardanoNetwork = network,
          headParams = headParams,
          initialEquityContributions =
              Map(HeadPeerNumber(0) -> Coin.ada(100), HeadPeerNumber(1) -> Coin.zero),
          blockZeroStartTime = None,
          blockZeroEndTime = None
        )
    }

    /** A `bootstrap.json` shaped exactly as [[BuildBootstrapConfig]] assembles it: the defaults'
      * `cardanoNetwork` / `headParams` / `initialEquityContributions` merged with the roster's peer
      * topology, the deployed script references, and the opening L2 state.
      */
    private def previewBootstrapJson: Json = {
        val network = CardanoNetwork.Preview
        given CardanoNetwork.Section = network
        val d = mkPreviewDefaults(coilQuorum = 2).asJson.asObject
            .getOrElse(fail("defaults did not encode to a JSON object"))
        Json.obj(
          "cardanoNetwork" -> d("cardanoNetwork").get,
          "headParams" -> d("headParams").get,
          "headPeers" -> Json.arr(
            Json.obj(
              "verificationKey" -> Json.fromString(
                "85f2378d9af901be0b6d096f8e4b5573274f9bb1d5bfadb86ecac6e51504bbad"
              ),
              "webSocketAddress" -> Json.fromString("ws://head-0:4001")
            )
          ),
          "coilPeers" -> Json.arr(),
          "scriptReferenceUtxos" -> Json.obj(
            "rulebasedTreasuryScriptInput" -> Json.fromString(
              "d17362c69150ccf4cf0974ee4223f5f8c84b9171d74cd210aa042860a1a32ecd#0"
            ),
            "disputeResolutionScriptInput" -> Json.fromString(
              "83ec7059e234a2d21b059ae81da478a2be994916675c7cede4617a8267a8e1ff#0"
            )
          ),
          "initialL2State" -> Json.arr(),
          "initialEquityContributions" -> d("initialEquityContributions").get
        )
    }
}
