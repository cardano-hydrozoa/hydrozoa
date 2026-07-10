package hydrozoa.app

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
}
