package hydrozoa.config.head.coil

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import test.{SeedPhrase, TestPeers}

/** Tests for the explicit-number coil-peer config ([[CoilPeers]] / [[CoilPeerData]]): the JSON is
  * keyed by the explicit [[CoilPeerNumber]] (not a vkey-derived order), the numbers must be
  * contiguous from 0, and the derivations read straight off the keyed map.
  */
class CoilPeersTest extends AnyFunSuite {

    private val peers = TestPeers.apply(SeedPhrase.Yaci, CardanoNetwork.Preprod, 3)
    private val vk0 = peers.walletFor(HeadPeerNumber(0)).exportVerificationKey
    private val vk1 = peers.walletFor(HeadPeerNumber(1)).exportVerificationKey

    test("non-empty CoilPeers round-trips through JSON keyed by number") {
        val cps = CoilPeers.indexed(
          List(
            CoilPeerData(vk0, HeadPeerNumber(1)),
            CoilPeerData(vk1, HeadPeerNumber(0))
          )
        )
        val json = cps.asJson
        assert(
          json.asObject.exists(o => o.contains("0") && o.contains("1")),
          s"keyed by number: $json"
        )
        assert(decode[CoilPeers](json.noSpaces) == Right(cps))
    }

    test("empty CoilPeers round-trips") {
        assert(decode[CoilPeers](CoilPeers.empty.asJson.noSpaces) == Right(CoilPeers.empty))
    }

    test("decode rejects non-contiguous coil peer numbers") {
        // A single coil keyed at 1 (gap at 0) violates the contiguity invariant.
        val json = Json.obj("1" -> CoilPeerData(vk0, HeadPeerNumber(0)).asJson)
        assert(decode[CoilPeers](json.noSpaces).isLeft)
    }

    test("indexed assigns numbers 0..n-1 by position and derivations read them") {
        val cps = CoilPeers.indexed(
          List(
            CoilPeerData(vk0, HeadPeerNumber(0)),
            CoilPeerData(vk1, HeadPeerNumber(1))
          )
        )
        assert(cps.verificationKeys == List(vk0, vk1))
        assert(cps.verificationKey(CoilPeerNumber(0)).contains(vk0))
        assert(cps.hubHeadPeerNumber(CoilPeerNumber(1)).contains(HeadPeerNumber(1)))
        assert(cps.hubbedBy(HeadPeerNumber(0)) == List(CoilPeerNumber(0)))
        assert(cps.hubHeadPeerNumbers == List(HeadPeerNumber(0), HeadPeerNumber(1)))
    }
}
