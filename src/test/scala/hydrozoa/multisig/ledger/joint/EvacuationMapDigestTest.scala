package hydrozoa.multisig.ledger.joint

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import io.circe.parser.decode
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite

/** The cross-repo contract for [[EvacuationMap.digest]].
  *
  * Sugar Rush computes the same digest in `types/src/types/evacuation_map.rs`, and the head refuses
  * to boot when its configured `initialEvacuationMap` disagrees with the ledger's — so these
  * vectors are a wire pin, not a regression guard. A change here must land on both sides together.
  */
class EvacuationMapDigestTest extends AnyFunSuite:

    private given CardanoNetwork.Section =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))

    /** Exactly the JSON `sugar-rush-ledger-server print-initial-evacuation-map` emits — the head
      * config's `initialEvacuationMap` encoding, keyed by the evacuation key's hex, valued by the
      * output's raw CBOR hex.
      */
    private val goldenJson: String =
        """{
          |  "ab00000000000000000000000000000000000000000000000000000000000000":
          |    "a2005839000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011a00989680"
          |}""".stripMargin

    /** The value `EvacuationMap::hash` produces for the same map in
      * `types/src/types/evacuation_map.rs`'s `digest_golden`.
      */
    private val goldenDigest = "6a0bf68bee4d771bd969fce92285d7e3da0683d01e4c2c44310e5a162ba84995"

    private def goldenMap: EvacuationMap =
        decode[EvacuationMap](goldenJson).fold(e => fail(s"golden did not decode: $e"), identity)

    test("a map printed by the remote ledger decodes into the head config's evacuation map") {
        assert(goldenMap.size == 1)
    }

    test("digest matches the value Sugar Rush computes for the same map") {
        assert(goldenMap.digest.toHex == goldenDigest)
    }

    test("the empty map has a defined digest, distinct from any populated one") {
        assert(EvacuationMap.empty.digest != goldenMap.digest)
    }

    test("digest round-trips through its JSON codec") {
        import io.circe.syntax.*
        val digest = goldenMap.digest
        assert(decode[EvacuationMapHash](digest.asJson.noSpaces) == Right(digest))
    }

    test("a digest of the wrong length is rejected rather than silently truncated") {
        assert(decode[EvacuationMapHash](""""abcd"""").isLeft)
    }
