package hydrozoa.multisig.consensus.transport

import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{AssetName, Hash32}
import scalus.uplc.builtin.ByteString

/** The counterpart half of boot-time validation: does the peer on the other end belong to this head
  * at all?
  *
  * ⚠️ **Both fields are load-bearing and neither implies the other.** A peer with the right
  * `headId` and different parameters derives different effects from the same blocks; a peer with
  * matching parameters and a different `headId` is simply another head. A check that looked at one
  * would let the other through, and the failure in both cases is divergence on content that is
  * structurally valid — the kind that surfaces late and reads as a consensus bug.
  */
class HeadIdentityTest extends AnyFunSuite {

    private def headId(s: String): HeadId = HeadId(AssetName(ByteString.fromString(s)))

    private def hash(b: Byte): Hash32 =
        Hash32.fromByteString(ByteString.fromArray(Array.fill[Byte](32)(b)))

    private val own = HeadIdentity(headId("ours"), hash(0x11))

    test("a counterpart in the same head is accepted") {
        assert(
          HeadIdentity.check(Some(HeadIdentity(headId("ours"), hash(0x11))), own) ==
              HeadIdentity.Check.Compatible
        )
    }

    test("a counterpart in another head is refused") {
        val result = HeadIdentity.check(Some(HeadIdentity(headId("theirs"), hash(0x11))), own)
        assert(result.isInstanceOf[HeadIdentity.Check.Mismatch])
        assert(
          result.asInstanceOf[HeadIdentity.Check.Mismatch].field == "headId",
          "the refusal must name which half disagreed"
        )
    }

    test("a counterpart with different head parameters is refused") {
        // The case the headId check alone cannot see: same head instance, different configuration,
        // so the two peers derive different effects from the same blocks.
        val result = HeadIdentity.check(Some(HeadIdentity(headId("ours"), hash(0x22))), own)
        assert(
          result == HeadIdentity.Check
              .Mismatch("headParamsHash", hash(0x11).toHex, hash(0x22).toHex)
        )
    }

    test("a counterpart that announces no head identity is refused, not waved through") {
        assert(HeadIdentity.check(None, own) == HeadIdentity.Check.Absent)
    }

    test("the refusal says what to fix") {
        val detail =
            HeadIdentity.describe(
              HeadIdentity.check(Some(HeadIdentity(headId("theirs"), hash(0x11))), own)
            )
        assert(detail.contains("headId"), s"unhelpful refusal: $detail")
        assert(detail.contains(headId("ours").toHex) && detail.contains(headId("theirs").toHex))
    }

    test("an identity round-trips through the wire codec") {
        import io.circe.syntax.*
        val decoded = io.circe.parser.decode[HeadIdentity](own.asJson.noSpaces)
        assert(decoded == Right(own), s"round-trip changed the identity: $decoded")
    }
}
