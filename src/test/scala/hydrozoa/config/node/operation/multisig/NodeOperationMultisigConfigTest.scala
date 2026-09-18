package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.number.PositiveInt
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** Decoding [[NodeOperationMultisigConfig]]. Every field is required: a node whose private config
  * fails to decode does not start at all, and refusing a config that omits `peerLiaisonOutboxDepth`
  * is preferable to starting on a default the operator never chose.
  */
class NodeOperationMultisigConfigTest extends AnyFunSuite {

    private val withoutOutboxDepth =
        """{
          |  "cardanoLiaisonPollingPeriod": 20000,
          |  "peerLiaisonMaxRequestsPerBatch": 500,
          |  "peerLiaisonResendInterval": 5000,
          |  "coilCatchUpStacks": 16,
          |  "rateLimits": { "softBlockMinPeriod": 100, "hardStackMinPeriod": 30000 }
          |}""".stripMargin

    private def withOutboxDepth(depth: Int): String =
        withoutOutboxDepth.replace(
          """"peerLiaisonMaxRequestsPerBatch": 500,""",
          s""""peerLiaisonMaxRequestsPerBatch": 500, "peerLiaisonOutboxDepth": $depth,"""
        )

    test("a config omitting peerLiaisonOutboxDepth is refused") {
        assert(decode[NodeOperationMultisigConfig](withoutOutboxDepth).isLeft)
    }

    test("peerLiaisonOutboxDepth decodes") {
        val decoded = decode[NodeOperationMultisigConfig](withOutboxDepth(4))
        assert(decoded.map(_.peerLiaisonOutboxDepth) == Right(PositiveInt.unsafeApply(4)))
    }

    test("a non-positive peerLiaisonOutboxDepth is refused") {
        assert(decode[NodeOperationMultisigConfig](withOutboxDepth(0)).isLeft)
    }

    test("the encoder round-trips through the decoder") {
        val config = NodeOperationMultisigConfig.default
        assert(decode[NodeOperationMultisigConfig](config.asJson.noSpaces) == Right(config))
    }

    test("a config omitting coilCatchUpStacks is refused") {
        val json = withOutboxDepth(2).replace("""  "coilCatchUpStacks": 16,""", "")
        assert(decode[NodeOperationMultisigConfig](json).isLeft)
    }

    /** The other direction of the same operational concern: a deployed config may still carry
      * `transplantStackNumber`, a field this config no longer has. The decoder reads named fields
      * and never asserts the object's shape, so an unknown key is ignored — but "ignored" is the
      * difference between a node that starts and one that does not, so it is pinned rather than
      * assumed.
      */
    test("a config still carrying transplantStackNumber decodes, with the field ignored") {
        val json = withOutboxDepth(2).replace(
          """"peerLiaisonResendInterval": 5000,""",
          """"peerLiaisonResendInterval": 5000, "transplantStackNumber": 42,"""
        )
        assert(
          decode[NodeOperationMultisigConfig](json) ==
              Right(
                NodeOperationMultisigConfig.default
                    .copy(cardanoLiaisonPollingPeriod = 20.seconds)
              )
        )
    }
}
