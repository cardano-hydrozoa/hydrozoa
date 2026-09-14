package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.number.PositiveInt
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite

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
}
