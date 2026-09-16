package hydrozoa.config.node.operation.multisig

import hydrozoa.lib.number.PositiveInt
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** Decoding [[NodeOperationMultisigConfig]], with the compatibility case that matters
  * operationally: `peerLiaisonOutboxCap` was added after nodes were already deployed, so a config
  * file written without it must still decode. A node whose private config fails to decode does not
  * start at all.
  */
class NodeOperationMultisigConfigTest extends AnyFunSuite {

    private val withoutOutboxCap =
        """{
          |  "cardanoLiaisonPollingPeriod": 20000,
          |  "peerLiaisonMaxRequestsPerBatch": 500,
          |  "peerLiaisonResendInterval": 5000,
          |  "rateLimits": { "softBlockMinPeriod": 100, "hardStackMinPeriod": 30000 }
          |}""".stripMargin

    test("a config predating peerLiaisonOutboxCap decodes, taking the default") {
        val decoded = decode[NodeOperationMultisigConfig](withoutOutboxCap)
        assert(
          decoded.map(_.peerLiaisonOutboxCap) ==
              Right(NodeOperationMultisigConfig.defaultPeerLiaisonOutboxCap)
        )
    }

    test("an explicit peerLiaisonOutboxCap is taken over the default") {
        val json = withoutOutboxCap.replace(
          """"peerLiaisonMaxRequestsPerBatch": 500,""",
          """"peerLiaisonMaxRequestsPerBatch": 500, "peerLiaisonOutboxCap": 64,"""
        )
        val decoded = decode[NodeOperationMultisigConfig](json)
        assert(decoded.map(_.peerLiaisonOutboxCap) == Right(PositiveInt.unsafeApply(64)))
    }

    test("the encoder round-trips through the decoder") {
        val config = NodeOperationMultisigConfig.default
        assert(decode[NodeOperationMultisigConfig](config.asJson.noSpaces) == Right(config))
    }

    test("a config predating coilCatchUpStacks decodes, taking the default") {
        assert(
          decode[NodeOperationMultisigConfig](withoutOutboxCap).map(_.coilCatchUpStacks) ==
              Right(NodeOperationMultisigConfig.defaultCoilCatchUpStacks)
        )
    }

    /** The other direction of the same operational concern: a deployed config may still carry
      * `transplantStackNumber`, a field this config no longer has. The decoder reads named fields
      * and never asserts the object's shape, so an unknown key is ignored — but "ignored" is the
      * difference between a node that starts and one that does not, so it is pinned rather than
      * assumed.
      */
    test("a config still carrying transplantStackNumber decodes, with the field ignored") {
        val json = withoutOutboxCap.replace(
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
