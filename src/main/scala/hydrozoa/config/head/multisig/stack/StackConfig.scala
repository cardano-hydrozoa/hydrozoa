package hydrozoa.config.head.multisig.stack

import hydrozoa.lib.number.PositiveInt
import io.circe.*
import io.circe.generic.semiauto.*

/** The per-stack composition limits the peers agree upon. Head-agreed (hashed into the treasury
  * datum), so every peer closes and validates stacks under the same ceiling.
  */
final case class StackConfig(
    override val maxMajorBlocksPerStack: PositiveInt
) extends StackConfig.Section {
    override transparent inline def stackConfig: StackConfig = this
}

object StackConfig {

    /** Applied when a head config produced before this section existed is decoded. Large on
      * purpose: a stack's hard-confirmation carries a fixed cost independent of its size, so a
      * small cap closes many tiny stacks and *lowers* throughput (see docs/spec/slow-consensus.md).
      */
    val default: StackConfig = StackConfig(PositiveInt.unsafeApply(1000))

    trait Section {
        def stackConfig: StackConfig

        /** The maximum number of Major blocks a single stack may cover. The slow leader closes on
          * at most this many, holding the rest for the next stack; a follower rejects a brief
          * spanning more.
          *
          * Majors are what a stack costs: each one derives a settlement, a fallback, rollouts and
          * refunds, and every peer signs and verifies all of them. A run of Minor blocks collapses
          * into a single partition, so minors are close to free and are deliberately not counted.
          */
        def maxMajorBlocksPerStack: PositiveInt = stackConfig.maxMajorBlocksPerStack
    }

    given stackConfigEncoder: Encoder[StackConfig] = deriveEncoder[StackConfig]

    given stackConfigDecoder: Decoder[StackConfig] = deriveDecoder[StackConfig]
}
