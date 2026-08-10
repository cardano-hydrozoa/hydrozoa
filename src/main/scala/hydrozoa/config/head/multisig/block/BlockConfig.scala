package hydrozoa.config.head.multisig.block

import hydrozoa.lib.number.PositiveInt
import io.circe.*
import io.circe.generic.semiauto.*

/** The per-block production limits the peers agree upon. Head-agreed (hashed into the treasury
  * datum), so every peer packs and reproduces blocks under the same ceiling.
  */
final case class BlockConfig(
    override val maxRequestsPerBlock: PositiveInt
) extends BlockConfig.Section {
    override transparent inline def blockConfig: BlockConfig = this
}

object BlockConfig {
    trait Section {
        def blockConfig: BlockConfig

        /** The maximum number of user requests a single block may hold. The leader packs at most
          * this many per block; the overflow rolls into the next block. Also anchors the mesh pull
          * ceiling and the request-sequencer backpressure (see docs/spec/fast-consensus.md).
          */
        def maxRequestsPerBlock: PositiveInt = blockConfig.maxRequestsPerBlock
    }

    given blockConfigEncoder: Encoder[BlockConfig] = deriveEncoder[BlockConfig]

    given blockConfigDecoder: Decoder[BlockConfig] = deriveDecoder[BlockConfig]
}
