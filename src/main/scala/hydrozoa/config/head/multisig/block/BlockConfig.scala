package hydrozoa.config.head.multisig.block

import hydrozoa.lib.number.PositiveInt
import io.circe.*
import io.circe.generic.semiauto.*

/** The per-block production limits the peers agree upon. Head-agreed (hashed into the treasury
  * datum), so every peer packs and reproduces blocks under the same ceiling.
  */
final case class BlockConfig(
    override val maxRequestsPerBlock: PositiveInt,
    override val backpressureCoefficient: PositiveInt = PositiveInt.unsafeApply(3)
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

        /** Multiplier on [[maxRequestsPerBlock]] for the **request-sequencer backpressure window**:
          * a peer admits at most `backpressureCoefficient * maxRequestsPerBlock` of its own
          * requests beyond its confirmed high-water before rejecting new ones. A larger coefficient
          * buffers more client requests through a transient block-production stall before shedding
          * load, at the cost of a deeper local mempool. It is purely a *local* admission depth —
          * the mesh pull ceiling and block reproduction still key off `maxRequestsPerBlock` alone
          * (depth-1 pipelining means a follower never needs more than one cap ahead to reproduce a
          * block), so raising it cannot break the cross-peer mempool bound. Default 3.
          */
        def backpressureCoefficient: PositiveInt = blockConfig.backpressureCoefficient
    }

    given blockConfigEncoder: Encoder[BlockConfig] = deriveEncoder[BlockConfig]

    /** Hand-written so a config produced before `backpressureCoefficient` existed still decodes
      * (defaulting to 3) rather than failing on the missing field.
      */
    given blockConfigDecoder: Decoder[BlockConfig] = Decoder.instance { c =>
        for {
            maxRequests <- c.get[PositiveInt]("maxRequestsPerBlock")
            coefficient <- c.getOrElse[PositiveInt]("backpressureCoefficient")(
              PositiveInt.unsafeApply(3)
            )
        } yield BlockConfig(maxRequests, coefficient)
    }
}
