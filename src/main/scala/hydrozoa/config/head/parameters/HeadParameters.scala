package hydrozoa.config.head.parameters

import hydrozoa.config.head.multisig.block.BlockConfig
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder}
import scalus.cardano.ledger.Hash32

/** The parameters that peers agree upon to run the protocol. These parameters get hashed into the
  * treasury datum.
  */
final case class HeadParameters(
    override val txTiming: TxTiming,
    override val fallbackContingency: FallbackContingency,
    override val disputeResolutionConfig: DisputeResolutionConfig,
    override val settlementConfig: SettlementConfig,
    override val blockConfig: BlockConfig,
    override val rateLimits: RateLimits,
    // QUESTION: (from Peter to Ilia): I don't think we need to pin the coil quorum here, do we?
    //   It will be in the multisig native script; the hash will change if the peers don't agree.
    override val coilQuorum: Int,
    override val l2ParamsHash: Hash32,
    override val l2Ledger: L2LedgerKind,
    override val identityIsomorphism: Boolean
) extends HeadParameters.Section {
    override transparent inline def headParameters: HeadParameters = this
}

object HeadParameters {

    given headParametersEncoder: Encoder[HeadParameters] = deriveEncoder[HeadParameters]

    /** Hand-written so a config produced before `rateLimits` moved here from the node's private
      * config still decodes, defaulting to [[RateLimits.default]] rather than failing on the
      * missing field. Every other field stays mandatory.
      */
    given headParametersDecoder(using CardanoNetwork.Section): Decoder[HeadParameters] =
        Decoder.instance { c =>
            for {
                txTiming <- c.get[TxTiming]("txTiming")
                fallbackContingency <- c.get[FallbackContingency]("fallbackContingency")
                disputeResolutionConfig <- c.get[DisputeResolutionConfig](
                  "disputeResolutionConfig"
                )
                settlementConfig <- c.get[SettlementConfig]("settlementConfig")
                blockConfig <- c.get[BlockConfig]("blockConfig")
                rateLimits <- c.getOrElse[RateLimits]("rateLimits")(RateLimits.default)
                coilQuorum <- c.get[Int]("coilQuorum")
                l2ParamsHash <- c.get[Hash32]("l2ParamsHash")
                l2Ledger <- c.get[L2LedgerKind]("l2Ledger")
                identityIsomorphism <- c.get[Boolean]("identityIsomorphism")
            } yield HeadParameters(
              txTiming = txTiming,
              fallbackContingency = fallbackContingency,
              disputeResolutionConfig = disputeResolutionConfig,
              settlementConfig = settlementConfig,
              blockConfig = blockConfig,
              rateLimits = rateLimits,
              coilQuorum = coilQuorum,
              l2ParamsHash = l2ParamsHash,
              l2Ledger = l2Ledger,
              identityIsomorphism = identityIsomorphism
            )
        }

    trait Section
        extends TxTiming.Section,
          FallbackContingency.Section,
          DisputeResolutionConfig.Section,
          SettlementConfig.Section,
          BlockConfig.Section,
          RateLimits.Section {
        def headParameters: HeadParameters

        override def rateLimits: RateLimits = headParameters.rateLimits

        /** A black-box, L2-specific blake2b-256 hash of the L2 parameters that the peers agree upon
          * during the negotiation phase.
          */
        def l2ParamsHash: Hash32 = headParameters.l2ParamsHash

        /** Which L2 ledger this head runs — `cardano-eutxo` or `any-remote` (agreed by all peers).
          */
        def l2Ledger: L2LedgerKind = headParameters.l2Ledger

        /** Identity isomorphism: when `true`, the exact L1 tx runs on L2 unchanged — the ledger
          * does NOT enforce the `headId` pin, which reopens cross-head replay
          * (docs/spec/l2-isomorphism.md). Default `false` (format isomorphism only; pin enforced).
          * Agreed by all peers — it changes the trust model.
          */
        def identityIsomorphism: Boolean = headParameters.identityIsomorphism

        def coilQuorum: Int = headParameters.coilQuorum

        // TODO: unimplemented, and on the wrong Section — `headParamsHash` digests the whole head
        //  config (this section plus the equity split, block-zero timing, coil hub topology and
        //  script references), so it belongs on `HeadConfig.Section`. The multisig treasury datum
        //  carries it, so a peer whose config differs cannot parse the initialization tx. Byte
        //  layout, the `l2ParamsHash` contract, and the five checks are in
        //  design/head-params-hash.md.
        final def headParamsHash: Hash32 = ???

        def txTiming: TxTiming = headParameters.txTiming

        def fallbackContingency: FallbackContingency =
            headParameters.fallbackContingency

        def disputeResolutionConfig: DisputeResolutionConfig =
            headParameters.disputeResolutionConfig

        def settlementConfig: SettlementConfig =
            headParameters.settlementConfig

        def blockConfig: BlockConfig =
            headParameters.blockConfig
    }
}
