package hydrozoa.config.head.parameters

import hydrozoa.config.head.multisig.block.BlockConfig
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import scalus.cardano.ledger.{Hash32, ProtocolParams}

/** The parameters that peers agree upon to run the protocol. They feed `headParamsHash`, which the
  * multisig regime datum carries.
  */
final case class HeadParameters(
    override val txTiming: TxTiming,
    override val fallbackContingency: FallbackContingency,
    override val disputeResolutionConfig: DisputeResolutionConfig,
    override val settlementConfig: SettlementConfig,
    override val blockConfig: BlockConfig,
    // QUESTION: (from Peter to Ilia): I don't think we need to pin the coil quorum here, do we?
    //   It will be in the multisig native script; the hash will change if the peers don't agree.
    override val coilQuorum: Int,
    override val l2ParamsHash: Hash32,
    override val l2Ledger: L2LedgerConfig,
    override val identityIsomorphism: Boolean
) extends HeadParameters.Section {
    override transparent inline def headParameters: HeadParameters = this
}

object HeadParameters {

    given headParametersEncoder: Encoder[HeadParameters] = deriveEncoder[HeadParameters]

    given headParametersDecoder(using CardanoNetwork.Section): Decoder[HeadParameters] =
        deriveDecoder[HeadParameters]

    trait Section
        extends TxTiming.Section,
          FallbackContingency.Section,
          DisputeResolutionConfig.Section,
          SettlementConfig.Section,
          BlockConfig.Section {
        def headParameters: HeadParameters

        /** A black-box, L2-specific blake2b-256 hash of the L2 parameters that the peers agree upon
          * during the negotiation phase.
          */
        def l2ParamsHash: Hash32 = headParameters.l2ParamsHash

        /** Which L2 ledger this head runs, and what its peers agreed about it. Agnostic by
          * construction: the head holds this and does not look inside — see [[L2LedgerConfig]].
          */
        def l2Ledger: L2LedgerConfig = headParameters.l2Ledger

        /** The `cardano-eutxo` ledger's agreed protocol parameters, when that is the backend this
          * head runs. `None` on any other, whose parameters are its own and never reach the head.
          */
        def cardanoEutxoProtocolParams: Option[ProtocolParams] = l2Ledger match {
            case L2LedgerConfig.CardanoEutxo(protocolParams) => Some(protocolParams)
            case L2LedgerConfig.AnyRemote                    => None
        }

        /** Identity isomorphism: when `true`, the exact L1 tx runs on L2 unchanged — the ledger
          * does NOT enforce the `headId` pin, which reopens cross-head replay
          * (docs/spec/l2-isomorphism.md). Default `false` (format isomorphism only; pin enforced).
          * Agreed by all peers — it changes the trust model.
          */
        def identityIsomorphism: Boolean = headParameters.identityIsomorphism

        def coilQuorum: Int = headParameters.coilQuorum

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
