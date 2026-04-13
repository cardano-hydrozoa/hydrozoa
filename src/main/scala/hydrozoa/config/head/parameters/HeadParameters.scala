package hydrozoa.config.head.parameters

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.settlement.SettlementConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.Durations.{DepositAbsorptionDuration, DepositMaturityDuration, DepositSubmissionDuration, InactivityMarginDuration, MinSettlementDuration, SilenceDuration}
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import hydrozoa.lib.number.PositiveInt
import scalus.cardano.ledger.Hash32

/** The parameters that peers agree upon to run the protocol. These parameters get hashed into the
  * treasury datum.
  */
final case class HeadParameters(
    override val txTiming: TxTiming,
    override val fallbackContingency: FallbackContingency,
    override val disputeResolutionConfig: DisputeResolutionConfig,
    override val settlementConfig: SettlementConfig,
    override val l2ParamsHash: Hash32
) extends HeadParameters.Section {
    override transparent inline def headParams: HeadParameters = this

    // TODO: We need this hash to put into the initialization tx's metadata,
    //  so that the head parameters are pinned by something signed by all peers.
    override lazy val headParamsHash: Hash32 = {
        ???
    }
}

object HeadParameters {
    trait Section
        extends TxTiming.Section,
          FallbackContingency.Section,
          DisputeResolutionConfig.Section,
          SettlementConfig.Section {

        /** A black-box, L2-specific blake2b-256 hash of the L2 parameters that the peers agree upon
          * during the negotiation phase.
          */
        def l2ParamsHash: Hash32

        def headParams: HeadParameters

        def headParamsHash: Hash32

        override transparent inline def maxDepositsAbsorbedPerBlock: PositiveInt =
            settlementConfig.maxDepositsAbsorbedPerBlock

        override transparent inline def minSettlementDuration: MinSettlementDuration =
            txTiming.minSettlementDuration

        override transparent inline def inactivityMarginDuration: InactivityMarginDuration =
            txTiming.inactivityMarginDuration

        override transparent inline def silenceDuration: SilenceDuration =
            txTiming.silenceDuration

        override transparent inline def depositSubmissionDuration: DepositSubmissionDuration =
            txTiming.depositSubmissionDuration

        override transparent inline def depositMaturityDuration: DepositMaturityDuration =
            txTiming.depositMaturityDuration

        override transparent inline def depositAbsorptionDuration: DepositAbsorptionDuration =
            txTiming.depositAbsorptionDuration

        override transparent inline def collectiveContingency: FallbackContingency.Collective =
            fallbackContingency.collectiveContingency

        override transparent inline def individualContingency: FallbackContingency.Individual =
            fallbackContingency.individualContingency

        override transparent inline def votingDuration: QuantizedFiniteDuration =
            disputeResolutionConfig.votingDuration
    }
}
