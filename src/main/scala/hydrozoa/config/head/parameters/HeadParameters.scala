package hydrozoa.config.head.parameters

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import scalus.cardano.ledger.Hash32

final case class HeadParameters(
    override val txTiming: TxTiming,
    override val fallbackContingency: FallbackContingency,
    override val disputeResolutionConfig: DisputeResolutionConfig,
) extends HeadParameters.Section {
    override transparent inline def headParameters: HeadParameters = this

    override transparent inline def minSettlementDuration: QuantizedFiniteDuration =
        txTiming.minSettlementDuration
    override transparent inline def inactivityMarginDuration: QuantizedFiniteDuration =
        txTiming.inactivityMarginDuration
    override transparent inline def silenceDuration: QuantizedFiniteDuration =
        txTiming.silenceDuration
    override transparent inline def depositMaturityDuration: QuantizedFiniteDuration =
        txTiming.depositMaturityDuration
    override transparent inline def depositAbsorptionDuration: QuantizedFiniteDuration =
        txTiming.depositAbsorptionDuration

    override transparent inline def collectiveContingency: FallbackContingency.Collective =
        fallbackContingency.collectiveContingency
    override transparent inline def individualContingency: FallbackContingency.Individual =
        fallbackContingency.individualContingency

    override transparent inline def votingDuration: QuantizedFiniteDuration =
        disputeResolutionConfig.votingDuration

    // TODO: We need this hash to put into the initialization tx's metadata,
    //  so that the head parameters are pinned by something signed by all peers.
    override lazy val headParametersHash: Hash32 = {
        val cbor = ???
        ???
    }
}

object HeadParameters {
    trait Section
        extends TxTiming.Section,
          FallbackContingency.Section,
          DisputeResolutionConfig.Section {
        def headParameters: HeadParameters

        def headParametersHash: Hash32
    }
}
