package hydrozoa.config.head.rulebased.scripts

import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.network.CardanoNetwork
import scalus.cardano.address.{Network, ShelleyAddress}

final case class RuleBasedScriptAddresses private (
    override val ruleBasedTreasuryAddress: ShelleyAddress,
    override val ruleBasedDisputeResolutionAddress: ShelleyAddress,
    override val ruleBasedRegimeAddress: ShelleyAddress,
) extends RuleBasedScriptAddresses.Section {
    override transparent inline def ruleBasedScriptAddresses: RuleBasedScriptAddresses = this
}

object RuleBasedScriptAddresses {
    def apply(cardanoNetwork: CardanoNetwork.Section): RuleBasedScriptAddresses =
        RuleBasedScriptAddresses(cardanoNetwork.network)

    def apply(network: Network): RuleBasedScriptAddresses =
        new RuleBasedScriptAddresses(
          ruleBasedTreasuryAddress = HydrozoaBlueprint.mkTreasuryAddress(network),
          ruleBasedDisputeResolutionAddress = HydrozoaBlueprint.mkDisputeAddress(network),
          ruleBasedRegimeAddress = HydrozoaBlueprint.mkRegimeAddress(network)
        )

    trait Section {
        def ruleBasedScriptAddresses: RuleBasedScriptAddresses

        def ruleBasedTreasuryAddress: ShelleyAddress =
            ruleBasedScriptAddresses.ruleBasedTreasuryAddress
        def ruleBasedDisputeResolutionAddress: ShelleyAddress =
            ruleBasedScriptAddresses.ruleBasedDisputeResolutionAddress
        def ruleBasedRegimeAddress: ShelleyAddress =
            ruleBasedScriptAddresses.ruleBasedRegimeAddress
    }
}
