package hydrozoa.config.head.rulebased.scripts

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.rulebased.ledger.dapp.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import scalus.cardano.address.ShelleyAddress

final case class RuleBasedScriptAddresses private (
    override val ruleBasedTreasuryAddress: ShelleyAddress,
    override val ruleBasedDisputeResolutionAddress: ShelleyAddress,
) extends RuleBasedScriptAddresses.Section {
    override transparent inline def ruleBasedScriptAddresses: RuleBasedScriptAddresses = this
}

object RuleBasedScriptAddresses {
    def apply(cardanoNetwork: CardanoNetwork): RuleBasedScriptAddresses =
        new RuleBasedScriptAddresses(
          ruleBasedTreasuryAddress = RuleBasedTreasuryScript.address(cardanoNetwork.network),
          ruleBasedDisputeResolutionAddress =
              DisputeResolutionScript.address(cardanoNetwork.network)
        )

    trait Section {
        def ruleBasedScriptAddresses: RuleBasedScriptAddresses

        def ruleBasedTreasuryAddress: ShelleyAddress
        def ruleBasedDisputeResolutionAddress: ShelleyAddress
    }
}
