package hydrozoa.config.head.network

import hydrozoa.config.head.rulebased.scripts.RuleBasedScriptAddresses
import hydrozoa.lib.number.PositiveInt
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.{CardanoInfo, Coin, EvaluatorMode, PlutusScriptEvaluator, ProtocolParams, ProtocolVersion, SlotConfig, TransactionOutput}
import scalus.cardano.txbuilder.TransactionBuilder

export CardanoNetwork.ensureMinAda

enum CardanoNetwork(_cardanoInfo: CardanoInfo) extends CardanoNetwork.Section {
    case Mainnet extends CardanoNetwork(CardanoInfo.mainnet)
    case Preprod extends CardanoNetwork(CardanoInfo.preprod)
    case Preview extends CardanoNetwork(CardanoInfo.preview)
    case Custom(override val cardanoInfo: CardanoInfo) extends CardanoNetwork(cardanoInfo)

    override transparent inline def cardanoNetwork: CardanoNetwork = this

    override def cardanoInfo: CardanoInfo = _cardanoInfo
    override def network: Network = _cardanoInfo.network
    override def slotConfig: SlotConfig = _cardanoInfo.slotConfig
    override def cardanoProtocolParams: ProtocolParams = _cardanoInfo.protocolParams

    lazy val ruleBasedScriptAddresses: RuleBasedScriptAddresses = RuleBasedScriptAddresses(this)

    override transparent inline def ruleBasedTreasuryAddress: ShelleyAddress =
        ruleBasedScriptAddresses.ruleBasedTreasuryAddress
    override transparent inline def ruleBasedDisputeResolutionAddress: ShelleyAddress =
        ruleBasedScriptAddresses.ruleBasedDisputeResolutionAddress
}

object CardanoNetwork {
    trait Section extends RuleBasedScriptAddresses.Section {
        def cardanoNetwork: CardanoNetwork

        def cardanoInfo: CardanoInfo
        def network: Network
        def slotConfig: SlotConfig
        def cardanoProtocolParams: ProtocolParams

        final def babbageUtxoMinLovelace(serializedSize: PositiveInt): Coin = Coin(
          (160 + serializedSize.convert) * cardanoProtocolParams.utxoCostPerByte
        )

        final def maxNonPlutusTxFee: Coin = Coin(
          cardanoProtocolParams.txFeeFixed + cardanoProtocolParams.maxTxSize * cardanoProtocolParams.txFeePerByte
        )

        final def plutusScriptEvaluatorForTxBuild: PlutusScriptEvaluator =
            PlutusScriptEvaluator(cardanoInfo, EvaluatorMode.EvaluateAndComputeCost)

        final def cardanoProtocolVersion: ProtocolVersion = cardanoProtocolParams.protocolVersion
    }

    extension [T <: TransactionOutput](self: T)
        def ensureMinAda(config: CardanoNetwork.Section): T =
            TransactionBuilder.ensureMinAda(self, config.cardanoProtocolParams).asInstanceOf[T]
}
