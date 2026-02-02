package hydrozoa.config.head.network

import hydrozoa.lib.number.PositiveInt
import scalus.cardano.address.Network
import scalus.cardano.ledger.{CardanoInfo, Coin, ProtocolParams, SlotConfig}

export hydrozoa.config.head.multisig.fallback.FallbackContingency.mkFallbackContingencyWithDefaults

enum CardanoNetwork(_cardanoInfo: CardanoInfo) extends CardanoNetwork.Section {
    case Mainnet extends CardanoNetwork(CardanoInfo.mainnet)
    case Preprod extends CardanoNetwork(CardanoInfo.preprod)
    case Preview extends CardanoNetwork(CardanoInfo.preview)
    case Custom(override val cardanoInfo: CardanoInfo) extends CardanoNetwork(cardanoInfo)

    override transparent inline def cardanoNetwork: CardanoNetwork = this

    override def cardanoInfo: CardanoInfo = _cardanoInfo
    override def network: Network = _cardanoInfo.network
    override def slotConfig: SlotConfig = _cardanoInfo.slotConfig
    override def params: ProtocolParams = _cardanoInfo.protocolParams
}

object CardanoNetwork {
    trait Section {
        def cardanoNetwork: CardanoNetwork

        def cardanoInfo: CardanoInfo
        def network: Network
        def slotConfig: SlotConfig
        def params: ProtocolParams

        final def babbageUtxoMinLovelace(serializedSize: PositiveInt): Coin = Coin(
          (160 + serializedSize.convert) * params.utxoCostPerByte
        )

        final def maxNonPlutusTxFee: Coin = Coin(
          params.txFeeFixed + params.maxTxSize * params.txFeePerByte
        )
    }
}
