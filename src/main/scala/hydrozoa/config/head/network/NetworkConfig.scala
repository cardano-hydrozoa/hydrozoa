package hydrozoa.config.head.network

import scalus.cardano.ledger.CardanoInfo

enum NetworkConfig(_cardanoInfo: CardanoInfo) {
    case Mainnet extends NetworkConfig(CardanoInfo.mainnet)
    case Preprod extends NetworkConfig(CardanoInfo.preprod)
    case Preview extends NetworkConfig(CardanoInfo.preview)
    case Custom(override val cardanoInfo: CardanoInfo) extends NetworkConfig(cardanoInfo)

    def cardanoInfo: CardanoInfo = _cardanoInfo
}

object NetworkConfig {}
