package hydrozoa.config.head

import cats.data.{NonEmptyList, NonEmptyMap}
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.config.head.rulebased.scripts.RuleBasedScriptAddresses
import hydrozoa.lib.cardano.scalus.QuantizedTime
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.ledger.block.Block
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.{CardanoInfo, Coin, Hash32, ProtocolParams, SlotConfig, TransactionOutput, Utxo, Utxos, Value}
import scalus.crypto.ed25519.VerificationKey

final case class HeadConfig private (
    override val cardanoNetwork: CardanoNetwork,
    override val headParams: HeadParameters,
    override val headPeers: HeadPeers,
    override val initialBlock: Block.MultiSigned.Initial,
    override val initializationParams: InitializationParameters,
) extends HeadConfig.Section {
    override transparent inline def headConfig: HeadConfig = this
}

object HeadConfig {
    def apply(
        cardanoNetwork: CardanoNetwork,
        headParams: HeadParameters,
        headPeers: HeadPeers,
        initialBlock: Block.MultiSigned.Initial,
        initializationParams: InitializationParameters,
    ): Option[HeadConfig] =
        Option.when(initialBlock.startTime == initializationParams.headStartTime)(
          new HeadConfig(cardanoNetwork, headParams, headPeers, initialBlock, initializationParams)
        )

    trait Section extends HeadConfig.Preinit.Section, InitialBlock.Section {
        def headConfig: HeadConfig

        override transparent inline def initialBlockSection: InitialBlock = InitialBlock(
          initialBlock
        )

        override transparent inline def headConfigPreInit: Preinit.HeadConfig = {
            Preinit.HeadConfig(
              headConfig.cardanoNetwork,
              headConfig.headParams,
              headConfig.headPeers,
              headConfig.initializationParams
            )
        }

        override transparent inline def headStartTime: QuantizedTime.QuantizedInstant =
            initialBlockSection.headStartTime
    }

    object Preinit {
        // TODO: rename to avoid HeadConfig.Preinit.HeadConfig?
        final case class HeadConfig(
            override val cardanoNetwork: CardanoNetwork,
            override val headParams: HeadParameters,
            override val headPeers: HeadPeers,
            override val initializationParams: InitializationParameters,
        ) extends Preinit.Section {
            override transparent inline def headConfigPreInit: Preinit.HeadConfig = this
        }

        trait Section
            extends CardanoNetwork.Section,
              HeadParameters.Section,
              HeadPeers.Section,
              InitializationParameters.Section {
            def headConfigPreInit: Preinit.HeadConfig

            override transparent inline def cardanoInfo: CardanoInfo = cardanoNetwork.cardanoInfo
            override transparent inline def network: Network = cardanoNetwork.network
            override transparent inline def slotConfig: SlotConfig = cardanoNetwork.slotConfig
            override transparent inline def cardanoProtocolParams: ProtocolParams =
                cardanoNetwork.cardanoProtocolParams

            override transparent inline def txTiming: TxTiming = headParams.txTiming

            override transparent inline def fallbackContingency: FallbackContingency =
                headParams.fallbackContingency

            override transparent inline def disputeResolutionConfig: DisputeResolutionConfig =
                headParams.disputeResolutionConfig

            override transparent inline def headParamsHash: Hash32 =
                headParams.headParamsHash

            override def headPeerNums: NonEmptyList[HeadPeerNumber] =
                headPeers.headPeerNums
            override transparent inline def headPeerIds: NonEmptyList[HeadPeerId] =
                headPeers.headPeerIds
            override transparent inline def headPeerVKeys: NonEmptyList[VerificationKey] =
                headPeers.headPeerVKeys
            override transparent inline def headPeerVKey(
                p: HeadPeerNumber
            ): Option[VerificationKey] =
                headPeers.headPeerVKey(p)
            override transparent inline def headPeerVKey(
                p: HeadPeerId
            ): Option[VerificationKey] =
                headPeers.headPeerVKey(p)
            override transparent inline def nHeadPeers: PositiveInt =
                headPeers.nHeadPeers
            override transparent inline def headMultisigScript: HeadMultisigScript =
                headPeers.headMultisigScript

            override def headStartTime: QuantizedTime.QuantizedInstant =
                initializationParams.headStartTime
            override transparent inline def initialL2Utxos: Utxos =
                initializationParams.initialL2Utxos
            override transparent inline def initialEquityContributions
                : NonEmptyMap[HeadPeerNumber, Coin] =
                initializationParams.initialEquityContributions
            override transparent inline def initialSeedUtxo: Utxo =
                initializationParams.initialSeedUtxo
            override transparent inline def initialAdditionalFundingUtxos: Utxos =
                initializationParams.initialAdditionalFundingUtxos
            override transparent inline def initialChangeOutputs: List[TransactionOutput] =
                initializationParams.initialChangeOutputs
            override transparent inline def initialEquityContributed: Coin =
                initializationParams.initialEquityContributed
            override transparent inline def initialFundingValue: Value =
                initializationParams.initialFundingValue
            override transparent inline def initialL2Value: Value =
                initializationParams.initialL2Value
            override transparent inline def headTokenNames: CIP67.HeadTokenNames =
                initializationParams.headTokenNames
            override transparent inline def initialEquityContributionsHash: Hash32 =
                initializationParams.initialEquityContributionsHash

            override transparent inline def ruleBasedScriptAddresses: RuleBasedScriptAddresses =
                cardanoNetwork.ruleBasedScriptAddresses

            override transparent inline def ruleBasedTreasuryAddress: ShelleyAddress =
                ruleBasedScriptAddresses.ruleBasedTreasuryAddress

            override transparent inline def ruleBasedDisputeResolutionAddress: ShelleyAddress =
                ruleBasedScriptAddresses.ruleBasedDisputeResolutionAddress
        }
    }
}
