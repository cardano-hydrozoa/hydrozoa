package hydrozoa.config.head

import hydrozoa.VerificationKeyBytes
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.HeadParameters
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.head.rulebased.dispute.DisputeResolutionConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.consensus.peer.{PeerId, PeerNumber}
import hydrozoa.multisig.ledger.block.Block
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import scalus.cardano.address.Network
import scalus.cardano.ledger.{CardanoInfo, Coin, Hash32, ProtocolParams, SlotConfig, TransactionOutput, Utxo, Utxos, Value}

final case class HeadConfig(
    override val cardanoNetwork: CardanoNetwork,
    override val headParams: HeadParameters,
    override val headPeers: HeadPeers,
    override val initialBlock: Block.MultiSigned.Initial,
    override val initializationParams: InitializationParameters,
) extends HeadConfig.Section

object HeadConfig {
    trait Section
        extends CardanoNetwork.Section,
          HeadParameters.Section,
          HeadPeers.Section,
          InitialBlock.Section,
          InitializationParameters.Section {

        override transparent inline def cardanoInfo: CardanoInfo = cardanoNetwork.cardanoInfo
        override transparent inline def network: Network = cardanoNetwork.network
        override transparent inline def slotConfig: SlotConfig = cardanoNetwork.slotConfig
        override transparent inline def cardanoParams: ProtocolParams = cardanoNetwork.cardanoParams

        override transparent inline def txTiming: TxTiming = headParams.txTiming
        override transparent inline def minSettlementDuration: QuantizedFiniteDuration =
            headParams.minSettlementDuration
        override transparent inline def inactivityMarginDuration: QuantizedFiniteDuration =
            headParams.inactivityMarginDuration
        override transparent inline def silenceDuration: QuantizedFiniteDuration =
            headParams.silenceDuration
        override transparent inline def depositMaturityDuration: QuantizedFiniteDuration =
            headParams.depositMaturityDuration
        override transparent inline def depositAbsorptionDuration: QuantizedFiniteDuration =
            headParams.depositAbsorptionDuration

        override transparent inline def fallbackContingency: FallbackContingency =
            headParams.fallbackContingency
        override transparent inline def collectiveContingency: FallbackContingency.Collective =
            headParams.collectiveContingency
        override transparent inline def individualContingency: FallbackContingency.Individual =
            headParams.individualContingency

        override transparent inline def disputeResolutionConfig: DisputeResolutionConfig =
            headParams.disputeResolutionConfig
        override transparent inline def votingDuration: QuantizedFiniteDuration =
            headParams.votingDuration

        override transparent inline def headParamsHash: Hash32 =
            headParams.headParamsHash

        override transparent inline def peerVKey(p: PeerId): VerificationKeyBytes =
            headPeers.peerVKey(p)
        override transparent inline def nPeers: PositiveInt =
            headPeers.nPeers
        override transparent inline def headMultisigScript: HeadMultisigScript =
            headPeers.headMultisigScript

        override transparent inline def initialL2Utxos: Utxos =
            initializationParams.initialL2Utxos
        override transparent inline def initialEquityContributions: Map[PeerNumber, Coin] =
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
    }
}
