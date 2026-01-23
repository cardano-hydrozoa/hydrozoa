package hydrozoa.config

import cats.data.NonEmptyList
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, InitializationTx, TxTiming}
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.protocol.types.Peer
import hydrozoa.{VerificationKeyBytes, Wallet}
import scala.collection.immutable.TreeMap
import scalus.cardano.address.Network
import scalus.cardano.ledger.{CardanoInfo, Coin}

private object HeadConfig2 {

    /** @param initialBlock
      *   the initial block with which the peers have agreed to start the head
      * @param cardanoInfo
      *   the blockchain settings of the Cardano blockchain network on which the head is deployed
      * @param headParameters
      *   the parameters of the head -- these get hashed and committed in the treasury utxo.
      * @param privateNodeSettings
      *   the settings that the hydrozoa node's user has defined for his own node. These also
      *   include the peer's private key for hydrozoa consensus.
      */
    final case class HeadConfig(
        initialBlock: InitialBlock,
        cardanoInfo: CardanoInfo,
        headParameters: HeadParameters,
        privateNodeSettings: PrivateNodeSettings
    ) {
        def headInstance: HeadInstance = HeadInstance(
          network = cardanoInfo.network,
          headMultisigScript = headParameters.headPeers.headMultisigScript,
          tokenNames = initialBlock.tokenNames,
          multisigRegimeUtxo = initialBlock.multisigRegimeUtxo
        )
    }

    final case class HeadParameters(
        headPeers: HeadPeers,
        multisigRegimeSettings: MultisigRegimeSettings,
        ruleBasedRegimeSettings: RuleBasedRegimeSettings,
        fallbackSettings: FallbackSettings
    )

    final case class PrivateNodeSettings(
        ownPeer: OwnPeer,
        multisigRegimeOperationalSettings: MultisigRegimeOperationalSettings,
        liquidationActorOperationalSettings: LiquidationActorOperationalSettings
    )

    final case class OwnPeer(
        peerId: Peer.Id,
        wallet: Wallet
    )

    final case class InitialBlock(
        startTime: QuantizedInstant,
        initializationTxSeq: InitializationTxSeq
    ) {
        def initializationTx: InitializationTx = initializationTxSeq.initializationTx
        def initialFallbackTx: FallbackTx = initializationTxSeq.fallbackTx
        def initialTreasury: MultisigTreasuryUtxo = initializationTx.treasuryProduced
        def initialKzgCommitment: KzgCommitment = IArray.from(initialTreasury.datum.commit.bytes)
        def tokenNames: TokenNames = initializationTx.tokenNames
        def multisigRegimeUtxo: MultisigRegimeUtxo = initializationTx.multisigRegimeUtxo
    }

    /** A Hydrozoa head is uniquely identified on the L1 blockchain by the L1's network, the head's
      * multisig script, and the head's token names.
      *
      * Additionally, the head's multisig regime utxo, if it exists, is a stable indicator that the
      * head exists and is in the multisig regime.
      */
    final case class HeadInstance(
        network: Network,
        headMultisigScript: HeadMultisigScript,
        tokenNames: TokenNames,
        multisigRegimeUtxo: MultisigRegimeUtxo
    )

    final case class HeadPeers(
        peerKeys: TreeMap[Peer.Id, VerificationKeyBytes]
    ) {
        def headMultisigScript: HeadMultisigScript =
            HeadMultisigScript(NonEmptyList.fromListUnsafe(peerKeys.values.toList))
    }

    final case class MultisigRegimeSettings(
        txTiming: TxTiming
    )

    final case class RuleBasedRegimeSettings(
        votingDuration: QuantizedFiniteDuration
    )

    /** Settings for the fallback into the rule-based regime. These settings define the individual
      * and collective deposits collected from the peers at head initialization to ensure that the
      * head can always fallback to the rule-based regime and resolve the dispute.
      *
      * @param collectiveContingency
      *   the collective deposit from all peers to cover common expenses
      * @param individualContingency
      *   the individual deposit from each peer to cover individual expenses
      */
    final case class FallbackSettings(
        collectiveContingency: CollectiveContingency,
        individualContingency: IndividualContingency,
        tallyFeeAllowance: Coin
    )

    /** Settings for the liquidation actor
      * @param withdrawalFeeWallet
      *   the wallet with which the liquidation actor will fund the fees for rule-based withdrawal
      *   transactions.
      * @param pollingPeriod
      *   when idle, the liquidation actor waits this long before polling the Cardano backend again.
      */
    final case class LiquidationActorOperationalSettings(
        withdrawalFeeWallet: Wallet,
        pollingPeriod: QuantizedFiniteDuration
    )

    /** @param pollingPeriod
      *   When idle, the Cardano Liaison waits this long before polling the Cardano backend again.
      */
    final case class MultisigRegimeOperationalSettings(
        pollingPeriod: QuantizedFiniteDuration
    )
}
