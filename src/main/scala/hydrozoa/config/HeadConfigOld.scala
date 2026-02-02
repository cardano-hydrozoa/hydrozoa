package hydrozoa.config

import cats.data.NonEmptyList
import hydrozoa.config.HeadConfig.Error.*
import hydrozoa.config.HeadConfig.{HeadInstanceL1, HeadParameters, InitialBlock, OwnPeer, PrivateNodeSettings}
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.multisig.consensus.peer.{PeerId, PeerWallet}
import hydrozoa.multisig.ledger.block.BlockEffects
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.HeadTokenNames
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, InitializationTx}
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.{AddressL1, VerificationKeyBytes}
import scala.collection.immutable.TreeMap
import scala.concurrent.duration.FiniteDuration
import scala.util.Try
import scalus.cardano.address.Network
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import spire.math.{Rational, UByte}

/** Raw config -- What gets parsed from file. Probably YAML */
// TODO: Move everything from "Assumptions.scala"
// QUESTION: Do we want a separate type to bridge "this is valid yaml" and "this is valid data for the HeadConfig
// smart constructor"? Or do we just want to define yaml parsing for each of these types? I'm in favor of the latter
case class RawConfig(
    // OwnPeer (need private key), equity/contingency payout address, equity share
    // QUESTION: does it make sense to change the rational to PeerEquityShare?
    ownPeer: (OwnPeer, AddressL1, Rational),
    otherPeers: List[PeerSection],
    receiveTimeout: FiniteDuration,
    initializationTxBytes: Array[Byte],
    initialFallbackTxBytes: Array[Byte],
    network: Network,
    tallyFeeAllowance: Coin,
    votingDuration: QuantizedFiniteDuration,
    txTiming: TxTiming,
    startTime: QuantizedInstant,
    // In order to validate the initialization tx seq, we need a resolved utxo set. Since the
    // actual utxos to-be-spent might be part of a larger tx chain, we can't just use a resolver like
    // blockfrost to query the chain. Thus, we pass in the UTxOs directly.
    resolvedUtxosForInitialization: ResolvedUtxos,
    // FIXME: I guess we need both the public and private key?
    withdrawalFeeWallet: PeerWallet,
    pollingPeriod: FiniteDuration

    // Augmented Initial block
    // Creation time
    // initial list of L2 utxos in CBOR
    // fully signed init tx
    // fully signed fallback zero tx
)

/** An element of a raw head config that describes a peer */
case class PeerSection(
    // The verification key of peer's key pair used to control peer's L1 address
    // and also for signing L2 block headers (what else?)
    verificationKeyBytes: VerificationKeyBytes,
    // The address to pay out back contingency deposits and equity shares.
    payoutAddress: AddressL1,
    // The peer's share of equity
    equityShare: Rational
)

/** NOTE: All constructors for case classes in this object are private[config]. They MUST be
  * constructed by parsing a [[RawConfig]] in order to maintain internal coherence.
  */
object HeadConfig {

    /** Smart constructor for HeadConfig */
    def parse(rawConfig: RawConfig): Either[Error, HeadConfig] =
        val peers = rawConfig.otherPeers.prepended(
          PeerSection(
            verificationKeyBytes = rawConfig.ownPeer._1.wallet.exportVerificationKeyBytes,
            payoutAddress = rawConfig.ownPeer._2,
            equityShare = rawConfig.ownPeer._3
          )
        )
        for {
            // Check key uniqueness
            keysUnique <- {

                val sortedVKs = peers.map(_.verificationKeyBytes).sortBy(_.bytes)
                val duplicates =
                    sortedVKs.groupMapReduce(identity)(_ => 1)(_ + _).filter((_, cnt) => cnt > 1)
                Either.cond(
                  duplicates.isEmpty,
                  sortedVKs,
                  Error.NonUniqueVerificationKey(duplicates.keys.toSet)
                )
            }
            // Check number of peers
            _ <- Either.cond(
              keysUnique.sizeIs < 255,
              (),
              Error.TooManyPeers(keysUnique.length)
            )
            // Attach indices to verification keys
            verificationKeys = keysUnique.zipWithIndex.map((k, i) => (UByte.apply(i), k)).toMap
            // Convert list of peer config section into map indexed by the verification key
            peerSectionMap = peers.map(s => s.verificationKeyBytes -> s).toMap
            // Contingency
            collectiveContingency = CollectiveContingency.apply(UByte(keysUnique.size))
            individualContingency = IndividualContingency.apply
            // Construct contingencyDepositsAndEquityShares
            peersShares = verificationKeys.map((id, i) =>
                val section = peerSectionMap(i)
                id -> (section.payoutAddress, section.equityShare)
            )
            contingencyDepositsAndEquityShares <- EquityShares.apply(
              peersShares,
              collectiveContingency,
              individualContingency
            )

            initializationTx: Transaction <-
                Try(Transaction.fromCbor(rawConfig.initializationTxBytes)).toEither.left.map(
                  CborDeserializationFailed(
                    "InitializationTx CBOR deserialization failed",
                    rawConfig.initializationTxBytes,
                    _
                  )
                )

            fallbackTx: Transaction <- Try(
              Transaction.fromCbor(rawConfig.initialFallbackTxBytes)
            ).toEither.left.map(
              CborDeserializationFailed(
                "Initial FallbackTx CBor deserialization failed",
                rawConfig.initialFallbackTxBytes,
                _
              )
            )

            // FIXME: Currently hardcoded
            protocolParams: ProtocolParams = ProtocolParams.fromBlockfrostJson(
              this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
            )

            slotConfig: SlotConfig <- rawConfig.network match {
                case Mainnet => Right(SlotConfig.mainnet)
                case Testnet => Right(SlotConfig.preview)
                case scalus.cardano.address.Network.Other(_) =>
                    Left(UnknownNetwork(rawConfig.network))
            }

            cardanoInfo: CardanoInfo = CardanoInfo(
              protocolParams = protocolParams,
              network = rawConfig.network,
              slotConfig = slotConfig
            )

            initializationTxSeq <- InitializationTxSeq
                .parse(
                  transactionSequence = (initializationTx, fallbackTx),
                  resolvedUtxos = rawConfig.resolvedUtxosForInitialization,
                  tallyFeeAllowance = rawConfig.tallyFeeAllowance,
                  votingDuration = rawConfig.votingDuration,
                  peerKeys = NonEmptyList.fromListUnsafe(verificationKeys.values.toList),
                  startTime = rawConfig.startTime,
                  txTiming = rawConfig.txTiming,
                  cardanoInfo = cardanoInfo
                )
                .left
                .map(InitializationTxSeqParseError(_))

            initialBlock = InitialBlock(
              startTime = rawConfig.startTime,
              effects = BlockEffects.MultiSigned.Initial(
                initializationTx = initializationTxSeq.initializationTx,
                fallbackTx = initializationTxSeq.fallbackTx
              )
            )

            headParameters = HeadParameters(
              headPeers = HeadPeers(
                TreeMap.from(
                  verificationKeys.map((id, pubKey) =>
                      (PeerId(id.toInt, verificationKeys.size), pubKey)
                  )
                )
              ),
              multisigRegimeSettings = MultisigRegimeSettings(
                txTiming = rawConfig.txTiming
              ),
              ruleBasedRegimeSettings = RuleBasedRegimeSettings(
                votingDuration = rawConfig.votingDuration
              ),
              fallbackSettings = FallbackSettings(
                collectiveContingency,
                individualContingency,
                rawConfig.tallyFeeAllowance
              )
            )

            privateNodeSettings = PrivateNodeSettings(
              ownPeer = rawConfig.ownPeer._1,
              multisigRegimeOperationalSettings =
                  MultisigRegimeOperationalSettings(rawConfig.pollingPeriod),
              liquidationActorOperationalSettings = LiquidationActorOperationalSettings(
                withdrawalFeeWallet = rawConfig.withdrawalFeeWallet,
                pollingPeriod = rawConfig.pollingPeriod
              )
            )

            headConfig: HeadConfig = HeadConfig(
              initialBlock = initialBlock,
              cardanoInfo = cardanoInfo,
              headParameters = headParameters,
              privateNodeSettings = privateNodeSettings
            )
        } yield headConfig

    enum Error extends Throwable {
        case NonUniqueVerificationKey(duplicates: Set[VerificationKeyBytes])
        case TooManyPeers(count: Int)
        case SharesMustSumToOne(total: Rational)
        case TooSmallCollateralDeposit(peer: UByte, minimal: Coin, actual: Coin)
        case TooSmallVoteDeposit(peer: UByte, minimal: Coin, actual: Coin)
        case CborDeserializationFailed(msg: String, bytes: Array[Byte], error: Throwable)
        case InitializationTxSeqParseError(wrapped: InitializationTxSeq.ParseError)
        case UnknownNetwork(network: Network)

        def explain: String = this match
            case NonUniqueVerificationKey(duplicates) =>
                s"A duplicate verification key(s) found: ${duplicates.map(_.bytes.toHex)}"
            case TooManyPeers(count)       => s"Too many peers: $count, maximum allowed is 255"
            case SharesMustSumToOne(total) => s"Shares do not sum up to one, got total: $total"
            case TooSmallCollateralDeposit(peer, minimal, actual) =>
                s"The peer $peer has too small collateral deposit: $actual, minimal is: {minimal}"
            case TooSmallVoteDeposit(peer, minimal, actual) =>
                s"The peer $peer has too small vote deposit: $actual, minimal is: {minimal}"
            case x: hydrozoa.config.HeadConfig.Error.CborDeserializationFailed =>
                s"CBOR deserialization failed: ${x.msg}"
            case hydrozoa.config.HeadConfig.Error.InitializationTxSeqParseError(wrapped) =>
                s"Initialization transaction sequence failed to parse. Error: $wrapped"
            case hydrozoa.config.HeadConfig.Error.UnknownNetwork(network) =>
                s"Expected either mainnet or testnet network ID, but received: ${network.networkId}"
    }

    final case class HeadParameters private[config] (
        headPeers: HeadPeers,
        multisigRegimeSettings: MultisigRegimeSettings,
        ruleBasedRegimeSettings: RuleBasedRegimeSettings,
        fallbackSettings: FallbackSettings
    )

    final case class PrivateNodeSettings private[config] (
        ownPeer: OwnPeer,
        multisigRegimeOperationalSettings: MultisigRegimeOperationalSettings,
        liquidationActorOperationalSettings: LiquidationActorOperationalSettings
    )

    final case class OwnPeer private[config] (
        peerId: PeerId,
        wallet: PeerWallet
    )

    final case class InitialBlock private[config] (
        startTime: QuantizedInstant,
        effects: BlockEffects.MultiSigned.Initial
    ) {
        def initialFallbackTx: FallbackTx = effects.fallbackTx

        def initialKzgCommitment: KzgCommitment = IArray.from(initialTreasury.datum.commit.bytes)

        def initialTreasury: MultisigTreasuryUtxo = initializationTx.treasuryProduced

        def tokenNames: HeadTokenNames = initializationTx.tokenNames

        def multisigRegimeUtxo: MultisigRegimeUtxo = initializationTx.multisigRegimeUtxo

        def initializationTx: InitializationTx = effects.initializationTx
    }

    /** A Hydrozoa head is uniquely identified on the L1 blockchain by the L1's network, the head's
      * multisig script, and the head's token names.
      *
      * Additionally, the head's multisig regime utxo, if it exists, is a stable indicator that the
      * head exists and is in the multisig regime.
      */
    final case class HeadInstanceL1 private[config] (
        network: Network,
        headMultisigScript: HeadMultisigScript,
        tokenNames: HeadTokenNames,
        multisigRegimeUtxo: MultisigRegimeUtxo
    )

    final case class HeadPeers private[config] (
        peerKeys: TreeMap[PeerId, VerificationKeyBytes]
    ) {
        def headMultisigScript: HeadMultisigScript =
            HeadMultisigScript(NonEmptyList.fromListUnsafe(peerKeys.values.toList))
    }

    final case class MultisigRegimeSettings private[config] (
        txTiming: TxTiming
    )

    final case class RuleBasedRegimeSettings private[config] (
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
    final case class FallbackSettings private[config] (
        collectiveContingency: CollectiveContingency,
        individualContingency: IndividualContingency,
        tallyFeeAllowance: Coin
    )

    /** Settings for the liquidation actor
      *
      * @param withdrawalFeeWallet
      *   the wallet with which the liquidation actor will fund the fees for rule-based withdrawal
      *   transactions.
      * @param pollingPeriod
      *   when idle, the liquidation actor waits this long before polling the Cardano backend again.
      */
    final case class LiquidationActorOperationalSettings private[config] (
        withdrawalFeeWallet: PeerWallet,
        pollingPeriod: FiniteDuration
    )

    /** @param pollingPeriod
      *   When idle, the Cardano Liaison waits this long before polling the Cardano backend again.
      */
    final case class MultisigRegimeOperationalSettings private[config] (
        pollingPeriod: FiniteDuration
    )
}

/** @param initialBlock
  *   the initial block with which the peers have agreed to start the head
  * @param cardanoInfo
  *   the blockchain settings of the Cardano blockchain network on which the head is deployed
  * @param headParameters
  *   the parameters of the head -- these get hashed and committed in the treasury utxo.
  * @param privateNodeSettings
  *   the settings that the hydrozoa node's user has defined for his own node. These also include
  *   the peer's private key for hydrozoa consensus.
  */
final case class HeadConfig private[config] (
    initialBlock: InitialBlock,
    cardanoInfo: CardanoInfo,
    headParameters: HeadParameters,
    privateNodeSettings: PrivateNodeSettings
) {
    def headInstance: HeadInstanceL1 = HeadInstanceL1(
      network = cardanoInfo.network,
      headMultisigScript = headParameters.headPeers.headMultisigScript,
      tokenNames = initialBlock.tokenNames,
      multisigRegimeUtxo = initialBlock.multisigRegimeUtxo
    )
}
