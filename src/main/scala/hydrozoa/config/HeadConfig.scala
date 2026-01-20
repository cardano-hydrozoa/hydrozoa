package hydrozoa.config

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.config.HeadConfig.Fields.*
import hydrozoa.config.HeadConfigError.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.TxTiming
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import scala.concurrent.duration.FiniteDuration
import scala.util.Try
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.rules.CardanoMutator
import scalus.cardano.ledger.{AssetName, CardanoInfo, Coin, PlutusScriptEvaluator, ProtocolParams, SlotConfig, Transaction}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import spire.math.{Rational, UByte}

// ===================================
// Raw config
// ===================================

/** Raw config -- What gets parsed from file. Probably YAML */
// TODO: Move everything from "Assumptions.scala"
// QUESTION: Do we want a separate type to bridge "this is valid yaml" and "this is valid data for the HeadConfig
// smart constructor"?

case class RawConfig(
    ownPeer: PeerSection,
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
    // actual utxos to-be-spent might be part of a larger tx chain, we can't just uses a resolver like
    // blockfrost to query the chain. Thus, we pass in the UTxOs directly.
    resolvedUtxosForInitialization: ResolvedUtxos,
    // FIXME: I guess we need both the public and private key?
    withdrawalFeePkh: VerificationKeyBytes

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

// ===================================
// Head config (parsed)
// ===================================

/** Fully parsed head config
  */
// TODO: Put initial block on its own, and then the "head parameters", check that the treasury's hashed parameters
// match the given head parameters

/** This is the "complete" config that the node uses. Individual components (actors, tx builders,
  * etc.) use polymorphic variants of this parameterized on individual fields via `HasXYZ` traits.
  * The idea is that this entire object can be passed around, while only giving individual
  * components access to the parts that concern them.
  */
// NOTE: this constructor is private on purpose. There are MANY dependencies on the fields in this configuration.
// You should ONLY build this view HeadConfig.parse.
//
// The arguments to the constructor are the pieces of data that are _parsed and validated_ from the RawConfig.
// The methods within the body are fields that can be _derived_ safely from the validated data in the constructor.
// TODO: This can be made opaque by given a bound
case class HeadConfig private (
    verificationKeys: Map[UByte, VerificationKeyBytes],
    // Collective contingency
    collectiveContingency: CollectiveContingency,
    // Individual contingency (every peer has the same contingency)
    individualContingency: IndividualContingency,
    // Peers shares
    equityShares: EquityShares,
    receiveTimeout: FiniteDuration,
    txTiming: TxTiming,
    startTime: QuantizedInstant,
    ownPeerPkh: VerificationKeyBytes,
    network: Network,
    cardanoInfo: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    withdrawalFeePkh: VerificationKeyBytes,
    initializationTxSeq: InitializationTxSeq,
    votingDuration: QuantizedFiniteDuration
) extends HasVerificationKeys,
      HasReceiveTimeout,
      HasTxTiming,
      HasStartTime,
      HasHeadMultisigScript,
      HasMultisigRegimeUtxo,
      HasTokenNames,
      HasOwnPeerPkh,
      HasNetwork,
      HasCardanoInfo,
      HasEvaluator,
      HasWithdrawalFeePkh,
      HasEquityShares,
      HasVotingDuration,
      HasTreasuryTokenName,
      HasInitialTreasury,
      HasInitialFallbackValidityStart,
      HasInitialKzgCommitment,
      HasInitializationTxSeq,
      HasHeadAddress {
    def headAddress: ShelleyAddress = headMultisigScript.mkAddress(network)

    // Should we parse this instead of doing fromListUnsafe?
    def headMultisigScript: HeadMultisigScript =
        HeadMultisigScript(NonEmptyList.fromListUnsafe(verificationKeys.values.toList))

    def tokenNames: TokenNames = initializationTxSeq.initializationTx.tokenNames

    def multisigRegimeUtxo: MultisigRegimeUtxo =
        initializationTxSeq.initializationTx.multisigRegimeWitness

    def initialFallbackValidityStart: QuantizedInstant =
        initializationTxSeq.fallbackTx.validityStart

    def initialKzgCommitment: KzgCommitment =
        IArray.from(initializationTxSeq.initializationTx.treasuryProduced.datum.commit.bytes)

    def initialTreasury: MultisigTreasuryUtxo =
        initializationTxSeq.initializationTx.treasuryProduced

    def treasuryTokenName: AssetName = tokenNames.headTokenName

}

// TODO: Make another config "NodeConfig" which should include node-specific information (blockfrost api key,
// peer private key)

object HeadConfig {

    /** Smart constructor for HeadConfig */
    def parse(rawConfig: RawConfig): Either[HeadConfigError, HeadConfig] =
        val peers = rawConfig.otherPeers.prepended(rawConfig.ownPeer)
        for {
            // Check key uniqueness
            keysUnique <- {

                val sortedVKs = peers.toList.map(_.verificationKeyBytes).sortBy(_.bytes)
                val duplicates =
                    sortedVKs.groupMapReduce(identity)(_ => 1)(_ + _).filter((_, cnt) => cnt > 1)
                Either.cond(
                  duplicates.isEmpty,
                  sortedVKs,
                  NonUniqueVerificationKey(duplicates.keys.toSet)
                )
            }
            // Check number of peers
            _ <- Either.cond(
              keysUnique.sizeIs < 255,
              (),
              HeadConfigError.TooManyPeers(keysUnique.length)
            )
            // Attach indices to verification keys
            verificationKeys0 = keysUnique.zipWithIndex.map((k, i) => (UByte.apply(i), k)).toMap
            // Convert list of peer config section into map indexed by the verification key
            peerSectionMap = peers.map(s => s.verificationKeyBytes -> s).toList.toMap
            // Contingency
            collectiveContingency0 = CollectiveContingency.apply(UByte(keysUnique.size))
            individualContingency0 = IndividualContingency.apply
            // Construct contingencyDepositsAndEquityShares
            peersShares = verificationKeys0.map((id, i) =>
                val section = peerSectionMap(i)
                id -> (section.payoutAddress, section.equityShare)
            )
            contingencyDepositsAndEquityShares <- EquityShares.apply(
              peersShares,
              collectiveContingency0,
              individualContingency0
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
            protocolParams0: ProtocolParams = ProtocolParams.fromBlockfrostJson(
              this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
            )

            slotConfig0: SlotConfig = rawConfig.network match {
                case Mainnet => SlotConfig.Mainnet
                case Testnet => SlotConfig.Preview
            }

            cardanoInfo0: CardanoInfo = CardanoInfo(
              protocolParams = protocolParams0,
              network = rawConfig.network,
              slotConfig = slotConfig0
            )
            evaluator0: PlutusScriptEvaluator = PlutusScriptEvaluator(
              cardanoInfo0,
              EvaluateAndComputeCost
            )

            initializationTxSeq0 <- InitializationTxSeq
                .parse(
                  transactionSequence = (initializationTx, fallbackTx),
                  expectedNetwork = rawConfig.network,
                  peerKeys = NonEmptyList.fromListUnsafe(verificationKeys0.values.toList),
                  expectedTallyFeeAllowance = rawConfig.tallyFeeAllowance,
                  expectedVotingDuration = rawConfig.votingDuration,
                  env = cardanoInfo0,
                  evaluator = evaluator0,
                  // QUESTION: We aren't responsible for submitting the InitializationTxSeq, so
                  // what subset of validators should we actually be using?
                  validators = CardanoMutator.allValidators.values.toSeq,
                  resolver = _ => rawConfig.resolvedUtxosForInitialization,
                  initializationRequestTimestamp = rawConfig.startTime,
                  txTiming = rawConfig.txTiming
                )
                .left
                .map(InitializationTxSeqParseError(_))

            headConfig: HeadConfig = ???
        } yield headConfig

    /** This object contains traits to allow for something similar to row polymorphism.
      */
    // TODO: These should all be macros/code-genned.
    object Fields {
        trait HasHeadAddress {
            def headAddress: ShelleyAddress
        }

        trait HasVerificationKeys {
            // Mapping form alphabetically assigned UByte index - FIXME: we decided to give up automatic sorting
            def verificationKeys: Map[UByte, VerificationKeyBytes]
        }

        trait HasReceiveTimeout {

            /** The receive timeout used for actor's preStart methods when we need an actor to
              * automatically run a command periodically
              *
              * @return
              */
            def receiveTimeout: FiniteDuration
        }

        trait HasHeadMultisigScript {

            /** The parameterized head multisig script pertaining to this hydrozoa head
              */
            def headMultisigScript: HeadMultisigScript
        }

        /** The token names associated with this head instance, as parametrized on the seed UTxO of
          * the initialization transaction
          */
        trait HasTokenNames {
            def tokenNames: TokenNames
        }

        trait HasOwnPeerPkh {
            def ownPeerPkh: VerificationKeyBytes
        }

        trait HasWithdrawalFeePkh {

            /** The PKH of the wallet that will pay for withdrawal TX fees and receive the change
              * NOTE: The is no coin selection algorithm right now. This means that:
              *   - This wallet MUST NOT contain anything but ADA-only utxos
              *   - Every utxo at this address will be spent at each withdrawal transaction
              *   - All ADA left-overs will be sent back to this address. Do NOT use this address
              *     for anything else. The delegation part will NOT be set
              */
            def withdrawalFeePkh: VerificationKeyBytes
        }

        trait HasMultisigRegimeUtxo {
            def multisigRegimeUtxo: MultisigRegimeUtxo
        }

        trait HasCardanoInfo {
            def cardanoInfo: CardanoInfo
        }

        trait HasEvaluator {
            def evaluator: PlutusScriptEvaluator
        }

        trait HasNetwork {
            def network: Network
        }

        trait HasTxTiming {
            def txTiming: TxTiming
        }

        trait HasStartTime {
            def startTime: QuantizedInstant
        }

        trait HasTallyFeeAllowance {
            def tallyFeeAllowance: Coin
        }

        trait HasEquityShares {
            def equityShares: EquityShares
        }

        trait HasVotingDuration {
            def votingDuration: QuantizedFiniteDuration
        }

        trait HasTreasuryTokenName {
            def treasuryTokenName: AssetName
        }

        trait HasInitialTreasury {
            def initialTreasury: MultisigTreasuryUtxo
        }

        trait HasInitialFallbackValidityStart {
            def initialFallbackValidityStart: QuantizedInstant
        }

        trait HasInitialKzgCommitment {
            def initialKzgCommitment: KzgCommitment
        }

        trait HasSlotConfig {
            def slotConfig: SlotConfig
        }

        trait HasInitializationTxSeq {
            def initializationTxSeq: InitializationTxSeq
        }
    }
}

enum HeadConfigError:
    case NonUniqueVerificationKey(duplicates: Set[VerificationKeyBytes])
    case TooManyPeers(count: Int)
    case SharesMustSumToOne(total: Rational)
    case TooSmallCollateralDeposit(peer: UByte, minimal: Coin, actual: Coin)
    case TooSmallVoteDeposit(peer: UByte, minimal: Coin, actual: Coin)
    case CborDeserializationFailed(msg: String, bytes: Array[Byte], error: Throwable)
    case InitializationTxSeqParseError(wrapped: InitializationTxSeq.ParseError)

    def explain: String = this match
        case NonUniqueVerificationKey(duplicates) =>
            s"A duplicate verification key(s) found: ${duplicates.map(_.bytes.toHex)}"
        case TooManyPeers(count)       => s"Too many peers: $count, maximum allowed is 255"
        case SharesMustSumToOne(total) => s"Shares do not sum up to one, got total: $total"
        case TooSmallCollateralDeposit(peer, minimal, actual) =>
            s"The peer ${peer} has too small collateral deposit: ${actual}, minimal is: {minimal}"
        case TooSmallVoteDeposit(peer, minimal, actual) =>
            s"The peer ${peer} has too small vote deposit: ${actual}, minimal is: {minimal}"
