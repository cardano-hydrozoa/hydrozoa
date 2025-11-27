package hydrozoa.config

import hydrozoa.*
import hydrozoa.config.HeadConfigError.{NonUniqueVerificationKey, SharesMustSumToOne}
import hydrozoa.lib.cardano.value.coin.Coin
import spire.implicits.*
import spire.math.{Rational, UByte}


// ===================================
// Raw config
// ===================================

/** Raw config */
case class RawConfig(
    peers: List[PeerSection]
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
// Head config (parsed
// ===================================

/** Fully parsed head config.
  */
case class HeadConfig(
    // Mapping form alphabetically assigned UByte index
    verificationKeys: Map[UByte, VerificationKeyBytes],
    // Collective contingency
    collectiveContingency: CollectiveContingency,
    // Individual contingency (every peer has the same contingency)
    individualContingency: IndividualContingency,
    // Peers shares
    equityShares: EquityShares
)

object HeadConfig:

    def parse(rawConfig: RawConfig): Either[HeadConfigError, HeadConfig] = for {
        // Check key uniqueness
        keysUnique <- {
            val sortedVKs = rawConfig.peers.map(_.verificationKeyBytes).sortBy(_.bytes)
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
        verificationKeys = keysUnique.zipWithIndex.map((k, i) => (UByte.apply(i), k)).toMap
        // Convert list of peer config section into map indexed by the verification key
        peerSectionMap = rawConfig.peers.map(s => s.verificationKeyBytes -> s).toMap
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
    } yield {

        HeadConfig(
          verificationKeys = verificationKeys,
          collectiveContingency = collectiveContingency,
          individualContingency = individualContingency,
          equityShares = contingencyDepositsAndEquityShares
        )
    }

end HeadConfig

enum HeadConfigError:
    case NonUniqueVerificationKey(duplicates: Set[VerificationKeyBytes])
    case TooManyPeers(count: Int)
    case SharesMustSumToOne(total: Rational)
    case TooSmallCollateralDeposit(peer: UByte, minimal: Coin, actual: Coin)
    case TooSmallVoteDeposit(peer: UByte, minimal: Coin, actual: Coin)

    def explain: String = this match
        case NonUniqueVerificationKey(duplicates) =>
            s"A duplicate verification key(s) found: ${duplicates.map(_.bytes.toHex)}"
        case TooManyPeers(count)       => s"Too many peers: $count, maximum allowed is 255"
        case SharesMustSumToOne(total) => s"Shares do not sum up to one, got total: $total"
        case TooSmallCollateralDeposit(peer, minimal, actual) =>
            s"The peer ${peer} has too small collateral deposit: ${actual}, minimal is: {minimal}"
        case TooSmallVoteDeposit(peer, minimal, actual) =>
            s"The peer ${peer} has too small vote deposit: ${actual}, minimal is: {minimal}"
