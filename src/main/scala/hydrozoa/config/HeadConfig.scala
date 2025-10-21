package hydrozoa.config

import hydrozoa.*
import spire.math.{Rational, UByte}

import HeadConfigError.{NonConsistentShares, NonUniqueVerificationKey}

// ===================================
// Raw config
// ===================================

/** Raw config
  */
case class RawConfig(
    peers: List[PeerSection]
)

/** An element of a raw head config that describes a peer.
  */
case class PeerSection(
    verificationKeyBytes: VerificationKeyBytes,
    equityWithdrawAddress: AddressL1,
    equityShare: Rational
)

// ===================================
// Head config (parsed
// ===================================

/** Fully parsed head config.
  */
case class HeadConfig(
    peers: Map[UByte, VerificationKeyBytes],
    shares: EquityShares
)

object HeadConfig:

    def parse(rawConfig: RawConfig): Either[HeadConfigError, HeadConfig] = for {
        keys <- {
            val sortedVKs = rawConfig.peers.map(_.verificationKeyBytes).sortBy(_.bytes)
            val duplicates =
                sortedVKs.groupMapReduce(identity)(_ => 1)(_ + _).filter((_, cnt) => cnt > 1)
            Either.cond(
              duplicates.isEmpty,
              sortedVKs,
              NonUniqueVerificationKey(duplicates.keys.toSet)
            )
        }
        _ <- Either.cond(keys.length < 255, (), HeadConfigError.TooManyPeers(keys.length))
        peers = keys.zipWithIndex.map((k, i) => (UByte.apply(i), k)).toMap
        equityShares <- EquityShares.apply(
          rawConfig.peers.map(i => (i.equityWithdrawAddress, i.equityShare))
        )
    } yield HeadConfig(
      peers = peers,
      shares = equityShares
    )

enum HeadConfigError:
    case NonUniqueVerificationKey(duplicates: Set[VerificationKeyBytes])
    case NonConsistentShares(total: Rational)
    case TooManyPeers(count: Int)

    def explain: String = this match
        case NonUniqueVerificationKey(duplicates) =>
            s"A duplicate verification key(s): ${duplicates.map(_.bytes.toHex)}"
        case NonConsistentShares(total) => s"Shares do not sum to 1, got total: $total"
        case TooManyPeers(count)        => s"Too many peers: $count, maximum allowed is 255"
