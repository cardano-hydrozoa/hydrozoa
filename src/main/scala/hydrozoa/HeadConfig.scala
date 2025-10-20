package hydrozoa

/** The head config is used in both multisig and rule-based regimes, so it's hard to decide where we
  * should put it.
  */

import hydrozoa.HeadConfigError.{NonConsistentShares, NonUniqueVerificationKey}
import scalus.cardano.ledger.Coin
import spire.compat.integral
import spire.implicits.eqOps
import spire.math.{Rational, UByte}
import spire.syntax.literals.r

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
            val verificationKeys = rawConfig.peers.map(_.verificationKeyBytes)
            val duplicates =
                verificationKeys.groupMapReduce(identity)(_ => 1)(_ + _).filter((_, cnt) => cnt > 1)
            Either.cond(
              duplicates.isEmpty,
              verificationKeys,
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

case class EquityShares private (private val shares: List[(AddressL1, Rational)]):
    def distribute(equity: Coin): EquityShares.Distribution = {
        val totalAmount = equity.value
        val distributedShares = shares.map { (address, share) =>
            val amount = (totalAmount * share).floor.toLong
            address -> Coin(amount)
        }.toMap

        val totalDistributed = distributedShares.values.map(_.value).sum
        val dust = Coin(totalAmount - totalDistributed)

        EquityShares.Distribution(distributedShares, dust)
    }

object EquityShares:
    def apply(
        shares: List[(AddressL1, Rational)]
    ): Either[HeadConfigError.NonConsistentShares, EquityShares] = {
        val total = shares.map(_._2).sum
        Either.cond(
          total === r"1",
          new EquityShares(shares),
          HeadConfigError.NonConsistentShares(total)
        )
    }

    case class Distribution(
        shares: Map[AddressL1, Coin],
        dust: Dust // Leftover after the distribution
    )

    type Dust = Coin

//val foo: UByte = ub"42"
//val shares = EquityShares
