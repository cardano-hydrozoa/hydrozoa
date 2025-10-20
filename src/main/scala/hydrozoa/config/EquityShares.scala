package hydrozoa.config

import hydrozoa.AddressL1
import scalus.cardano.ledger.Coin
import spire.compat.integral
import spire.implicits.eqOps
import spire.math.Rational
import spire.syntax.literals.r

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
