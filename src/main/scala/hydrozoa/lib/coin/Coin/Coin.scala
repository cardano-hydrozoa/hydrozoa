package hydrozoa.lib.coin.Coin

import scala.annotation.targetName
import scala.math.BigDecimal.RoundingMode.RoundingMode
import scala.math.Integral.Implicits.infixIntegralOps

enum CoinArithmeticError:
    case CoinOverflowError
    case CoinUnderflowError

import hydrozoa.lib.coin.Coin.CoinArithmeticError.*

/** Non-negative and bounded amount of coin. Can be used in tx outputs without any additional
  * checks. Some operations
  */
type Coin = Coin.Coin

/** Unbounded integral amount of coin, usually represents a change in the amount and used during
  * calculations.
  */
type CoinUnbounded = Coin.Unbounded

/** Like [[CoinUnbounded]] but contain the fractional part. */
type CoinFractional = Coin.Fractional

object Coin {

    // TODO: Can be Int with the sign bit trick
    opaque type Coin = Long

    def apply(i: Int): Either[CoinArithmeticError, Coin] =
        if i < 0 then Left(CoinUnderflowError)
        else Right(i.toLong)

    def apply(bi: BigInt): Either[CoinArithmeticError, Coin] = bi match
        case _ if bi >= 0 && bi <= Int.MaxValue => Right(bi.intValue)
        case _ if bi > Int.MaxValue             => Left(CoinOverflowError)
        case _                                  => Left(CoinUnderflowError)

    // TODO: more constructors
    // TODO: unsafe constructors?
    def unsafeFromInt(i: Int) = apply(i).toOption.get
    def unsafeFromLong(l: Long) = apply(l.toInt).toOption.get

    val zero: Coin = 0L

    // TODO: ada(ada: Int): Either?
    val ada: Coin = 1_000_000L

    given Conversion[Coin, Long] = identity

    extension (self: Coin)

        @targetName("addCoin")
        infix def +(other: Coin): Unbounded = Unbounded(BigInt(self) + (BigInt(other)))

        @targetName("subtractCoin")
        infix def -(other: Coin): Unbounded = Unbounded(BigInt(self) - (BigInt(other)))

        @targetName("addUnbounded")
        infix def +(unbounded: Unbounded): Either[CoinArithmeticError, Coin] = Coin(
          BigInt(self) + unbounded
        )

        @targetName("subtractUnbounded")
        infix def +(unbounded: Unbounded): Either[CoinArithmeticError, Coin] = Coin(
          BigInt(self) - unbounded
        )

        @targetName("negate")
        def unary_- : Unbounded = Unbounded(BigInt(-self))

        @targetName("scalarMultiplyIntegral")
        infix def *[N](coefficient: N)(using int: Integral[N]): Unbounded = Unbounded(
          coefficient.toLong * self
        )

        @targetName("scalarMultiplyNumeric")
        infix def *[N](coefficient: N)(using num: Numeric[N]): Fractional =
            Fractional(BigDecimal(self * num.toDouble(coefficient)))

        def toUnbounded: Unbounded = Unbounded(BigInt(self))
        def toFractional: Fractional = Fractional(BigDecimal(self))
        def toInt: Int = self.toInt

    // Aggregation operations over Coin
    object Aggregate:

        def sum(coins: List[Coin]): Unbounded = Unbounded(coins.map(_.toUnbounded.convert).sum)

        def average(coins: List[Coin]): Fractional =
            Fractional(coins.map(_.toFractional.convert).sum / coins.length)

        def max(coins: List[Coin]): Coin = unsafeFromLong(coins.map(_.convert).max)

        def min(coins: List[Coin]): Coin = unsafeFromLong(coins.map(_.convert).min)

    // ===================================
    // Coin.Unbounded
    // ===================================

    type Unbounded = Unbounded.Type

    object Unbounded {
        opaque type Type = BigInt

        def apply(i: BigInt): Unbounded = i

        given Conversion[Unbounded, BigInt] = identity

        extension (self: Unbounded)

            @targetName("addUnbounded")
            infix def +(other: Unbounded): Unbounded = (self: BigInt) + (other: BigInt)

            @targetName("subtractUnbounded")
            infix def -(other: Unbounded): Unbounded = (self: BigInt) - (other: BigInt)

            @targetName("negate")
            def unary_- : Unbounded = -(self: BigInt)

            @targetName("scalarMultiplyIntegral")
            infix def *[N](coefficient: N)(using int: Integral[N]): Unbounded =
                (self: BigInt) * BigInt(coefficient.toLong)

            @targetName("scalarMultiplyNumeric")
            infix def *[N](coefficient: N)(using num: Numeric[N]): Fractional =
                Fractional(BigDecimal(self) * BigDecimal(num.toDouble(coefficient)))

            def toCoin: Either[CoinArithmeticError, Coin] = self match
                case _ if self >= 0 && self <= Int.MaxValue => Right(self.intValue)
                case _ if self > Int.MaxValue               => Left(CoinOverflowError)
                case _                                      => Left(CoinUnderflowError)
    }

    // ===================================
    // Coin.Fractional
    // ===================================

    type Fractional = Fractional.Type

    object Fractional {
        opaque type Type = BigDecimal

        def apply(x: BigDecimal): Fractional = x

        given Conversion[Fractional, BigDecimal] = identity

        extension (self: Fractional)

            @targetName("addFractional")
            infix def +(other: Fractional): Fractional = (self: BigDecimal) + (other: BigDecimal)

            @targetName("subtractFractional")
            infix def -(other: Fractional): Fractional = (self: BigDecimal) - (other: BigDecimal)

            @targetName("negate")
            def unary_- : Fractional = -(self: BigDecimal)

            @targetName("scalarMultiplyNumeric")
            infix def *[N](coefficient: N)(using num: Numeric[N]): Fractional =
                Fractional((self: BigDecimal) * BigDecimal(num.toDouble(coefficient)))

            def toCoin(mode: RoundingMode): Either[CoinArithmeticError, Coin] =
                self.round(mode) match
                    case _ if self >= 0 && self <= Int.MaxValue => Right(self.intValue)
                    case _ if self > Int.MaxValue               => Left(CoinOverflowError)
                    case _                                      => Left(CoinUnderflowError)

            def round(mode: RoundingMode): Unbounded = Unbounded(self.setScale(0, mode).toBigInt)
    }
}
