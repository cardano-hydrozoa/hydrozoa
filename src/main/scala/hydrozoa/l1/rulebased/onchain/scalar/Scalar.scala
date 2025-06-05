package hydrozoa.l1.rulebased.onchain.scalar

import scalus.builtin.Builtins.{byteStringToInteger, integerToByteString}
import scalus.builtin.ByteString
import scalus.prelude.{Option, require}
import scalus.prelude.Option.{None, Some}
import scalus.{Compile, Ignore}

import java.math.BigInteger
import scala.annotation.tailrec

case class Scalar(private val unScalar: BigInt)

@Compile
object Scalar:

    // The prime number defining the scalar field of the BLS12-381 curve.
    val fieldPrime: BigInt = BigInt(
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
    )

    // Constants
    def zero: Scalar = new Scalar(BigInt(0))

    def one: Scalar = new Scalar(BigInt(1))

    // Construction
    def apply(n: BigInt): Option[Scalar] =
        if n >= 0 && n < fieldPrime then Some(new Scalar(n))
        else None

    def applyUnsafe(n: BigInt): Scalar =
        require(0 <= n && n < fieldPrime, "value not in the field")
        new Scalar(n)

    // TODO: Why doesn't it work?
    @Ignore
    def apply(s: String): Option[Scalar] =
        val n = BigInt(BigInteger(s))
        Scalar.apply(n)

    // Conversions
    def fromByteStringBigEndian(bytes: ByteString): Option[Scalar] =
        Scalar.apply(byteStringToInteger(true, bytes))

    def fromByteStringBigEndianUnsafe(bytes: ByteString): Scalar =
        Scalar.applyUnsafe(byteStringToInteger(true, bytes))

    def fromByteStringLittleEndian(bytes: ByteString): Option[Scalar] =
        Scalar.apply(byteStringToInteger(false, bytes))

    extension (self: Scalar)

        // Combining

        // Adds two `Scalar` elements, ensuring the result stays within the finite field range.
        def add(addend: Scalar): Scalar =
            new Scalar((self.unScalar + addend.unScalar) % fieldPrime)

        /// Multiplies two `Scalar` elements, with the result constrained within the finite field.
        def mul(multiplier: Scalar): Scalar =
            new Scalar(self.unScalar * multiplier.unScalar % fieldPrime)

        // Divides one `Scalar` element by another, returning `None` if the divisor is zero.
        def div(divisor: Scalar): Option[Scalar] = {
            if (divisor.unScalar == zero.unScalar) then None
            else Some(self.mul(divisor.scale(fieldPrime - 2)))
        }

        // Subtracts one `Scalar` element from another, with the result wrapped within the finite field range.
        def sub(subtrahend: Scalar): Scalar =
            val difference = self.unScalar - subtrahend.unScalar
            new Scalar(
              if difference >= 0 then difference
              else fieldPrime + difference
            )

        // Modifying

        // Exponentiates a `Scalar` element by a non-negative integer exponent,
        // using repeated squaring.
        // Note that this function returns `scalar.zero` for negative exponents.
        // A dedicated builtin function for this is in the making, see CIP 109.
        def scale(e: BigInt): Scalar =
            if e < 0 then zero
            else if e == BigInt(0) then one
            else if e % 2 == BigInt(0) then self.mul(self).scale(e / 2)
            else self.mul(self.mul(self).scale((e - 1) / 2))

        // A faster version of `scale` for the case where the exponent is a power of two.
        // That is, the exponent `e = 2^k` for some non-negative integer `k`.
        // Which is used a lot in zk-SNARKs.
        def scale2(k: BigInt): Scalar =

            @tailrec
            def go(self: Scalar, k: BigInt): Scalar =
                if k == BigInt(0) then self
                else go(self.mul(self), k - 1)

            if k < 0 then zero
            else go(self, k)

        def neg: Scalar =
            if self.unScalar == BigInt(0) then self
            else new Scalar(fieldPrime - self.unScalar)

        // Calculates the multiplicative inverse of an `Scalar` element, returning `None` if the element is zero.
        def recip: Option[Scalar] = one.div(self)

        // Transforming

        def toInt: BigInt = self.unScalar

        // Converts a `Scalar` element to a Big-Endian (most-significant bits first) `ByteString`.
        def toByteStringBigEndian(size: BigInt): ByteString = {
            integerToByteString(true, size, self.unScalar)
        }

        // Converts a `Scalar` element to aLittle-Endian (least-significant bits first) `ByteString`.
        def toByteStringLittleEndian(size: BigInt): ByteString = {
            integerToByteString(false, size, self.unScalar)
        }

end Scalar
