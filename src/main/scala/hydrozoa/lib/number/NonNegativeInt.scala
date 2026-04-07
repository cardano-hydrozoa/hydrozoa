package hydrozoa.lib.number

type NonNegativeInt = NonNegativeInt.NonNegativeInt

object NonNegativeInt {
    opaque type NonNegativeInt = scala.Int

    def apply(i: scala.Int): Option[NonNegativeInt] = if i >= 0 then Some(i) else None

    def unsafeApply(i: scala.Int): NonNegativeInt = {
        require(i >= 0)
        i
    }

    given Conversion[NonNegativeInt, scala.Int] = identity
}
