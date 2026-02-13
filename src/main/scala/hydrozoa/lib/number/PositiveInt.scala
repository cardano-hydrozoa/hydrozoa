package hydrozoa.lib.number

type PositiveInt = PositiveInt.PositiveInt

object PositiveInt {
    opaque type PositiveInt = scala.Int

    def apply(i: scala.Int): Option[PositiveInt] = if i >= 0 then Some(i) else None

    def unsafeApply(i: scala.Int): PositiveInt = {
        require(i >= 0)
        i
    }

    given Conversion[PositiveInt, scala.Int] = identity
}
