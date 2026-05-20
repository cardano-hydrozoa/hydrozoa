package hydrozoa.lib.cardano.value

import hydrozoa.lib.cardano.value.coin.Coin
import hydrozoa.lib.cardano.value.multiasset.MultiAsset
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import coin.Gen.Arb.given
import multiasset.Gen.Arb.given

import registry.*
import registry.scalacheck.*

object Gen {
    import org.scalacheck.Gen

    val registry =
        gen[Value] +:
            gen[Value.Unbounded] +:
            gen[Value.Fractional] +:
            arb[Coin] +:
            arb[Coin.Unbounded] +:
            arb[Coin.Fractional] +:
            arb[MultiAsset] +:
            arb[MultiAsset.Unbounded] +:
            arb[MultiAsset.Fractional]

    object Arb {
        implicit val valueArb: Arbitrary[Value] =
            Arbitrary(registry.make[Gen[Value]])

        implicit val valueUnboundedArb: Arbitrary[Value.Unbounded] =
            Arbitrary(registry.make[Gen[Value.Unbounded]])

        implicit val valueFractionalArb: Arbitrary[Value.Fractional] =
            Arbitrary(registry.make[Gen[Value.Fractional]])
    }
}
