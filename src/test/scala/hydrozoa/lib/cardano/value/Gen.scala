package hydrozoa.lib.cardano.value

import hydrozoa.lib.cardano.value.coin.Coin
import hydrozoa.lib.cardano.value.multiasset.MultiAsset
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen as Gen0

import coin.Gen.Arb.given
import multiasset.Gen.Arb.given

import registry.*
import registry.scalacheck.*

object Gen {
    private val registry =
        genFun[Value] +:
            genFun[Value.Unbounded] +:
            genFun[Value.Fractional] +:
            value(arbitrary[Coin]: Gen0[Coin]) +:
            value(arbitrary[Coin.Unbounded]: Gen0[Coin.Unbounded]) +:
            value(arbitrary[Coin.Fractional]: Gen0[Coin.Fractional]) +:
            value(arbitrary[MultiAsset]: Gen0[MultiAsset]) +:
            value(arbitrary[MultiAsset.Unbounded]: Gen0[MultiAsset.Unbounded]) +:
            value(arbitrary[MultiAsset.Fractional]: Gen0[MultiAsset.Fractional])

    object Arb {
        implicit val valueArb: Arbitrary[Value] =
            Arbitrary(registry.make[Gen0[Value]])

        implicit val valueUnboundedArb: Arbitrary[Value.Unbounded] =
            Arbitrary(registry.make[Gen0[Value.Unbounded]])

        implicit val valueFractionalArb: Arbitrary[Value.Fractional] =
            Arbitrary(registry.make[Gen0[Value.Fractional]])
    }
}
