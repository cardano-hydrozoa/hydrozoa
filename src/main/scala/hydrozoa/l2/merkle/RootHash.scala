package hydrozoa.l2.merkle

import hydrozoa.infra.decodeHex
import supranational.blst.P2

// TODO: generalize for different type of commitments

type RH32UtxoSetL2 = Unit // H32[UtxoSetL2]

object RH32UtxoSetL2:
    def dummy: RH32UtxoSetL2 = () // H32.hash(IArray()) // TODO: implement

// BLS common stuff

val infG2Point = P2(
  decodeHex(
    "c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
  ).toArray
)

val infG2 = IArray.unsafeFromArray(infG2Point.compress())
