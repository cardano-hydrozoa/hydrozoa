package hydrozoa.l2.commitment

import hydrozoa.infra.{decodeHex, encodeHex}
import supranational.blst.P2

// TODO: generalize for different type of commitments (?)

// BLS common stuff

// The point at infinity AKA zero point in G2.
val infG2Point: P2 =
    P2(
      IArray
          .genericWrapArray(
            decodeHex(
              "c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
            )
          )
          .toArray
    )

val infG2: IArray[Byte] =
    IArray.unsafeFromArray(infG2Point.compress())

// Hex-encoded IArray[Byte]
type UtxoSetCommitment = String

val infG2hex: UtxoSetCommitment = encodeHex(infG2)
