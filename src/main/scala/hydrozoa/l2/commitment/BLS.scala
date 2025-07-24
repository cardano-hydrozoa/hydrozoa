package hydrozoa.l2.commitment

import hydrozoa.infra.{decodeHex, encodeHex}
import scalus.prelude.crypto.bls12_381.{G1, G2}
import supranational.blst.{P1, P2}

// BLS common stuff

// Hex-encoded IArray[Byte]
type UtxoSetCommitment = String

// The point at infinity AKA zero point in G1.
val infG1Point: P1 =
    P1(
      IArray
          .genericWrapArray(
            decodeHex(
              G1.zero.toCompressedByteString.toHex
            )
          )
          .toArray
    )

val infG1: IArray[Byte] =
    IArray.unsafeFromArray(infG1Point.compress())

val infG1hex: UtxoSetCommitment = encodeHex(infG1)

// The point at infinity AKA zero point in G2.
val infG2Point: P2 =
    P2(
      IArray
          .genericWrapArray(
            decodeHex(
              G2.zero.toCompressedByteString.toHex
            )
          )
          .toArray
    )

val infG2: IArray[Byte] =
    IArray.unsafeFromArray(infG2Point.compress())

val infG2hex: UtxoSetCommitment = encodeHex(infG2)
