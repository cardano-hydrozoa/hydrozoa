package hydrozoa.multisig.ledger.commitment

import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.cardano.onchain.plutus.v3.TxInInfo
import scalus.crypto.accumulator.BilinearAccumulatorProver
import scalus.uplc.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.bls12_381.G1Element
import scalus.|>
import supranational.blst.Scalar

export KzgCommitment.asG1Element
export KzgCommitment.kzgCommitment

object KzgCommitment {
    trait Produced {
        def kzgCommitment: KzgCommitment
    }

    // WARNING: you can't just `==` IArray, because it doesn't compare on the value of the elements.
    // Let's stop using tedious IArray in favor of ByteString
    type KzgCommitment = ByteString

    extension (self: KzgCommitment)
        // def asByteString: ByteString = ByteString.fromArray(IArray.genericWrapArray(self).toArray)
        def asG1Element: G1Element = G1Element(self)

    extension (utxos: Utxos)
        def kzgCommitment: KzgCommitment =
            KzgCommitment.calculateKzgCommitment(hashToScalar(utxos))

    def empty: KzgCommitment = Map.empty.asInstanceOf[Utxos].kzgCommitment

    def hashToScalar(utxo: Utxos): SList[Scalar] =

        def toPlutus(ti: TransactionInput, to: TransactionOutput): TxInInfo =
            LedgerToPlutusTranslation.getTxInInfoV3(ti, Map(ti -> to))

        // Calculate hashes
        val scalars = SList.from(
          utxo.toList
              .map(e =>
                  toPlutus(e._1, e._2)
                      |> (_.toData)
                      |> serialiseData
                      |> blake2b_224
                      |> (_.bytes)
                      |> Scalar().from_bendian
              )
        )

        // println(s"utxos hashes: ${scalars.map(e => BigInt.apply(e.to_bendian()))}")
        scalars

    extension (self: Scalar)
        def asScalusScalar: ScalusScalar =
            ScalusScalar.fromByteStringBigEndianUnsafe(ByteString.fromArray(self.to_bendian()))

    /** Calculates the commitment for the pairing-based accumulator.
      *
      * The commitment is the polynomial `P(x) = product (x + s_i)` over the set's scalars,
      * evaluated at the setup's secret `tau` in the exponent — that is, the multi-scalar
      * multiplication of `P`'s coefficients against the SRS's G1 ladder.
      *
      * Both halves come from `scalus.crypto.accumulator`: the coefficients from a binary subproduct
      * tree with NTT multiplication (`O(n log^2 n)`), and the evaluation from a Pippenger
      * multi-scalar multiplication.
      *
      * @param scalars
      *   utxo set (active, though might be any)
      * @return
      *   G1 point that corresponds to the commitment
      */
    def calculateKzgCommitment(scalars: SList[Scalar]): KzgCommitment = {
        val setup = TrustedSetup.accumulatorSetupG1

        // The product polynomial has one coefficient more than it has roots, so an n-element set
        // needs n + 1 points off the ladder.
        val size = scalars.length.toInt + 1
        assert(
          size <= setup.g1Powers.length,
          s"There are more UTxOs than supported by the setup: $size"
        )

        BilinearAccumulatorProver
            .accumulateG1(setup, scalars.toScalaList.map(asFieldElement).toVector)
            .toCompressedByteString
    }

    /** A `blst` scalar as the plain non-negative integer the accumulator prover takes. `blst`
      * reduces on the way in, so the round trip through big-endian bytes is exact.
      */
    private def asFieldElement(scalar: Scalar): BigInt = BigInt(1, scalar.to_bendian())
}
