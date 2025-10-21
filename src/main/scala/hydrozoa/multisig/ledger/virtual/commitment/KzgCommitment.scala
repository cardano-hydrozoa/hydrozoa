package hydrozoa.multisig.ledger.virtual.commitment

import com.bloxbean.cardano.client.util.HexUtil
import java.math.BigInteger
import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.ledger.api.v3.TxInInfo
import scalus.prelude.List as SList
import scalus.prelude.crypto.bls12_381.G1
import scalus.|>
import supranational.blst.{P1, Scalar}

object KzgCommitment {

    type KzgCommitment = IArray[Byte]

    def hashToScalar(utxo: UTxO): SList[Scalar] =

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

    /** Calculates the commitment for the pairing-based accumulator.
      *
      * @param utxo
      *   utxo set (active, though might be any)
      * @return
      *   G1 point that corresponds to the commitment
      */
    def calculateCommitment(scalars: SList[Scalar]): KzgCommitment = {

        // println(s"elems: ${scalars.length}")
        // println(s"elems: ${scalars.map(e => BigInt.apply(e.to_bendian()))}")

        // Get as much from the setup as we need: n + 1 elements
        val size = scalars.length.toInt + 1
        val srs = TrustedSetup.takeSrsG1(size)

        // Check the size of the setup is big enough
        assert(
          size == srs.length,
          s"There are more UTxOs than supported by the setup: ${size}"
        )

        val finalPoly = mkFinalPoly(scalars)

        // println(s"finalPoly: ${finalPoly.map(e => BigInt.apply(e.to_bendian()))}")

        val commitment = evalFinalPoly(srs, finalPoly).compress()
        println(s"UTxO set commitment is: ${HexUtil.encodeHexString(commitment)}")
        IArray.unsafeFromArray(commitment)
    }

    /** Multiply normalized N binomials represented by their only coeeficients to get a final
      * polynomial with N+1 coefficients. Uses the schoolbook convolution, which is `O(N^2)`. This
      * is alleviated by the rather quick Montgomery multiplication the underlying `blst` library
      * uses.
      *
      * Example: for (x+2)(x+3)(x+5)(x+7)(x+11) = 2310 + 2927 x + 1358 x^2 + 288 x^3 + 28 x^4 + x^5
      *
      * @param binomials
      *   coefficients for binomials, order doesn't matter
      * @return
      *   the coefficients for the final polynomial, with the lowest-degree coefficient coming first
      */
    def mkFinalPoly(binomials: SList[Scalar]): SList[Scalar] =
        val zero = Scalar(BigInteger("0"))
        val one = Scalar(BigInteger("1"))

        binomials
            .foldLeft(SList.single(one.dup())): (acc, term) =>
                // We need to clone the whole `acc` since `mul` mutates it
                // and the final adding gets mutated `shiftedPoly`
                val shiftedPoly: SList[Scalar] = SList.Cons((zero.dup()), acc.map(_.dup))
                val multipliedPoly = acc.map(s => s.mul(term)).appended(zero.dup())
                SList.map2(shiftedPoly, multipliedPoly)((l, r) => l.add(r))

    /** Evaluates the commitment to the final polynomial using the given SRS.
      *
      * TODO: use multi-scalar multiplication, once we have it in the java-blst
      *
      * @param srsG1
      *   setup, should be big enough, controlled by the caller
      * @param finalPoly
      *   coefficients of the final polynimial
      * @return
      *   commitment, a point in G1
      */
    def evalFinalPoly(
        srsG1: SList[P1],
        finalPoly: SList[Scalar]
    ): P1 =
        // Multiply
        val subsetPoints: SList[P1] =
            SList.map2(finalPoly, srsG1): (sb, st) =>
                // This dup is needed, since otherwise we modify the loaded SRS itself
                st.dup().mult(sb)
        // Add
        val zero = P1(G1.zero.toCompressedByteString.bytes)
        subsetPoints.foldLeft(zero.dup()): (a, b) =>
            a.add(b)
}
