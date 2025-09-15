package hydrozoa.multisig.ledger.virtual.commitment

import com.bloxbean.cardano.client.util.HexUtil
import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.ledger.api.v3.TxInInfo
import scalus.prelude.crypto.bls12_381.G1
import scalus.prelude.{asScalus, List as SList}
import supranational.blst.{P1, Scalar}

import java.math.BigInteger

object KzgCommitment {

    // Hex-encoded IArray[Byte]
    type KzgCommitment = IArray[Byte]

    // TODO: use a type wrapper for the return value
    def getUtxosActiveCommitment(utxo: UTxO): IArray[Byte] = {

        def toPlutus(ti: TransactionInput, to: TransactionOutput): TxInInfo =
            // FIXME: why does that compile? should be `to`, not `utxo`?
            LedgerToPlutusTranslation.getTxInInfoV3(ti, utxo)

        val elemsRaw = utxo.toList
            .map(e => blake2b_224(serialiseData(toPlutus(e._1, e._2).toData)).toHex)
            .asScalus
        println(s"utxos active hashes raw: $elemsRaw")

        val elems = utxo.toList
            .map(e =>
                Scalar().from_bendian(blake2b_224(serialiseData(toPlutus(e._1, e._2).toData)).bytes)
            )
            .asScalus
        println(s"utxos active hashes: ${elems.map(e => BigInt.apply(e.to_bendian()))}")

        // Read as much as we need
        // TODO: we need to cache it not to read it every time we make a commitment
        val g1Monomials = TrustedSetup.g1Monomials(elems.length.toInt + 1)
        // Check the size of the setup is big enough
        assert(elems.length + 1 == g1Monomials.length, "Number of UTxOs should fit the setup")

        val commitmentPoint = getG1Commitment(g1Monomials, elems)
        val commitment = IArray.unsafeFromArray(commitmentPoint.compress())
        println(s"UTxO set commitment is: ${HexUtil.encodeHexString(commitment.toArray)}")
        commitment
    }

    // TODO: use multi-scalar multiplication, once we have it in the java-blst
    private def getG1Commitment(
        setup: SList[P1],
        subset: SList[Scalar]
    ): P1 =
        // Multiply
        val subsetPoints: SList[P1] =
            SList.map2(mkFinalPoly(subset), setup): (sb, st) =>
                st.mult(sb)
        // Add
        val zero = P1(G1.zero.toCompressedByteString.bytes)
        subsetPoints.foldLeft(zero.dup()): (a, b) =>
            a.add(b)

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
    private def mkFinalPoly(binomials: SList[Scalar]): SList[Scalar] =
        val zero = Scalar(BigInteger("0"))
        val one = new Scalar(BigInteger("1"))

        binomials
            .foldLeft(SList.single(one)): (acc, term) =>
                // We need to clone the whole `acc` since `mul` mutates it
                // and the final adding gets mutated `shiftedPoly`
                val shiftedPoly: SList[Scalar] = SList.Cons(zero, acc.map(_.dup))
                val multipliedPoly = acc.map(s => s.mul(term)).appended(zero)
                SList.map2(shiftedPoly, multipliedPoly)((l, r) => l.add(r))

}
