package hydrozoa.multisig.ledger.virtual.commitment

import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.ledger.{LedgerToPlutusTranslation, TransactionInput, TransactionOutput, UTxO}
import scalus.ledger.api.v3.TxInInfo
import scalus.sir.SIRBuiltins.{blake2b_224, serialiseData}
import supranational.blst.P2

object Commitment {
    type KzgCommitment = String

    // The point at infinity AKA zero point in G2.
    val infG2Point: P2 =
        P2(
            ByteString
                .fromHex(
                    "c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
                )
                .bytes
        )

    val infG2: IArray[Byte] =
        IArray.unsafeFromArray(infG2Point.compress())

    // Hex-encoded IArray[Byte]
    type UtxoSetCommitment = String

    val infG2hex: UtxoSetCommitment = ByteString.fromArray(infG2.toArray).toHex

    def getUtxosActiveCommitment(utxo: UTxO): IArray[Byte] = {
        def toPlutus(ti: TransactionInput, to: TransactionOutput): TxInInfo =
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

        val setup = mkDummySetupG2(elems.length.toInt)

        val setupBS = setup.map(e => BLS12_381_G2_Element.apply(e).toCompressedByteString)
        setupBS.foreach(println)

        val commitmentPoint = getG2Commitment(setup, elems)
        val commitment = IArray.unsafeFromArray(commitmentPoint.compress())
        println(s"Commitment: ${(ByteString.fromArray(commitment.toArray)).toHex}")
        commitment
    }

    ////////////////////////////////////////////
    // BLS Stuff

    // TODO: this will be gone as soon as we get a setup ceremony up and running.
    val tau = Scalar(BigInteger("42"))

    def mkDummySetupG2(n: Int): SList[P2] = {
        val setup =
            (1 to n + 1).map(i =>
                P2.generator().dup().mult(tau.dup().mul(Scalar(BigInteger(i.toString))))
            )
        SList.Cons(P2.generator(), setup.toList.asScalus)
    }

    def mkDummySetupG1(n: Int): SList[P1] = {
        val setup =
            (1 to n + 1).map(i =>
                P1.generator().dup().mult(tau.dup().mul(Scalar(BigInteger(i.toString))))
            )
        SList.Cons(P1.generator(), setup.toList.asScalus)
    }


    /*
     * Multiply a list of n coefficients that belong to a binomial each to get a final polynomial of degree n+1
     * Example: for (x+2)(x+3)(x+5)(x+7)(x+11)=x^5 + 28 x^4 + 288 x^3 + 1358 x^2 + 2927 x + 2310
     */
    def getFinalPoly(binomial_poly: SList[Scalar]): SList[Scalar] = {
        binomial_poly
            .foldLeft(SList.single(new Scalar(BigInteger("1")))): (acc, term) =>
                // We need to clone the whole `acc` since `mul` mutates it
                // and final adding gets mutated `shiftedPoly`
                val shiftedPoly: SList[Scalar] =
                    SList.Cons(Scalar(BigInteger("0")), acc.map(_.dup))
                val multipliedPoly = acc.map(s => s.mul(term)).appended(Scalar(BigInteger("0")))
                SList.map2(shiftedPoly, multipliedPoly)((l, r) => l.add(r))
    }

    // TODO: use multi-scalar multiplication
    def getG2Commitment(
                           setup: SList[P2],
                           subset: SList[Scalar]
                       ): P2 = {
        val subsetInG2 =
            SList.map2(getFinalPoly(subset), setup): (sb, st) =>
                st.mult(sb)

        val zero = infG2Point
        require(zero.is_inf())

        subsetInG2.foldLeft(zero.dup()): (a, b) =>
            a.add(b)
    }

    @main
    def dumpSetupG1(): Unit = {
        val setup = mkDummySetupG1(6)
        val setupBS = setup.map(e => BLS12_381_G1_Element.apply(e).toCompressedByteString)
        setupBS.foreach(println)

        //    println(encodeHex(IArray.unsafeFromArray(P1.generator().compress())))
        //    println(G1.generator.toCompressedByteString)
    }

}
