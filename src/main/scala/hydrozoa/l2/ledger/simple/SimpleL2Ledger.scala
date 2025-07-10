package hydrozoa.l2.ledger.simple

import cats.implicits.toBifunctorOps
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{Piper, decodeBech32AddressL2, decodeHex, encodeHex, plutusAddressAsL2}
import hydrozoa.l1.rulebased.onchain.scalar.Scalar as ScalusScalar
import hydrozoa.l2.ledger.*
import hydrozoa.l2.commitment.infG2Point
import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, ByteString}
import scalus.builtin.Data.toData
import scalus.ledger.api.v2.OutputDatum.NoOutputDatum
import scalus.ledger.api.v3
import scalus.prelude.{AssocMap, List as SList, Option as SOption, given}
import scalus.prelude.List.{Cons, asScala, asScalus}
import scalus.prelude.Option.{None as SNone, Some as SSome}
import scalus.prelude.crypto.bls12_381.G1
import supranational.blst.{P1, P2, Scalar}

import java.math.BigInteger
import scala.collection.mutable
import scala.collection.mutable

/** This object defines types and constructors for Hydrozoa's L2 ledger and contains a class that
  * implements L2LedgerModule.
  */
object SimpleL2Ledger:

    opaque type LedgerUtxoId = v3.TxOutRef
    opaque type LedgerOutput = v3.TxOut

    // Opaque, can be stored and provided back to the ledger.
    opaque type LedgerUtxoSetOpaque = Map[LedgerUtxoId, LedgerOutput]

    given CanEqual[LedgerUtxoSetOpaque, LedgerUtxoSetOpaque] = CanEqual.derived

    def mkLedgerForHead(): L2LedgerModule[HydrozoaHeadLedger, LedgerUtxoSetOpaque] =
        new SimpleL2Ledger()

    def mkLedgerForBlockProducer(
        utxoSet: Map[UtxoIdL2, OutputL2]
    ): L2LedgerModule[BlockProducerLedger, LedgerUtxoSetOpaque] =
        val ledger = new SimpleL2Ledger[BlockProducerLedger]()
        ledger.replaceUtxosActive(utxoSet |> liftUtxoSet)
        ledger

    // UTxO conversions between Hydrozoa types and Ledger types
    private def liftOutputRef(UtxoIdL2: UtxoIdL2): LedgerUtxoId =
        val sTxId = v3.TxId(ByteString.fromHex(UtxoIdL2.txId.hash))
        val sTxIx = BigInt(UtxoIdL2.outputIx.ix)
        v3.TxOutRef(sTxId, sTxIx)

    private def unliftOutputRef(outputRef: LedgerUtxoId): UtxoIdL2 =
        UtxoIdL2(TxId(outputRef.id.hash.toHex), TxIx(outputRef.idx.toChar))

    private def liftOutput(output: OutputL2): LedgerOutput =
        val address = decodeBech32AddressL2(output.address)
        val value = v3.Value.lovelace(output.coins)
        v3.TxOut(address = address, value = value)

    private def unliftOutput(output: LedgerOutput): Output[L2] =
        val SSome(e) = AssocMap.get(output.value)(ByteString.empty)
        val SSome(coins) = AssocMap.get(e)(ByteString.empty)
        Output[L2](plutusAddressAsL2(output.address).asL2, coins, emptyTokens)

    private def liftUtxoSet(utxoSet: Map[UtxoIdL2, OutputL2]): LedgerUtxoSetOpaque =
        utxoSet.map(_.bimap(liftOutputRef, liftOutput))

    // This is not private, since it's used by MBT suite
    def unliftUtxoSet(utxosSetOpaque: LedgerUtxoSetOpaque): Map[UtxoIdL2, OutputL2] =
        utxosSetOpaque.map(_.bimap(unliftOutputRef, unliftOutput))

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

    // The implementation
    private class SimpleL2Ledger[InstancePurpose <: LedgerPurpose]
        extends L2LedgerModule[InstancePurpose, LedgerUtxoSetOpaque]:

        private val log = Logger(getClass)

        override type LedgerTransaction = L2Transaction | L2Withdrawal

        type SubmissionError = String

        private type UtxosSetOpaqueMutable = mutable.Map[LedgerUtxoId, LedgerOutput]
        private val activeState: UtxosSetOpaqueMutable = mutable.Map.empty

        override def isEmpty: Boolean = activeState.isEmpty

        override def getUtxosActive: LedgerUtxoSetOpaque = activeState.clone.toMap

        override def getUtxosActiveCommitment: IArray[Byte] = {
            val elemsRaw = activeState.clone.toList
                .map(e => blake2b_224(serialiseData(e.toData)).toHex)
                .asScalus
            log.info(s"utxos active hashes raw: $elemsRaw")

            val elems = activeState.clone.toList
                .map(e => Scalar().from_bendian(blake2b_224(serialiseData(e.toData)).bytes))
                .asScalus
            log.info(s"utxos active hashes: ${elems.map(e => BigInt.apply(e.to_bendian()))}")

            val setup = mkDummySetupG2(elems.length.toInt)

            val setupBS = setup.map(e => BLS12_381_G2_Element.apply(e).toCompressedByteString)
            setupBS.foreach(println)

            val commitmentPoint = getG2Commitment(setup, elems)
            val commitment = IArray.unsafeFromArray(commitmentPoint.compress())
            log.info(s"Commitment: ${(encodeHex(commitment))}")
            commitment
        }

        override def getState: UtxoSetL2 =
            UtxoSet[L2](unliftUtxoSet(activeState.clone().toMap))

        override def replaceUtxosActive(activeState: LedgerUtxoSetOpaque): Unit =
            this.activeState.clear()
            this.activeState.addAll(activeState)

        override def addGenesisUtxos(utxoSet: UtxoSetL2): Unit =
            liftUtxoSet(utxoSet.utxoMap) |> this.activeState.addAll

        override def getOutput(utxoId: UtxoIdL2): OutputL2 =
            activeState(utxoId |> liftOutputRef) |> unliftOutput

        override def flushAndGetState: UtxoSetL2 =
            val ret = activeState.clone()
            activeState.clear()
            UtxoSet[L2](ret.toMap.map((k, v) => (unliftOutputRef(k), unliftOutput(v))))

        override def submit(
            event: LedgerTransaction
        ): Either[(TxId, SubmissionError), (TxId, (UtxoSetL2, UtxoSetL2))] =
            event match
                case tx: L2Transaction        => submitTransaction(tx)
                case withdrawal: L2Withdrawal => submitWithdrawal(withdrawal)

        private val emptyUtxoSet = UtxoSet[L2](Map.empty[UtxoIdL2, Output[L2]])

        private def submitTransaction(tx: L2Transaction) =

            def checkSumInvariant(
                inputs: List[LedgerOutput],
                outputs: List[LedgerOutput]
            ): Boolean =
                val before: v3.Value =
                    inputs.map(_.value).fold(v3.Value.zero)(v3.Value.plus)
                val after: v3.Value =
                    outputs.map(_.value).fold(v3.Value.zero)(v3.Value.plus)
                before == after

            val txId = calculateTxHash(tx)

            resolveInputs(tx.inputs) match
                case Left(extraneous) =>
                    Left(txId, s"Extraneous utxos in transaction $txId: $extraneous")
                case Right(oldUtxos) =>
                    // Outputs
                    val newUtxos = tx.outputs.zipWithIndex.map(output =>
                        val txIn = liftOutputRef(UtxoIdL2(txId, TxIx(output._2.toChar)))
                        val txOut = liftOutput(Output.apply(output._1))
                        (txIn, txOut)
                    )

                    val (inputRefs, inputs, _) = oldUtxos.unzip3

                    if !checkSumInvariant(inputs, newUtxos.map(_._2)) then
                        Left(txId, s"Sum invariant is not hold for tx $txId")
                    else
                        // FIXME: atomicity
                        inputRefs.foreach(activeState.remove)
                        newUtxos.foreach(activeState.put.tupled)

                        Right((txId, (emptyUtxoSet, emptyUtxoSet)))

        private def submitWithdrawal(withdrawal: L2Withdrawal) =
            val txId = calculateWithdrawalHash(withdrawal)

            resolveInputs(withdrawal.inputs) match
                case Left(extraneous) => Left(txId, s"Extraneous utxos in withdrawal: $extraneous")
                case Right(resolved) =>
                    val (withdrawnRefs, withdrawnOutputs, (withdrawnPub)) = resolved.unzip3
                    val (withdrawnRefsPub, withdrawnOutputsPub) = withdrawnPub.unzip
                    // FIXME: atomicity
                    withdrawnRefs.foreach(activeState.remove)

                    Right(
                      txId,
                      (
                        emptyUtxoSet,
                        UtxoSet[L2](withdrawnRefsPub.zip(withdrawnOutputsPub).toMap)
                      )
                    )

        /** Tries to resolve output refs.
          *
          * @param inputs
          *   output refs to resolve
          * @return
          *   Left if
          */
        private def resolveInputs(
            inputs: List[UtxoIdL2]
        ): Either[List[UtxoIdL2], List[(LedgerUtxoId, LedgerOutput, (UtxoIdL2, Output[L2]))]] =
            inputs
                .map { e =>
                    val outputRefInt = liftOutputRef(e)
                    activeState.get(outputRefInt) match
                        case Some(output) => Right(outputRefInt, output, (e, unliftOutput(output)))
                        case None         => Left(e)
                }
                .partitionMap(identity) match
                case (Nil, resolved) => Right(resolved)
                case (extraneous, _) => Left(extraneous)

        override def toLedgerTransaction(tx: L2Transaction | L2Withdrawal): LedgerTransaction = tx

        override def cloneForBlockProducer()(using
            InstancePurpose =:= HydrozoaHeadLedger
        ): L2LedgerModule[BlockProducerLedger, LedgerUtxoSetOpaque] =
            val ledgerForBlockProduction = new SimpleL2Ledger[BlockProducerLedger]()
            ledgerForBlockProduction.replaceUtxosActive(activeState.clone().toMap)
            ledgerForBlockProduction

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
    val setup = SimpleL2Ledger.mkDummySetupG1(5)
    val setupBS = setup.map(e => BLS12_381_G1_Element.apply(e).toCompressedByteString)
    setupBS.foreach(println)

//    println(encodeHex(IArray.unsafeFromArray(P1.generator().compress())))
//    println(G1.generator.toCompressedByteString)
}
