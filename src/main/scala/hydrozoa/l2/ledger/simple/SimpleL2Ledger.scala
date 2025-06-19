package hydrozoa.l2.ledger.simple

import cats.implicits.toBifunctorOps
import hydrozoa.*
import hydrozoa.infra.{Piper, decodeBech32AddressL2, plutusAddressAsL2}
import hydrozoa.l2.ledger.*

import scala.collection.mutable
import scalus.builtin.ByteString
import scalus.prelude.AssocMap
import scalus.prelude.Option.{Some as SSome, None as SNone}
import scalus.prelude.{Option as SOption, given}
import scalus.ledger.api.v1 as scalus

/** This object defines types and constructors for Hydrozoa's L2 ledger and contains a class that
  * implements L2LedgerModule.
  */
object SimpleL2Ledger:

    opaque type LedgerUtxoId = scalus.TxOutRef
    opaque type LedgerOutput = scalus.TxOut

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
        val sTxId = scalus.TxId(ByteString.fromHex(UtxoIdL2.txId.hash))
        val sTxIx = BigInt(UtxoIdL2.outputIx.ix)
        scalus.TxOutRef(sTxId, sTxIx)

    private def unliftOutputRef(outputRef: LedgerUtxoId): UtxoIdL2 =
        UtxoIdL2(TxId(outputRef.id.hash.toHex), TxIx(outputRef.idx.toChar))

    private def liftOutput(output: OutputL2): LedgerOutput =
        val address = decodeBech32AddressL2(output.address)
        val value = scalus.Value.lovelace(output.coins)
        scalus.TxOut(address = address, value = value, datumHash = SNone)

    private def unliftOutput(output: LedgerOutput): Output[L2] =
        val SSome(e) = AssocMap.get(output.value)(ByteString.empty)
        val SSome(coins) = AssocMap.get(e)(ByteString.empty)
        Output[L2](plutusAddressAsL2(output.address).asL2, coins, emptyTokens)

    private def liftUtxoSet(utxoSet: Map[UtxoIdL2, OutputL2]): LedgerUtxoSetOpaque =
        utxoSet.map(_.bimap(liftOutputRef, liftOutput))

    // This is not private, since it's used by MBT suite
    def unliftUtxoSet(utxosSetOpaque: LedgerUtxoSetOpaque): Map[UtxoIdL2, OutputL2] =
        utxosSetOpaque.map(_.bimap(unliftOutputRef, unliftOutput))

    // The implementation
    private class SimpleL2Ledger[InstancePurpose <: LedgerPurpose]
        extends L2LedgerModule[InstancePurpose, LedgerUtxoSetOpaque]:

        override type LedgerTransaction = L2Transaction | L2Withdrawal

        type SubmissionError = String

        private type UtxosSetOpaqueMutable = mutable.Map[LedgerUtxoId, LedgerOutput]
        private val activeState: UtxosSetOpaqueMutable = mutable.Map.empty

        override def isEmpty: Boolean = activeState.isEmpty

        override def getUtxosActive: LedgerUtxoSetOpaque = activeState.clone.toMap

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
                val before: scalus.Value =
                    inputs.map(_.value).fold(scalus.Value.zero)(scalus.Value.plus)
                val after: scalus.Value =
                    outputs.map(_.value).fold(scalus.Value.zero)(scalus.Value.plus)
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
