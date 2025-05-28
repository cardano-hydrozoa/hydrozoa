package hydrozoa.l2.ledger.simple

import cats.implicits.toBifunctorOps
import cats.implicits.toBifunctorOps

import hydrozoa.*
import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.infra.{decodeBech32AddressL1, decodeBech32AddressL2, plutusAddressAsL2}
import hydrozoa.l1.multisig.state.{DepositUtxos, depositDatum}
import hydrozoa.l2.ledger.*

import scala.collection.mutable
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

import scalus.builtin.ByteString

import scalus.prelude.AssocMap
import scalus.prelude.Maybe.{Just, Nothing}
import scalus.prelude.Prelude.given_Eq_ByteString

import scalus.ledger.api.v1 as scalus

//
//    private def handleGenesis(event: GenesisL2) =
//        val (_, txId) = AdaSimpleLedger.asTxL2(event.genesis)
//
//        val utxoDiff = event.genesis.outputs.zipWithIndex
//            .map(output =>
//                val txIn = UtxoIdL2(txId, TxIx(output._2.toChar))
//                val txOut = Output[L2](output._1.address.asL1, output._1.coins)
//                (txIn, txOut)
//            )
//            .toSet
//
//        val utxoDiffInt = event.genesis.outputs.zipWithIndex
//            .map(output =>
//                val txIn = liftOutputRef(UtxoIdL2(txId, TxIx(output._2.toChar)))
//                val txOut = liftOutput(output._1.address, output._1.coins)
//                (txIn, txOut)
//            )
//            .toSet
//
//        activeState.addAll(utxoDiffInt)
//        Right((txId, utxoDiff))
//

/** This object defines types and constructors for Hydrozoa's L2 ledger and contains a class that
  * implements L2LedgerModule.
  */
object SimpleHydrozoaL2Ledger:

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

    private class SimpleL2Ledger[InstancePurpose <: LedgerPurpose]
        extends L2LedgerModule[InstancePurpose, LedgerUtxoSetOpaque]:

        override type LedgerTransaction = L2Transaction | L2Withdrawal

        type SubmissionError = String

        type UtxosSetOpaqueMutable = mutable.Map[LedgerUtxoId, LedgerOutput]
        private val activeState: UtxosSetOpaqueMutable = mutable.Map.empty

        override def isEmpty: Boolean = activeState.isEmpty

        override def getUtxosActive: LedgerUtxoSetOpaque = activeState.clone.toMap

        override def replaceUtxosActive(activeState: LedgerUtxoSetOpaque): Unit =
            this.activeState.clear()
            this.activeState.addAll(activeState)

        override def getState: UtxoSetL2 =
            UtxoSet[L2, Unit](unliftUtxoSet(activeState.clone().toMap))

        override def getOutput(utxoId: UtxoIdL2): OutputL2 =
            activeState(utxoId |> liftOutputRef) |> unliftOutput

        override def flushAndGetState: UtxoSetL2 =
            val ret = activeState.clone()
            activeState.clear()
            UtxoSet[L2, Unit](ret.toMap.map((k, v) => (unliftOutputRef(k), unliftOutput(v))))

        override def cloneForBlockProducer()(using
            InstancePurpose =:= HydrozoaHeadLedger
        ): L2LedgerModule[BlockProducerLedger, LedgerUtxoSetOpaque] =
            val ledgerForBlockProduction = new SimpleL2Ledger[BlockProducerLedger]()
            ledgerForBlockProduction.replaceUtxosActive(activeState.clone().toMap)
            ledgerForBlockProduction

        override def toLedgerTransaction(tx: L2Transaction | L2Withdrawal): LedgerTransaction = tx

        override def addGenesisUtxos(utxoSet: UtxoSetL2): Unit =
            liftUtxoSet(utxoSet.utxoMap) |> this.activeState.addAll

        override def submit(
            event: LedgerTransaction
        ): Either[(TxId, SubmissionError), (TxId, (UtxoSetL2, UtxoSetL2))] =
            event match
                case tx: L2Transaction        => submitTransaction(tx)
                case withdrawal: L2Withdrawal => submitWithdrawal(withdrawal)

        private val emptyUtxoSet = UtxoSet[L2, Unit](Map.empty[UtxoIdL2, Output[L2]])

        private def submitTransaction(tx: L2Transaction) =
            val (_, txId) = asTxL2(tx)

            resolveInputs(tx.inputs) match
                case Left(extraneous) =>
                    Left(txId, s"Extraneous utxos in transaction $txId: $extraneous")
                case Right(oldUtxos) =>
                    // Outputs
                    val newUtxos = tx.outputs.zipWithIndex.map(output =>
                        val txIn = liftOutputRef(UtxoIdL2(txId, TxIx(output._2.toChar)))
                        val txOut = liftOutput(output._1.toOutput)
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
            val (_, txId) = asTxL2(withdrawal)

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
                        UtxoSet[L2, Unit](withdrawnRefsPub.zip(withdrawnOutputsPub).toMap)
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

    def unliftUtxoSet(utxosSetOpaque: LedgerUtxoSetOpaque): Map[UtxoIdL2, OutputL2] =
        utxosSetOpaque.map(_.bimap(unliftOutputRef, unliftOutput))

    // TODO: review

    def asTxL2(event: L2Transaction | L2Withdrawal): (TxL2, TxId) =
        event match
            case transaction: L2Transaction =>
                val cardanoTx = mkCardanoTxForL2Transaction(transaction)
                val txId = txHash(cardanoTx)
                println(s"L2 transaction event, txId: $txId, content: ${serializeTxHex(cardanoTx)}")
                (cardanoTx, txId)
            case withdrawal: L2Withdrawal =>
                val cardanoTx = mkCardanoTxForL2Withdrawal(withdrawal)
                val txId = txHash(cardanoTx)
                println(s"L2 withdrawal event, txId: $txId, content: ${serializeTxHex(cardanoTx)}")
                (cardanoTx, txId)

    def mkTransactionEvent(tx: L2Transaction): L2EventTransaction =
        val (_, txId) = asTxL2(tx)
        L2EventTransaction(txId, tx)

    def mkWithdrawalEvent(withdrawal: L2Withdrawal): L2EventWithdrawal =
        val (_, txId) = asTxL2(withdrawal)
        L2EventWithdrawal(txId, withdrawal)

    //

    def liftUtxoSet(utxoSet: Map[UtxoIdL2, OutputL2]): LedgerUtxoSetOpaque =
        utxoSet.map(_.bimap(liftOutputRef, liftOutput))

    def liftOutputRef(UtxoIdL2: UtxoIdL2): LedgerUtxoId =
        val sTxId = scalus.TxId(ByteString.fromHex(UtxoIdL2.txId.hash))
        val sTxIx = BigInt(UtxoIdL2.outputIx.ix)
        scalus.TxOutRef(sTxId, sTxIx)

    def unliftOutputRef(outputRef: LedgerUtxoId): UtxoIdL2 =
        UtxoIdL2(TxId(outputRef.id.hash.toHex), TxIx(outputRef.idx.toChar))

    def unwrapTxIn(outputRef: LedgerUtxoId): scalus.TxOutRef = outputRef

    def liftOutput(output: OutputL2): LedgerOutput =
        val address = decodeBech32AddressL2(output.address)
        val value = scalus.Value.lovelace(output.coins)
        scalus.TxOut(address = address, value = value, datumHash = Nothing)

    def unliftOutput(output: LedgerOutput): Output[L2] =
        val Just(e) = AssocMap.lookup(output.value)(ByteString.empty)
        val Just(coins) = AssocMap.lookup(e)(ByteString.empty)
        Output[L2](plutusAddressAsL2(output.address).asL2, coins)

    def unwrapTxOut(output: LedgerOutput): scalus.TxOut = output

    def checkSumInvariant(inputs: List[LedgerOutput], outputs: List[LedgerOutput]): Boolean =
        val before: scalus.Value = inputs.map(_.value).fold(scalus.Value.zero)(scalus.Value.plus)
        val after: scalus.Value = outputs.map(_.value).fold(scalus.Value.zero)(scalus.Value.plus)
        before == after
