package hydrozoa.l2.ledger

import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.l1.multisig.state.DepositUtxos
import hydrozoa.l2.ledger.event.*
import hydrozoa.l2.ledger.state.*

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

sealed trait TInstancePurpose
sealed trait THydrozoaHead extends TInstancePurpose
sealed trait TBlockProduction extends TInstancePurpose

case class AdaSimpleLedger[InstancePurpose <: TInstancePurpose] private (
    verifier: Verifier[L2Event]
) extends L2Ledger[
      Utxos,
      SimpleGenesis,
      SimpleTransaction,
      SimpleWithdrawal,
      UtxosDiff,
      L2Event,
      L2EventHash,
      Verifier[L2Event]
    ]:

    val activeState: Utxos = mutable.Map.empty

    /** Makes a copy of the current ledger for block production purposes.
      * @param ev
      *   evidence that we are cloning Hydrozoa ledger, not its clone
      * @return
      *   cloned ledger
      */
    def blockProduction(implicit
        ev: InstancePurpose =:= THydrozoaHead
    ): AdaSimpleLedger[TBlockProduction] =
        val ledgerForBlockProduction: AdaSimpleLedger[TBlockProduction] = copy()
        ledgerForBlockProduction.updateUtxosActive(activeState.clone())
        ledgerForBlockProduction

    override def submit[E1 <: L2Event](
        event: E1
    ): Either[(L2EventHash, String), (L2EventHash, Option[TxL2], event.UtxosDiff)] =
        require(verifier.isValid(event), true)
        event match
            case genesis: L2Genesis       => handleGenesis(genesis)
            case tx: L2Transaction        => handleTransaction(tx)
            case withdrawal: L2Withdrawal => handleWithdrawal(withdrawal)

    override def evaluate[E1 <: L2Event](event: E1): Either[String, (L2EventHash, Option[TxL2])] =
        event match
            case event: L2Genesis =>
                val s = s"L2 simple genesis: ${event.genesis}"
                val cardanoTx = mkVirtualGenesisTx(event.genesis)
                val txId = txHash(cardanoTx)
                println(s"L2 genesis event, txId: $txId, content: ${serializeTxHex(cardanoTx)}")
                Right(txId, Some(cardanoTx))
            case event: L2Transaction =>
                val s = s"L2 simple transaction: ${event.transaction}"
                val cardanoTx = mkVirtualTransactionL2(event.transaction)
                val txId = txHash(cardanoTx)
                println(s"L2 tx event, txId: $txId, content: ${serializeTxHex(cardanoTx)}")
                Right(txId, Some(cardanoTx))
            case event: L2Withdrawal =>
                val s = s"Simple withdrawing: ${event.withdrawal}"
                resolveInputs(event.withdrawal.inputs) match
                    case Left(extraneous) => Left(s"Extreneous utxos found: $extraneous")
                    case Right(resolved)  =>
                        // FIXME: why doesn't `resolved.map(unwrapTxOut) work?
                        val cardanoTx = mkVirtualWithdrawalTx(
                          event.withdrawal,
                          resolved.map(o => unwrapTxOut(o))
                        )
                        val txId = txHash(cardanoTx)
                        println(
                          s"L2 withdrawal event, txId: $txId, content: ${serializeTxHex(cardanoTx)}"
                        )
                        Right(txId, Some(cardanoTx))

    /** Tries to resolve output refs.
      * @param inputs
      *   output refs to resolve
      * @return
      *   Left if
      */
    private def resolveInputs(
        inputs: List[OutputRefL2]
    ): Either[List[OutputRefL2], List[OutputInt]] =
        inputs
            .map(e => activeState.get(liftOutputRef(e)).toRight(e))
            .partitionMap(identity) match
            case (Nil, resolved) => Right(resolved)
            case (extraneous, _) => Left(extraneous)

    private def handleGenesis(event: L2Genesis) =
        val Right(txId, mbCardanoTx) = evaluate(event)

        val utxoDiff = event.genesis.outputs.zipWithIndex
            .map(output =>
                val txIn = liftOutputRef(OutputRefL2(txId, TxIx(output._2)))
                val txOut = liftOutput(output._1.address, output._1.coins)
                (txIn, txOut)
            )
            .toSet
        activeState.addAll(utxoDiff)
        Right((txId, mbCardanoTx, utxoDiff))

    private def handleTransaction(event: L2Transaction) =
        val Right(txId, mbCardanoTx) = evaluate(event)

        // Inputs
        val spentRefs = event.transaction.inputs.map(i => liftOutputRef(i))
        val extraneousRefs = spentRefs.filterNot(activeState.contains)

        if extraneousRefs.nonEmpty then Left(txId, s"Extraneous inputs in the tx: $extraneousRefs")
        else
            val spentOutputs = spentRefs.map(activeState.get).map(_.get)

            // Outputs
            val newUtxos = event.transaction.outputs.zipWithIndex.map(output =>
                val txIn = liftOutputRef(OutputRefL2(txId, TxIx(output._2)))
                val txOut = liftOutput(output._1.address, output._1.coins)
                (txIn, txOut)
            )

            if !checkSumInvariant(spentOutputs, newUtxos.map(_._2)) then
                Left(txId, s"Sum invariant is not hold for tx $txId")
            else
                // FIXME: atomicity
                spentRefs.foreach(activeState.remove)
                newUtxos.foreach(activeState.put.tupled)

                Right((txId, mbCardanoTx, Set[(OutputRefInt, OutputInt)]()))

    private def handleWithdrawal(event: L2Withdrawal) =
        val Right(txId, mbCardanoTx) = evaluate(event)

        // Inputs
        val withdrawnRefs = event.withdrawal.inputs.map(i => liftOutputRef(i))
        val extraneousRefs = withdrawnRefs.filterNot(activeState.contains)

        if extraneousRefs.nonEmpty then
            Left(txId, s"Extraneous inputs in the withdrawal: $extraneousRefs")
        else
            val withdrawnOutputs = withdrawnRefs.map(activeState.get).map(_.get)

            withdrawnRefs.foreach(activeState.remove)
            Right(txId, mbCardanoTx, withdrawnRefs.zip(withdrawnOutputs).toSet)

    private def eventHash(s: String): TxId =
        TxId(encodeHex(CryptoHash.H32.hash(IArray.from(s.getBytes)).bytes))

    override def isEmpty: Boolean = activeState.isEmpty

    override def flush: Utxos =
        val ret = activeState.clone()
        activeState.clear()
        ret

    override def updateUtxosActive(activeState: Utxos): Unit =
        // TODO: revise
        this.activeState.clear()
        this.activeState.addAll(activeState)

object AdaSimpleLedger:
    def apply(): AdaSimpleLedger[THydrozoaHead] = AdaSimpleLedger[THydrozoaHead](NoopVerifier)
    def mkGenesis(address: AddressBechL2, ada: Int): L2Genesis =
        GenesisL2Event(SimpleGenesis(Seq.empty, List(SimpleOutput(address, ada))))
    def mkTransaction(input: OutputRefL2, address: AddressBechL2, ada: Int): L2Transaction =
        TransactionL2Event(
          SimpleTransaction(inputs = List(input), outputs = List(SimpleOutput(address, ada)))
        )
    def mkWithdrawal(utxo: OutputRefL2): L2Withdrawal =
        WithdrawalL2Event(SimpleWithdrawal(List(utxo)))

case class SimpleGenesis(
    // FIXME: these are needed for virtual tx only
    virtualInputs: Seq[OutputRef[L1]],
    outputs: List[SimpleOutput]
)

object SimpleGenesis:
    def apply(ds: DepositUtxos): SimpleGenesis =
        SimpleGenesis(
          ds.map.keySet.toSeq,
          ds.map.values.map(o => SimpleOutput(liftAddress(o.address), o.coins)).toList
        )

// FIXME: implement
def liftAddress(l: AddressBechL1): AddressBechL2 = AddressBechL2.apply(l.bech32)

case class SimpleTransaction(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[OutputRefL2],
    outputs: List[SimpleOutput]
)

case class SimpleWithdrawal(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[OutputRefL2]
)

case class SimpleOutput(
    address: AddressBechL2,
    coins: BigInt
)

type L2Event = AnyL2Event[SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosDiff]

type L2Genesis = GenesisL2Event[SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosDiff]

def mkL2G(simple: SimpleGenesis) =
    GenesisL2Event[SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosDiff](simple)

type L2Transaction =
    TransactionL2Event[SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosDiff]

def mkL2T(simple: SimpleTransaction) =
    TransactionL2Event[SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosDiff](simple)

type L2Withdrawal = WithdrawalL2Event[SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosDiff]

def mkL2W(simple: SimpleWithdrawal) =
    WithdrawalL2Event[SimpleGenesis, SimpleTransaction, SimpleWithdrawal, UtxosDiff](simple)

type L2EventHash = TxId

object NoopVerifier extends Verifier[Any]:
    def isValid(_event: Any) = true
