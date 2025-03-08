package hydrozoa.l2.ledger

import hydrozoa.infra.{CryptoHash, encodeHex}
import hydrozoa.l2.ledger.event.*
import hydrozoa.l2.ledger.state.{UTxOs, checkSumInvariant, mkTxIn, mkTxOut}
import hydrozoa.{AddressBechL2, TxId, TxIx}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class AdaSimpleLedger(verifier: Verifier[L2Event])
    extends L2Ledger[UTxOs, L2Event, L2EventHash, Verifier[L2Event]]:
    val activeState: UTxOs = mutable.Map.empty

    def submit(event: L2Event): L2EventHash =
        require(verifier.isValid(event), true)
        event match
            case genesis: L2Genesis       => handleGenesis(genesis)
            case tx: L2Transaction        => handleTransaction(tx)
            case withdrawal: L2Withdrawal => handleWithdrawal(withdrawal)

    private def handleGenesis(event: L2Genesis): L2EventHash =
        val s = s"L2 genesis event: ${event.genesis}"
        println(s)
        val txId = eventHash(s)

        // TODO: atomicity
        event.genesis.utxosAdded.zipWithIndex.foreach(output =>
            val txIn = mkTxIn(txId, TxIx(output._2))
            val txOut = mkTxOut(output._1.address, output._1.amount)
            activeState.put(txIn, txOut)
        )
        txId

    private def handleTransaction(event: L2Transaction): L2EventHash =
        
        val s = s"L2 transaction event: ${event.transaction}"
        println(s)
        val txId = eventHash(s)

        // Inputs
        val spentRefs = event.transaction.inputs.map(i => mkTxIn(i._1, i._2))
        val extraneousRefs = spentRefs.filterNot(activeState.contains)

        if extraneousRefs.nonEmpty then
            throw IllegalArgumentException(s"Extraneous inputs in the tx: $extraneousRefs")
        
        val spentOutputs = spentRefs.map(activeState.get).map(_.get)
        
        // Outputs
        val newUtxos = event.transaction.outputs.zipWithIndex.map(output =>
            val txIn = mkTxIn(txId, TxIx(output._2))
            val txOut = mkTxOut(output._1.address, output._1.amount)
            (txIn, txOut)
        )

        if !checkSumInvariant(spentOutputs, newUtxos.map(_._2)) then
            throw IllegalArgumentException(s"Sum invariant is not hold for tx $txId")    
        
        // FIXME: atomicity
        spentRefs.foreach(activeState.remove)
        newUtxos.foreach(activeState.put.tupled)

        txId

    private def handleWithdrawal(d: L2Withdrawal): L2EventHash =
        val txIn = mkTxIn(d.withdrawal.utxoRef._1, d.withdrawal.utxoRef._2)
        activeState.remove(txIn) match
            case Some(txOut) =>
                val s = s"L2 withdraw event: $txOut"
                val txId = eventHash(s)
                println(s"$s, withdrawal id: $txId")
                txId
            case None => throw IllegalArgumentException(s"Withdrawal utxo not found: $d")

    private def eventHash(s: String): TxId =
        TxId(encodeHex(CryptoHash.H32.hash(IArray.from(s.getBytes)).bytes))

    def event(hash: L2EventHash): Option[L2Event] = ???

    def allEvents: Set[L2EventHash] = ???

    def isEmpty: Boolean = activeState.isEmpty

object AdaSimpleLedger:
    def mkGenesis(address: AddressBechL2, ada: Int): L2Genesis =
        GenesisL2Event(SimpleGenesis(Set(SimpleUtxo(address, ada))))
    def mkTransaction(input: (TxId, TxIx), address: AddressBechL2, ada: Int): L2Transaction =
        TransactionL2Event(
          SimpleTransaction(inputs = Set(input), outputs = Set(SimpleUtxo(address, ada)))
        )
    def mkWithdrawal(utxo: (TxId, TxIx)): L2Withdrawal =
        WithdrawalL2Event(SimpleWithdrawal(utxo))

case class SimpleGenesis(
    utxosAdded: Set[SimpleUtxo]
)

case class SimpleTransaction(
    inputs: Set[(TxId, TxIx)],
    outputs: Set[SimpleUtxo]
)

case class SimpleWithdrawal(
    utxoRef: (TxId, TxIx) // FIXME: multiple
)

case class SimpleUtxo(
    address: AddressBechL2,
    amount: Int
)

type L2Event = AnyL2Event[SimpleGenesis, SimpleTransaction, SimpleWithdrawal]
type L2Genesis = GenesisL2Event[SimpleGenesis, SimpleTransaction, SimpleWithdrawal]
type L2Transaction = TransactionL2Event[SimpleGenesis, SimpleTransaction, SimpleWithdrawal]
type L2Withdrawal = WithdrawalL2Event[SimpleGenesis, SimpleTransaction, SimpleWithdrawal]
type L2EventHash = TxId

object NoopVerifier extends Verifier[Any]:
    def isValid(_event: Any) = true
