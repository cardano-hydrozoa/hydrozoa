package hydrozoa.l2.ledger

import hydrozoa.infra.{CryptoHash, encodeHex}
import hydrozoa.l2.ledger.event.*
import hydrozoa.l2.ledger.state.*
import hydrozoa.{AddressBechL2, TxId, TxIx}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class AdaSimpleLedger(verifier: Verifier[L2Event])
    extends L2Ledger[
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

    override def submit[E1 <: L2Event](
        event: E1
    ): Either[(L2EventHash, String), (L2EventHash, event.UtxosDiff)] =
        require(verifier.isValid(event), true)
        event match
            case genesis: L2Genesis       => handleGenesis(genesis)
            case tx: L2Transaction        => handleTransaction(tx)
            case withdrawal: L2Withdrawal => handleWithdrawal(withdrawal)

    private def handleGenesis(event: L2Genesis) =
        val s = s"L2 genesis event: ${event.genesis}"
        val txId = eventHash(s)
        println(s"L2 genesis txId: $txId, content: $s")

        val utxoDiff = event.genesis.utxosAdded.zipWithIndex.map(output =>
            val txIn = mkTxIn(txId, TxIx(output._2))
            val txOut = mkTxOut(output._1.address, output._1.coins)
            (txIn, txOut)
        )
        activeState.addAll(utxoDiff)
        Right((txId, utxoDiff))

    private def handleTransaction(event: L2Transaction) =

        val s = s"L2 simple transaction: ${event.transaction}"
        val txId = eventHash(s)
        println(s"L2 tx txId: $txId, content: $s")

        // Inputs
        val spentRefs = event.transaction.inputs.map(i => mkTxIn(i._1, i._2))
        val extraneousRefs = spentRefs.filterNot(activeState.contains)

        if extraneousRefs.nonEmpty then Left(txId, s"Extraneous inputs in the tx: $extraneousRefs")
        else
            val spentOutputs = spentRefs.map(activeState.get).map(_.get)

            // Outputs
            val newUtxos = event.transaction.outputs.zipWithIndex.map(output =>
                val txIn = mkTxIn(txId, TxIx(output._2))
                val txOut = mkTxOut(output._1.address, output._1.coins)
                (txIn, txOut)
            )

            if !checkSumInvariant(spentOutputs, newUtxos.map(_._2)) then
                Left(txId, s"Sum invariant is not hold for tx $txId")
            else
                // FIXME: atomicity
                spentRefs.foreach(activeState.remove)
                newUtxos.foreach(activeState.put.tupled)

                Right((txId, Set[(TxIn, TxOut)]()))

    private def handleWithdrawal(d: L2Withdrawal) =
        val txIn = mkTxIn(d.withdrawal.utxoRef._1, d.withdrawal.utxoRef._2)
        val s = s"Simple withdrawing: $txIn"
        val txId = eventHash(s)
        println(s"L2 withdraw: $txId, content: $s")
        activeState.remove(txIn) match
            case Some(txOut) => Right(txId, Set((txIn, txOut)))
            case None        => Left(txId, s"Withdrawal utxo not found: $d")

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
