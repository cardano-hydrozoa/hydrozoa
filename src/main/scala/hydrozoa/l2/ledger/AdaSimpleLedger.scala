package hydrozoa.l2.ledger

import hydrozoa.infra.{CryptoHash, encodeHex}
import hydrozoa.l2.ledger.event.*
import hydrozoa.l2.ledger.state.{UTxOs, mkTxIn, mkTxOut}
import hydrozoa.{AddressBechL2, TxId, TxIx}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class AdaSimpleLedger extends L2Ledger[UTxOs, L2Event, L2EventHash, Verifier[L2Event]]:
    val activeState: UTxOs = mutable.Map.empty

    def submit(event: L2Event): L2EventHash =
        require(NoopVerifier.isValid(event), true)
        event match
            case deposit: L2Deposit       => handleDeposit(deposit)
            case tx: L2Transaction        => ???
            case withdrawal: L2Withdrawal => ???

    private def handleDeposit(d: L2Deposit): L2EventHash = {
        val s = s"Depositing ${d.deposit.amount} ADA to ${d.deposit.address}"
        println(s)
        val hash = CryptoHash.H32.hash(IArray.from(s.getBytes))
        val txId = TxId(encodeHex(hash.bytes))
        val txIn = mkTxIn(txId, TxIx(0))
        val txOut = mkTxOut(d.deposit.address, d.deposit.amount)
        activeState.put(txIn, txOut)
        txId
    }

    def event(hash: L2EventHash): Option[L2Event] = ???

    def allEvents: Set[L2EventHash] = ???

    def isEmpty: Boolean = activeState.isEmpty

object AdaSimpleLedger:
    def mkDeposit(address: AddressBechL2, ada: Int): L2Deposit =
        DepositEvent(SimpleDeposit(address, ada))
    def mkTransaction(from: AddressBechL2, to: AddressBechL2, ada: Int): L2Transaction =
        TransactionEvent(SimpleTransaction(from, to, ada))
    def mkWithdrawals(utxo: (TxId, TxIx)): L2Withdrawal =
        WithdrawalEvent(SimpleWithdrawal(utxo))

case class SimpleDeposit(
    address: AddressBechL2,
    amount: Int
)

case class SimpleTransaction(
    from: AddressBechL2,
    to: AddressBechL2,
    ada: Int
)

case class SimpleWithdrawal(
    utxo: (TxId, TxIx)
)

type L2Event = Event[SimpleDeposit, SimpleTransaction, SimpleWithdrawal]
type L2Deposit = DepositEvent[SimpleDeposit, SimpleTransaction, SimpleWithdrawal]
type L2Transaction = TransactionEvent[SimpleDeposit, SimpleTransaction, SimpleWithdrawal]
type L2Withdrawal = WithdrawalEvent[SimpleDeposit, SimpleTransaction, SimpleWithdrawal]
type L2EventHash = TxId

object NoopVerifier extends Verifier[Any]:
    def isValid(_event: Any) = true
