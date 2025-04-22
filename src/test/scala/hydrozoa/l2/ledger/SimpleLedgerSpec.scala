package hydrozoa.l2.ledger

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import hydrozoa.infra.CryptoHash.H32
import hydrozoa.infra.encodeHex
import hydrozoa.node.rest.{SubmitRequestL2, given}
import hydrozoa.{AddressBechL2, OutputRefL2, TxId, TxIx}

def mkLedger: AdaSimpleLedger[THydrozoaHead] = {
    AdaSimpleLedger()
}

val address = AddressBechL2(
  "addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex"
)

val address2 = AddressBechL2(
  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
)

def doSampleGenesis(ledger: AdaSimpleLedger[THydrozoaHead]): L2EventHash = {
    val event = SimpleGenesis(address, 100)
    val Right(hash, _) = ledger.submit(AdaSimpleLedger.mkGenesisEvent(event))
    println(s"Genesis $hash submitted: $event")
    println(ledger.activeState)
    hash
}

class SimpleLedgerSpec extends munit.ScalaCheckSuite {

    test("init empty ledger") {
        val ledger = mkLedger
        assert(ledger.isEmpty)
    }

    test("genesis event") {
        val ledger = mkLedger
        doSampleGenesis(ledger)
        assert(!ledger.isEmpty)
    }

    test("withdraw genesis utxo") {
        val ledger = mkLedger
        val genesis = doSampleGenesis(ledger)
        val ix = TxIx(0)

        val withdrawal = SimpleWithdrawal(OutputRefL2(genesis, ix))
        println(writeToString(SubmitRequestL2.Withdrawal(withdrawal)))
        ledger.submit(AdaSimpleLedger.mkWithdrawalEvent(withdrawal))
        assert(ledger.isEmpty)
    }

    test("non-existent utxo can't be withdrawn") {
        val ledger = mkLedger

        val txId = TxId(encodeHex(H32.hash(IArray.empty).bytes))
        val ix = TxIx(0)

        val withdrawal = SimpleWithdrawal(OutputRefL2(txId, ix))
        val Left(_) = ledger.submit(AdaSimpleLedger.mkWithdrawalEvent(withdrawal))
    }

    test("correct transaction") {
        val ledger = mkLedger

        val txId = doSampleGenesis(ledger)
        val ix = TxIx(0)

        val transaction = SimpleTransaction(OutputRefL2(txId, ix), address2, 100)

        println(writeToString(SubmitRequestL2.Transaction(transaction)))

        ledger.submit(AdaSimpleLedger.mkTransactionEvent(transaction))
        println(ledger.activeState)
    }

    test("transaction sum invariant violation") {
        val ledger = mkLedger

        val txId = doSampleGenesis(ledger)
        val ix = TxIx(0)

        val transaction = SimpleTransaction(OutputRefL2(txId, ix), address2, 101)
        val Left(_) = ledger.submit(AdaSimpleLedger.mkTransactionEvent(transaction))
    }

}
