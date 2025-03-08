package hydrozoa.l2.ledger

import hydrozoa.infra.CryptoHash.H32
import hydrozoa.infra.encodeHex
import hydrozoa.{AddressBechL2, TxId, TxIx}

def mkLedger: AdaSimpleLedger = {
    AdaSimpleLedger(NoopVerifier)
}

val address = AddressBechL2(
  "addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex"
)

def doSampleGenesis(ledger: AdaSimpleLedger): L2EventHash = {
    val event = AdaSimpleLedger.mkGenesis(address, 100)
    val hash = ledger.submit(event)
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
        val event = AdaSimpleLedger.mkWithdrawal((genesis, ix))
        ledger.submit(event)
        assert(ledger.isEmpty)
    }

    test("non-existent utxo can't be withdrawn") {
        val ledger = mkLedger

        val txId = TxId(encodeHex(H32.hash(IArray.empty).bytes))
        val ix = TxIx(0)

        val event = AdaSimpleLedger.mkWithdrawal((txId, ix))

        intercept[java.lang.IllegalArgumentException] {
            ledger.submit(event)
        }
    }
}
