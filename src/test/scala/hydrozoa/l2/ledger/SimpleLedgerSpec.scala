package hydrozoa.l2.ledger

import hydrozoa.AddressBechL2
import hydrozoa.l2.ledger.event.DepositEvent

class SimpleLedgerSpec extends munit.ScalaCheckSuite {

    test("init empty ledger") {
        val ledger = AdaSimpleLedger()
        assert(ledger.isEmpty)
    }

    test("deposit") {
        val ledger = AdaSimpleLedger()

        val address = AddressBechL2(
          "addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex"
        )

        val deposit: L2Event = DepositEvent(SimpleDeposit(address, 100))
        println(s"Deposit event is: $deposit")

        val hash = ledger.submit(deposit)
        println(s"Deposit hash is: $hash")
        print(ledger.activeState)

        assert(!ledger.isEmpty)
    }

}
