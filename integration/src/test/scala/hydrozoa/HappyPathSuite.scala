package hydrozoa

import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import hydrozoa.node.server.DepositRequest
import hydrozoa.sut.{HydrozoaFacade, LocalFacade}
import munit.FunSuite

import java.lang.Thread.sleep

/** This integration test runs simple Hydrozoa happy-path.
  */
class HappyPathSuite extends FunSuite {

    private val testPeers = Set(Alice, Bob, Carol, Daniella)

    private var sut: HydrozoaFacade = _

    override def beforeEach(context: BeforeEach): Unit = sut = LocalFacade.apply(testPeers)

    override def afterEach(context: AfterEach): Unit = sut.shutdownSut()

    test("Hydrozoa happy-path scenario") {

        val result = for

            // 1. Initialize the head
            initTxId <- sut.initializeHead(
              Alice,
              100,
              TxId("6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508"),
              TxIx(0)
            )

            // _ = cardano.awaitTx(initTxId)
            _ = sleep(3_000)

            deposit1 <- sut.deposit(
              Alice,
              DepositRequest(
                initTxId,
                TxIx(1),
                100_000_000,
                None,
                AddressBechL2(
                  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
                ),
                None,
                AddressBechL1(
                  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
                ),
                None
              )
            )

            // _ = cardano.awaitTx(deposit1.depositId.txId)
            _ = sleep(3_000)

            deposit2 <- sut.deposit(
              Alice,
              DepositRequest(
                deposit1.depositId.txId,
                TxIx(1),
                100_000_000,
                None,
                AddressBechL2(
                  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
                ),
                None,
                AddressBechL1(
                  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
                ),
                None
              )
            )

            // _ = cardano.awaitTx(deposit2.depositId.txId)
            _ = sleep(3_000)

        // major1 <- sut.produceBlock(false)
        // _ = cardano.awaitTx(txHash(major1._1.l1Effect.asInstanceOf[TxL1]))

        // utxoL2 = major1._2.head

        // _ <- node.submitL2(Withdrawal(SimpleWithdrawal(utxoL2._1)))

        // major2 <- node.produceBlock(true)
        // _ = cardano.awaitTx(txHash(major2._1.l1Effect.asInstanceOf[TxL1]))

        // finalBlock <- node.produceBlock(false)
        // _ = cardano.awaitTx(txHash(finalBlock._1.l1Effect.asInstanceOf[TxL1]))
        yield ()

        result match
            case Right(_)  => ()
            case Left(err) => fail(err)

    }
}
