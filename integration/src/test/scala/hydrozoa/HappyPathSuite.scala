package hydrozoa

import hydrozoa.infra.txHash
import hydrozoa.l2.ledger.L2Withdrawal
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import hydrozoa.node.server.DepositRequest
import hydrozoa.sut.{HydrozoaFacade, LocalFacade}
import munit.FunSuite

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
              testPeers.-(Alice).map(TestPeer.mkWalletId),
              100,
              TxId("6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508"),
              TxIx(0)
            )

             _ = sut.awaitTxL1(initTxId)

            deposit1 <- sut.deposit(
              Alice,
              DepositRequest(
                initTxId,
                TxIx(1),
                100_000_000,
                None,
                AddressBech[L2](
                  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
                ),
                None,
                AddressBech[L1](
                  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
                ),
                None
              )
            )

             _ = sut.awaitTxL1(deposit1.depositId.txId)

            deposit2 <- sut.deposit(
              Alice,
              DepositRequest(
                deposit1.depositId.txId,
                TxIx(1),
                100_000_000,
                None,
                AddressBech[L2](
                  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
                ),
                None,
                AddressBech[L1](
                  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
                ),
                None
              )
            )

            _ = sut.awaitTxL1(deposit2.depositId.txId)

            major1 <- sut.produceBlock(false)

             _ = sut.awaitTxL1(txHash(major1._1.l1Effect.asInstanceOf[TxL1]))

            utxoL2 = sut.stateL2().head

            _ <- sut.submitL2(L2Withdrawal(List(utxoL2._1)))

            major2 <- sut.produceBlock(nextBlockFinal = true)

             _ = sut.awaitTxL1(txHash(major2._1.l1Effect.asInstanceOf[TxL1]))

            finalBlock <- sut.produceBlock(false)

             _ = sut.awaitTxL1(txHash(finalBlock._1.l1Effect.asInstanceOf[TxL1]))

        yield finalBlock

        result match
            case Right(_)  => ()
            case Left(err) => fail(err)

    }
}
