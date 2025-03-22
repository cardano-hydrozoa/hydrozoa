package hydrozoa

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.txHash
import hydrozoa.l1.CardanoL1
import hydrozoa.l2.ledger.SimpleWithdrawal
import hydrozoa.mkDefaultHydrozoaNode
import hydrozoa.node.rest.SubmitRequestL2.Withdrawal
import hydrozoa.node.server.{DepositRequest, Node}
import munit.FunSuite

/** This integration test runs simple Hydrozoa happy-path.
  */
class HappyPathSuite extends FunSuite {

    private val (log: Logger, node: Node, cardano: CardanoL1) = mkDefaultHydrozoaNode

    override def beforeAll(): Unit = {
//        val params = Try(node...)
//        assume(
//            params.isSuccess,
//            "This test requires a Blockfrost API available. Start Yaci Devkit before running this test."
//        )
        assume(true, "Env is presumably set up and reset.")
    }

    test("Hydrozoa happy-path scenario") {
        val result = for
            initTxId <- node.initializeHead(
              100,
              TxId("6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508"),
              TxIx(0)
            )

            _ = cardano.awaitTx(initTxId)

            deposit1 <- node.deposit(
              DepositRequest(
                initTxId,
                TxIx(1),
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

            _ = cardano.awaitTx(deposit1.depositId.txId)

            deposit2 <- node.deposit(
              DepositRequest(
                deposit1.depositId.txId,
                TxIx(1),
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

            _ = cardano.awaitTx(deposit2.depositId.txId)

            major1 <- node.handleNextBlock(false)
            _ = cardano.awaitTx(txHash(major1._1.l1Effect.asInstanceOf[TxL1]))

            utxoL2 = major1._2.head

            _ <- node.submitL2(Withdrawal(SimpleWithdrawal(utxoL2._1)))

            major2 <- node.handleNextBlock(true)
            _ = cardano.awaitTx(txHash(major2._1.l1Effect.asInstanceOf[TxL1]))

            finalBlock <- node.handleNextBlock(false)
            _ = cardano.awaitTx(txHash(finalBlock._1.l1Effect.asInstanceOf[TxL1]))

        yield ()

        result match
            case Right(_)  => ()
            case Left(err) => fail(err)
    }
}
