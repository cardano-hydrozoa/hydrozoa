package hydrozoa

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.txHash
import hydrozoa.l1.CardanoL1
import hydrozoa.l2.ledger.L2Withdrawal
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import hydrozoa.node.rest.SubmitRequestL2.Withdrawal
import hydrozoa.node.server.{DepositRequest, Node}
import munit.FunSuite

/** This integration test runs simple Hydrozoa happy-path.
  */
class HappyPathSuite extends FunSuite {

    private val knownPeers = Set(Bob, Carol, Daniella)
    private val headPeers = knownPeers.take(2)

    private val (log: Logger, node: Node, cardano: CardanoL1) = mkSimpleHydrozoaNode(
      ownPeerWallet = mkWallet(Alice),
      knownPeers = knownPeers.map(mkWallet),
      useL1Mock = true,
      pp = Some(Utils.protocolParams)
    )

    override def beforeAll(): Unit = {
        assume(true, "Env is presumably set up and reset.")
    }

    test("Hydrozoa happy-path scenario") {
        val result = for

            // 1. Initialize the head
            initTxId <- node.initializeHead(
              // headPeers.map(mkWalletId),
              100,
              TxId("6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508"),
              TxIx(0)
            )

            _ = cardano.awaitTx(initTxId)

            deposit1 <- node.deposit(
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

            _ = cardano.awaitTx(deposit1.depositId.txId)

            deposit2 <- node.deposit(
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

            _ = cardano.awaitTx(deposit2.depositId.txId)

            major1 <- node.handleNextBlock(false)
            _ = cardano.awaitTx(txHash(major1._1.l1Effect.asInstanceOf[TxL1]))

            utxoL2 = major1._2.utxoMap.toList.head

            _ <- node.submitL2(Withdrawal(L2Withdrawal(List(utxoL2._1))))

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
