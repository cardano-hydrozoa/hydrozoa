//package hydrozoa
//
//import com.typesafe.scalalogging.Logger
//import hydrozoa.infra.txHash
//import hydrozoa.l2.ledger.L2EventWithdrawal
//import hydrozoa.node.TestPeer
//import hydrozoa.node.TestPeer.*
//import hydrozoa.node.server.DepositRequest
//import hydrozoa.sut.{HydrozoaFacade, LocalFacade}
//import munit.FunSuite
//import sttp.client4.Response
//import sttp.client4.quick.*
//
//import scala.concurrent.duration.Duration
//
///** This integration test runs simple Hydrozoa happy-path.
//  */
//class HappyPathSuite extends FunSuite {
//
//    override val munitTimeout = Duration(1, "m")
//
//    private val useYaci = true;
//
//    private val log = Logger(getClass)
//
//    private val testPeers = Set(Alice, Bob, Carol, Daniella)
//
//    private var sut: HydrozoaFacade = _
//
//    override def beforeEach(context: BeforeEach): Unit =
//        if (useYaci)
//            // Reset Yaci DevKit
//            log.info("Resetting Yaci...")
//            val _: Response[String] = quickRequest
//                .post(uri"http://localhost:10000/local-cluster/api/admin/devnet/reset")
//                .send()
//
//    sut = LocalFacade.apply(testPeers, useYaci = useYaci, None, None)
//
//    override def afterEach(context: AfterEach): Unit = sut.shutdownSut()
//
//    test("Hydrozoa happy-path scenario") {
//
//        val result = for
//
//            // 1. Initialize the head
//            initTxId <- sut.initializeHead(
//              Alice,
//              testPeers.-(Alice).map(TestPeer.mkWalletId),
//              100,
//              TxId("6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508"),
//              TxIx(0)
//            )
//
//            _ = sut.awaitTxL1(initTxId)
//
//            // Deposit change from initialization transaction
//            deposit1 <- sut.deposit(
//              Alice,
//              DepositRequest(
//                initTxId,
//                TxIx(1),
//                100_000_000,
//                None,
//                AddressBech[L2](
//                  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
//                ),
//                None,
//                AddressBech[L1](
//                  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
//                ),
//                None
//              )
//            )
//
//            deposit1Tx = sut.awaitTxL1(deposit1.depositId.txId).toRight("Deposit tx is missing")
//
//            deposit2 <- sut.deposit(
//              Alice,
//              DepositRequest(
//                deposit1.depositId.txId,
//                TxIx(1),
//                100_000_000,
//                None,
//                AddressBech[L2](
//                  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
//                ),
//                None,
//                AddressBech[L1](
//                  "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
//                ),
//                None
//              )
//            )
//
//            deposit2Tx = sut.awaitTxL1(deposit2.depositId.txId).toRight("Deposit tx is missing")
//
//            major1 <- sut.produceBlock(false)
//
//            settlement1Tx = sut
//                .awaitTxL1(txHash(major1._1.l1Effect.asInstanceOf[TxL1]))
//                .toRight("Settlement tx is missing")
//
//            utxoL2 = sut.stateL2().head
//
//            _ <- sut.submitL2(L2EventWithdrawal(List(utxoL2._1)))
//
//            major2 <- sut.produceBlock(nextBlockFinal = true)
//
//            settlement2Tx = sut
//                .awaitTxL1(txHash(major2._1.l1Effect.asInstanceOf[TxL1]))
//                .toRight("Settlement tx is missing")
//
//            finalBlock <- sut.produceBlock(false)
//
//            finalTx = sut
//                .awaitTxL1(txHash(finalBlock._1.l1Effect.asInstanceOf[TxL1]))
//                .toRight("Final tx is missing")
//        yield (
//          deposit1Tx,
//          major1,
//          settlement1Tx,
//          deposit2Tx,
//          major2,
//          settlement2Tx,
//          finalBlock,
//          finalTx
//        )
//
//        result match
//            case Right(_)  => ()
//            case Left(err) => fail(err)
//
//    }
//}
