package hydrozoa

import hydrozoa.infra.txHash
import hydrozoa.l2.ledger.L2Transaction
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import hydrozoa.node.server.DepositRequest
import hydrozoa.sut.{HydrozoaFacade, LocalFacade}
import munit.FunSuite
import scala.concurrent.duration.*

/** This integration test runs "unhappy" case, when a head switches to rule-based regime and goes
  * throw an onchain dispute.
  */
class DisputeSuite extends FunSuite {

    private val testPeers = Set(Alice, Bob, Carol, Daniella)

    private var sut: HydrozoaFacade = _

    override def beforeEach(context: BeforeEach): Unit = sut =
        LocalFacade.apply(testPeers, useYaci = false)

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

            // 2. Make a deposit
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

            // 2. Make another deposit
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

            // Make a major block
            major1 <- sut.produceBlock(false)
            _ = sut.awaitTxL1(txHash(major1._1.l1Effect.asInstanceOf[TxL1]))

            // L2 tx + minor block 1.1
            utxoL2 = sut.stateL2().head
            _ <- sut.submitL2(
              L2Transaction(
                List(utxoL2._1),
                List(
                  OutputNoTokens(
                    AddressBech[L2](
                      "addr_test1qrh3nrahcd0pj6ps3g9htnlw2jjxuylgdhfn2s5rxqyrr43yzewr2766qsfeq6stl65t546cwvclpqm2rpkkxtksgxuq90xn5f"
                    ),
                    utxoL2._2.coins,
                    None
                  )
                )
              )
            )
            minor1_1 <- sut.produceBlock(false)

            // Another L2 tx + minor block 1.2
            utxoL2 = sut.stateL2().head
            _ <- sut.submitL2(
              L2Transaction(
                List(utxoL2._1),
                List(
                  OutputNoTokens(
                    AddressBech[L2](
                      "addr_test1qrzufj3g0ua489yt235wtc3mrjrlucww2tqdnt7kt5rs09grsag6vxw5v053atks5a6whke03cf2qx3h3g2nhsmzwv3sgml3ed"
                    ),
                    utxoL2._2.coins,
                    None
                  )
                )
              )
            )
            minor1_2 <- sut.produceBlock(false)

            // Another L2 tx + minor block 1.3
            utxoL2 = sut.stateL2().head
            _ <- sut.submitL2(
              L2Transaction(
                List(utxoL2._1),
                List(
                  OutputNoTokens(
                    AddressBech[L2](
                      "addr_test1qqm87edtdxc7vu2u34dpf9jzzny4qhk3wqezv6ejpx3vgrwt46dz4zq7vqll88fkaxrm4nac0m5cq50jytzlu0hax5xqwlraql"
                    ),
                    utxoL2._2.coins,
                    None
                  )
                )
              )
            )
            minor1_2 <- sut.produceBlock(false, true)

            _ = Thread.sleep(5000)
        yield major1

        result match
            case Right(_)  => ()
            case Left(err) => fail(err)

    }
}
