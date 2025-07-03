package hydrozoa

import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.spec.Script
import com.typesafe.scalalogging.Logger
import hydrozoa.deploy.mkDeployTx
import hydrozoa.infra.{serializeTxHex, toEither, txHash}
import hydrozoa.l1.rulebased.onchain.{DisputeResolutionScript, TreasuryValidatorScript}
import hydrozoa.l2.ledger.L2Transaction
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import hydrozoa.node.server.DepositRequest
import hydrozoa.sut.{HydrozoaFacade, LocalFacade}
import munit.FunSuite
import sttp.client4.Response
import sttp.client4.quick.*
import sttp.model.MediaType.ApplicationJson

import scala.concurrent.duration.Duration

/** This integration test runs "unhappy" case, when a head switches to rule-based regime and goes
  * throw an onchain dispute.
  */
class DisputeSuite extends FunSuite {

    override val munitTimeout = Duration(1, "m")

    private val useYaci = true;

    private val log = Logger(getClass)

    private val testPeers = Set(Alice, Bob, Carol, Daniella)

    private var sut: HydrozoaFacade = _

    override def beforeEach(context: BeforeEach): Unit =
        def topupNodeWallets(peers: Set[TestPeer], ada: Int, count: Int) =
            assert(count > 1)
            assert(ada >= 1 && ada <= 100)
            val fs: Seq[() => Unit] =
                for
                    p <- peers.toList
                    _ <- List.range(1, count + 1)
                    fs = () =>
                        val addr = account(p).getEnterpriseAddress.toBech32
                        val body = s"{\"address\": \"$addr\", \"adaAmount\": $ada}"
                        log.info(body)
                        // List.range(1, count + 1)
                        //    .foreach(_ =>
                        val _: Response[String] = quickRequest
                            .post(
                              uri"http://localhost:10000/local-cluster/api/addresses/topup"
                            )
                            .contentType(ApplicationJson)
                            .body(body)
                            .send()
                        //    )
                yield fs

            // Yaci gets mad and fails if we run it in parallel.
            // supervised { par(fs) }
            fs.foreach(_())

        def deployHydrozoaScript(
            backendService: BackendService,
            peer: TestPeer,
            script: Script
        ): UtxoIdL1 = {
            val tx = mkDeployTx(backendService, peer, script)
            log.info(s"deployment tx is: ${serializeTxHex(tx)}")
            backendService.getTransactionService.submitTransaction(tx.bytes).toEither match
                case Right(txId) =>
                    log.info(s"$txId")
                    UtxoIdL1.apply(TxId(txId), TxIx(0))
                case Left(err) =>
                    log.error(s"Can't deploy reference scripts: $err")
                    throw RuntimeException()
        }

        val (mbTreasuryScriptRefUtxoId, mbDisputeScriptRefUtxoId) = if (useYaci)
            // Reset Yaci DevKit
            log.info("Resetting Yaci...")
            val _: Response[String] = quickRequest
                .post(uri"http://localhost:10000/local-cluster/api/admin/devnet/reset")
                .send()

            // Topup nodes' wallets - every participant gets 3 utxos with 10 ada each
            log.info("Topping up peers' wallets...")
            topupNodeWallets(testPeers, 10, 3)

            // Deploy reference scripts
            val backendService = BFBackendService("http://localhost:8080/api/v1/", "")

            val treasuryScriptRefUtxoId =
                deployHydrozoaScript(backendService, Julia, TreasuryValidatorScript.plutusScript)
            val disputeScriptRefUtxoId =
                deployHydrozoaScript(backendService, Isabel, DisputeResolutionScript.plutusScript)

            (Some(treasuryScriptRefUtxoId), Some(disputeScriptRefUtxoId))
        else (None, None)

        // Make SUT
        log.info("Making a Hydrozoa head uing a local network...")
        sut = LocalFacade.apply(
          testPeers,
          useYaci = useYaci,
          mbTreasuryScriptRefUtxoId,
          mbDisputeScriptRefUtxoId
        )

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
            major1SettlementTx = sut
                .awaitTxL1(txHash(major1._1.l1Effect.asInstanceOf[TxL1]))
                .toRight("No settlement tx for th major 1")

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
            minor1_3 <- sut.produceBlock(false)

            // Another L2 tx + minor block 1.4
            utxoL2 = sut.stateL2().head
            _ <- sut.submitL2(
              L2Transaction(
                List(utxoL2._1),
                List(
                  OutputNoTokens(
                    AddressBech[L2](
                      "addr_test1qp0qu4cypvrwn4c7pu50zf3x9qu2drdsk545l5dnsa7a5gsr6htafuvutm36rm23hdnsw7w7r82q4tljuh55drxqt30q6vm8vs"
                    ),
                    utxoL2._2.coins,
                    None
                  )
                )
              )
            )
            minor1_4 <- sut.produceBlock(false, true)

            _ = Thread.sleep(5000)
        yield (major1, major1SettlementTx, minor1_1, minor1_2, minor1_3, minor1_4)

        result match
            case Right(_)  => ()
            case Left(err) => fail(err)

    }
}
