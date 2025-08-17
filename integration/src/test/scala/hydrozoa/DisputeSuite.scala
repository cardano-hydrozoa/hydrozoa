package hydrozoa

import scala.language.implicitConversions
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.spec.Script
import com.typesafe.scalalogging.Logger
import hydrozoa.deploy.mkDeployTx
import hydrozoa.infra.transitionary.toScalus
import hydrozoa.infra.{encodeHex, serializeTxHex, toEither}
import hydrozoa.l1.rulebased.onchain.{DisputeResolutionScript, TreasuryValidatorScript}
import hydrozoa.l2.ledger.L2EventTransaction
import hydrozoa.node.{TestPeer, l2EventTransactionFromInputsAndPeer}
import hydrozoa.node.TestPeer.*
import hydrozoa.node.server.DepositRequest
import hydrozoa.node.state.L1BlockEffect.SettlementTxEffect
import hydrozoa.sut.{HydrozoaFacade, LocalFacade}
import munit.FunSuite
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.TransactionHash
import sttp.client4.Response
import sttp.client4.quick.*
import sttp.model.MediaType.ApplicationJson

import scala.concurrent.duration.Duration

/** This integration test runs "unhappy" case, when a head switches to rule-based regime and goes
  * throw an onchain dispute.
  */
class DisputeSuite extends FunSuite {

    override val munitTimeout = Duration(2, "m")

    private val useYaci = true;

    private val log = Logger(getClass)

    private val testPeers = Set(Alice, Bob, Carol, Daniella)

    private var sut: HydrozoaFacade = _

    override def beforeEach(context: BeforeEach): Unit =
        def topUpNodeWallets(peers: Set[TestPeer], ada: Int, count: Int) =
            assert(count >= 1)
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
            log.info(
              s"deployment tx for script hash ${encodeHex(IArray.unsafeFromArray(script.getScriptHash))} is: ${serializeTxHex(tx)}"
            )
            backendService.getTransactionService.submitTransaction(tx.toCbor).toEither match
                case Right(txId) =>
                    log.info(s"$txId")
                    UtxoId[L1](TransactionHash.fromHex(txId), TxIx(0))
                case Left(err) =>
                    log.error(s"Can't deploy reference scripts: $err")
                    throw RuntimeException(err)
        }

        val (mbTreasuryScriptRefUtxoId, mbDisputeScriptRefUtxoId) = if (useYaci)
            // Reset Yaci DevKit
            log.info("Resetting Yaci...")
            val _: Response[String] = quickRequest
                .post(uri"http://localhost:10000/local-cluster/api/admin/devnet/reset")
                .send()

            // Top up nodes' wallets - every participant gets 3 utxos with 10 ada each
            log.info("Topping up peers' wallets...")
            topUpNodeWallets(testPeers, 50, 1)

            println(TreasuryValidatorScript.scriptHashString)
            println(TreasuryValidatorScript.scriptHash)

            // Deploy reference scripts
            val backendService = BFBackendService("http://localhost:8080/api/v1/", "")

            val treasuryScriptRefUtxoId =
                deployHydrozoaScript(
                  backendService,
                  Julia,
                  TreasuryValidatorScript.plutusScript
                )

            val disputeScriptRefUtxoId =
                deployHydrozoaScript(
                  backendService,
                  Isabel,
                  DisputeResolutionScript.plutusScript
                )

            (Some(treasuryScriptRefUtxoId), Some(disputeScriptRefUtxoId))
        else (None, None)

        log.info(
          s"mbTreasuryScriptRefUtxoId=$mbTreasuryScriptRefUtxoId, mbDisputeScriptRefUtxoId=$mbDisputeScriptRefUtxoId"
        )

        // Make SUT
        log.info("Making a Hydrozoa head using a local network...")
        sut = LocalFacade.apply(
          testPeers,
          useYaci = useYaci,
          mbTreasuryScriptRefUtxoId,
          mbDisputeScriptRefUtxoId
        )

    override def afterEach(context: AfterEach): Unit = sut.shutdownSut()

    test("Hydrozoa dispute scenario") {
        val result = for

            // 1. Initialize the head
            initTxId <- sut.initializeHead(
              Alice,
              testPeers.-(Alice).map(TestPeer.mkWalletId),
              100,
              TransactionHash.fromHex("6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508"),
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
                Address[L2](TestPeer.address(Alice)),
                None,
                Address[L1](TestPeer.address(Alice)),
                None
              )
            )
            _ = sut.awaitTxL1(deposit1.depositId.transactionId)

            // 2. Make another deposit
            deposit2 <- sut.deposit(
              Alice,
              DepositRequest(
                deposit1.depositId.transactionId,
                TxIx(1),
                100_000_000,
                None,
                Address[L2](TestPeer.address(Alice)),
                None,
                Address[L1](
                  TestPeer.address(Alice)
                ),
                None
              )
            )
            _ = sut.awaitTxL1(deposit2.depositId.transactionId)

            // Make a major block
            major1 <- sut.produceBlock(false)
            major1SettlementTx = sut
                .awaitTxL1(major1._1.l1Effect.asInstanceOf[SettlementTxEffect].effect.untagged.id)
                .toRight("No settlement tx for th major 1")

            // L2 tx + minor block 1.1
            utxoL2 = sut.stateL2().head

            _ <- sut.submitL2(
              l2EventTransactionFromInputsAndPeer(
                inputs = Set(utxoL2._1),
                utxoSet = sut.stateL2().toMap,
                inPeer = Alice,
                outPeer = Bob
              )
            )

            minor1_1 <- sut.produceBlock(false)

            // Another L2 tx + minor block 1.2
            utxoL2: (UtxoId[L2], OutputL2) = sut.stateL2().head
            _ <- sut.submitL2(
              l2EventTransactionFromInputsAndPeer(
                inputs = Set(utxoL2._1),
                utxoSet = sut.stateL2().toMap,
                inPeer = Alice,
                outPeer = Carol
              )
            )
            minor1_2 <- sut.produceBlock(false)

            // Another L2 tx + minor block 1.3
            utxoL2 = sut.stateL2().head
            _ <- sut.submitL2(
              l2EventTransactionFromInputsAndPeer(
                inputs = Set(utxoL2._1),
                utxoSet = sut.stateL2().toMap,
                inPeer = Alice,
                outPeer = Daniella
              )
            )
            minor1_3 <- sut.produceBlock(false)

            // Another L2 tx + minor block 1.4
            utxoL2 = sut.stateL2().head
            _ <- sut.submitL2(
              l2EventTransactionFromInputsAndPeer(
                inputs = Set(utxoL2._1),
                utxoSet = sut.stateL2().toMap,
                inPeer = Alice,
                outPeer = Erin
              )
            )
            minor1_4 <- sut.produceBlock(false)

            _ = sut.runDispute()

            _ = Thread.sleep(5000)
        yield (major1, major1SettlementTx, minor1_1, minor1_2, minor1_3, minor1_4)

        result match
            case Right(_)  => ()
            case Left(err) => fail(err)

    }
}
