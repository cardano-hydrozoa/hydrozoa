package hydrozoa

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.transitionary.toScalus
import hydrozoa.node.TestPeer.*
import hydrozoa.node.rest.SubmitRequestL2.Transaction
import hydrozoa.node.server.DepositRequest
import hydrozoa.node.{TestPeer, l2EventWithdrawalFromInputsAndPeer}
import hydrozoa.sut.{HydrozoaFacade, LocalFacade}
import munit.FunSuite
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.TransactionHash
import sttp.client4.Response
import sttp.client4.quick.*

import scala.concurrent.duration.Duration

/** This integration test runs simple Hydrozoa happy-path on a Yaci dev net that is reset before
  * each test run to ensure a clean UTxO state.
  *
  * It uses 4 static (i.e., they are the same each time) peers: Alice, Bob, Carol, and Daniella.
  *
  * Alice is the initiator of the head. She starts the head with:
  *   - 100 ADA,
  *   - TxId("6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508"),
  *   - TxIx(0)
  *
  * Then, Alice deposits 100 ADA two times in a row for inclusion in the first major block.
  *
  * The first major block is created by a randomly selected leader.
  *
  * Then, Alice submits a withdrawal for both UTxOs.
  */
class HappyPathSuite extends FunSuite {

    override val munitTimeout = Duration(1, "m")

    private val useYaci = true;

    private val log = Logger(getClass)

    private val testPeers = Set(Alice, Bob, Carol, Daniella)

    private var sut: HydrozoaFacade = _

    // We reset Yaci before each test run in order to have clean UTxO state.
    // Any alternative would be to nonce the protocol/regenerate wallets.
    override def beforeEach(context: BeforeEach): Unit =
        if (useYaci)
            // Reset Yaci DevKit
            log.info("Resetting Yaci...")
            val _: Response[String] = quickRequest
                .post(uri"http://localhost:10000/local-cluster/api/admin/devnet/reset")
                .send()

    sut = LocalFacade.apply(testPeers, useYaci = useYaci, None, None)

    override def afterEach(context: AfterEach): Unit = sut.shutdownSut()

    test("Hydrozoa happy-path scenario") {

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

            // Deposit change from initialization transaction
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

            deposit1Tx = sut.awaitTxL1(deposit1.depositId.transactionId).toRight("Deposit tx is missing")

            deposit2 <- sut.deposit(
              Alice,
              DepositRequest(
                deposit1.depositId.transactionId,
                TxIx(1),
                100_000_000,
                None,
                Address[L2](TestPeer.address(Alice)),
                None,
                Address[L1](TestPeer.address(Alice)),
                None
              )
            )

            deposit2Tx = sut.awaitTxL1(deposit2.depositId.transactionId).toRight("Deposit tx is missing")

            major1 <- sut.produceBlock(false)

            // Note: The `asInstanceOf` will fail if a minor block is produced instead of a major block
            settlement1Tx = sut
                .awaitTxL1(major1._1.l1Effect.asInstanceOf[TxL1].id)
                .toRight("Settlement tx is missing")

            // This gets the state from the local facade, which gets the state from a random node, which
            // gets the state from the node Actor, by querying it's head state (if it exists), looking at the
            // open phase reader, and asking for it's "state L2"
            utxoL2 = sut.stateL2().head

            _ <- sut.submitL2(l2EventWithdrawalFromInputsAndPeer(Set(utxoL2._1), Alice))

            major2 <- sut.produceBlock(nextBlockFinal = true)
            // Note: The `asInstanceOf` will fail if a minor block is produced instead of a major block
            settlement2Tx = sut
                .awaitTxL1(major2._1.l1Effect.asInstanceOf[TxL1].id)
                .toRight("Settlement tx is missing")

            finalBlock <- sut.produceBlock(false)
            finalTx = sut
                .awaitTxL1((finalBlock._1.l1Effect.asInstanceOf[TxL1].id))
                .toRight("Final tx is missing")
        yield (
          deposit1Tx,
          major1,
          settlement1Tx,
          deposit2Tx,
          major2,
          settlement2Tx,
          finalBlock,
          finalTx
        )

        result match
            case Right(_)  => ()
            case Left(err) => fail(err)

    }
}
