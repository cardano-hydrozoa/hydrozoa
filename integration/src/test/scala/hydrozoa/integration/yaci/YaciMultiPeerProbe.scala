package hydrozoa.integration.yaci

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport
import hydrozoa.integration.harness.{MultiPeerDisputeProperties, MultiPeerHeadHarness}
import hydrozoa.multisig.backend.cardano.CardanoBackend as L1Backend
import hydrozoa.multisig.consensus.RequestSequencer
import org.scalacheck.Prop.propBoolean
import org.scalacheck.PropertyM
import scala.concurrent.duration.*
import scalus.cardano.ledger.TransactionHash
import test.{TestM, TestMFixedEnv}

/** Brings up a multi-peer head against a real Yaci devnet end-to-end: [[YaciSetup.prepare]] funds
  * peers + deploys the script references, [[MultiPeerHeadHarness.genDisputeMnc]] generates the
  * `MultiNodeConfig` from those real inputs, and the harness runs with `CardanoBackend.Mode.Yaci`.
  * The scenario submits a kick request and asserts the head's initialization tx lands on L1.
  *
  * Requires Docker; excluded from the default test run (see build.sbt).
  */
object YaciMultiPeerProbe extends MultiPeerDisputeProperties("Yaci MultiPeer"):

    private val nHeadPeers: Int = 2

    private final case class Ctx(
        harness: MultiPeerHeadHarness.Harness[Option[RequestSequencer.Handle]],
        initTxId: TransactionHash,
    )

    private val ctxTestM = TestMFixedEnv[Ctx]()
    import ctxTestM.*

    val _ = property("ws: head initializes on a Yaci devnet") =
        test.TestM.run[Ctx, Boolean](scenario, resource)

    private def scenario: TestM[Ctx, Boolean] =
        for
            ctx <- ask
            _ <- lift(MultiPeerHeadHarness.submitKickRequest(ctx.harness))
            _ <- lift(awaitTxKnown(ctx.harness.cardanoBackend, ctx.initTxId))
        yield true

    /** Acquire the devnet, prepare its config inputs, generate the config, and stand up the head.
      */
    private def resource: PropertyM[IO, Resource[IO, Ctx]] =
        for
            alloc <- PropertyM.run(YaciDevnet.resource().allocated)
            ready <- PropertyM.run(YaciSetup.prepare(alloc._1, nHeadPeers))
            takeoffAndMnc <- MultiPeerHeadHarness.genDisputeMnc(
              transportMode = Transport.Mode.WebSocket,
              testPeers = ready.testPeers,
              testPeerToUtxos = ready.genesisByPeer,
              takeoffOffset = 120.seconds,
              scriptReferenceUtxos = Some(ready.scriptReferenceUtxos),
            )
        yield
            val (takeoffTime, mnc) = takeoffAndMnc
            MultiPeerHeadHarness
                .disputeHarnessResource(
                  label = "yaci-multipeer",
                  transportMode = Transport.Mode.WebSocket,
                  multiNodeConfig = mnc,
                  testPeers = ready.testPeers,
                  takeoffTime = takeoffTime,
                  tracer = MultiPeerHeadHarness.humanFormatTracer(nHeadPeers),
                  wrapBackend = (_, b) => b,
                  cardanoBackendMode = MultiPeerHeadHarness.CardanoBackend.Mode
                      .Yaci(ready.network, alloc._1.blockfrostApiBaseUri),
                )
                .map(Ctx(_, mnc.headConfig.initializationTx.tx.id))
                .onFinalize(alloc._2)

    /** Poll the backend until the head's initialization tx is known on L1. */
    private def awaitTxKnown(
        backend: L1Backend[IO],
        txId: TransactionHash,
        attemptsLeft: Int = 60
    ): IO[Unit] =
        backend.isTxKnown(txId).flatMap {
            case Right(true) => IO.unit
            case _ if attemptsLeft > 0 =>
                IO.sleep(3.seconds) *> awaitTxKnown(backend, txId, attemptsLeft - 1)
            case other =>
                IO.raiseError(new RuntimeException(s"init tx $txId never landed on L1: $other"))
        }
