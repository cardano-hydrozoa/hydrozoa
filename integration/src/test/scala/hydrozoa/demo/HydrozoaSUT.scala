package hydrozoa.demo

import hydrozoa.l2.ledger.{SimpleTransaction, SimpleWithdrawal}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.{Alice, Bob, Carol}
import hydrozoa.node.rest.{NodeRestApi, SubmitRequestL2}
import hydrozoa.node.server.*
import hydrozoa.node.state.NodeState
import hydrozoa.{TxId, TxIx}
import ox.par
import sttp.client4.{DefaultSyncBackend, UriContext}
import sttp.tapir.DecodeResult
import sttp.tapir.client.sttp4.SttpClientInterpreter

/** Hydrozoa peers' network facade.
  */
trait HydrozoaSUT:
    def initializeHead(
        initiator: TestPeer,
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): Either[InitializationError, TxId]

    def deposit(
        depositor: TestPeer,
        r: DepositRequest
    ): Either[DepositError, DepositResponse]

    def produceBlock(headPeers: Set[TestPeer]): Either[String, Unit]

    def submitL2(
        event: SimpleTransaction | SimpleWithdrawal
    ): Either[String, TxId]

    def shutdownSut(): Unit

val peers = Map.from(
  List(
    Alice -> uri"http://localhost:8093",
    Bob -> uri"http://localhost:8094",
    Carol -> uri"http://localhost:8095"
  )
)

class RealHydrozoaSUT extends HydrozoaSUT:
    private val backend = DefaultSyncBackend.apply()

    override def initializeHead(
        initiator: TestPeer,
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): Either[InitializationError, TxId] =
        val response = SttpClientInterpreter()
            .toRequest(NodeRestApi.initEndpoint, baseUri = peers.get(initiator))
            .apply(ada, txId.hash, txIx.ix)
            .send(backend)

        response.body match
            case DecodeResult.Value(v) => v.map(TxId.apply)
            case _                     => Left("decoding failed")

    override def deposit(
        depositor: TestPeer,
        r: DepositRequest
    ): Either[DepositError, DepositResponse] =

        val response = SttpClientInterpreter()
            .toRequest(NodeRestApi.depositEndpoint, baseUri = peers.get(depositor))
            .apply(
              r.txId.hash,
              r.txIx.ix.longValue,
              r.deadline,
              r.address.bech32,
              None, // r.datum, // FIXME:
              r.refundAddress.bech32,
              None // r.refundDatum // FIXME:
            )
            .send(backend)

        response.body match
            case DecodeResult.Value(v) => v
            case _                     => Left("decoding failed")

    override def produceBlock(headPeers: Set[TestPeer]): Either[String, Unit] =

        val lazyResponses = headPeers
            .map(p =>
                () =>
                    val response = SttpClientInterpreter()
                        .toRequest(NodeRestApi.awaitBlockEndpoint, baseUri = peers.get(p))
                        .apply(())
                        .send(backend)

                    response.body match
                        case DecodeResult.Value(v) => v
                        case _                     => Left("decoding failed")
            )
            .toSeq

        val results = par(lazyResponses)
        println("*****************************************************")
        println(results)
        // TODO: check lefts
        Right(())

    override def submitL2(
        event: SimpleTransaction | SimpleWithdrawal
    ): Either[InitializationError, TxId] =
        val response = SttpClientInterpreter()
            .toRequest(
              NodeRestApi.submitL2Endpoint,
              baseUri = peers.get(Alice)
            ) // FIXME: use random peer
            .apply(SubmitRequestL2.apply(event))
            .send(backend)

        response.body match
            case DecodeResult.Value(v) => v.map(TxId.apply)
            case _                     => Left("decoding failed")

    override def shutdownSut(): Unit = ()

object RealHydrozoaSUT:
    def apply(): RealHydrozoaSUT = new RealHydrozoaSUT

type NodeStateInspector = NodeState
