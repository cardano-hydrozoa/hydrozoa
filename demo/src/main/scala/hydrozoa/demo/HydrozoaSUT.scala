package hydrozoa.demo

import hydrozoa.*
import hydrozoa.l2.ledger.{L2Transaction, L2Withdrawal}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.{Alice, Bob, Carol}
import hydrozoa.node.rest.{NodeRestApi, SubmitRequestL2}
import hydrozoa.node.server.*
import hydrozoa.node.state.NodeState
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

    def submitL2(
        event: L2Transaction | L2Withdrawal
    ): Either[String, TxId]

    def shutdownSut(): Unit

    def stateL2(): List[(UtxoId[L2], Output[L2])]

val demoPeers = Map.from(
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
            .toRequest(NodeRestApi.initEndpoint, baseUri = demoPeers.get(initiator))
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
            .toRequest(NodeRestApi.depositEndpoint, baseUri = demoPeers.get(depositor))
            .apply(
              r.txId.hash,
              r.txIx.ix.longValue,
              r.depositAmount,
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

    override def submitL2(
        event: L2Transaction | L2Withdrawal
    ): Either[InitializationError, TxId] =

        val lazyResponses = demoPeers
            .map(p =>
                () =>
                    SttpClientInterpreter()
                        .toRequest(
                          NodeRestApi.submitL2Endpoint,
                          baseUri = Some(p._2)
                        )
                        .apply(SubmitRequestL2.apply(event))
                        .send(backend)
            )
            .toList

        val randomNode = Seq(lazyResponses(event.toString.length % lazyResponses.size))

        val results = par(randomNode)

        results.head.body match
            case DecodeResult.Value(v) => v.map(TxId.apply)
            case _                     => Left("decoding failed")

    override def shutdownSut(): Unit = ()

    override def stateL2(): List[(UtxoId[L2], Output[L2])] =
        val response = SttpClientInterpreter()
            .toRequest(
              NodeRestApi.stateL2Endpoint,
              // FIXME: use random peer
              baseUri = demoPeers.get(Alice)
            )
            .apply(())
            .send(backend)

        response.body match
            case DecodeResult.Value(v) =>
                v match
                    case Right(r)  => r
                    case Left(err) => throw RuntimeException("error getting L2 state from head")
            case _ => throw RuntimeException("decoding error")

object RealHydrozoaSUT:
    def apply(): RealHydrozoaSUT = new RealHydrozoaSUT

type NodeStateInspector = NodeState
