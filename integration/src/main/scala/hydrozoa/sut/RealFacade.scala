package hydrozoa.sut

import hydrozoa.*
import hydrozoa.l2.ledger.{L2EventGenesis, L2EventTransaction, L2EventWithdrawal}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.Alice
import hydrozoa.node.rest.{InitRequest, NodeRestApi, SubmitRequestL2}
import hydrozoa.node.server.*
import hydrozoa.node.state.{BlockRecord, WalletId}
import ox.par
import scalus.cardano.ledger.TransactionHash
import sttp.client4.DefaultSyncBackend
import sttp.model.Uri
import sttp.tapir.DecodeResult
import sttp.tapir.client.sttp4.SttpClientInterpreter

import scala.language.implicitConversions

/** The real facade to Hydrozoa uses HTTP API to interact with nodes. Nodes should be up and ready.
  */
class RealFacade(peers: Map[TestPeer, Uri]) extends HydrozoaFacade:
    private val backend = DefaultSyncBackend.apply()

    override def initializeHead(
        initiator: TestPeer,
        otherHeadPeers: Set[WalletId],
        ada: Long,
        txId: TransactionHash,
        txIx: TxIx
    ): Either[InitializationError, TransactionHash] =
        val request = InitRequest(otherHeadPeers.toList, ada, txId, txIx)

        val response = SttpClientInterpreter()
            .toRequest(NodeRestApi.initEndpoint, baseUri = peers.get(initiator))
            .apply(request)
            .send(backend)

        response.body match
            case DecodeResult.Value(v) => v.map(TransactionHash.fromHex)
            case _                     => Left("decoding failed")

    override def deposit(
        depositor: TestPeer,
        r: DepositRequest
    ): Either[DepositError, DepositResponse] =

        val response = SttpClientInterpreter()
            .toRequest(NodeRestApi.depositEndpoint, baseUri = peers.get(depositor))
            .apply(
              r.txId.toHex,
              r.txIx.toLong,
              r.depositAmount,
              r.deadline,
              // FIXME: Partial
              r.address.toBech32.get,
              None, // r.datum, // FIXME:
              // FIXME: Partial
              r.refundAddress.toBech32.get,
              None // r.refundDatum // FIXME:
            )
            .send(backend)

        response.body match
            case DecodeResult.Value(v) => v
            case _                     => Left("decoding failed")

    override def awaitTxL1(txId: TransactionHash): Option[TxL1] = ???

    override def submitL2(
        tx: L2EventTransaction | L2EventWithdrawal
    ): Either[InitializationError, TransactionHash] =

        val lazyResponses = peers
            .map(p =>
                () =>
                    SttpClientInterpreter()
                        .toRequest(
                          NodeRestApi.submitL2Endpoint,
                          baseUri = Some(p._2)
                        )
                        .apply(SubmitRequestL2.apply(tx))
                        .send(backend)
            )
            .toList

        val randomNode = Seq(lazyResponses(tx.toString.length % lazyResponses.size))

        val results = par(randomNode)

        results.head.body match
            case DecodeResult.Value(v) => v.map(TransactionHash.fromHex)
            case _                     => Left("decoding failed")

    override def stateL2(): List[(UtxoId[L2], Output[L2])] =
        val response = SttpClientInterpreter()
            .toRequest(
              NodeRestApi.stateL2Endpoint,
              // FIXME: use random peer
              baseUri = peers.get(Alice)
            )
            .apply(())
            .send(backend)

        response.body match
            case DecodeResult.Value(v) =>
                v match
                    case Right(r)  => r.map((utxoId, output) => utxoId -> Output.apply(output))
                    case Left(err) => throw RuntimeException("error getting L2 state from head")
            case _ => throw RuntimeException("decoding error")

    override def produceBlock(
        nextBlockFinal: Boolean
    ): Either[String, (BlockRecord, Option[(TransactionHash, L2EventGenesis)])] =
        throw RuntimeException(
          "Real Hydrozoa facade doesn't support lockstep block producing"
        )

    override def runDispute(): Unit = throw RuntimeException(
      "Real Hydrozoa facade doesn't implement explicit run dispute method"
    )

    override def shutdownSut(): Unit = ()

object RealFacade:
    def apply(peers: Map[TestPeer, Uri]): HydrozoaFacade = new RealFacade(peers)
