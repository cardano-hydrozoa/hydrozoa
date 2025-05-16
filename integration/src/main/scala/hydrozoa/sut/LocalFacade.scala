package hydrozoa.sut

import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.l2.consensus.network.transport.SimNetwork
import hydrozoa.l2.ledger.{SimpleTransaction, SimpleWithdrawal, UtxosSet}
import hydrozoa.node.TestPeer
import hydrozoa.node.rest.SubmitRequestL2.{Transaction, Withdrawal}
import hydrozoa.node.server.*
import hydrozoa.node.state.BlockRecord
import ox.logback.InheritableMDC
import ox.resilience.{RetryConfig, retry}
import ox.{Ox, forkDiscard, forkUser, never, supervised}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt

class LocalFacade(
    peers: Map[TestPeer, Node]
) extends HydrozoaFacade:

    private def randomNode =
        val millis = System.currentTimeMillis().toString.takeRight(3).toInt
        peers.values.toList(millis % peers.size)

    override def initializeHead(
        initiator: TestPeer,
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): Either[InitializationError, TxId] = peers(initiator).initializeHead(ada, txId, txIx)

    override def deposit(
        depositor: TestPeer,
        depositRequest: DepositRequest
    ): Either[DepositError, DepositResponse] = peers(depositor).deposit(depositRequest)

    override def submitL2(
        event: SimpleTransaction | SimpleWithdrawal
    ): Either[InitializationError, TxId] =
        val request = event match
            case tx: SimpleTransaction => Transaction(tx)
            case wd: SimpleWithdrawal  => Withdrawal(wd)
        val submitterNode = peers.values.toList(event.toString.length % peers.size)
        submitterNode.submitL2(request)

    override def stateL2(): List[(UtxoId[L2], Output[L2])] =
        randomNode.stateL2()

    override def produceBlock(
        nextBlockFinal: Boolean
    ): Either[String, (BlockRecord, UtxosSet, UtxosSet)] =
        randomNode.handleNextBlock(nextBlockFinal)

    override def shutdownSut(): Unit = ()

object LocalFacade:
    private val log = Logger("LocalFacade")
    def apply(
        peers: Set[TestPeer],
        pp: ProtocolParams,
        useYaci: Boolean = false
    )(using Ox): HydrozoaFacade =

        InheritableMDC.init

        val nodes = mutable.Map.empty[TestPeer, Node]

        forkUser {
            supervised {
                val simNetwork = SimNetwork.apply(peers.toList)

                peers.foreach(peer =>
                    forkDiscard {
                        runNode(
                          simNetwork,
                          peer,
                          log,
                          (p, n) => {
                              nodes.put(p, n); ()
                          }
                        )
                    }
                )
                never
            }
        }

        retry(RetryConfig.delayForever(100.millis))
            (if nodes.size < peers.size then throw RuntimeException())

        new LocalFacade(nodes.toMap)
