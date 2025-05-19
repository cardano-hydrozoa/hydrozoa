package hydrozoa.sut

import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.l2.consensus.network.transport.SimNetwork
import hydrozoa.l2.ledger.{SimpleTransaction, SimpleWithdrawal, UtxosSet}
import hydrozoa.node.TestPeer
import hydrozoa.node.rest.SubmitRequestL2.{Transaction, Withdrawal}
import hydrozoa.node.server.*
import hydrozoa.node.state.{BlockRecord, WalletId}
import ox.*
import ox.logback.InheritableMDC
import ox.resilience.{RetryConfig, retry}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.util.Try

class LocalFacade(
    peers: Map[TestPeer, Node],
    shutdown: () => Unit
) extends HydrozoaFacade:

    private val log = Logger(getClass)

    private def randomNode =
        val millis = System.currentTimeMillis().toString.takeRight(3).toInt
        peers.values.toList(millis % peers.size)

    override def initializeHead(
        initiator: TestPeer,
        otherHeadPeers: Set[WalletId],
        ada: Long,
        txId: TxId,
        txIx: TxIx
    ): Either[InitializationError, TxId] = peers(initiator).initializeHead(otherHeadPeers, ada, txId, txIx)

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

    override def shutdownSut(): Unit =
        log.info("shutting SUT down...")
        shutdown()

object LocalFacade:
    private val log = Logger("LocalFacade")
    def apply(
        peers: Set[TestPeer]
    ): HydrozoaFacade =

        InheritableMDC.init

        val nodes = mutable.Map.empty[TestPeer, Node]
        var shutdownFlag = false

        val thread = new Thread {
            override def run(): Unit =
                supervised {
                    val simNetwork = SimNetwork.apply(peers.toList)
                    peers.foreach(peer =>
                        forkDiscard {
                            LocalNode.runNode(
                              simNetwork = simNetwork,
                              ownPeer = peer,
                              useYaci = false,
                              pp = Some(Utils.protocolParams),
                              nodeCallback = (p, n) => {
                                  discard(nodes.put(p, n))
                              }
                            )
                        }
                    )
                    repeatUntil(shutdownFlag)
                }
        }

        thread.start()

        // Waiting for all nodes to initialize
        // TODO: This hangs up once in a while
        retry(RetryConfig.delayForever(100.millis))(
          if nodes.size < peers.size then throw RuntimeException()
        )

        new LocalFacade(nodes.toMap, () => shutdownFlag = true)

// TODO: Is there something similar in stdlib?
def discard(_any: Any): Unit = ()
