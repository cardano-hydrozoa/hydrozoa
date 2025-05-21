package hydrozoa.sut

import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.l2.consensus.network.transport.SimNetwork
import hydrozoa.l2.ledger.{SimpleGenesis, SimpleTransaction, SimpleWithdrawal}
import hydrozoa.node.TestPeer
import hydrozoa.node.rest.SubmitRequestL2.{Transaction, Withdrawal}
import hydrozoa.node.server.*
import hydrozoa.node.state.{BlockRecord, WalletId}
import ox.*
import ox.logback.InheritableMDC
import ox.resilience.{RetryConfig, retry}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt

class LocalFacade(
    peers: Map[TestPeer, Node],
    shutdown: Long => Unit
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
    ): Either[InitializationError, TxId] =
        peers(initiator).initializeHead(otherHeadPeers, ada, txId, txIx)

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
        randomNode.stateL2().map((utxoId, output) => utxoId -> Output.apply(output))

    override def produceBlock(
        nextBlockFinal: Boolean
    ): Either[String, (BlockRecord, Option[(TxId, SimpleGenesis)])] =
        val answers = peers.values.map(node =>
            node.produceNextBlockLockstep(nextBlockFinal)
        )
        answers.find(a => a.isRight) match
            case None => Left("Block can't be produced at the moment")
            case Some(answer) => Right(answer.right.get)

    override def shutdownSut(): Unit =
        log.info("shutting SUT down...")
        shutdown(Thread.currentThread().threadId())

val shutdownFlag: mutable.Map[Long, Boolean] = mutable.Map.empty

object LocalFacade:
    private val log = Logger("LocalFacade")
    def apply(
        peers: Set[TestPeer],
        autonomousBlocks: Boolean = false,
        useYaci: Boolean = false
    ): HydrozoaFacade =

        InheritableMDC.init

        val nodes = mutable.Map.empty[TestPeer, Node]

        val thread = new Thread {
            override def run(): Unit =
                supervised {
                    val simNetwork = SimNetwork.apply(peers.toList)
                    peers.foreach(peer =>
                        forkDiscard {
                            LocalNode.runNode(
                              simNetwork = simNetwork,
                              ownPeer = peer,
                              autonomousBlocks = autonomousBlocks,
                              useYaci = useYaci,
                              pp = Some(Utils.protocolParams),
                              nodeCallback = (p, n) => {
                                  synchronized(nodes.put(p, n))
                              }
                            )
                        }
                    )

                    // Wait for shutdown flag
                    retry(RetryConfig.delayForever(50.millis))(
                      if (!shutdownFlag.contains(Thread.currentThread().threadId()))
                          throw RuntimeException()
                    )
                    println(s"thread=${Thread.currentThread().threadId()} is done")
                }
        }

        thread.start()

        // Waiting for all nodes to initialize
        retry(RetryConfig.delayForever(100.millis))(
          {
              val nodeSize = synchronized(nodes.size)
              if (nodeSize < peers.size) then
                  // println(s"nodeSize=${nodeSize}")
                  throw RuntimeException()
          }
        )

        new LocalFacade(
          nodes.toMap,
          threadId => shutdownFlag.put(threadId, true)
        )

//// TODO: Is there something similar in stdlib?
//def discard(_any: Any): Unit = ()
