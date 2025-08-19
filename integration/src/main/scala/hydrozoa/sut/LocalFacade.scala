package hydrozoa.sut

import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.l1.{CardanoL1Mock, YaciCluster}
import hydrozoa.l1.YaciCluster.YaciClusterInfo
import hydrozoa.l2.consensus.network.transport.SimNetwork
import hydrozoa.l2.ledger.{L2EventGenesis, L2EventTransaction, L2EventWithdrawal}
import hydrozoa.node.TestPeer
import hydrozoa.node.rest.SubmitRequestL2.{Transaction, Withdrawal}
import hydrozoa.node.server.*
import hydrozoa.node.state.{BlockRecord, WalletId}
import ox.*
import ox.logback.InheritableMDC
import ox.resilience.{RetryConfig, retry}
import scalus.cardano.ledger.TransactionHash
import sttp.client4.UriContext

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

/** This facade is used in the happy-path suite. When waiting for Txs to appear on L1 or submitting
  * Txs to L2, it selects a random peer.
  * @param peers
  * @param shutdown
  */
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
        txId: TransactionHash,
        txIx: TxIx
    ): Either[InitializationError, TransactionHash] =
        log.info("SUT: initializing head...")
        val ret = peers(initiator).initializeHead(otherHeadPeers, ada, txId, txIx)
        ret match
            case Left(_)  => ret
            case Right(_) =>
                // Wait till all nodes are switched to `Open` phase, makes sense only for Right
                log.info(s"waiting for initialization...")
                retry(RetryConfig.delayForever(100.millis))({
                    val uptimes = peers.values.map(_.nodeState.ask(_.mbInitializedOn))
                    log.info(s"node uptimes: $uptimes")
                    if (!uptimes.forall(_.isDefined)) throw IllegalStateException()
                })
                ret

    override def deposit(
        depositor: TestPeer,
        depositRequest: DepositRequest
    ): Either[DepositError, DepositResponse] =
        log.info("SUT: depositing into head...")
        val ret = peers(depositor).deposit(depositRequest)
        ret match
            case Left(_)                 => ret
            case Right(_, depositTxHash) =>
                // Wait till all nodes learn about the deposit utxo
                retry(RetryConfig.delayForever(100.millis))({
                    // println(s"waiting for deposit utxo from tx: $depositTxHash")
                    val veracity = peers.values.map(
                      _.nodeState.ask(
                        _.head.openPhase(
                          _.stateL1.depositUtxos.utxoMap.keys
                              .map(_.transactionId)
                              .toSeq
                              .contains(depositTxHash.transactionId)
                        )
                      )
                    )
                    // println(veracity)
                    if (!veracity.forall(e => e)) throw IllegalStateException()
                })
                ret

    override def awaitTxL1(txId: TransactionHash): Option[TxL1] = randomNode.awaitTxL1(txId)

    override def submitL2(
        tx: L2EventTransaction | L2EventWithdrawal
    ): Either[String, TransactionHash] =
        log.info("SUT: submitting L2 transaction/withdrawal...")

        val request = tx match
            case tx: L2EventTransaction => Transaction(tx)
            case wd: L2EventWithdrawal  => Withdrawal(wd)
        val ret = randomNode.submitL2(request)
        ret match
            case Left(_)     => ret
            case Right(txId) =>
                // Wait till all nodes learn about the submitted event
                retry(RetryConfig.delayForever(100.millis))({
                    println(s"waiting for L2 event id: $txId to propagate over all nodes")
                    val veracity =
                        peers.values.map(_.nodeState.ask(_.head.openPhase(_.isL2EventInPool(txId))))
                    println(s"$veracity")
                    if (!veracity.forall(e => e)) throw IllegalStateException()
                })
                ret

    override def stateL2(): List[(UtxoId[L2], Output[L2])] =
        randomNode.stateL2().map((utxoId, output) => utxoId -> Output.apply(output))

    override def produceBlock(
        nextBlockFinal: Boolean
    ): Either[String, (BlockRecord, Option[(TransactionHash, L2EventGenesis)])] =
        log.info(
          s"SUT: producing a block in a lockstep manner " +
              s" nextBlockFinal = $nextBlockFinal"
        )

        // Here we run requests to all nodes in parallel.
        // This is important, since one of this calls will be blocked
        // until the leader returns the block record.
        // This is convenient, since we don't know who the leader is
        // and also allow to propagate flags like quitConsensusImmediately
        // to all nodes.
        val requests = peers.values
            .map(node => () => node.produceNextBlockLockstep(nextBlockFinal))
            .toSeq

        val answers = supervised(
          par(requests)
        )

        log.info("Got all requests from nodes.")

        answers.find(a => a.isRight) match
            case None =>
                answers.foreach(a => log.error(s"Lockstep block answer was: $a"))
                Left("Block can't be produced at the moment")
            case Some(answer) =>
                log.info(
                  s"Block details are here #${answer.right.get._1.block.blockHeader.blockNum}"
                )
                Right(answer.right.get)

    override def runDispute(): Unit =
        log.info("running test dispute...")
        val requests = peers.values
            .map(node => () => node.runDispute())
            .toSeq

        val _ = supervised(
          par(requests)
        )

    override def shutdownSut(): Unit =
        log.info("shutting SUT down...")
        shutdown(Thread.currentThread().threadId())

// This might be just a boolean flag, we don't need thread ids here anymore
val shutdownFlag: mutable.Map[Long, Boolean] = mutable.Map.empty

object LocalFacade:

    private val log = Logger("LocalFacade")

    def apply(
        peers: Set[TestPeer],
        autonomousBlocks: Boolean = false,
        yaciCluster: Option[YaciClusterInfo] = None,
        mbTreasuryScriptRefUtxoId: Option[UtxoIdL1],
        mbDisputeScriptRefUtxoId: Option[UtxoIdL1]
    ): HydrozoaFacade =

        InheritableMDC.init

        val nodes = mutable.Map.empty[TestPeer, Node]

        val thread = new Thread {
            override def run(): Unit =
                supervised {
                    val simNetwork = SimNetwork.apply(peers.toList)
                    val cardanoL1Mock = CardanoL1Mock()

                    peers.foreach(peer =>
                        forkDiscard {
                            LocalNode.runNode(
                              simNetwork = simNetwork,
                              mbCardanoL1Mock =
                                  if yaciCluster.isDefined then None else Some(cardanoL1Mock),
                              ownPeer = peer,
                              autonomousBlocks = autonomousBlocks,
                              mockOrYaci = yaciCluster.toRight(Utils.protocolParams).map(i => (YaciCluster.blockfrostApiBaseUri,i)),
                              mbTreasuryScriptRefUtxoId = mbTreasuryScriptRefUtxoId,
                              mbDisputeScriptRefUtxoId = mbDisputeScriptRefUtxoId,
                              nodeCallback = (p, n) => {
                                  synchronized(nodes.put(p, n))
                              }
                            )
                        }
                    )

                    // Wait for any shutdown flag
                    retry(RetryConfig.delayForever(50.millis))(
                      if (shutdownFlag.isEmpty) {
                          // log.warn(s"Keep going, no shutdown flags: ${shutdownFlag}")
                          throw RuntimeException()
                      }
                    )
                    println(s"Exiting facade's thread=${Thread.currentThread().threadId()}")
                }
        }

        thread.start()

        // Waiting for all nodes to initialize
        retry(RetryConfig.delayForever(100.millis))(
          {
              val nodeSize = synchronized(nodes.size)
              if (nodeSize < peers.size) then throw RuntimeException()
          }
        )

        new LocalFacade(
          nodes.toMap,
          threadId => shutdownFlag.put(threadId, true)
        )
