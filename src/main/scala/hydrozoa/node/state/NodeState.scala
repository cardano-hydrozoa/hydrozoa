package hydrozoa.node.state

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.{Piper, sequence, txHash}
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.event.MultisigL1EventSource
import hydrozoa.l1.multisig.tx.InitTx
import hydrozoa.l1.rulebased.tx.deinit.DeinitTxBuilder
import hydrozoa.l1.rulebased.tx.resolution.ResolutionTxBuilder
import hydrozoa.l1.rulebased.tx.tally.TallyTxBuilder
import hydrozoa.l1.rulebased.tx.vote.VoteTxBuilder
import hydrozoa.l1.rulebased.tx.withdraw.WithdrawTxBuilder
import hydrozoa.l2.block.BlockProducer
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.l2.consensus.network.HeadPeerNetwork
import hydrozoa.l2.ledger.L2EventLabel.{L2EventTransactionLabel, L2EventWithdrawalLabel}
import hydrozoa.node.TestPeer
import hydrozoa.node.monitoring.{Metrics, PrometheusMetrics}
import hydrozoa.node.state.HeadPhase.Finalized
import hydrozoa.{AddressBechL1, CurrencySymbol, NativeScript, TokenName, VerificationKeyBytes}
import ox.channels.ActorRef
import scalus.cardano.ledger.Script.Native

import scala.collection.mutable

/** The class that provides read-write and read-only access to the state of the node.
  */
class NodeState(autonomousBlocks: Boolean):

    val log: Logger = Logger(getClass)

    // Actors

    private var multisigL1EventSource: ActorRef[MultisigL1EventSource] = _

    def setMultisigL1EventSource(multisigL1EventSource: ActorRef[MultisigL1EventSource]): Unit =
        this.multisigL1EventSource = multisigL1EventSource

    private var blockProductionActor: ActorRef[BlockProducer] = _

    def setBlockProductionActor(blockProductionActor: ActorRef[BlockProducer]): Unit =
        this.blockProductionActor = blockProductionActor

    private var cardano: ActorRef[CardanoL1] = _

    def setCardano(cardano: ActorRef[CardanoL1]): Unit =
        this.cardano = cardano

    private var network: ActorRef[HeadPeerNetwork] = _

    def setNetwork(network: ActorRef[HeadPeerNetwork]) =
        this.network = network

    private var metrics: ActorRef[Metrics] = _

    def setMetrics(metrics: ActorRef[Metrics]): Unit =
        this.metrics = metrics

    // TODO: move away
    private var voteTxBuilder: VoteTxBuilder = _

    def setVoteTxBuilder(builder: VoteTxBuilder): Unit = this.voteTxBuilder = builder

    private var tallyTxBuilder: TallyTxBuilder = _

    def setTallyTxBuilder(builder: TallyTxBuilder): Unit = this.tallyTxBuilder = builder

    private var resolutionTxBuilder: ResolutionTxBuilder = _

    def setResolutionTxBuilder(builder: ResolutionTxBuilder): Unit = this.resolutionTxBuilder =
        builder

    private var withdrawTxBuilder: WithdrawTxBuilder = _

    def setWithdrawTxBuilder(builder: WithdrawTxBuilder): Unit = this.withdrawTxBuilder = builder

    private var deinitTxBuilder: DeinitTxBuilder = _

    def setDeinitTxBuilder(builder: DeinitTxBuilder): Unit = this.deinitTxBuilder = builder

    //

    private var ownPeer: TestPeer = _

    // All known peers in a peer network (not to confuse with head's peers)
    private val knownPeers: mutable.Set[WalletId] = mutable.Set.empty

    def getKnownPeers: Set[WalletId] = knownPeers.toSet

    // All learned peers' verification keys
    private val knownPeersVKeys: mutable.Map[WalletId, VerificationKeyBytes] = mutable.Map.empty

    def getVerificationKeys(peers: Set[WalletId]): Option[Set[VerificationKeyBytes]] =
        val mbList = peers.toList.map(knownPeersVKeys.get) |> sequence
        mbList.map(_.toSet)

    def getVerificationKeyMap(peers: Set[WalletId]): Map[WalletId, VerificationKeyBytes] =
        val ret = knownPeersVKeys.view.filterKeys(peers.contains)
        assert(peers.size == ret.size, "All VK should present.")
        ret.toMap

    def saveKnownPeersVKeys(keys: Map[WalletId, VerificationKeyBytes]): Unit =
        log.info(s"Saving learned verification keys for known peers: $keys")
        knownPeersVKeys.addAll(keys)

    def autonomousBlockProduction: Boolean = autonomousBlocks

    // The head state. Currently, we support only one head per a [set] of nodes.
    private var headState: Option[HeadStateGlobal] = None

    def tryInitializeHead(params: InitializingHeadParams): Unit =

        def initializeHead(): Unit = {
            log.info(s"Initializing Hydrozoa head...")
            this.headState = Some(HeadStateGlobal(params))
            this.headState.get.setBlockProductionActor(blockProductionActor)
            this.headState.get.setMetrics(metrics)
            this.headState.get.setCardano(cardano)
            this.headState.get.setNetwork(network)
            this.headState.get.setVoteTxBuilder(voteTxBuilder)
            this.headState.get.setTallyTxBuilder(tallyTxBuilder)
            this.headState.get.setResolutionTxBuilder(resolutionTxBuilder)
            this.headState.get.setWithdrawTxBuilder(withdrawTxBuilder)
            this.headState.get.setDeinitTxBuilder(deinitTxBuilder)
            log.info(s"Setting up L1 event sourcing...")
            val initTxId = params.initTx |> txHash
            multisigL1EventSource.tell(
              _.awaitInitTx(
                initTxId,
                params.headAddress,
                params.headNativeScript,
                params.beaconTokenName
              )
            )
            // Reset head-bound metrics
            metrics.tell(m =>
                m.resetBlocksCounter()
                m.clearBlockSize()
                m.setPoolEventsL2(L2EventTransactionLabel, 0)
                m.setPoolEventsL2(L2EventWithdrawalLabel, 0)
                m.clearLiquidity()
            )
        }

        headState match
            case None => initializeHead()
            case Some(head) =>
                head.headPhase match
                    case Finalized => initializeHead()
                    case _ =>
                        val err = "The only supported head is already exists and not finalized yet."
                        log.warn(err)
                        throw new IllegalStateException(err)

    // Returns read-write API for head state.
    def head: HeadState = getOrThrow

    // TODO: move to state-safe reader
    def mbInitializedOn: Option[Long] = headState.map(_.initializedOn)

    // Returns read-only API for head state.
    val reader: HeadStateReader = new {
        override def currentPhase: HeadPhase = getOrThrow.currentPhase
        override def multisigRegimeReader[A](foo: MultisigRegimeReader => A): A =
            getOrThrow.multisigRegimeReader(foo)
        override def initializingPhaseReader[A](foo: InitializingPhaseReader => A): A =
            getOrThrow.initializingPhaseReader(foo)
        override def openPhaseReader[A](foo: OpenPhaseReader => A): A =
            getOrThrow.openPhaseReader(foo)
        override def finalizingPhaseReader[A](foo: FinalizingPhaseReader => A): A =
            getOrThrow.finalizingPhaseReader(foo)
    }

    private def getOrThrow = {
        headState match
            case Some(v) => v
            case None =>
                val err = "The head is missing."
                log.warn(err)
                throw IllegalStateException(err)
    }

object NodeState:
    def apply(
        knownPeers: Set[WalletId],
        autonomousBlocks: Boolean = true
    ): NodeState =
        val nodeState = new NodeState(autonomousBlocks)
        nodeState.knownPeers.addAll(knownPeers)
        nodeState

// FIXME: add pub key
case class WalletId(
    name: String
)

case class InitializingHeadParams(
    ownPeer: WalletId,
    headPeerVKs: Map[WalletId, VerificationKeyBytes],
    headParams: HeadParams,
    headNativeScript: Native,
    headMintingPolicy: CurrencySymbol,
    headAddress: AddressBechL1,
    beaconTokenName: TokenName,
    seedAddress: AddressBechL1,
    initTx: InitTx,
    initializedOn: Long, // system time, milliseconds
    autonomousBlocks: Boolean
)
