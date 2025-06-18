package hydrozoa.node.state

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{Piper, txFees, txHash, verKeyHash}
import hydrozoa.l1.multisig.state.*
import hydrozoa.l1.multisig.tx.*
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.block.{Block, BlockProducer, BlockTypeL2, zeroBlock}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.l2.ledger.*
import hydrozoa.l2.ledger.event.NonGenesisL2EventLabel.{
    TransactionL2EventLabel,
    WithdrawalL2EventLabel
}
import hydrozoa.l2.ledger.event.{
    NonGenesisL2EventLabel,
    TransactionEventL2,
    WithdrawalEventL2,
    nonGenesisLabel
}
import hydrozoa.l2.ledger.state.UtxosSetOpaque
import hydrozoa.node.monitoring.PrometheusMetrics
import hydrozoa.node.server.*
import hydrozoa.node.state.HeadPhase.{Finalized, Finalizing, Initializing, Open}
import ox.channels.ActorRef

import scala.CanEqual.derived
import scala.collection.mutable

enum HeadPhase derives CanEqual:
    case Initializing
    case Open
    // If one of the peers indicated in AckMinor/AckMajor2 that they want the next block be final
    // and the last, we move the head into Finalizing phase.
    case Finalizing
    case Finalized
    // case Dispute
    // case Closed

trait HeadStateReader:
    // All regimes/phases
    def currentPhase: HeadPhase
    // Regime-specific APIs
    def multisigRegimeReader[A](foo: MultisigRegimeReader => A): A
    // Phase-specific APIs
    def initializingPhaseReader[A](foo: InitializingPhaseReader => A): A
    def openPhaseReader[A](foo: OpenPhaseReader => A): A
    def finalizingPhaseReader[A](foo: FinalizingPhaseReader => A): A

// Just aliases since we can't use the same names for HeadStateReader anb HeadState traits
extension (r: HeadStateReader) {
    def multisigRegime[A](foo: MultisigRegimeReader => A): A = r.multisigRegimeReader(foo)
    def initializingPhase[A](foo: InitializingPhaseReader => A): A = r.initializingPhaseReader(foo)
    def openPhase[A](foo: OpenPhaseReader => A): A = r.openPhaseReader(foo)
    def finalizationPhase[A](foo: FinalizingPhaseReader => A): A = r.finalizingPhaseReader(foo)
}

trait HeadState:
    // All regimes/phases
    def currentPhase: HeadPhase
    // Dumps the current state into log
    def dumpState(): Unit
    // Phase-specific APIs
    def initializingPhase[A](foo: InitializingPhase => A): A
    def openPhase[A](foo: OpenPhase => A): A
    def finalizingPhase[A](foo: FinalizingPhase => A): A

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Readers hierarchy
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sealed trait HeadStateReaderApi

sealed trait InitializingPhaseReader extends HeadStateReaderApi:
    def headPeers: Set[WalletId]

trait MultisigRegimeReader extends HeadStateReaderApi:
    def headPeers: Set[WalletId]
    def headNativeScript: NativeScript
    def headMintingPolicy: CurrencySymbol
    def headBechAddress: AddressBechL1
    def beaconTokenName: TokenName
    def seedAddress: AddressBechL1
    def treasuryUtxoId: UtxoIdL1
    def stateL1: MultisigHeadStateL1

sealed trait OpenPhaseReader extends MultisigRegimeReader:
    def immutablePoolEventsL2: Seq[NonGenesisL2]
    def immutableBlocksConfirmedL2: Seq[BlockRecord]
    def immutableEventsConfirmedL2: Seq[(EventL2, Int)]
    def l2Tip: Block
    def l2LastMajor: Block
    def peekDeposits: DepositUtxos
    def depositTimingParams: (UDiffTimeMilli, UDiffTimeMilli, UDiffTimeMilli) // TODO: explicit type
    def blockLeadTurn: Int
    def isBlockLeader: Boolean
    def isBlockPending: Boolean
    def pendingOwnBlock: OwnBlock

sealed trait FinalizingPhaseReader extends MultisigRegimeReader:
    def l2Tip: Block
    def isBlockLeader: Boolean
    def pendingOwnBlock: OwnBlock
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Head state manager hierarchy
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sealed trait HeadStateApi

sealed trait InitializingPhase extends HeadStateApi with InitializingPhaseReader:
    def openHead(treasuryUtxo: TreasuryUtxo): Unit

sealed trait OpenPhase extends HeadStateApi with OpenPhaseReader:
    def enqueueDeposit(depositId: UtxoIdL1, postDatedRefund: PostDatedRefundTx): Unit
    def poolEventL2(event: NonGenesisL2): Unit
    def newTreasuryUtxo(treasuryUtxo: TreasuryUtxo): Unit
    def removeDepositUtxos(depositIds: Set[UtxoIdL1]): Unit
    def addDepositUtxos(depositUtxos: DepositUtxos): Unit
    def stateL2: AdaSimpleLedger[THydrozoaHead]
    def applyBlockRecord(block: BlockRecord, mbGenesis: Option[(TxId, SimpleGenesis)] = None): Unit
    def applyBlockEvents(
        blockNum: Int,
        eventsValid: Seq[(TxId, NonGenesisL2EventLabel)],
        eventsInvalid: Seq[(TxId, NonGenesisL2EventLabel)],
        depositsAbsorbed: Seq[UtxoIdL1]
    ): Map[TxId, NonGenesisL2]
    def requestFinalization(): Unit
    def isFinalizationRequested: Boolean
    def switchToFinalizingPhase(): Unit

sealed trait FinalizingPhase extends HeadStateApi with FinalizingPhaseReader:
    def stateL2: AdaSimpleLedger[THydrozoaHead]
    def finalizeHead(block: BlockRecord): Unit

/** It's global in a sense that the same value spans over all possible states a head might be in.
  * Probably we can split it up in the future. Doesn't expose fiels; instead implements
  * HeadStateReader and HeadState methods to work with specific regimes/phases.
  */
class HeadStateGlobal(
    var headPhase: HeadPhase,
    val ownPeer: WalletId,
    val headPeerVKs: Map[WalletId, VerificationKeyBytes],
    val headParams: HeadParams,
    val headNativeScript: NativeScript,
    val headMintingPolicy: CurrencySymbol,
    val headAddress: AddressBechL1,
    val beaconTokenName: TokenName,
    val seedAddress: AddressBechL1,
    val initTx: InitTx, // Block #0 L1-effect
    val initializedOn: Long
) extends HeadStateReader
    with HeadState {
    self =>

    private val log = Logger(getClass)

    private var blockProductionActor: ActorRef[BlockProducer] = _

    def setBlockProductionActor(blockProductionActor: ActorRef[BlockProducer]): Unit =
        this.blockProductionActor = blockProductionActor

    private var metrics: ActorRef[PrometheusMetrics] = _

    def setMetrics(metrics: ActorRef[PrometheusMetrics]): Unit =
        this.metrics = metrics

    override def currentPhase: HeadPhase = headPhase

    // Round-robin peer's turn. Having it we always can decide whether
    // the node should be a leader of the next block.
    private var blockLeadTurn: Option[Int] = None
    // Flag that indicated the node is the leader of the next block.
    private var isBlockLeader: Option[Boolean] = None
    // Flag that allows the check whether "no new block awaits the peers’ confirmation"
    // [If true] it postpones the creation of the next block until
    // we are done with the previous one.
    // FIXME: 1. Suspend actors
    // FIXME: 2. Try to create a block when set back to false
    private var isBlockPending: Option[Boolean] = None
    private var pendingOwnBlock: Option[OwnBlock] = None

    // Flag to store the fact of node's being asked for finalization.
    private var mbIsFinalizationRequested: Option[Boolean] = None

    // Pool: L2 events + pending deposits
    private val poolEventsL2: mutable.Buffer[NonGenesisL2] = mutable.Buffer()
    private val poolDeposits: mutable.Buffer[PendingDeposit] = mutable.Buffer()

    // Hydrozoa L2 blocks, confirmed events, and deposits handled
    private val blocksConfirmedL2: mutable.Buffer[BlockRecord] = mutable.Buffer()
    // TODO: these events won't contain genesis events
    private val eventsConfirmedL2: mutable.Buffer[(EventL2, Int)] = mutable.Buffer()
    private val depositHandled: mutable.Set[UtxoIdL1] = mutable.Set.empty
    private val depositL1Effects: mutable.Buffer[DepositRecord] = mutable.Buffer()

    // L1&L2 states
    private var stateL1: Option[MultisigHeadStateL1] = None
    private var stateL2: Option[AdaSimpleLedger[THydrozoaHead]] = None

    // HeadStateReader
    override def multisigRegimeReader[A](foo: MultisigRegimeReader => A): A =
        headPhase match
            case Initializing => foo(MultisigRegimeReaderImpl())
            case Open         => foo(MultisigRegimeReaderImpl())
            case Finalizing   => foo(MultisigRegimeReaderImpl())
            case _            => throw IllegalStateException("The head is not in multisig regime.")

    override def initializingPhaseReader[A](foo: InitializingPhaseReader => A): A =
        headPhase match
            case Initializing => foo(InitializingPhaseReaderImpl())
            case _ => throw IllegalStateException("The head is not in Initializing state.")

    override def openPhaseReader[A](foo: OpenPhaseReader => A): A =
        headPhase match
            case Open => foo(OpenPhaseReaderImpl())
            case _    => throw IllegalStateException("The head is not in Open state.")

    override def finalizingPhaseReader[A](foo: FinalizingPhaseReader => A): A =
        headPhase match
            case Finalizing => foo(FinalizingPhaseReaderImpl())
            case _          => throw IllegalStateException("The head is not in Finalizing state.")

    // HeadState
    override def initializingPhase[A](foo: InitializingPhase => A): A =
        headPhase match
            case Initializing => foo(InitializingPhaseImpl())
            case _ => throw IllegalStateException("The head is not in Initializing state.")

    override def openPhase[A](foo: OpenPhase => A): A =
        headPhase match
            case Open => foo(OpenPhaseImpl())
            case _    => throw IllegalStateException("The head is not in Open state.")

    override def finalizingPhase[A](foo: FinalizingPhase => A): A =
        headPhase match
            case Finalizing => foo(FinalizingPhaseImpl())
            case _          => throw IllegalStateException("The head is not in Finalizing state.")

    // Subclasses that implements APIs (readers)

    private class MultisigRegimeReaderImpl extends MultisigRegimeReader:
        def headPeers: Set[WalletId] = self.headPeerVKs.keySet
        def headNativeScript: NativeScript = self.headNativeScript
        def headMintingPolicy: CurrencySymbol = self.headMintingPolicy
        def beaconTokenName: TokenName = self.beaconTokenName
        def seedAddress: AddressBechL1 = self.seedAddress
        def treasuryUtxoId: UtxoIdL1 = self.stateL1.get.treasuryUtxo.ref
        def headBechAddress: AddressBechL1 = self.headAddress
        def stateL1: MultisigHeadStateL1 = self.stateL1.get

    private class InitializingPhaseReaderImpl extends InitializingPhaseReader:
        def headPeers: Set[WalletId] = self.headPeerVKs.keySet

    private class OpenPhaseReaderImpl extends MultisigRegimeReaderImpl with OpenPhaseReader:
        def immutablePoolEventsL2: Seq[NonGenesisL2] = self.poolEventsL2.toSeq
        def immutableBlocksConfirmedL2: Seq[BlockRecord] = self.blocksConfirmedL2.toSeq
        def immutableEventsConfirmedL2: Seq[(EventL2, Int)] = self.eventsConfirmedL2.toSeq
        def l2Tip: Block = l2Tip_
        def l2LastMajor: Block = self.blocksConfirmedL2
            .findLast(_.block.blockHeader.blockType == Major)
            .map(_.block)
            .getOrElse(zeroBlock)
        def peekDeposits: DepositUtxos =
            // Subtracts deposits that are known to have been handled yet, though their utxo may be still
            // on stateL1.depositUtxos.
            self.stateL1.get.depositUtxos.map.view
                .filterKeys(k => !self.depositHandled.contains(k))
                .toMap
                |> UtxoSet.apply[L1, DepositTag]

        def depositTimingParams: (UDiffTimeMilli, UDiffTimeMilli, UDiffTimeMilli) =
            val headParams = self.headParams
            val consensusParams = headParams.l2ConsensusParams
            (
              consensusParams.depositMarginMaturity,
              headParams.minimalDepositWindow,
              consensusParams.depositMarginExpiry
            )

        def blockLeadTurn: Int = self.blockLeadTurn.get
        def isBlockLeader: Boolean = self.isBlockLeader.get
        def isBlockPending: Boolean = self.isBlockPending.get
        def pendingOwnBlock: OwnBlock = self.pendingOwnBlock.get

    private class FinalizingPhaseReaderImpl
        extends MultisigRegimeReaderImpl
        with FinalizingPhaseReader:
        def l2Tip: Block = l2Tip_
        def isBlockLeader: Boolean = self.isBlockLeader.get
        def pendingOwnBlock: OwnBlock = self.pendingOwnBlock.get

    private def l2Tip_ = blocksConfirmedL2.map(_.block).lastOption.getOrElse(zeroBlock)

    // Subclasses that implements APIs (writers)
    private final class InitializingPhaseImpl
        extends InitializingPhaseReaderImpl
        with InitializingPhase:
        def openHead(
            treasuryUtxo: TreasuryUtxo
        ): Unit =
            self.blockLeadTurn = Some(nodeRoundRobinTurn)
            self.isBlockLeader = Some(self.blockLeadTurn.get == 1)
            self.isBlockPending = Some(false)
            self.mbIsFinalizationRequested = Some(false)
            self.headPhase = Open
            self.stateL1 = Some(MultisigHeadStateL1(treasuryUtxo))
            self.stateL2 = Some(AdaSimpleLedger())

            metrics.tell(_.setTreasuryLiquidity(treasuryUtxo.output.coins.toLong))

            log.info(
              s"Opening head, is block leader: ${self.isBlockLeader.get}, turn: ${self.blockLeadTurn.get}"
            )

    def nodeRoundRobinTurn: Int =
        /* Let's say we have three peers: Alice, Bob, and Carol.
           And their VKH happen to give the same order.
           Then on Alice this function will return 1, so Alice's turns will be 1,4,7,...
           For Bob, it will return 2, so it will give turns 2,5,8,...
           Lastly, for Carol it will give 0, so 3,6,9 will be her turns.
         */
        val headSize = headPeerVKs.size
        val headPeersVKH = headPeerVKs.map((k, v) => (k, v.verKeyHash))
        val turns = headPeersVKH.values.toList.sorted
        val ownVKH = headPeersVKH(ownPeer)
        val ownIndex = turns.indexOf(ownVKH) + 1
        ownIndex % headSize

    private class OpenPhaseImpl extends OpenPhaseReaderImpl with OpenPhase:

        def enqueueDeposit(depositId: UtxoIdL1, postDatedRefund: PostDatedRefundTx): Unit =
            log.info(s"Enqueueing deposit: $depositId")
            self.poolDeposits.append(PendingDeposit(depositId, postDatedRefund))
            metrics.tell(_.setDepositQueueSize(poolDeposits.size))

        def poolEventL2(event: NonGenesisL2): Unit =
            log.info(s"Pooling event: $event")
            self.poolEventsL2.append(event)
            log.info(
              s"isBlockLeader: ${this.isBlockLeader}, isBlockPending: ${this.isBlockPending}"
            )
            // Metrics
            val txs = self.poolEventsL2.map(nonGenesisLabel(_)).count(_ == TransactionL2EventLabel)
            metrics.tell(m =>
                m.setPoolEventsL2(TransactionL2EventLabel, txs)
                m.setPoolEventsL2(WithdrawalL2EventLabel, self.poolEventsL2.size - txs)
            )
            // Try produce block
            if this.isBlockLeader && !this.isBlockPending then
                log.info("Producing block due to getting new L2 event...")
                produceBlock(false)

        private def produceBlock(finalizing: Boolean): Unit = {
            self.isBlockPending = Some(true)
            val (block, utxosActive, _, utxosWithdrawn, mbGenesis) =
                blockProductionActor.ask(
                  _.produceBlock(
                    stateL2.blockProduction,
                    if finalizing then Seq.empty else immutablePoolEventsL2,
                    if finalizing then UtxoSet(Map.empty) else peekDeposits,
                    l2Tip.blockHeader,
                    timeCurrent,
                    finalizing
                  )
                )
            self.pendingOwnBlock = Some(OwnBlock(block, utxosActive, utxosWithdrawn, mbGenesis))
        }

        override def newTreasuryUtxo(treasuryUtxo: TreasuryUtxo): Unit =
            log.info("Net treasury utxo")
            self.stateL1.get.treasuryUtxo = treasuryUtxo
            metrics.tell(_.setTreasuryLiquidity(treasuryUtxo.output.coins.toLong))

        def removeDepositUtxos(depositIds: Set[UtxoIdL1]): Unit =
            log.info(s"Removing deposit utxos that don't exist anymore: $depositIds")
            depositIds.foreach(self.stateL1.get.depositUtxos.map.remove)
            updateDepositLiquidity()

        def addDepositUtxos(depositUtxos: DepositUtxos): Unit =
            log.info(s"Adding new deposit utxos: $depositUtxos")
            self.stateL1.get.depositUtxos.map.addAll(depositUtxos.map)
            log.info(
              s"isBlockLeader: ${this.isBlockLeader}, isBlockPending: ${this.isBlockPending}"
            )
            updateDepositLiquidity()
            if this.isBlockLeader && !this.isBlockPending then
                log.info("Producing new block due to observing a new deposit utxo...")
                produceBlock(false)

        private def updateDepositLiquidity(): Unit =
            val coins = self.stateL1.get.depositUtxos.map.values.map(_.coins).sum
            metrics.tell(_.setDepositsLiquidity(coins.toLong))

        override def stateL2: AdaSimpleLedger[THydrozoaHead] = self.stateL2.get

        override def applyBlockRecord(
            record: BlockRecord,
            mbGenesis: Option[(TxId, SimpleGenesis)] = None
        ): Unit =
            log.info(s"Applying block ${record.block.blockHeader.blockNum}")

            val body = record.block.blockBody
            val blockHeader = record.block.blockHeader
            val blockNum = blockHeader.blockNum

            // Clear up
            self.isBlockPending = Some(false)
            self.pendingOwnBlock = None

            // Next block leader
            val nextBlockNum = blockHeader.blockNum + 1
            self.isBlockLeader = Some(nextBlockNum % headPeerVKs.size == this.blockLeadTurn)

            // State
            self.blocksConfirmedL2.append(record)

            val confirmedEvents = applyBlockEvents(
              blockNum,
              body.eventsValid,
              body.eventsInvalid,
              body.depositsAbsorbed
            )

            // NB: this should be run before replacing utxo set
            val volumeWithdrawn = record.block.validWithdrawals.toList
                .map(confirmedEvents(_).asInstanceOf[WithdrawalL2])
                .flatMap(_.withdrawal.inputs)
                .map(stateL2.getOutput)
                .map(_.coins)
                .sum
                .toLong

            record.l2Effect match
                case utxoSet: MinorBlockL2Effect => stateL2.replaceUtxosActive(utxoSet)
                case _: FinalBlockL2Effect       => ()

            // NB: should be run after confirmMempoolEvents
            metrics.tell(m => {
                val blockType = blockHeader.blockType

                val validTxs =
                    body.eventsValid
                        .map(_._2)
                        .count(_ == TransactionL2EventLabel)
                val validWds = body.eventsValid.size - validTxs
                val invalidTxs = body.eventsInvalid
                    .map(_._2)
                    .count(_ == TransactionL2EventLabel)
                val invalidWds = body.eventsInvalid.size - invalidTxs

                m.observeBlockSize(TransactionL2EventLabel, validTxs)
                m.observeBlockSize(WithdrawalL2EventLabel, validWds)

                m.incEventsL2Handled(TransactionL2EventLabel, true, validTxs)
                m.incEventsL2Handled(TransactionL2EventLabel, false, invalidTxs)
                m.incEventsL2Handled(WithdrawalL2EventLabel, true, validWds)
                m.incEventsL2Handled(WithdrawalL2EventLabel, true, invalidWds)

                // FIXME: should be calculated on L1
                mbGenesis.foreach(g => m.addInboundL1Volume(g._2.volume()))

                val volumeTransacted = record.block.validTransactions.toList
                    .map(confirmedEvents(_).asInstanceOf[TransactionL2])
                    .map(_.transaction.volume())
                    .sum
                m.addTransactedL2Volume(volumeTransacted)

                // FIXME: should be calculated on L1
                m.addOutboundL1Volume(volumeWithdrawn)

                blockType match
                    case Minor =>
                        m.incBlocksMinor()

                    case _ =>
                        m.incBlocksMajor()
                        val depositsNum = body.depositsAbsorbed.size
                        m.observeBlockSize("deposit", depositsNum)
                        m.incAbsorbedDeposits(depositsNum)
                        mbSettlementFees(record.l1Effect).map(m.observeSettlementCost)
            })

            log.info(
              s"nextBlockNum: $nextBlockNum, isBlockLeader: ${this.isBlockLeader}, isBlockPending: ${this.isBlockPending}"
            )

        override def applyBlockEvents(
            blockNum: Int,
            eventsValid: Seq[(TxId, NonGenesisL2EventLabel)],
            eventsInvalid: Seq[(TxId, NonGenesisL2EventLabel)],
            depositsAbsorbed: Seq[UtxoIdL1]
        ): Map[TxId, NonGenesisL2] =

            def applyValidEvent(blockNum: Int, txId: EventHash): (EventHash, NonGenesisL2) =
                log.info(s"Marking event $txId as validated by block $blockNum")
                self.poolEventsL2.indexWhere(e => e.getEventId == txId) match
                    case -1 => throw IllegalStateException(s"pool event $txId was not found")
                    case i =>
                        val event = self.poolEventsL2.remove(i)
                        self.eventsConfirmedL2.append((event, blockNum))
                        // Remove possible duplicates form the pool
                        self.poolEventsL2.filter(e => e.getEventId == event.getEventId)
                            |> self.poolEventsL2.subtractAll
                        // Dump L2 tx
                        TxDump.dumpL2Tx(event match
                            case TransactionL2(_, transaction) =>
                                AdaSimpleLedger.asTxL2(transaction)._1
                            case WithdrawalL2(_, withdrawal) =>
                                AdaSimpleLedger.asTxL2(withdrawal)._1
                        )
                        (txId, event)

            // 1. Handle valid events
            log.info(s"Handle valid events: $eventsValid")
            val validEvents = eventsValid.map((txId, _) => applyValidEvent(blockNum, txId))

            // 2. Process handled deposits
            // 2.1 Add to handled
            self.depositHandled.addAll(depositsAbsorbed.toSet)
            // 2.2 Remove from pool
            depositsAbsorbed.foreach(d =>
                val depositId = UtxoIdL1(d.txId, d.outputIx)
                val ix = self.poolDeposits.indexWhere(p => p.depositId == depositId)
                ix match
                    case -1 =>
                        throw IllegalStateException(
                          s"pool deposit doesn't contain deposit $depositId"
                        )
                    case _ => self.poolDeposits.remove(ix)
            )
            // Metrics - FIXME: factor out
            metrics.tell(_.setDepositQueueSize(poolDeposits.size))

            // 3. Remove invalid events
            log.info(s"Removing invalid events: $eventsInvalid")
            self.poolEventsL2.filter(e => eventsInvalid.map(_._1).contains(e.getEventId))
                |> self.poolEventsL2.subtractAll

            // Metrics - FIXME: factor out
            val txs = self.poolEventsL2.map(nonGenesisLabel(_)).count(_ == TransactionL2EventLabel)
            metrics.tell(m =>
                m.setPoolEventsL2(TransactionL2EventLabel, txs)
                m.setPoolEventsL2(WithdrawalL2EventLabel, self.poolEventsL2.size - txs)
            )

            validEvents.toMap

        override def requestFinalization(): Unit =
            log.info("Head finalization has been requested, next block will be final.")
            self.mbIsFinalizationRequested = Some(true)

        override def isFinalizationRequested: Boolean = self.mbIsFinalizationRequested.get

        override def switchToFinalizingPhase(): Unit =
            if this.isBlockLeader && !this.isBlockPending then
                log.info("Producing final block...")
                produceBlock(true)
            self.headPhase = Finalizing

    private class FinalizingPhaseImpl extends FinalizingPhaseReaderImpl with FinalizingPhase:
        def stateL2: AdaSimpleLedger[THydrozoaHead] = self.stateL2.get

        def finalizeHead(record: BlockRecord): Unit =
            require(
              record.block.blockHeader.blockType == Final,
              "Non-final block in finalizing phase."
            )
            log.info(s"Applying final block ${record.block.blockHeader.blockNum}")

            // TODO: here we need to preserve everything probably to be able to resubmit
            self.headPhase = Finalized
            self.isBlockPending = None
            self.isBlockLeader = None
            self.pendingOwnBlock = None
            self.mbIsFinalizationRequested = None
            self.poolEventsL2.clear()
            self.poolDeposits.clear()
            self.blocksConfirmedL2.clear()
            self.eventsConfirmedL2.clear()
            self.depositHandled.clear()
            self.depositL1Effects.clear()
            self.stateL1 = None
            self.stateL2 = None

            metrics.tell(m => {
                val header = record.block.blockHeader
                val body = record.block.blockBody
                val blockType = header.blockType
                val blockNum = header.blockNum

                val validTxs =
                    body.eventsValid
                        .map(_._2)
                        .count(_ == TransactionL2EventLabel)
                val validWds = body.eventsValid.size - validTxs
                val invalidTxs = body.eventsInvalid
                    .map(_._2)
                    .count(_ == TransactionL2EventLabel)
                val invalidWds = body.eventsInvalid.size - invalidTxs

                m.observeBlockSize(TransactionL2EventLabel, validTxs)
                m.observeBlockSize(WithdrawalL2EventLabel, validWds)

                m.incEventsL2Handled(TransactionL2EventLabel, true, validTxs)
                m.incEventsL2Handled(TransactionL2EventLabel, false, invalidTxs)
                m.incEventsL2Handled(WithdrawalL2EventLabel, true, validWds)
                m.incEventsL2Handled(WithdrawalL2EventLabel, true, invalidWds)
                // TODO: L2 total is not updated here

                blockType match
                    case Minor =>
                        m.incBlocksMinor()

                    case _ =>
                        m.incBlocksMajor()
                        val depositsNum = body.depositsAbsorbed.size
                        m.observeBlockSize("deposit", depositsNum)
                        m.incAbsorbedDeposits(depositsNum)
                        mbSettlementFees(record.l1Effect).map(m.observeSettlementCost)
            })

            log.info("Head was closed.")

    override def dumpState(): Unit =
        currentPhase match
            case HeadPhase.Open =>
                log.trace(
                  "-----------------------   Open: L1 State --------------------------------------" +
                      s"\n${openPhase(_.stateL1)}"
                )

                log.trace(
                  "-----------------------   Open: POOL    ---------------------------------------" +
                      s"\n${openPhase(_.immutablePoolEventsL2)}"
                )

                log.trace(
                  "-----------------------   Open: L2 State   ------------------------------------" +
                      s"\n${openPhase(_.stateL2.getUtxosActive)}"
                )
                log.trace(
                  "------------------------  Open: BLOCKS   --------------------------------------" +
                      s"\n${openPhase(_.immutableBlocksConfirmedL2)}"
                )
                log.trace(
                  "------------------------  Open: EVENTS   --------------------------------------" +
                      s"\n${openPhase(_.immutableEventsConfirmedL2)}"
                )

            case HeadPhase.Finalizing =>
                log.trace(
                  "-----------------------   Finalizing: L1 State --------------------------------------" +
                      s"\n${finalizingPhase(_.stateL1)}"
                )
                log.trace(
                  "-----------------------   Finalizing: L2 State   ------------------------------------" +
                      s"${finalizingPhase(_.stateL2.getUtxosActive)}"
                )
            case _ => println("dumpState is missing due to nodes's being in wrong phase.")
}

object HeadStateGlobal:
    private val log = Logger(getClass)
    def apply(params: InitializingHeadParams): HeadStateGlobal =
        log.info(s"Creating head state with parameters: $params")
        assert(params.headPeerVKs.size >= 2, "The number of peers should be >= 2")
        new HeadStateGlobal(
          headPhase = Initializing,
          ownPeer = params.ownPeer,
          headPeerVKs = params.headPeerVKs,
          headParams = params.headParams,
          headNativeScript = params.headNativeScript,
          headMintingPolicy = params.headMintingPolicy,
          headAddress = params.headAddress,
          beaconTokenName = params.beaconTokenName,
          seedAddress = params.seedAddress,
          initTx = params.initTx,
          initializedOn = params.initializedOn
        )

case class BlockRecord(
    block: Block,
    l1Effect: L1BlockEffect,
    // TODO: this is missing for final block
    l1PostDatedEffect: L1PostDatedBlockEffect,
    l2Effect: L2BlockEffect
)

case class OwnBlock(
    block: Block,
    utxosActive: UtxosSetOpaque,
    utxosWithdrawn: UtxosSet,
    mbGenesis: Option[(TxId, SimpleGenesis)]
)

case class PendingDeposit(
    depositId: UtxoIdL1,
    postDatedRefundTx: PostDatedRefundTx
)

type DepositRecord = AbsorbedDeposit | RefundedDeposit

case class AbsorbedDeposit(
    depositId: UtxoIdL1,
    postDatedRefundTx: PostDatedRefundTx,
    blockNum: Int
)

case class RefundedDeposit(
    depositId: UtxoIdL1,
    postDatedRefundTx: Option[PostDatedRefundTx],
    immediateRefundTx: Option[TxL1]
)

type L1BlockEffect = InitTx | SettlementTx | FinalizationTx | MinorBlockL1Effect
type MinorBlockL1Effect = Unit
type L1PostDatedBlockEffect = Unit

type L2BlockEffect = MinorBlockL2Effect | MajorBlockL2Effect | FinalBlockL2Effect
type MinorBlockL2Effect = UtxosSetOpaque
type MajorBlockL2Effect = UtxosSetOpaque
type FinalBlockL2Effect = Unit

given CanEqual[L2BlockEffect, L2BlockEffect] = CanEqual.derived

def maybeMultisigL1Tx(l1Effect: L1BlockEffect): Option[TxL1] = l1Effect match
    case _: MinorBlockL1Effect             => None
    case someTx: MultisigTx[MultisigTxTag] => someTx |> toL1Tx |> Some.apply

// NB: this won't work as an extension method on union type L1BlockEffect
def mbTxHash(l1BlockEffect: L1BlockEffect): Option[TxId] = l1BlockEffect match
    case _: MinorBlockL1Effect => None
    case tx: MultisigTx[_]     => tx |> txHash |> Some.apply

def mbSettlementFees(l1BlockEffect: L1BlockEffect): Option[Long] = l1BlockEffect match
    case tx: SettlementTx => tx |> txFees |> Some.apply
    case _                => None
