package hydrozoa.node.state

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.api.model
import com.bloxbean.cardano.client.api.model.Utxo as BBUtxo
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.util.HexUtil
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.transitionary.toHydrozoaNativeScript
import hydrozoa.infra.{
    Piper,
    decodeHex,
    encodeHex,
    extractVoteTokenNameFromFallbackTx,
    serializeTxHex,
    txFees,
    txHash,
    verKeyHash
}
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.state.*
import hydrozoa.l1.multisig.tx.*
import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.VoteDatum
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.TreasuryDatum
import hydrozoa.l1.rulebased.onchain.TreasuryValidator.TreasuryDatum.Resolved
import hydrozoa.l1.rulebased.onchain.{
    DisputeResolutionScript,
    TreasuryValidatorScript,
    hashVerificationKey
}
import hydrozoa.l1.rulebased.tx.deinit.{DeinitTxBuilder, DeinitTxRecipe}
import hydrozoa.l1.rulebased.tx.resolution.{ResolutionTxBuilder, ResolutionTxRecipe}
import hydrozoa.l1.rulebased.tx.tally.{TallyTxBuilder, TallyTxRecipe}
import hydrozoa.l1.rulebased.tx.vote.{VoteTxBuilder, VoteTxRecipe}
import hydrozoa.l1.rulebased.tx.withdraw.{WithdrawTxBuilder, WithdrawTxRecipe}
import hydrozoa.l2.block.*
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.l2.consensus.network.{HeadPeerNetwork, ReqDeinit}
import hydrozoa.l2.ledger.*
import hydrozoa.l2.ledger.L2EventLabel.{L2EventTransactionLabel, L2EventWithdrawalLabel}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.account
import hydrozoa.node.monitoring.Metrics
import hydrozoa.node.state.HeadPhase.{Finalized, Finalizing, Initializing, Open}
import ox.channels.ActorRef
import ox.resilience.{RetryConfig, retry}
import scalus.bloxbean.Interop
import scalus.builtin.Data.fromData
import scalus.prelude.crypto.bls12_381.G2

import scala.CanEqual.derived
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.mutable
import scala.concurrent.duration.DurationInt

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

    /** Used only for testing. Tries to look up block's effects.
      *
      * @return
      *   Block record and optional genesis if effects for block are ready.
      */
    def getBlockRecord(block: Block): Option[(BlockRecord, Option[(TxId, L2Genesis)])]

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
    def immutablePoolEventsL2: Seq[L2Event]
    def immutableBlocksConfirmedL2: Seq[BlockRecord]
    def immutableEventsConfirmedL2: Seq[(L2Event, Int)]
    def l2Tip: Block
    def l2LastMajorRecord: BlockRecord
    def l2LastMajor: Block
    def lastKnownTreasuryUtxoId: UtxoIdL1
    def peekDeposits: DepositUtxos
    def depositTimingParams: (UDiffTimeMilli, UDiffTimeMilli, UDiffTimeMilli) // TODO: explicit type
    def blockLeadTurn: Int
    def isBlockLeader: Boolean
    def isBlockPending: Boolean
    def pendingOwnBlock: OwnBlock
    def isQuitConsensusImmediately: Boolean

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
    def poolEventL2(event: L2Event): Unit

    /** Used only for testing. Checks whether an non-genesis event is in the pool.
      * @param txId
      * @return
      */
    def isL2EventInPool(txId: TxId): Boolean

    def setNewTreasuryUtxo(treasuryUtxo: TreasuryUtxo): Unit
    def removeDepositUtxos(depositIds: Set[UtxoIdL1]): Unit
    def addDepositUtxos(depositUtxos: DepositUtxos): Unit
    def stateL2: L2LedgerModule[HydrozoaHeadLedger, HydrozoaL2Ledger.LedgerUtxoSetOpaque]
    def applyBlockRecord(block: BlockRecord, mbGenesis: Option[(TxId, L2Genesis)] = None): Unit
    def applyBlockEvents(
        blockNum: Int,
        eventsValid: Seq[(TxId, L2EventLabel)],
        eventsInvalid: Seq[(TxId, L2EventLabel)],
        depositsAbsorbed: Seq[UtxoIdL1]
    ): Map[TxId, L2Event]
    def requestFinalization(): Unit
    def isNextBlockFinal: Boolean
    def switchToFinalizingPhase(): Unit

    /** As an API (trait's) method is used only for testing. Tries to run block creation routine and
      * consensus on the block. Returns the block, for getting the block record use
      * `getBlockRecord`.
      * @param nextBlockFinal
      * @param force
      * @return
      */
    def tryProduceBlock(
        nextBlockFinal: Boolean,
        force: Boolean = false,
        quitConsensusImmediately: Boolean
    ): Either[String, Block]

sealed trait FinalizingPhase extends HeadStateApi with FinalizingPhaseReader:
    def stateL2: L2LedgerModule[HydrozoaHeadLedger, HydrozoaL2Ledger.LedgerUtxoSetOpaque]
    def tryProduceFinalBlock(force: Boolean): Either[String, Block]
    def newTreasuryUtxo(treasuryUtxo: TreasuryUtxo): Unit
    def finalizeHead(block: BlockRecord): Unit

/** It's global in a sense that the same value spans over all possible states a head might be in.
  * Probably we can split it up in the future. Doesn't expose fields; instead implements
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
    val initializedOn: Long,
    val autonomousBlocks: Boolean
) extends HeadStateReader
    with HeadState {
    self =>

    private val log = Logger(getClass)

    private var blockProductionActor: ActorRef[BlockProducer] = _

    def setBlockProductionActor(blockProductionActor: ActorRef[BlockProducer]): Unit =
        this.blockProductionActor = blockProductionActor

    private var metrics: ActorRef[Metrics] = _

    def setMetrics(metrics: ActorRef[Metrics]): Unit =
        this.metrics = metrics

    private var cardano: ActorRef[CardanoL1] = _

    def setCardano(cardano: ActorRef[CardanoL1]): Unit =
        this.cardano = cardano

    private var network: ActorRef[HeadPeerNetwork] = _

    def setNetwork(network: ActorRef[HeadPeerNetwork]) =
        this.network = network

    // TODO: remove

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

    override def currentPhase: HeadPhase = headPhase

    // Round-robin peer's turn. Having it we always can decide whether
    // the node should be a leader of the next block.
    private var blockLeadTurn: Option[Int] = None
    // Flag that indicated the node is the leader of the next block.
    private var isBlockLeader: Option[Boolean] = None
    // Flag that allows the check whether "no new block awaits the peers’ confirmation"
    // [If true] it postpones the creation of the next block until
    // we are done with the previous one.
    // FIXME: Try to create a block when this flag is set back to false
    private var isBlockPending: Option[Boolean] = None
    private var pendingOwnBlock: Option[OwnBlock] = None

    // Flag to store the fact of node's being asked for finalization.
    private var mbIsNextBlockFinal: Option[Boolean] = None

    // TODO: handle it the same way we do `mbIsNextBlockFinal`
    //   so all nodes can actively submit transactions
    private var mbQuitConsensusImmediately: Option[Boolean] = None

    // Pool: L2 events + pending deposits
    private val poolEventsL2: mutable.Buffer[L2Event] = mutable.Buffer()
    private val poolDeposits: mutable.Buffer[PendingDeposit] = mutable.Buffer()

    // Hydrozoa L2 blocks, confirmed events, and deposits handled
    private val blocksConfirmedL2: mutable.Buffer[BlockRecord] = mutable.Buffer()
    // TODO: having two lists won't work when we are to replay events, we need order
    private val genesisEventsConfirmedL2: mutable.Map[Int, (TxId, L2Genesis)] = mutable.Map()
    // TODO: these events won't contain genesis events
    private val nonGenesisEventsConfirmedL2: mutable.Buffer[(L2Event, Int)] = mutable.Buffer()
    private val depositHandled: mutable.Set[UtxoIdL1] = mutable.Set.empty
    private val depositL1Effects: mutable.Buffer[DepositRecord] = mutable.Buffer()

    // L1&L2 states
    private var stateL1: Option[MultisigHeadStateL1] = None
    private var stateL2
        : Option[L2LedgerModule[HydrozoaHeadLedger, HydrozoaL2Ledger.LedgerUtxoSetOpaque]] = None

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
            case _ => throw IllegalStateException("The head is not in Initializing phase.")

    override def openPhaseReader[A](foo: OpenPhaseReader => A): A =
        headPhase match
            case Open => foo(OpenPhaseReaderImpl())
            case _    => throw IllegalStateException("The head is not in Open phase.")

    override def finalizingPhaseReader[A](foo: FinalizingPhaseReader => A): A =
        headPhase match
            case Finalizing => foo(FinalizingPhaseReaderImpl())
            case _          => throw IllegalStateException("The head is not in Finalizing phase.")

    // HeadState
    override def initializingPhase[A](foo: InitializingPhase => A): A =
        headPhase match
            case Initializing => foo(InitializingPhaseImpl())
            case _ => throw IllegalStateException("The head is not in Initializing phase.")

    override def openPhase[A](foo: OpenPhase => A): A =
        headPhase match
            case Open => foo(OpenPhaseImpl())
            case _ =>
                val msg = "The head is not in Open phase."
                log.error(msg)
                throw RuntimeException(msg)

    override def finalizingPhase[A](foo: FinalizingPhase => A): A =
        headPhase match
            case Finalizing => foo(FinalizingPhaseImpl())
            case _          => throw IllegalStateException("The head is not in Finalizing phase.")

    override def getBlockRecord(
        block: Block
    ): Option[(BlockRecord, Option[(TxId, L2Genesis)])] =
        // TODO: shall we use Map not Buffer?
        self.blocksConfirmedL2.find(_.block == block) match
            case None => None
            case Some(blockRecord) =>
                val mbGenesis = self.genesisEventsConfirmedL2.get(block.blockHeader.blockNum)
                Some(blockRecord, mbGenesis)

    // Subclasses that implements APIs (readers)

    private class MultisigRegimeReaderImpl extends MultisigRegimeReader:
        def headPeers: Set[WalletId] = self.headPeerVKs.keySet
        def headNativeScript: NativeScript = self.headNativeScript
        def headMintingPolicy: CurrencySymbol = self.headMintingPolicy
        def beaconTokenName: TokenName = self.beaconTokenName
        def seedAddress: AddressBechL1 = self.seedAddress
        def treasuryUtxoId: UtxoIdL1 = self.stateL1.get.treasuryUtxo.unTag.ref
        def headBechAddress: AddressBechL1 = self.headAddress
        def stateL1: MultisigHeadStateL1 = self.stateL1.get

    private class InitializingPhaseReaderImpl extends InitializingPhaseReader:
        def headPeers: Set[WalletId] = self.headPeerVKs.keySet

    private class OpenPhaseReaderImpl extends MultisigRegimeReaderImpl with OpenPhaseReader:
        def immutablePoolEventsL2: Seq[L2Event] = self.poolEventsL2.toSeq
        def immutableBlocksConfirmedL2: Seq[BlockRecord] = self.blocksConfirmedL2.toSeq
        def immutableEventsConfirmedL2: Seq[(L2Event, Int)] = self.nonGenesisEventsConfirmedL2.toSeq
        def l2Tip: Block = l2Tip_
        def l2LastMajorRecord: BlockRecord = self.blocksConfirmedL2
            .findLast(_.block.blockHeader.blockType == Major)
            // FIXME: here a fallback tx for initialization tx should go
            .getOrElse(???)
        def l2LastMajor: Block = self.blocksConfirmedL2
            .findLast(_.block.blockHeader.blockType == Major)
            .map(_.block)
            .getOrElse(zeroBlock)
        def lastKnownTreasuryUtxoId: UtxoIdL1 = self.blocksConfirmedL2
            .findLast(_.block.blockHeader.blockType == Major)
            .map(record => UtxoIdL1.apply(txHash(maybeMultisigL1Tx(record.l1Effect).get), TxIx(0)))
            .getOrElse(treasuryUtxoId)
        def peekDeposits: DepositUtxos =
            // Subtracts deposits that are known to have been handled yet, though their utxo may be still
            // on stateL1.depositUtxos.
            self.stateL1.get.depositUtxos.utxoMap.view
                .filterKeys(k => !self.depositHandled.contains(k))
                .toMap
                |> TaggedUtxoSet.apply

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
        def isQuitConsensusImmediately: Boolean = self.mbQuitConsensusImmediately.getOrElse(false)

    private class FinalizingPhaseReaderImpl
        extends MultisigRegimeReaderImpl
        with FinalizingPhaseReader:
        def l2Tip: Block = l2Tip_
        def isBlockLeader: Boolean = self.isBlockLeader.get
        def pendingOwnBlock: OwnBlock = self.pendingOwnBlock.get

    private def l2Tip_ = blocksConfirmedL2.lastOption.map(_.block).getOrElse(zeroBlock)

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
            self.mbIsNextBlockFinal = Some(false)
            self.headPhase = Open
            self.stateL1 = Some(MultisigHeadStateL1(treasuryUtxo))
            // TODO: here we decide which ledger we are using
            self.stateL2 = Some(HydrozoaL2Ledger.mkLedgerForHead())

            metrics.tell(_.setTreasuryLiquidity(treasuryUtxo.unTag.output.coins.toLong))

            log.info(
              s"Opening head, is block leader: ${self.isBlockLeader.get}, turn: ${self.blockLeadTurn.get}"
            )

    private def nodeRoundRobinTurn: Int =
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

        def poolEventL2(event: L2Event): Unit =
            log.info(s"Pooling event: $event")
            self.poolEventsL2.append(event)
            // Metrics
            val txs = self.poolEventsL2.map(l2EventLabel).count(_ == L2EventTransactionLabel)
            metrics.tell(m =>
                m.setPoolEventsL2(L2EventTransactionLabel, txs)
                m.setPoolEventsL2(L2EventWithdrawalLabel, self.poolEventsL2.size - txs)
            )
            // Try produce block
            log.info("Try to produce a block due to getting new L2 event...")
            tryProduceBlock(false)

        def isL2EventInPool(txId: TxId): Boolean =
            self.poolEventsL2.map(_.getEventId).contains(txId)

        override def tryProduceBlock(
            nextBlockFinal: Boolean,
            force: Boolean = false,
            // This is a test-only flag that forces the node to submit fallback
            // and other role-based regime txs immediately (when applying the block record).
            quitConsensusImmediately: Boolean = false
        ): Either[String, Block] = {
            // The testing facade calls tryProduceBlock on all nodes every time
            // a test suite wants to produce a block.
            // This has twofold effect:
            // 1. Sets `quitConsensusImmediately` flag (on all nodes).
            self.mbQuitConsensusImmediately = Some(quitConsensusImmediately)
            // 2. The leader produces the block and starts consensus on it.
            if (self.autonomousBlocks || force)
                && this.isBlockLeader && !this.isBlockPending
            then
                log.info(s"Trying to produce next minor/major block...")

                val finalizing = self.headPhase == Finalizing

                val tipHeader = l2Tip.blockHeader
                blockProductionActor.ask(
                  _.produceBlock(
                    stateL2.cloneForBlockProducer(),
                    immutablePoolEventsL2,
                    peekDeposits,
                    tipHeader,
                    timeCurrent,
                    finalizing
                  )
                ) match
                    case Right(block, utxosActive, _, utxosWithdrawn, mbGenesis) =>
                        self.pendingOwnBlock = Some(
                          OwnBlock(block, utxosActive, utxosWithdrawn, mbGenesis)
                        )
                        self.isBlockPending = Some(true)
                        self.mbIsNextBlockFinal = Some(nextBlockFinal)
                        Right(block)
                    case Left(err) =>
                        // TODO: this arguably should never happen
                        setLeaderFlag(tipHeader.blockNum + 1)
                        Left(err)
            else
                val msg = s"Block is not going to be produced: " +
                    s"autonomousBlocks=${self.autonomousBlocks} " +
                    s"force=$force " +
                    s"quitConsensusImmedaitely=$quitConsensusImmediately " +
                    s"isBlockLeader=${this.isBlockLeader} " +
                    s"isBlockPending=${this.isBlockPending} "
                log.info(msg)
                Left(msg)
        }

        override def setNewTreasuryUtxo(treasuryUtxo: TreasuryUtxo): Unit =
            log.info(s"Setting a new treasury utxo: $treasuryUtxo")
            self.stateL1.get.treasuryUtxo = treasuryUtxo
            metrics.tell(_.setTreasuryLiquidity(treasuryUtxo.unTag.output.coins.toLong))

        def removeDepositUtxos(depositIds: Set[UtxoIdL1]): Unit =
            log.info(s"Removing deposit utxos that don't exist anymore: $depositIds")
            depositIds.foreach(self.stateL1.get.depositUtxos.utxoMap.remove)
            updateDepositLiquidity()

        def addDepositUtxos(depositUtxos: DepositUtxos): Unit =
            log.info(s"Adding new deposit utxos: $depositUtxos")
            self.stateL1.get.depositUtxos.utxoMap.addAll(depositUtxos.unTag.utxoMap)
            updateDepositLiquidity()
            log.info("Try to produce a new block due to observing a new deposit utxo...")
            tryProduceBlock(false)

        private def updateDepositLiquidity(): Unit =
            val coins = self.stateL1.get.depositUtxos.utxoMap.values.map(_.coins).sum
            metrics.tell(_.setDepositsLiquidity(coins.toLong))

        override def stateL2
            : L2LedgerModule[HydrozoaHeadLedger, HydrozoaL2Ledger.LedgerUtxoSetOpaque] =
            self.stateL2.get

        override def applyBlockRecord(
            record: BlockRecord,
            mbGenesis: Option[(TxId, L2Genesis)] = None
        ): Unit =
            log.info(s"Applying block ${record.block.blockHeader.blockNum}")

            val body = record.block.blockBody
            val blockHeader = record.block.blockHeader
            val blockNum = blockHeader.blockNum

            // Clear up
            self.isBlockPending = Some(false)
            self.pendingOwnBlock = None

            // Next block leader
            val nextBlockNum = blockNum + 1
            setLeaderFlag(nextBlockNum)

            // State
            self.blocksConfirmedL2.append(record)
            mbGenesis.foreach(g => self.genesisEventsConfirmedL2.put(blockNum, g))

            val confirmedEvents = applyBlockEvents(
              blockNum,
              body.eventsValid,
              body.eventsInvalid,
              body.depositsAbsorbed
            )

            // NB: this should be run before replacing utxo set
            val volumeWithdrawn = record.block.validWithdrawals.toList
                .map(confirmedEvents(_).asInstanceOf[L2EventWithdrawal])
                .flatMap(_.withdrawal.inputs)
                .map(stateL2.getOutput)
                .map(_.coins)
                .sum
                .toLong

            record.l2Effect.foreach(stateL2.replaceUtxosActive)

            // NB: should be run after confirmMempoolEvents
            metrics.tell(m => {
                val blockType = blockHeader.blockType

                val validTxs =
                    body.eventsValid
                        .map(_._2)
                        .count(_ == L2EventTransactionLabel)
                val validWds = body.eventsValid.size - validTxs
                val invalidTxs = body.eventsInvalid
                    .map(_._2)
                    .count(_ == L2EventTransactionLabel)
                val invalidWds = body.eventsInvalid.size - invalidTxs

                m.observeBlockSize(L2EventTransactionLabel, validTxs)
                m.observeBlockSize(L2EventWithdrawalLabel, validWds)

                m.incEventsL2Handled(L2EventTransactionLabel, true, validTxs)
                m.incEventsL2Handled(L2EventTransactionLabel, false, invalidTxs)
                m.incEventsL2Handled(L2EventWithdrawalLabel, true, validWds)
                m.incEventsL2Handled(L2EventWithdrawalLabel, true, invalidWds)

                // FIXME: should be calculated on L1
                mbGenesis.foreach(g => m.addInboundL1Volume(g._2.volume()))

                val volumeTransacted = record.block.validTransactions.toList
                    .map(confirmedEvents(_).asInstanceOf[L2EventTransaction])
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

            // TODO: L1 effects submission should be carried on by a separate process
            record.l1Effect |> maybeMultisigL1Tx match {
                case Some(settlementTx) =>
                    log.info(s"Submitting settlement tx: ${txHash(settlementTx)}")
                    val ret = cardano.ask(_.submit(settlementTx))
                    log.info(s"settlementResult = $ret")
                case _ =>
            }

            // Test dispute routine
            def runTestDispute() = {

                // Submit fallback tx
                // TODO: wait till validity range is hit
                val fallbackTx = l2LastMajorRecord.l1PostDatedEffect.get
                val fallbackTxHash = txHash(fallbackTx)
                log.info(s"Submitting fallback tx: $fallbackTxHash")
                val fallbackResult = cardano.ask(_.submit(fallbackTx))
                log.info(s"fallbackResult = $fallbackResult")
                // Since fallback is the same tx for all nodes, we can use awaitTx here.
                cardano.ask(_.awaitTx(fallbackTxHash))

                // Build and submit a vote
                // The idea for testing (tallying) is as follows:
                // every node should submit a minor block they produced.
                val turn = self.blockLeadTurn.get
                val voteBlock = blocksConfirmedL2.clone().dropRightInPlace(turn).lastOption.get

                log.info(s"voteBlock = $voteBlock")

                voteBlock.block.blockHeader.blockType match {
                    // Voting is possible
                    case Minor =>
                        // Temporary code for building Vote tx
                        val disputeAddress = DisputeResolutionScript.address(networkL1static)
                        val voteUtxoId =
                            cardano
                                .ask(_.utxosAtAddress(disputeAddress))
                                .find(u =>
                                    val datum = getVoteDatum(u)
                                    // TODO better way to get own key, which should be always defined
                                    val value = headPeerVKs.get(ownPeer).get
                                    val ownVk = hashVerificationKey(value)
                                    datum.peer.isDefined &&
                                    datum.peer.get == ownVk
                                ) match {
                                case Some(utxo) =>
                                    UtxoIdL1.apply(TxId(utxo.getTxHash), TxIx(utxo.getOutputIndex))
                                case None => throw RuntimeException("Vote UTxO was not found")
                            }

                        val ownAddress = AddressBech[L1](
                          account(TestPeer.valueOf(ownPeer.name)).getEnterpriseAddress.toBech32
                        )

                        // Temporarily
                        val ownAccount = account(TestPeer.valueOf(ownPeer.name))

                        val unresolvedTreasuryUtxo = UtxoIdL1.apply(fallbackTxHash, TxIx(0))
                        val recipe = VoteTxRecipe(
                          voteUtxoId,
                          // treasury is always the first output, though this is arguably not the best way to get it
                          unresolvedTreasuryUtxo,
                          mkOnchainBlockHeader(voteBlock.block.blockHeader),
                          voteBlock.l1Effect.asInstanceOf[MinorBlockL1Effect],
                          ownAddress,
                          ownAccount
                        )
                        log.info(s"Vote tx recipe: $recipe")
                        val Right(voteTx) = voteTxBuilder.buildVoteTxDraft(recipe)
                        val voteTxHash = txHash(voteTx)
                        log.info(s"Vote tx: ${serializeTxHex(voteTx)}")

                        log.info(s"Submitting vote tx: $voteTxHash")
                        val voteResult = cardano.ask(_.submit(voteTx))
                        log.info(s"voteResult = $voteResult")
                        cardano.ask(_.awaitTx(voteTxHash))

                        runTallying(unresolvedTreasuryUtxo, ownAccount)

                        runResolution(unresolvedTreasuryUtxo, ownAccount)

                        // Waiting till the resolved treasury appears
                        val resolvedTreasury = retry(RetryConfig.delayForever(3.seconds)) {

                            log.info("Checking for resolved treasury utxo")
                            val treasuryAddress = TreasuryValidatorScript.address(networkL1static)
                            val beaconTokenUnit = encodeHex(
                              this.headMintingPolicy.bytes ++ decodeHex(
                                this.beaconTokenName.tokenNameHex
                              )
                            )

                            // TODO: use more effective endpoint that based on vote tokens' assets.
                            cardano
                                .ask(_.utxosAtAddress(treasuryAddress))
                                .find(u =>
                                    fromData[TreasuryDatum](
                                      Interop.toScalusData(
                                        PlutusData
                                            .deserialize(HexUtil.decodeHexString(u.getInlineDatum))
                                      )
                                    ) match {
                                        case Resolved(_) =>
                                            u.getAmount.asScala
                                                .map(_.getUnit)
                                                .contains(beaconTokenUnit)
                                        case _ => false
                                    }
                                ) match {
                                case Some(utxo) =>
                                    UtxoIdL1(TxId(utxo.getTxHash), TxIx(utxo.getOutputIndex))
                                case None =>
                                    log.info("Resolved treasury utxo was not found")
                                    // just wait
                                    throw RuntimeException(
                                      "Waiting for resolved treasury"
                                    )
                            }
                        }

                        // The condition is not required, just a way to speed up tests a tad and simplify logs
                        if (turn == 0) {
                            val withdrawalTxHash = runWithdrawal(resolvedTreasury, ownAccount)
                            // The rest of treasury should be always the first output
                            val restTreasury = UtxoIdL1(withdrawalTxHash, TxIx(0))
                            runDeinit(restTreasury, ownAccount)
                        }

                    // Voting is not possible, the only way to go is to wait until dispute is over by its timeout.
                    // (Should not happen)
                    case _ => throw RuntimeException("Vote block is not a minor block, can't vote")
                }

                def getVoteDatum(a: model.Utxo) = {
                    fromData[VoteDatum](
                      Interop.toScalusData(
                        PlutusData
                            .deserialize(HexUtil.decodeHexString(a.getInlineDatum))
                      )
                    )
                }

                def getVoteUtxos: List[BBUtxo] = {
                    val voteTokenName = extractVoteTokenNameFromFallbackTx(
                      l2LastMajorRecord.l1PostDatedEffect.get
                    )

                    val disputeAddress = DisputeResolutionScript.address(networkL1static)

                    // Returns L1 state - a list of vote utxos sorted in ascending key order.
                    // TODO: use more effective endpoint that based on vote tokens' assets.
                    val sortedVoteUtxoIds =
                        cardano
                            .ask(_.utxosAtAddress(disputeAddress))
                            .filter(u =>
                                val voteTokenUnit = encodeHex(
                                  this.headMintingPolicy.bytes
                                ) + voteTokenName.tokenNameHex
                                val units = u.getAmount.asScala.map(_.getUnit)
                                units.contains(voteTokenUnit)
                            )
                            .sortWith((a, b) =>
                                val datumA = getVoteDatum(a)
                                val datumB = getVoteDatum(b)
                                datumA.key < datumB.key
                            )

                    log.info(s"sortedVoteUtxoIds: ${sortedVoteUtxoIds.map(_.getTxHash)}")
                    sortedVoteUtxoIds
                }

                // Move to Tx.scala-like module
                def getUtxoId(utxo: BBUtxo): UtxoIdL1 = UtxoIdL1.apply(
                  utxo.getTxHash |> TxId.apply,
                  utxo.getOutputIndex |> TxIx.apply
                )

                def runTallying(unresolvedTreasuryUtxo: UtxoIdL1, ownAccount: Account): Unit = {

                    type TallyTx = TxL1

                    def makeTallies(voteUtxos: List[BBUtxo]): List[TallyTx] = voteUtxos match
                        case x :: y :: zs => List(makeTally(x, y)) ++ makeTallies(zs)
                        case _            => List.empty

                    def makeTally(voteA: BBUtxo, voteB: BBUtxo): TallyTx = {
                        val recipe = TallyTxRecipe(
                          getUtxoId(voteA),
                          getUtxoId(voteB),
                          unresolvedTreasuryUtxo,
                          ownAccount
                        )
                        tallyTxBuilder.buildTallyTxDraft(recipe) match {
                            case Right(tx) => tx
                            case Left(err) =>
                                log.error(err)
                                throw RuntimeException(err)
                        }
                    }

                    retry(RetryConfig.delayForever(3.seconds)) {
                        log.info("Running tallying...")
                        val txs = getVoteUtxos |> makeTallies
                        log.debug(s"tallying round txs are: ${txs.map(serializeTxHex)}")
                        if txs.isEmpty then ()
                        else {
                            txs.foreach(t => cardano.ask(_.submit(t)))
                            throw RuntimeException(
                              "Some tallying txs have been submitted, the next round is needed"
                            )
                        }
                    }
                }

                def runResolution(unresolvedTreasuryUtxo: UtxoIdL1, ownAccount: Account): Unit = {
                    getVoteUtxos match {
                        case talliedVoteUtxo :: Nil =>
                            val talliedVote = getUtxoId(talliedVoteUtxo)
                            val recipe =
                                ResolutionTxRecipe(talliedVote, unresolvedTreasuryUtxo, ownAccount)

                            val resolutionTx = resolutionTxBuilder.buildResolutionTx(recipe) match {
                                case Right(tx) => tx
                                case Left(err) =>
                                    log.error(err)
                                    throw RuntimeException(err)
                            }

                            log.info(s"Resolution tx is: ${serializeTxHex(resolutionTx)}")
                            val submitResult = cardano.ask(_.submit(resolutionTx))
                            log.info(
                              s"resolution tx submit result (might be left for some nodes): $submitResult"
                            )

                            retry(RetryConfig.delayForever(3.seconds)) {
                                log.info("Running resolving...")
                                val votes = getVoteUtxos
                                log.debug(s"votes number is: ${votes.length}")
                                if votes.isEmpty then ()
                                else {
                                    // just wait
                                    throw RuntimeException(
                                      "Still see votes, waiting for the resolution tx to get through"
                                    )
                                }
                            }
                        case _vote1 :: _vote2 :: _vs =>
                            val msg = "More than one vote."
                            log.error(msg)
                            throw RuntimeException(msg)
                        case Nil =>
                            log.info("No votes found, likely resolution has been done.")
                    }
                }

                def runWithdrawal(resolvedTreasury: UtxoIdL1, ownAccount: Account): TxId = {

                    log.info("Running withdraw...")

                    // TODO: at this point a specific set of utxos should be restored
                    // Now, for testing we are assuming we can just use L2 ledger directly.
                    // Also, we now try to withdraw all utxos from the ledger in one go.
                    val utxos = stateL2.flushAndGetState
                    // Since we are removing all utxos, proof := g2
                    val proof = G2.generator.toCompressedByteString.toHex

                    val recipe = WithdrawTxRecipe(
                      utxos,
                      resolvedTreasury,
                      proof,
                      ownAccount
                    )

                    log.info(s"Withdraw tx recipe: $recipe")

                    val withdrawTx = withdrawTxBuilder.buildWithdrawTx(recipe) match {
                        case Right(tx) => tx
                        case Left(err) =>
                            log.error(err)
                            throw RuntimeException(err)
                    }

                    log.info(s"Withdraw tx is: ${serializeTxHex(withdrawTx)}")
                    val submitResult = cardano.ask(_.submit(withdrawTx))
                    log.info(s"Withdraw tx submission result is: $submitResult")
                    submitResult match {
                        case Right(txHash) =>
                            log.info(s"Withdraw tx submitted, tx hash id is: $txHash")
                            cardano.ask(_.awaitTx(txHash))
                            txHash
                        case Left(err) =>
                            log.error(s"Withdraw tx submission failed with: $err")
                            throw RuntimeException(err)
                    }
                }

                def runDeinit(resolvedTreasury: UtxoIdL1, ownAccount: Account): Unit = {
                    // Build and propose a deinit transaction.
                    // For testing purposes we are going to build a tx that:
                    // - sends all funds from the treasury to the initial seeder
                    // - burns all head tokens
                    val recipe = DeinitTxRecipe(
                      resolvedTreasury,
                      self.seedAddress,
                      self.headNativeScript,
                      self.headMintingPolicy,
                      ownAccount
                    )

                    val Right(deinitTxDraft) = deinitTxBuilder.buildDeinitTxDraft(recipe)

                    val headPeers = this.headPeers

                    // Fire and forget for now, arguably should be .ask
                    network.tell(_.reqDeinit(ReqDeinit(deinitTxDraft, headPeers)))
                    log.info("Waiting for the deinit tx...")
                    cardano.ask(
                      _.awaitTx(txHash(deinitTxDraft), RetryConfig.delay(30, 1.second))
                    )
//                    // This is the end of the head
//                    System.exit(0)
                }
            }
            end runTestDispute

            // FIXME: this should be removed in the production version (or moved somewhere)
            if isQuitConsensusImmediately then {
                log.warn("Running test dispute")
                runTestDispute()
            }

            // Done
            log.info(
              s"nextBlockNum: $nextBlockNum, isBlockLeader: ${this.isBlockLeader}, isBlockPending: ${this.isBlockPending}"
            )

        private def setLeaderFlag(nextBlockNum: Int): Unit = {
            self.isBlockLeader = Some(nextBlockNum % headPeerVKs.size == this.blockLeadTurn)
            log.info(s"isBlockLeader: ${self.isBlockLeader} for block ${nextBlockNum}")
        }

        override def applyBlockEvents(
            blockNum: Int,
            eventsValid: Seq[(TxId, L2EventLabel)],
            eventsInvalid: Seq[(TxId, L2EventLabel)],
            depositsAbsorbed: Seq[UtxoIdL1]
        ): Map[TxId, L2Event] =

            def applyValidEvent(blockNum: Int, txId: TxId): (TxId, L2Event) =
                log.info(s"Marking event $txId as validated by block $blockNum")
                self.poolEventsL2.indexWhere(e => e.getEventId == txId) match
                    case -1 => throw IllegalStateException(s"pool event $txId was not found")
                    case i =>
                        val event = self.poolEventsL2.remove(i)
                        self.nonGenesisEventsConfirmedL2.append((event, blockNum))
                        // Remove possible duplicates form the pool
                        self.poolEventsL2.filter(e => e.getEventId == event.getEventId)
                            |> self.poolEventsL2.subtractAll
//                        // Dump L2 tx
//                        TxDump.dumpL2Tx(event match
//                            case L2EventTransaction(_, transaction) =>
//                                HydrozoaL2Ledger.asTxL2(transaction)._1
//                            case L2EventWithdrawal(_, withdrawal) =>
//                                HydrozoaL2Ledger.asTxL2(withdrawal)._1
//                        )
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
            log.info(s"Pool events before removing: ${self.poolEventsL2.size}")

            log.info(s"Removing invalid events: $eventsInvalid")
            self.poolEventsL2.filter(e => eventsInvalid.map(_._1).contains(e.getEventId))
                |> self.poolEventsL2.subtractAll

            log.info(s"Pool events after removing: ${self.poolEventsL2.size}")

            // Metrics - FIXME: factor out
            val txs = self.poolEventsL2.map(l2EventLabel).count(_ == L2EventTransactionLabel)
            metrics.tell(m =>
                m.setPoolEventsL2(L2EventTransactionLabel, txs)
                m.setPoolEventsL2(L2EventWithdrawalLabel, self.poolEventsL2.size - txs)
            )

            validEvents.toMap

        override def requestFinalization(): Unit =
            log.info("Head finalization has been requested, next block will be final.")
            self.mbIsNextBlockFinal = Some(true)

        override def isNextBlockFinal: Boolean = self.mbIsNextBlockFinal.get

        override def switchToFinalizingPhase(): Unit =
            log.info("Putting head into Finalizing phase.")
            self.headPhase = Finalizing
            log.info("Try to produce the final block...")
            tryProduceBlock(true)

    private class FinalizingPhaseImpl extends FinalizingPhaseReaderImpl with FinalizingPhase:
        def stateL2: L2LedgerModule[HydrozoaHeadLedger, HydrozoaL2Ledger.LedgerUtxoSetOpaque] =
            self.stateL2.get

        override def tryProduceFinalBlock(
            force: Boolean
        ): Either[String, Block] =
            if (self.autonomousBlocks || force)
                && this.isBlockLeader && !self.isBlockPending.get
            then
                log.info(s"Trying to produce the final block ${l2Tip.blockHeader.blockNum + 1}...")

                val tipHeader = l2Tip.blockHeader
                blockProductionActor.ask(
                  _.produceBlock(
                    stateL2.cloneForBlockProducer(),
                    Seq.empty,
                    TaggedUtxoSet.apply(),
                    tipHeader,
                    timeCurrent,
                    true
                  )
                ) match
                    case Right(block, utxosActive, _, utxosWithdrawn, mbGenesis) =>
                        self.pendingOwnBlock = Some(
                          OwnBlock(block, utxosActive, utxosWithdrawn, mbGenesis)
                        )
                        self.isBlockPending = Some(true)
                        Right(block)
                    case Left(err) =>
                        // TODO: this arguably should never happen
                        // setLeaderFlag(tipHeader.blockNum + 1)
                        Left(err)
            else
                val msg = s"Block ${l2Tip.blockHeader.blockNum + 1} is not going to be produced: " +
                    s"autonomousBlocks=${self.autonomousBlocks} " +
                    s"force=$force " +
                    s"isBlockLeader=${this.isBlockLeader} " +
                    s"isBlockPending=${self.isBlockPending} "
                log.info(msg)
                Left(msg)

        // TODO: duplication with open phase
        override def newTreasuryUtxo(treasuryUtxo: TreasuryUtxo): Unit =
            log.info("Net treasury utxo")
            self.stateL1.get.treasuryUtxo = treasuryUtxo
            metrics.tell(_.setTreasuryLiquidity(treasuryUtxo.unTag.output.coins.toLong))

        override def finalizeHead(record: BlockRecord): Unit =
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
            self.mbIsNextBlockFinal = None
            self.poolEventsL2.clear()
            self.poolDeposits.clear()
            self.blocksConfirmedL2.clear()
            // Adding final block record, it's used by tests for now
            self.blocksConfirmedL2.append(record)
            self.nonGenesisEventsConfirmedL2.clear()
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
                        .count(_ == L2EventTransactionLabel)
                val validWds = body.eventsValid.size - validTxs
                val invalidTxs = body.eventsInvalid
                    .map(_._2)
                    .count(_ == L2EventTransactionLabel)
                val invalidWds = body.eventsInvalid.size - invalidTxs

                m.observeBlockSize(L2EventTransactionLabel, validTxs)
                m.observeBlockSize(L2EventWithdrawalLabel, validWds)

                m.incEventsL2Handled(L2EventTransactionLabel, true, validTxs)
                m.incEventsL2Handled(L2EventTransactionLabel, false, invalidTxs)
                m.incEventsL2Handled(L2EventWithdrawalLabel, true, validWds)
                m.incEventsL2Handled(L2EventWithdrawalLabel, true, invalidWds)
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

            record.l1Effect |> maybeMultisigL1Tx match {
                case Some(finalizationTx) =>
                    // Submit finalization tx
                    log.info(s"Submitting finalization tx: ${txHash(finalizationTx)}")
                    val res = cardano.ask(_.submit(finalizationTx))
                    log.info(s"Finalization tx submission result is: ${res}")
                case _ => assert(false, "Impossible: finalization tx should always present")
            }

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
          headNativeScript = params.headNativeScript.toHydrozoaNativeScript,
          headMintingPolicy = params.headMintingPolicy,
          headAddress = params.headAddress,
          beaconTokenName = params.beaconTokenName,
          seedAddress = params.seedAddress,
          initTx = params.initTx,
          initializedOn = params.initializedOn,
          autonomousBlocks = params.autonomousBlocks
        )

// TODO: we can optimize it (probably parameterized on block type)
case class BlockRecord(
    block: Block,
    l1Effect: L1BlockEffect,
    // Is not defined for minor blocks
    l1PostDatedEffect: L1PostDatedBlockEffect,
    l2Effect: L2BlockEffect
)

case class OwnBlock(
    block: Block,
    utxosActive: HydrozoaL2Ledger.LedgerUtxoSetOpaque,
    utxosWithdrawn: UtxoSetL2,
    mbGenesis: Option[(TxId, L2Genesis)]
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

// It's not an "effect", but rather its parts - all nodes signatures that
// can be turned into a voting transaction.
type MinorBlockL1Effect = Seq[Ed25519Signature]

// This is not defined for minor and final blocks, so we have to use Option here.
// Probably we can do it better.
type L1PostDatedBlockEffect = Option[TxL1]

// Always None for final block, always Some for minor block,
// None or Some for major (None in case a major block is produced
// to refresh a L1 treasury utxo.
type L2BlockEffect = Option[HydrozoaL2Ledger.LedgerUtxoSetOpaque]

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
