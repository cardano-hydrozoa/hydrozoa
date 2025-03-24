package hydrozoa.node.state
import hydrozoa.*
import hydrozoa.l1.multisig.state.*
import hydrozoa.l1.multisig.tx.{FinalizationTx, InitializationTx, SettlementTx}
import hydrozoa.l2.block.BlockTypeL2.Major
import hydrozoa.l2.block.{Block, BlockTypeL2, zeroBlock}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.l2.ledger.*
import hydrozoa.l2.ledger.event.NonGenesisL2EventLabel
import hydrozoa.node.server.*
import hydrozoa.node.state.HeadPhase.{Finalized, Finalizing, Initializing, Open}

import scala.collection.mutable

enum HeadPhase:
    case Initializing
    case Open
    case Finalizing
    case Finalized
    // case Dispute
    // case Closed

trait HeadStateReader:
    // All regimes/phases
    def currentState: HeadPhase
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
    def currentState: HeadPhase
    // Phase-specific APIs
    def initializingPhase[A](foo: InitializingPhase => A): A
    def openPhase[A](foo: OpenPhase => A): A
    def finalizingPhase[A](foo: FinalizingPhase => A): A

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Readers hierarchy
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sealed trait HeadStateReaderApi

sealed trait InitializingPhaseReader extends HeadStateReaderApi:
    def headPeers: List[Peer]

sealed trait MultisigRegimeReader extends HeadStateReaderApi:
    def headNativeScript: NativeScript
    def headBechAddress: AddressBechL1
    def beaconTokenName: String // TODO: use more concrete type
    def seedAddress: AddressBechL1
    def currentTreasuryRef: UtxoIdL1
    def stateL1: MultisigHeadStateL1

sealed trait OpenPhaseReader extends MultisigRegimeReader:
    def immutablePoolEventsL2: Seq[NonGenesisL2]
    def immutableBlocksConfirmedL2: Seq[BlockRecord]
    def immutableEventsConfirmedL2: Seq[(EventL2, Int)]
    def l2Tip: Block
    def l2LastMajor: Block
    def peekDeposits: DepositUtxos
    def depositTimingParams: (UDiffTimeMilli, UDiffTimeMilli, UDiffTimeMilli) // TODO: explicit type

sealed trait FinalizingPhaseReader extends MultisigRegimeReader:
    def l2Tip: Block

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Head state manager hierarchy
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

sealed trait HeadStateApi

sealed trait InitializingPhase extends HeadStateApi with InitializingPhaseReader:
    def openHead(
        headParams: HeadParams,
        headNativeScript: NativeScript,
        headBechAddress: AddressBechL1,
        beaconTokenName: String,
        treasuryUtxo: TreasuryUtxo,
        seedAddress: AddressBechL1
    ): Unit

sealed trait OpenPhase extends HeadStateApi with OpenPhaseReader:
    def enqueueDeposit(deposit: DepositUtxo): Unit
    def poolEventL2(event: NonGenesisL2): Unit
    def newTreasury(txId: TxId, txIx: TxIx, coins: BigInt): Unit
    def stateL2: AdaSimpleLedger[THydrozoaHead]
    def addBlock(block: BlockRecord): Unit
    def confirmMempoolEvents(
        blockNum: Int,
        eventsValid: Seq[(TxId, NonGenesisL2EventLabel)],
        mbGenesis: Option[(TxId, SimpleGenesis)],
        eventsInvalid: Seq[(TxId, NonGenesisL2EventLabel)]
    ): Unit
    def removeAbsorbedDeposits(deposits: Seq[UtxoId[L1]]): Unit
    def finalizeHead(): Unit

sealed trait FinalizingPhase extends HeadStateApi with FinalizingPhaseReader:
    def closeHead(block: BlockRecord): Unit
    def stateL2: AdaSimpleLedger[THydrozoaHead]
    def confirmValidMempoolEvents(
        blockNum: Int,
        eventsValid: Seq[(TxId, NonGenesisL2EventLabel)]
    ): Unit

/** It's global in a sense that the same value spans over all possible states a head might be in.
  * Probably we can split it up in the future. Doesn't expose fiels; instead implements
  * HeadStateReader and HeadState methods to work with specific regimes/phases.
  */
class HeadStateGlobal(var headPhase: HeadPhase, val headPeers: List[Peer])
    extends HeadStateReader
    with HeadState {
    self =>

    //
    def currentState: HeadPhase = headPhase

    //
    private var initialTreasury: Option[TreasuryUtxo] = None
    private var headParams: Option[HeadParams] = None
    private var headNativeScript: Option[NativeScript] = None
    private var headBechAddress: Option[AddressBechL1] =
        None // FIXME: can be obtained from stateL1.treasuryUtxo
    private var beaconTokenName: Option[String] = None // FIXME: use more sHeadStateApipecific type
    private var seedAddress: Option[AddressBechL1] = None

    // Hydrozoa blocks
    private val genesisBlock: Unit = () // FIXME: use instead zeroBlock
    private val blocksConfirmedL2: mutable.Buffer[BlockRecord] = mutable.Buffer()
    // Currently pending block
    private val blockPending: Option[BlockRecord] = None

    private val eventsConfirmedL2: mutable.Buffer[(EventL2, Int)] = mutable.Buffer()
    private val poolEventsL2: mutable.Buffer[NonGenesisL2] = mutable.Buffer()
    private var finalizing: Option[Boolean] = None
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
        def headNativeScript: NativeScript = self.headNativeScript.get
        def beaconTokenName: String = self.beaconTokenName.get
        def seedAddress: AddressBechL1 = self.seedAddress.get
        def currentTreasuryRef: UtxoIdL1 = self.stateL1.get.treasuryUtxo.ref
        def headBechAddress: AddressBechL1 = self.headBechAddress.get
        def stateL1: MultisigHeadStateL1 = self.stateL1.get

    private class InitializingPhaseReaderImpl extends InitializingPhaseReader:
        def headPeers: List[Peer] = self.headPeers

    private class OpenPhaseReaderImpl extends MultisigRegimeReaderImpl with OpenPhaseReader:
        def immutablePoolEventsL2: Seq[NonGenesisL2] = self.poolEventsL2.toSeq
        def immutableBlocksConfirmedL2: Seq[BlockRecord] = self.blocksConfirmedL2.toSeq
        def immutableEventsConfirmedL2: Seq[(EventL2, Int)] = self.eventsConfirmedL2.toSeq
        def l2Tip: Block = l2Tip_
        def l2LastMajor: Block = self.blocksConfirmedL2
            .findLast(_.block.blockHeader.blockType == Major)
            .map(_.block)
            .getOrElse(zeroBlock)
        def peekDeposits: DepositUtxos = UtxoSet(self.stateL1.get.depositUtxos)
        def depositTimingParams: (UDiffTimeMilli, UDiffTimeMilli, UDiffTimeMilli) =
            val headParams = self.headParams.get
            val consensusParams = headParams.l2ConsensusParams
            (
              consensusParams.depositMarginMaturity,
              headParams.minimalDepositWindow,
              consensusParams.depositMarginExpiry
            )

    private class FinalizingPhaseReaderImpl
        extends MultisigRegimeReaderImpl
        with FinalizingPhaseReader:
        def l2Tip: Block = l2Tip_

    private def l2Tip_ = blocksConfirmedL2.map(_.block).lastOption.getOrElse(zeroBlock)

    // Subclasses that implements APIs (writers)
    private final class InitializingPhaseImpl
        extends InitializingPhaseReaderImpl
        with InitializingPhase:
        def openHead(
            headParams: HeadParams,
            headNativeScript: NativeScript,
            headBechAddress: AddressBechL1,
            beaconTokenName: String,
            treasuryUtxo: TreasuryUtxo,
            seedAddress: AddressBechL1
        ): Unit =
            self.headPhase = Open
            self.headParams = Some(headParams)
            self.headNativeScript = Some(headNativeScript)
            self.headBechAddress = Some(headBechAddress)
            self.beaconTokenName = Some(beaconTokenName)
            self.initialTreasury = Some(treasuryUtxo)
            self.seedAddress = Some(seedAddress)
            self.stateL1 = Some(MultisigHeadStateL1(treasuryUtxo))
            self.stateL2 = Some(AdaSimpleLedger())

    private class OpenPhaseImpl extends OpenPhaseReaderImpl with OpenPhase:

        def enqueueDeposit(d: DepositUtxo): Unit =
            self.stateL1.map(s => s.depositUtxos.map.put(d.ref, d.output))

        def poolEventL2(event: NonGenesisL2): Unit = self.poolEventsL2.append(event)

        def newTreasury(txId: TxId, txIx: TxIx, coins: BigInt): Unit =
            self.stateL1.get.treasuryUtxo =
                mkUtxo[L1, TreasuryTag](txId, txIx, self.headBechAddress.get, coins)

        def stateL2: AdaSimpleLedger[THydrozoaHead] = self.stateL2.get

        def addBlock(block: BlockRecord): Unit = self.blocksConfirmedL2.append(block)

        // FIXME: this is too complex for state management, should be a part of effect
        def confirmMempoolEvents(
            blockNum: Int,
            eventsValid: Seq[(TxId, NonGenesisL2EventLabel)],
            mbGenesis: Option[(TxId, SimpleGenesis)],
            eventsInvalid: Seq[(TxId, NonGenesisL2EventLabel)]
        ): Unit =
            // 1. Add valid events
            eventsValid.foreach((txId, _) => markEventAsValid(blockNum, txId))
            // 2. Add genesis if exists
            mbGenesis.foreach((txId, genesis) =>
                // FIXME: timeCurrent
                val event = AdaSimpleLedger.mkGenesisEvent(genesis)
                self.eventsConfirmedL2.append((event, blockNum))
                // Dump L2 tx
                TxDump.dumpL2Tx(AdaSimpleLedger.asTxL2(genesis)._1)
            )
            // 3. Remove invalid events
            eventsInvalid.foreach((txId, _) =>
                self.poolEventsL2.indexWhere(e => e.getEventId == txId) match
                    case -1 => throw IllegalStateException(s"pool event $txId was not found")
                    case i  => self.poolEventsL2.remove(i)
            )

        def removeAbsorbedDeposits(deposits: Seq[UtxoId[L1]]): Unit =
            deposits.foreach(self.stateL1.get.depositUtxos.map.remove)

        def finalizeHead(): Unit = headPhase = Finalizing

    private class FinalizingPhaseImpl extends FinalizingPhaseReaderImpl with FinalizingPhase:
        def stateL2: AdaSimpleLedger[THydrozoaHead] = self.stateL2.get
        def closeHead(finalBlock: BlockRecord): Unit =
            self.blocksConfirmedL2.append(finalBlock)
            self.headPhase = Finalized
        def confirmValidMempoolEvents(
            blockNum: Int,
            eventsValid: Seq[(TxId, NonGenesisL2EventLabel)]
        ): Unit = eventsValid.foreach((txId, _) => markEventAsValid(blockNum, txId))

    private def markEventAsValid(blockNum: Int, txId: EventHash): Unit = {
        self.poolEventsL2.indexWhere(e => e.getEventId == txId) match
            case -1 => throw IllegalStateException(s"pool event $txId was not found")
            case i =>
                val event = self.poolEventsL2.remove(i)
                self.eventsConfirmedL2.append((event, blockNum))
                // Dump L2 tx
                TxDump.dumpL2Tx(event match
                    case TransactionL2(_, transaction) =>
                        AdaSimpleLedger.asTxL2(transaction)._1
                    case WithdrawalL2(_, withdrawal) =>
                        AdaSimpleLedger.asTxL2(withdrawal)._1
                )
    }
}

object HeadStateGlobal:
    def apply(peers: List[Peer]): HeadStateGlobal =
        new HeadStateGlobal(headPhase = Initializing, headPeers = peers)

case class BlockRecord(
    block: Block,
    l1Effect: L1BlockEffect,
    l1PostDatedEffect: L1PostDatedBlockEffect,
    l2Effect: L2BlockEffect
)

type L1BlockEffect = InitializationTx | SettlementTx | FinalizationTx | MinorBlockL1Effect
type MinorBlockL1Effect = Unit
type L1PostDatedBlockEffect = Unit
type L2BlockEffect = Unit
