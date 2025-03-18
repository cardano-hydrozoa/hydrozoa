package hydrozoa.node.server
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.l1.multisig.state.*
import hydrozoa.l2.block.BlockTypeL2.Major
import hydrozoa.l2.block.{Block, MempoolEventTypeL2, zeroBlock}
import hydrozoa.l2.consensus.{HeadParams, L2ConsensusParams}
import hydrozoa.l2.event.{L2Event, L2GenesisEvent, L2NonGenesisEvent}
import hydrozoa.l2.ledger.{AdaSimpleLedger, SimpleGenesis, THydrozoaHead}

import scala.collection.mutable

/* The head state. Currently, we support only one head per a [set] of nodes.
 */

sealed trait HeadState

sealed trait MultisigRegime

sealed trait RuleBasedRegime

private case class Initializing(nodes: Array[PeerNode]) extends HeadState with MultisigRegime

private case class Open(
    headParams: HeadParams,
    headNativeScript: NativeScript,
    headBechAddress: AddressBechL1, // FIXME: can be obtained from stateL1.treasuryUtxo
    beaconTokenName: String, // FIXME: use more specific type
    seedAddress: AddressBechL1
)(initialTreasury: TreasuryUtxo)
    extends HeadState
    with MultisigRegime {
    // FIXME: use instead zeroBlock
    // FIXME: keep block record
    val genesisBlock: Unit = ()
    val blocksConfirmedL2: mutable.Buffer[Block] = mutable.Buffer() // BlockRecord
    // FIXME:
    val blockPending: Option[BlockRecord] = None
    val eventsConfirmedL2: mutable.Buffer[(L2Event, Int)] = mutable.Buffer()
    val poolEventsL2: mutable.Buffer[L2NonGenesisEvent] = mutable.Buffer()
    var finalizing = false
    // TODO: add peers
    var stateL1: MultisigHeadStateL1 = MultisigHeadStateL1.empty(initialTreasury)
    val stateL2: AdaSimpleLedger[THydrozoaHead] = AdaSimpleLedger()
}

private case class Finalizing() extends HeadState with MultisigRegime

private case class Closed() extends HeadState with RuleBasedRegime

private case class Disputing() extends HeadState with RuleBasedRegime

/*
 * APIs for every possible state of the head + additional state when the head is absent.
 */

trait StateApi

trait HeadAbsentState extends StateApi:
    def initializeHead(
        headParams: HeadParams,
        headNativeScript: NativeScript,
        headBechAddress: AddressBechL1,
        beaconTokenName: String,
        treasuryUtxo: TreasuryUtxo,
        seedAddress: AddressBechL1
    ): Unit

trait OpenNodeState extends StateApi:
    def currentTreasuryRef: OutputRefL1
    def headNativeScript: NativeScript
    def headBechAddress: AddressBechL1
    def beaconTokenName: String // TODO: use more concrete type
    def seedAddress: AddressBechL1
    def depositTimingParams: (UDiffTime, UDiffTime, UDiffTime) // TODO: explicit type
    def peekDeposits: DepositUtxos
    def immutablePoolEventsL2: Seq[L2NonGenesisEvent]
    def immutableBlocksConfirmedL2: Seq[Block]
    def immutableEventsConfirmedL2: Seq[(L2Event, Int)]
    def enqueueDeposit(deposit: DepositUtxo): Unit
    def poolEventL2(event: L2NonGenesisEvent): Unit
    def stateL1: MultisigHeadStateL1
    def stateL2: AdaSimpleLedger[THydrozoaHead]
    def l2Tip: Block
    def l2LastMajor: Block
    def finalizing: Boolean
    def setFinalizing: Unit
    def newTreasury(txId: TxId, txIx: TxIx, coins: BigInt): Unit
    def addBlock(block: Block): Unit
    def confirmMempoolEvents(
        blockNum: Int,
        eventsValid: Seq[(TxId, MempoolEventTypeL2)],
        mbGenesis: Option[(TxId, SimpleGenesis)],
        eventsInvalid: Seq[(TxId, MempoolEventTypeL2)]
    ): Unit
    def removeAbsorbedDeposits(deposits: Seq[OutputRef[L1]]): Unit
    def finalizeHead(): Unit

/** The class that provides read-write access to the state.
  * @param log
  */
class NodeStateManager(log: Logger) { self =>

    private var knownPee: mutable.Set[PeerNode] = mutable.Set.empty

    private var headState: Option[HeadState] = None

    private class HeadAbsentStateImpl extends HeadAbsentState:
        def initializeHead(
            headParams: HeadParams,
            headNativeScript: NativeScript,
            headBechAddress: AddressBechL1,
            beaconTokenName: String,
            treasuryUtxo: TreasuryUtxo,
            seedAddress: AddressBechL1
        ): Unit =
            val newHead = Open(
              headParams,
              headNativeScript,
              headBechAddress,
              beaconTokenName,
              seedAddress
            )(treasuryUtxo)
            self.headState = Some(newHead)

    private class OpenNodeStateImpl(openState: Open) extends OpenNodeState:

        def currentTreasuryRef: OutputRefL1 = openState.stateL1.treasuryUtxo.ref
        def headNativeScript: NativeScript = openState.headNativeScript
        def headBechAddress: AddressBechL1 = openState.headBechAddress
        def beaconTokenName: String = openState.beaconTokenName
        def seedAddress: AddressBechL1 = openState.seedAddress

        def depositTimingParams: (UDiffTime, UDiffTime, UDiffTime) =
            val Open(
              HeadParams(
                L2ConsensusParams(depositMarginMaturity, depositMarginExpiry),
                minimalDepositWindow
              ),
              _,
              a,
              _,
              _
            ) = openState

            (depositMarginMaturity, minimalDepositWindow, depositMarginExpiry)

        def immutablePoolEventsL2: Seq[L2NonGenesisEvent] = openState.poolEventsL2.toSeq
        def immutableBlocksConfirmedL2: Seq[Block] = openState.blocksConfirmedL2.toSeq
        def immutableEventsConfirmedL2: Seq[(L2Event, Int)] = openState.eventsConfirmedL2.toSeq
        def peekDeposits: DepositUtxos = UtxoSet(openState.stateL1.depositUtxos)
        def enqueueDeposit(d: DepositUtxo): Unit =
            openState.stateL1.depositUtxos.map.put(d.ref, d.output)
        def poolEventL2(event: L2NonGenesisEvent): Unit = openState.poolEventsL2.append(event)
        def finalizing: Boolean = openState.finalizing
        def setFinalizing: Unit = openState.finalizing = true
        def stateL1: MultisigHeadStateL1 = openState.stateL1
        def stateL2: AdaSimpleLedger[THydrozoaHead] = openState.stateL2
        def l2Tip: Block = openState.blocksConfirmedL2.lastOption.getOrElse(zeroBlock)
        def l2LastMajor: Block = openState.blocksConfirmedL2
            .findLast(_.blockHeader.blockType == Major)
            .getOrElse(zeroBlock)

        def newTreasury(txId: TxId, txIx: TxIx, coins: BigInt): Unit =
            log.info(s"New treasury utxo is $openState.treasuryRef")
            openState.stateL1.treasuryUtxo =
                mkUtxo[L1, TreasuryTag](txId, txIx, headBechAddress, coins)

        def addBlock(block: Block): Unit = openState.blocksConfirmedL2.append(block)

        def confirmMempoolEvents(
            blockNum: Int,
            eventsValid: Seq[(TxId, MempoolEventTypeL2)],
            mbGenesis: Option[(TxId, SimpleGenesis)],
            eventsInvalid: Seq[(TxId, MempoolEventTypeL2)]
        ): Unit =
            // Add valid events
            eventsValid.foreach((txId, _) =>
                openState.poolEventsL2.indexWhere(e => e.getEventId == txId) match
                    case -1 => throw IllegalStateException(s"pool event $txId was not found")
                    case i =>
                        val event = openState.poolEventsL2.remove(i)
                        openState.eventsConfirmedL2.append((event, blockNum))
                    // FIXME:
                    // val (tx, _) = stateL2.adopt(event)
                    // TxDump.dumpTx(tx)
            )
            // 2. Add genesis if exists
            mbGenesis.foreach((txId, g) =>
                // FIXME: timeCurrent
                val event = L2GenesisEvent(timeCurrent, txId, g)
                openState.eventsConfirmedL2.append((event, blockNum))
                // FIXME:
                // val (tx, _) = stateL2.adopt(event)
                // TxDump.dumpTx(tx)
            )
            // 3. Remove invalid events
            eventsInvalid.foreach((txId, _) =>
                openState.poolEventsL2.indexWhere(e => e.getEventId == txId) match
                    case -1 => throw IllegalStateException(s"pool event $txId was not found")
                    case i  => openState.poolEventsL2.remove(i)
            )

        def removeAbsorbedDeposits(deposits: Seq[OutputRef[L1]]): Unit =
            deposits.foreach(openState.stateL1.depositUtxos.map.remove)

        def finalizeHead(): Unit =
            headState = None

    def asAbsent[A](foo: HeadAbsentState => A): A =
        headState match
            case None => foo(HeadAbsentStateImpl())
            // Should be this: (initializing state is missing now).
            // case Some(Initializing(_)) => foo(HeadAbsentStateImpl())
            case _ =>
                throw IllegalStateException("The head is missing or not in Initializing state.")

    def asOpen[A](foo: OpenNodeState => A): A =
        headState match
            case Some(open: Open) => foo(OpenNodeStateImpl(open))
            case _ => throw IllegalStateException("The head is missing or not in a Open state.")

}

class HeadStateReader(manager: NodeStateManager) {
    def headNativeScript: NativeScript = manager.asOpen(_.headNativeScript)
    def beaconTokenName: String = manager.asOpen(_.beaconTokenName)
    def headBechAddress: AddressBechL1 = manager.asOpen(_.headBechAddress)
    def depositTimingParams: (UDiffTime, UDiffTime, UDiffTime) =
        manager.asOpen(_.depositTimingParams)
    def currentTreasuryRef: OutputRefL1 = manager.asOpen(_.currentTreasuryRef)
    def seedAddress: AddressBechL1 = manager.asOpen(_.seedAddress)
}

/** Represent a node (Hydrozoa process) run by a peer (a user/operator).
  */
case class PeerNode()

case class BlockRecord()
// Remove

case class SettledDeposit(
    txId: TxId,
    txIx: TxIx
)
