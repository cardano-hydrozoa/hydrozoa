package hydrozoa.node.server
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.l2.block.BlockTypeL2.Major
import hydrozoa.l2.block.{Block, zeroBlock}
import hydrozoa.l2.consensus.{HeadParams, L2ConsensusParams}
import hydrozoa.l2.event.{L2Event, L2NonGenesisEvent}
import hydrozoa.l2.ledger.{AdaSimpleLedger, NoopVerifier}

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
    val blocksConfirmedL2: mutable.Seq[Block] = mutable.Seq[Block]()
    val confirmedEventsL2: mutable.Set[L2Event] = mutable.Set()
    val poolEventsL2: mutable.Set[L2NonGenesisEvent] = mutable.Set()
    var finalizing = false
    // TODO: peers
    var stateL1 = MultisigHeadStateL1.empty(initialTreasury)
    val stateL2 = AdaSimpleLedger(NoopVerifier)

    def timeCurrent(): Unit = ()

    // Additional to spec, likely will be gone
//    val awaitingDeposits: mutable.Set[AwaitingDeposit] = mutable.Set[AwaitingDeposit]()
//    var treasuryRef: Option[(TxId, TxIx)] = None
    var majorVersion = 0 // FIXME: use blocksConfirmedL2
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
    def currentMajorVersion: Int
    def currentTreasuryRef: (TxId, TxIx)
    def headNativeScript: NativeScript
    def headBechAddress: AddressBechL1
    def beaconTokenName: String // TODO: use more concrete type
    def seedAddress: AddressBechL1
    def depositTimingParams: (UDiffTime, UDiffTime, UDiffTime) // TODO: explicit type
    def peekDeposits: DepositUtxos
    def poolEventsL2: mutable.Set[L2NonGenesisEvent]
    def enqueueDeposit(deposit: DepositUtxo): Unit
    def stateL1: MultisigHeadStateL1
    def stateL2: AdaSimpleLedger
    def l2Tip: Block
    def l2LastMajor: Block
    def finalizing: Boolean
    def majorBlockL2Effect(
        txId: TxId,
        txIx: TxIx,
        newMajor: Int,
        absorbedDeposits: Set[SettledDeposit]
    ): Unit
    def finalizeHead(): Unit

/** The class that provides read-write access to the state.
  * @param log
  */
class NodeStateManager(log: Logger) { self =>

    private var knownPeerNodes: mutable.Set[PeerNode] = mutable.Set.empty

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
        def currentMajorVersion: Int = openState.majorVersion

        def currentTreasuryRef: (TxId, TxIx) =
            val ref = openState.stateL1.treasuryUtxo.ref
            (ref.id, ref.ix)

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

        def poolEventsL2 = openState.poolEventsL2

        def peekDeposits: DepositUtxos = UtxoSet(openState.stateL1.depositUtxos)
        def enqueueDeposit(d: DepositUtxo): Unit =
            openState.stateL1.depositUtxos.map.put(d.ref, d.output)
        def finalizing: Boolean = openState.finalizing
        def stateL1: MultisigHeadStateL1 = openState.stateL1
        def stateL2: AdaSimpleLedger = openState.stateL2

        def l2Tip: Block = openState.blocksConfirmedL2.lastOption.getOrElse(zeroBlock)
        def l2LastMajor: Block = openState.blocksConfirmedL2
            .findLast(_.blockHeader.blockType == Major)
            .getOrElse(zeroBlock)

        // FIXME: review
        def majorBlockL2Effect(
            txId: TxId,
            txIx: TxIx,
            newMajor: Int,
            absorbedDeposits: Set[SettledDeposit]
        ): Unit =
            if newMajor == openState.majorVersion + 1 then
                // TODO: verify all absorbed deposits are on the list
                // TODO: atomicity
                // TODO: create L2 utxos
                absorbedDeposits
                    .map(sd => mkOutputRef(sd.txId, sd.txIx))
                    .map(openState.stateL1.depositUtxos.map.remove)
                log.info(s"Settled deposits: $absorbedDeposits")
                openState.majorVersion = newMajor
                log.info(s"Step into next major version $newMajor")
                openState.stateL1.treasuryUtxo = ??? // treasuryRef = Some(txId, txIx)
                log.info(s"New treasury utxo is $openState.treasuryRef")
            else
                throw IllegalStateException(
                  s"Can't step into wrong major version, expected: " +
                      s"${openState.majorVersion + 1}, got: $newMajor"
                )

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
    def currentTreasuryRef: (TxId, TxIx) = manager.asOpen(_.currentTreasuryRef)
    def seedAddress: AddressBechL1 = manager.asOpen(_.seedAddress)
}

/** Represent a node (Hydrozoa process) run by a peer (a user/operator).
  */
case class PeerNode()

// Additional stuff

type DepositUtxo = Utxo[L1, DepositTag]
type DepositUtxos = UtxoSet[L1, DepositTag]
type MutableDepositUtxos = MutableUtxoSet[L1, DepositTag]

type RolloutUtxo = Utxo[L1, RolloutTag]
type RolloutUtxos = UtxoSet[L1, RolloutTag]
type MutableRolloutUtxos = MutableUtxoSet[L1, RolloutTag]

type TreasuryUtxo = Utxo[L1, TreasuryTag]

case class MultisigHeadStateL1(
    var treasuryUtxo: TreasuryUtxo,
    depositUtxos: MutableDepositUtxos,
    rolloutUtxos: MutableRolloutUtxos
)

// tags
final class DepositTag
final class RolloutTag
final class TreasuryTag

object MultisigHeadStateL1:
    def empty(treasuryUtxo: TreasuryUtxo): MultisigHeadStateL1 =
        MultisigHeadStateL1(
          treasuryUtxo,
          MutableUtxoSet[L1, DepositTag](mutable.Map.empty),
          MutableUtxoSet[L1, RolloutTag](mutable.Map.empty)
        )

// Remove

case class SettledDeposit(
    txId: TxId,
    txIx: TxIx
)
