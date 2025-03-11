package hydrozoa.node.server
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.l2.block.Block
import hydrozoa.l2.consensus.{HeadParams, L2ConsensusParams}
import hydrozoa.l2.event.{L2Event, L2NonGenesis}
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
    headBechAddress: AddressBechL1,
    beaconTokenName: String, // FIXME: use more specific type
    seedAddress: AddressBechL1
) extends HeadState
    with MultisigRegime {
    val blocksConfirmedL2: mutable.Seq[Block] = mutable.Seq[Block]()
    val confirmedEventsL2: mutable.Set[L2Event] = mutable.Set()
    val poolEventsL2: mutable.Set[L2NonGenesis] = mutable.Set()
    var finalizing = false
    // TODO: peers
    // TODO: stateL1
    val stateL2 = AdaSimpleLedger(NoopVerifier)

    def timeCurrent(): Unit = ()

    // Additional to spec, likely will be gone
    val awaitingDeposits: mutable.Set[AwaitingDeposit] = mutable.Set[AwaitingDeposit]()
    var treasuryRef: Option[(TxId, TxIx)] = None
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
        treasuryRef: (TxId, TxIx),
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

    def peekDeposits: Set[AwaitingDeposit]
    def poolEventsL2: mutable.Set[L2NonGenesis]

    def enqueueDeposit(deposit: AwaitingDeposit): Unit

    def stateL2: AdaSimpleLedger
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
            treasuryRef: (TxId, TxIx),
            seedAddress: AddressBechL1
        ): Unit =
            val newHead = Open(
              headParams,
              headNativeScript,
              headBechAddress,
              beaconTokenName,
              seedAddress
            )
            newHead.treasuryRef = Some(treasuryRef)
            self.headState = Some(newHead)

    private class OpenNodeStateImpl(openState: Open) extends OpenNodeState:
        def currentMajorVersion: Int = openState.majorVersion

        def currentTreasuryRef: (TxId, TxIx) = openState.treasuryRef match
            case Some(ref) => ref
            case None => throw IllegalStateException("Corrupted node state: missing treasury ref.")

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

        def peekDeposits: Set[AwaitingDeposit] = openState.awaitingDeposits.toList.toSet
        def enqueueDeposit(deposit: AwaitingDeposit): Unit = openState.awaitingDeposits.add(deposit)
        def finalizing: Boolean = openState.finalizing
        def stateL2: AdaSimpleLedger = openState.stateL2

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
                absorbedDeposits.map(awaited).map(openState.awaitingDeposits.remove)
                log.info(s"Settled deposits: $absorbedDeposits")
                openState.majorVersion = newMajor
                log.info(s"Step into next major version $newMajor")
                openState.treasuryRef = Some(txId, txIx)
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
            case _ => throw IllegalStateException("The head is missing or in a Open state.")

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

case class AwaitingDeposit(
    txId: TxId,
    txIx: TxIx
)

def awaited(d: SettledDeposit): AwaitingDeposit = AwaitingDeposit(d.txId, d.txIx)

case class SettledDeposit(
    txId: TxId,
    txIx: TxIx
)
