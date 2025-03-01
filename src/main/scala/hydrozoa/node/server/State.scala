package hydrozoa.node.server
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.l2.consensus.{HeadParams, L2ConsensusParams}
import hydrozoa.node.server.HeadState.{Free, MultisigRegime}

import scala.collection.mutable

// Milestone 2: shared global state
class HeadStateManager(log: Logger) {

    private var headState: HeadState = Free(Array.empty)
    private val awaitingDeposits = mutable.Set[AwaitingDeposit]()

    // transitions
    def init(
        headParams: HeadParams,
        headNativeScript: NativeScript,
        headBechAddress: AddressBechL1,
        treasuryRef: (TxId, TxIx)
    ): Unit =
        headState = MultisigRegime(headParams, headNativeScript, headBechAddress, treasuryRef, 0)

    // operations over a particular state
    def addDeposit(deposit: AwaitingDeposit) =
        headState match
            case MultisigRegime(_, _, _, _, _) =>
                awaitingDeposits.add(deposit)
            case _ => log.error(s"Deposits can be queued only in multisig regime.")

    // utils
    def headNativeScript(): Option[NativeScript] = headState match
        case MultisigRegime(_, s, _, _, _) => Some(s)

    def headBechAddress(): Option[AddressBechL1] = headState match
        case MultisigRegime(_, _, a, _, _) => Some(a)

    def depositTimingParams(): Option[(UDiffTime, UDiffTime, UDiffTime)] = headState match
        case MultisigRegime(
              HeadParams(
                L2ConsensusParams(depositMarginMaturity, depositMarginExpiry),
                minimalDepositWindow
              ),
              _,
              a,
              _,
              _
            ) =>
            Some(depositMarginMaturity, minimalDepositWindow, depositMarginExpiry)

    def peekDeposits: Set[AwaitingDeposit] = awaitingDeposits.toList.toSet
}

// A read-only wrapper around HeadStateManager
// TODO: probbaly should be a singleton object
class HeadStateReader(manager: HeadStateManager) {
    def headNativeScript(): Option[NativeScript] = manager.headNativeScript()
    def headBechAddress(): Option[AddressBechL1] = manager.headBechAddress()
    def depositTimingParams(): Option[(UDiffTime, UDiffTime, UDiffTime)] =
        manager.depositTimingParams()
}

// TODO: revise
enum HeadState:
    // Hydrozoa node is running and can be asked to prepare the init tx.
    case Free(knownPeers: Array[Peer])
    // The init transaction is settled.
    case MultisigRegime(
        headParams: HeadParams,
        headNativeScript: NativeScript,
        headBechAddress: AddressBechL1,
        treasuryRef: (TxId, TxIx),
        majorVersion: Int
    )

case class Peer()

case class AwaitingDeposit(
    txId: TxId,
    txIx: TxIx
)
