package hydrozoa.node.server
import hydrozoa.l2.consensus.{HeadParams, L2ConsensusParams}
import hydrozoa.node.server.HeadState.{Free, MultisigRegime}
import hydrozoa.{AddressBechL1, NativeScript, UDiffTime}

// Milestone 2: shared global state
class HeadStateManager {
    private var headState: HeadState = Free(Array.empty)

    // transitions
    def init(
        headParams: HeadParams,
        headNativeScript: NativeScript,
        headBechAddress: AddressBechL1
    ): Unit =
        headState = MultisigRegime(headParams, headNativeScript, headBechAddress)

    // utils
    def headNativeScript(): Option[NativeScript] = headState match
        case MultisigRegime(_, s, _) => Some(s)

    def headBechAddress(): Option[AddressBechL1] = headState match
        case MultisigRegime(_, _, a) => Some(a)

    def depositTimingParams(): Option[(UDiffTime, UDiffTime, UDiffTime)] = headState match
        case MultisigRegime(
              HeadParams(
                L2ConsensusParams(depositMarginMaturity, depositMarginExpiry),
                minimalDepositWindow
              ),
              _,
              a
            ) =>
            Some(depositMarginMaturity, minimalDepositWindow, depositMarginExpiry)
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
        headBechAddress: AddressBechL1
    )

case class Peer()
