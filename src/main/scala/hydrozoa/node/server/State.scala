package hydrozoa.node.server
import hydrozoa.{AddressBechL1, NativeScript}
import hydrozoa.node.server.HeadState.{Free, MultisigRegime}

// Milestone 2: shared global state
class HeadStateManager {
    private var headState: HeadState = Free(Array.empty)

    // transitions
    def init(headNativeScript: NativeScript, headBechAddress: AddressBechL1): Unit =
        headState = MultisigRegime(headNativeScript, headBechAddress)

    // utils
    def headNativeScript(): Option[NativeScript] = headState match
        case MultisigRegime(s, _) => Some(s)

    def headBechAddress(): Option[AddressBechL1] = headState match
        case MultisigRegime(_, a) => Some(a)
}

// A read-only wrapper around HeadStateManager
// TODO: probbaly should be a singleton object
class HeadStateReader(manager: HeadStateManager) {
    def headNativeScript(): Option[NativeScript] = manager.headNativeScript()
    def headBechAddress(): Option[AddressBechL1] = manager.headBechAddress()
}

// TODO: revise
enum HeadState:
    // Hydrozoa node is running and can be asked to prepare the init tx.
    case Free(knownPeers: Array[Peer])
    // The init transaction is settled.
    case MultisigRegime(headNativeScript: NativeScript, headBechAddress: AddressBechL1)

case class Peer()
