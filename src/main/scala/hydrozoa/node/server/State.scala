package hydrozoa.node.server
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.l2.consensus.{HeadParams, L2ConsensusParams}
import hydrozoa.node.server.HeadState.{Free, MultisigRegime}

import scala.collection.mutable

// Milestone 2: shared global state
class HeadStateManager(log: Logger) {

    private var headState: HeadState = Free(Array.empty)

    // TODO: separate objects for every state
    private val awaitingDeposits = mutable.Set[AwaitingDeposit]()
    private var treasuryRef: Option[(TxId, TxIx)] = None
    private var majorVersion = 0

    // transitions
    def init(
        headParams: HeadParams,
        headNativeScript: NativeScript,
        headBechAddress: AddressBechL1,
        newTreasuryRef: (TxId, TxIx)
    ): Unit =
        headState = MultisigRegime(headParams, headNativeScript, headBechAddress)
        treasuryRef = Some(newTreasuryRef)
        majorVersion = 0

    // operations over a particular state  - namely MultiSig
    def enqueueDeposit(deposit: AwaitingDeposit) =
        headState match
            case MultisigRegime(_, _, _) =>
                awaitingDeposits.add(deposit)
            case _ => log.error(s"Deposits can be queued only in multisig regime.")

    def peekDeposits: Set[AwaitingDeposit] = awaitingDeposits.toList.toSet

    def currentMajorVersion = majorVersion

    def currentTreasuryRef = treasuryRef

    def stepMajor(
        txId: TxId,
        txIx: TxIx,
        newMajor: Int,
        absorbedDeposits: Set[SettledDeposit]
    ): Unit =
        if newMajor == majorVersion + 1 then
            // TODO: verify all absorbed deposits are on the list
            // TODO: atomicity
            // TODO: create L2 utxos
            absorbedDeposits.map(awaited).map(awaitingDeposits.remove)
            log.info(s"Settled deposits: $absorbedDeposits")
            majorVersion = newMajor
            log.info(s"Step into next major version $newMajor")
            treasuryRef = Some(txId, txIx)
            log.info(s"New treasury utxo is $treasuryRef")
        else
            log.error(
              s"Can't step into wrong major version, expected: ${majorVersion + 1}, got: $newMajor"
            )

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
    def headNativeScript: Option[NativeScript] = manager.headNativeScript()
    def headBechAddress: Option[AddressBechL1] = manager.headBechAddress()
    def depositTimingParams: Option[(UDiffTime, UDiffTime, UDiffTime)] =
        manager.depositTimingParams()
    def currentMajorVersion = manager.currentMajorVersion
    def currentTreasuryRef = manager.currentTreasuryRef match
        case Some(x) => x // FIXME
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

case class AwaitingDeposit(
    txId: TxId,
    txIx: TxIx
)

case class SettledDeposit(
    txId: TxId,
    txIx: TxIx
)

def awaited(d: SettledDeposit): AwaitingDeposit = AwaitingDeposit(d.txId, d.txIx)
