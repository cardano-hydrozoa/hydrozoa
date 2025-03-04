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
    private var nextBlockFinal = false
    // FIXME: move to HeadState, doesn't change over the course of head's lifecycle
    private var seedAddress: Option[AddressBechL1] = None

    // transitions
    def init(
        headParams: HeadParams,
        headNativeScript: NativeScript,
        headBechAddress: AddressBechL1,
        beaconTokenName: String,
        treasuryRef: (TxId, TxIx),
        seedAddress: AddressBechL1
    ): Unit = headState match
        case Free(_) =>
            headState =
                MultisigRegime(headParams, headNativeScript, headBechAddress, beaconTokenName)
            this.treasuryRef = Some(treasuryRef)
            this.seedAddress = Some(seedAddress)
        case _ => log.error(s"Initialization can happen only in free state.")

    def finalize_(): Unit =
        headState match
            case MultisigRegime(_, _, _, _) =>
                headState = Free(Array.empty) // TODO:
                treasuryRef = None
                seedAddress = None
                majorVersion = 0
                nextBlockFinal = false
                awaitingDeposits.clear()
            case _ => log.error(s"Finalization can happen only in multisig regime.")

    // operations over a particular state  - namely MultiSig
    def enqueueDeposit(deposit: AwaitingDeposit) =
        headState match
            case MultisigRegime(_, _, _, _) =>
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
        headState match
            case MultisigRegime(_, _, _, _) =>
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
            case _ => log.error(s"Stepping into the next major block requires multisig regime.")

    // utils
    def headNativeScript: Option[NativeScript] = headState match
        case MultisigRegime(_, s, _, _) => Some(s)

    def beaconTokenName: Option[String] = headState match
        case MultisigRegime(_, _, _, tn) => Some(tn)

    def headBechAddress: Option[AddressBechL1] = headState match
        case MultisigRegime(_, _, a, _) => Some(a)

    // FIXME: rename
    def getSeedAddress = seedAddress

    def depositTimingParams: Option[(UDiffTime, UDiffTime, UDiffTime)] = headState match
        case MultisigRegime(
              HeadParams(
                L2ConsensusParams(depositMarginMaturity, depositMarginExpiry),
                minimalDepositWindow
              ),
              _,
              a,
              _
            ) =>
            Some(depositMarginMaturity, minimalDepositWindow, depositMarginExpiry)

}

// A read-only wrapper around HeadStateManager
// TODO: probbaly should be a singleton object
class HeadStateReader(manager: HeadStateManager) {
    def headNativeScript: Option[NativeScript] = manager.headNativeScript
    def beaconTokenName: Option[String] = manager.beaconTokenName
    def headBechAddress: Option[AddressBechL1] = manager.headBechAddress
    def depositTimingParams: Option[(UDiffTime, UDiffTime, UDiffTime)] =
        manager.depositTimingParams
    def currentMajorVersion = manager.currentMajorVersion
    def currentTreasuryRef = manager.currentTreasuryRef match
        case Some(x) => x // FIXME
    def seedAddress = manager.getSeedAddress
    def peekDeposits = manager.peekDeposits
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
        beaconTokenName: String // FIXME: type
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

/** Temporary withdrawal - once we have L2 ledger we can remove lovelace and address fields.
  *
  * @param txId
  * @param txIx
  * @param lovelace
  * @param address
  */
case class Withdrawal(
    txId: TxId,
    txIx: TxIx,
    lovelace: Int,
    address: AddressBechL1
)
