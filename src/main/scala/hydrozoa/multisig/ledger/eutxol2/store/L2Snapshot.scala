package hydrozoa.multisig.ledger.eutxol2.store

import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.ledger.eutxol2.tx.L2Genesis
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l2.L2CommandNumber
import scalus.cardano.ledger.Utxos

/** The **recoverable subset** of [[EutxoL2Ledger.State]] persisted in a snapshot (§R2b).
  *
  * Only `commandNumber` + `activeUtxos` + `pendingDeposits` are kept — the fields a recovered
  * ledger needs to resume producing blocks, plus its commit commandNumber.
  */
final case class L2Snapshot(
    commandNumber: L2CommandNumber,
    activeUtxos: Utxos,
    pendingDeposits: Map[RequestId, L2Genesis]
)

object L2Snapshot:
    /** Project a full ledger state down to its persisted subset. */
    def fromState(state: EutxoL2Ledger.State): L2Snapshot =
        L2Snapshot(state.commandNumber, state.activeUtxos, state.pendingDeposits)
