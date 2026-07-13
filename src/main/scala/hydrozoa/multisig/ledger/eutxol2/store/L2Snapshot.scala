package hydrozoa.multisig.ledger.eutxol2.store

import hydrozoa.multisig.ledger.eutxol2.tx.L2Genesis
import hydrozoa.multisig.ledger.eutxol2.{EutxoL2Ledger, TransientTokens}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l2.L2CommandNumber
import scalus.cardano.ledger.Utxos

/** The **recoverable subset** of [[EutxoL2Ledger.State]] persisted in a snapshot (§R2b).
  *
  * Only `commandNumber` + `activeUtxos` + `transientTokens` + `pendingDeposits` are kept — the
  * fields a recovered ledger needs to resume producing blocks, plus its commit commandNumber.
  * `errors` and `confirmations` are transient, client-facing proxy data (excluded from the
  * commandNumber + log, §R2b); a restored ledger starts them empty. Keeping them out also avoids
  * needing a codec for `EnrichedTx.Serialized` (the `confirmations` value type).
  */
final case class L2Snapshot(
    commandNumber: L2CommandNumber,
    activeUtxos: Utxos,
    transientTokens: TransientTokens,
    pendingDeposits: Map[RequestId, L2Genesis]
)

object L2Snapshot:
    /** Project a full ledger state down to its persisted subset. */
    def fromState(state: EutxoL2Ledger.State): L2Snapshot =
        L2Snapshot(
          state.commandNumber,
          state.activeUtxos,
          state.transientTokens,
          state.pendingDeposits
        )
