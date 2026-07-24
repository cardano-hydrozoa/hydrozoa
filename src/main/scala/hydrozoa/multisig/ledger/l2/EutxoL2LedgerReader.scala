package hydrozoa.multisig.ledger.l2

import scalus.cardano.address.Address
import scalus.cardano.ledger.Utxos

/** The read-only L2-ledger queries the user-facing HTTP server exposes for the **EUTXO reference
  * ledger** (`GET /l2/cardano-eutxo/utxos`, `GET /l2/cardano-eutxo/transactions`).
  *
  * Implemented only by [[hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger]], which holds its state
  * locally. A node wired to a remote ledger has no such reader, so the server is handed `None` and
  * the L2-query endpoints are not mounted. Handing the server only this narrow view lets it read L2
  * state without being able to issue ledger commands.
  */
trait EutxoL2LedgerReader[F[_]]:
    /** The current L2 utxo set restricted to `address` — the outputs that address controls right
      * now. A live read of the ledger's committed state (no consensus round-trip).
      */
    def utxosByAddress(address: Address): F[Utxos]

    /** The most recently applied L2 commands, newest first, at most `limit` — a window over the
      * ledger's own command log projected to lightweight [[L2TxSummary]]s. Every entry is a command
      * that committed.
      */
    def recentTransactions(limit: Int): F[Vector[L2TxSummary]]
