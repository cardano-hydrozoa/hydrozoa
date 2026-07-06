package hydrozoa.multisig.ledger.l2

import scalus.cardano.address.Address
import scalus.cardano.ledger.Utxos

/** The read-only L2-ledger queries the user-facing HTTP server exposes (`GET /api/l2/utxos`,
  * `GET /api/l2/transactions`). A narrow view of [[L2Ledger]] — the server is handed only this, so
  * it can read L2 state but cannot issue ledger commands.
  *
  * These reads are meaningful only for a ledger that holds its state locally (the EUTXO reference
  * ledger, [[hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger]]); an implementation that owns its
  * state elsewhere (a remote black box) returns empty results.
  */
trait L2LedgerReader[F[_]]:
    /** The current L2 utxo set restricted to `address` — the outputs that address controls right
      * now. A live read of the ledger's committed state (no consensus round-trip).
      */
    def utxosByAddress(address: Address): F[Utxos]

    /** The most recently applied L2 commands, newest first, at most `limit` — a window over the
      * ledger's own command log projected to lightweight [[L2TxSummary]]s. Every entry is a command
      * that committed.
      */
    def recentTransactions(limit: Int): F[Vector[L2TxSummary]]
