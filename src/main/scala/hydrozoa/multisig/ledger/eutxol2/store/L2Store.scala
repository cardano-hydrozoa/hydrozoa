package hydrozoa.multisig.ledger.eutxol2.store

import hydrozoa.multisig.ledger.l2.{L2LedgerCommand, L2CommandNumber}

/** The durable store backing `EutxoL2Ledger`'s crash recovery (§R2b).
  *
  * Purpose-built and command-number-keyed — *not* the consensus `BackendStore` (which is hardwired
  * to the consensus `Cf` enum). It is the L2's own store, mirroring how a real black-box L2
  * (SugarRush) owns its persistence wholly outside the consensus layer. Two logical column
  * families, both keyed by [[L2CommandNumber]]:
  *
  *   - **log** — append-only `commandNumber -> ` applied [[L2LedgerCommand.Real]]. The source of
  *     truth: for the EUTXO ledger the command *is* the diff (re-applying it via the deterministic
  *     mutator core reproduces the next state), so an event-sourced log needs no separate diff
  *     type.
  *   - **snapshot** — `commandNumber -> ` full [[L2Snapshot]] (the recoverable subset of state). A
  *     restore accelerator written every `SnapshotInterval` commits; genesis (commandNumber 0) is
  *     implicit, so an empty snapshot CF is fine.
  *
  * `restoreTo(S)` reads `latestSnapshotAtOrBefore(S)` (or genesis) and re-folds
  * `logRange(from, S]`. See `design/recovery-implementation-plan.md` R2b.
  */
trait L2Store[F[_]]:
    /** Append `command` to the log at `commandNumber`. Called once per committed real command. */
    def appendLog(commandNumber: L2CommandNumber, command: L2LedgerCommand.Real): F[Unit]

    /** Write a full-state snapshot at `commandNumber`. Called every `SnapshotInterval` commits. */
    def putSnapshot(commandNumber: L2CommandNumber, snapshot: L2Snapshot): F[Unit]

    /** The snapshot with the greatest key `<= commandNumber`, if any (none ⇒ restore from genesis).
      */
    def latestSnapshotAtOrBefore(
        commandNumber: L2CommandNumber
    ): F[Option[(L2CommandNumber, L2Snapshot)]]

    /** The logged commands with key in `(fromExclusive, toInclusive]`, in ascending commandNumber
      * order.
      */
    def logRange(
        fromExclusive: L2CommandNumber,
        toInclusive: L2CommandNumber
    ): F[List[L2LedgerCommand.Real]]

object L2Store:
    /** Write a full-state snapshot every this many commits (§R2b; tune later, not config-driven).
      */
    val SnapshotInterval: Long = 100L
