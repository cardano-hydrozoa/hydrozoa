package hydrozoa.multisig.ledger.eutxol2.store

import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2LedgerCommand}

/** The durable store backing `EutxoL2Ledger`'s crash recovery (§R2b).
  *
  * Purpose-built and command-number-keyed — *not* the consensus `BackendStore` (which is hardwired
  * to the consensus `Cf` enum). It is the L2's own store, mirroring how a real black-box L2
  * (SugarRush) owns its persistence wholly outside the consensus layer. Two logical column
  * families, both keyed by [[L2CommandNumber]]:
  *
  *   - **log** — append-only `commandNumber -> ` applied [[L2LedgerCommand]]. The source of truth:
  *     for the EUTXO ledger the command *is* the diff (re-applying it via the deterministic mutator
  *     core reproduces the next state), so an event-sourced log needs no separate diff type.
  *   - **snapshot** — `commandNumber -> ` full [[L2Snapshot]] (the recoverable subset of state). A
  *     restore accelerator written every `SnapshotInterval` commits; genesis (commandNumber 0) is
  *     implicit, so an empty snapshot CF is fine.
  *
  * Plus a single **tip** (highest command number seen, applied or rejected) so `restoreTo` can
  * reject a target beyond what was processed. `restoreTo(S)` reads `latestSnapshotAtOrBefore(S)`
  * (or genesis) and re-folds `logRange(from, S]`. See `docs/l2-ledger-command-coordination.md`.
  */
trait L2Store[F[_]]:
    /** Append `command` to the log at `commandNumber`. Called once per committed real command. */
    def appendLog(commandNumber: L2CommandNumber, command: L2LedgerCommand): F[Unit]

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
    ): F[List[L2LedgerCommand]]

    /** Record the recovery **tip** — the highest command number the ledger has processed, applied
      * *or* rejected. Written on every command (a rejected command advances the tip past the last
      * logged one, so the tip is not derivable from the log alone). `restoreTo` uses it to reject a
      * target beyond what the ledger has actually seen.
      */
    def putTip(commandNumber: L2CommandNumber): F[Unit]

    /** The recovery tip. `None` only when nothing has ever been persisted (a genuinely fresh store
      * ⇒ genesis). A store written before the tip was tracked (no tip entry, but a non-empty
      * log/snapshot) is treated as legacy: the tip is derived from the highest persisted command
      * number, so `restoreTo` does not spuriously reject a valid target on such a store.
      */
    def getTip: F[Option[L2CommandNumber]]

    /** Record the ledger's frozen state — the command number of the `ApplyDepositDecisions` that
      * froze it, or `None` when not frozen. Written when the ledger freezes on a decision it cannot
      * apply, and adjusted by `restoreTo` (a rewind to before the freeze clears it), so a frozen
      * ledger stays frozen across a crash.
      */
    def putFrozenAt(commandNumber: Option[L2CommandNumber]): F[Unit]

    /** The command number of the decision that froze the ledger, or `None` if it is not frozen. */
    def getFrozenAt: F[Option[L2CommandNumber]]

object L2Store:
    /** Write a full-state snapshot every this many commits (§R2b; tune later, not config-driven).
      * TODO: make parameter?
      */
    val SnapshotInterval: Long = 100L
