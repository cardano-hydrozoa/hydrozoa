package hydrozoa.multisig.ledger.eutxol2.store

import cats.effect.IO
import hydrozoa.multisig.ledger.l2.{L2LedgerCommand, L2Serial}

/** The durable store backing `EutxoL2Ledger`'s crash recovery (§R2b).
  *
  * Purpose-built and serial-keyed — *not* the consensus `BackendStore` (which is hardwired to the
  * consensus `Cf` enum). It is the L2's own store, mirroring how a real black-box L2 (SugarRush)
  * owns its persistence wholly outside the consensus layer. Two logical column families, both keyed
  * by [[L2Serial]]:
  *
  *   - **log** — append-only `serial -> ` applied [[L2LedgerCommand.Real]]. The source of truth:
  *     for the EUTXO ledger the command *is* the diff (re-applying it via the deterministic mutator
  *     core reproduces the next state), so an event-sourced log needs no separate diff type.
  *   - **snapshot** — `serial -> ` full [[L2Snapshot]] (the recoverable subset of state). A restore
  *     accelerator written every `SnapshotInterval` commits; genesis (serial 0) is implicit, so an
  *     empty snapshot CF is fine.
  *
  * `restoreTo(S)` reads `latestSnapshotAtOrBefore(S)` (or genesis) and re-folds
  * `logRange(from, S]`. See `design/recovery-implementation-plan.md` R2b.
  */
trait L2Store[F[_]]:
    /** Append `command` to the log at `serial`. Called once per committed real command. */
    def appendLog(serial: L2Serial, command: L2LedgerCommand.Real): F[Unit]

    /** Write a full-state snapshot at `serial`. Called every `SnapshotInterval` commits. */
    def putSnapshot(serial: L2Serial, snapshot: L2Snapshot): F[Unit]

    /** The snapshot with the greatest key `<= serial`, if any (none ⇒ restore from genesis). */
    def latestSnapshotAtOrBefore(serial: L2Serial): F[Option[(L2Serial, L2Snapshot)]]

    /** The logged commands with key in `(fromExclusive, toInclusive]`, in ascending serial order.
      */
    def logRange(fromExclusive: L2Serial, toInclusive: L2Serial): F[List[L2LedgerCommand.Real]]

object L2Store:
    /** Write a full-state snapshot every this many commits (§R2b; tune later, not config-driven).
      */
    val SnapshotInterval: Long = 100L
