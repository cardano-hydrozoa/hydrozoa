# L2 ledger contract (Gummiworm ↔ Sugar Rush)

The contract an L2 ledger must implement to serve as a Gummiworm backend. It **extends** the current
command/response protocol (`/whitepaper/sugar-rush/commands`) so that Gummiworm ↔ ledger connectivity
is **retry-safe** and the two sides can **co-anchor on crash recovery**. Nothing here changes what a
command *means* — only how commands are numbered, how you answer them, and what you must persist to be
restorable. "You" is the ledger; "Gummiworm" is the driving node.

Design rationale: `docs/l2-ledger-command-coordination.md`.

## Why

Each Gummiworm peer drives its **own** copy of the ledger. If a transient connection drop caused a
peer to silently drop a command, that peer's block would diverge from the others' and consensus would
wedge. So Gummiworm never treats a transport failure as a verdict — it **retries the same command
forever** until you answer. For that to be safe, you must **deduplicate re-sent commands**, which
needs a stable per-command key. That key is the **command number**.

## 1. Every command carries a monotonic command number

Gummiworm assigns each command a number and stamps it on the command (see [wire format](#wire)). You
do **not** invent your own number — you **validate and adopt** Gummiworm's.

Track your current tip `T` (the number of the last command you processed, applied *or* rejected).
The number advances on **every** command you evaluate, not only the ones that change state — it is a
coordination sequence, not a state version. Per incoming number:

| incoming | meaning | what you do |
|---|---|---|
| `T + 1` | fresh | evaluate it; **advance `T` to `T+1` whether you apply or reject**; if applied, mutate state + append to your log |
| `T` | duplicate — Gummiworm re-sent the last command after a lost ack | **replay the cached outcome** — `Duplicate` if you applied it, `Rejected` if you rejected it; do not re-evaluate, do not advance |
| anything else (`> T+1` or `< T`) | out of order / desync | reject with `OutOfOrder(current = T)`; change nothing |

A window of one is enough — Gummiworm sends strictly one command at a time and waits for your answer,
so the only duplicate you can ever see is a resend of the immediately-previous command.

## 2. How to respond

Replace today's `Success | Failure` with:

```typescript
type GummiwormResponse =
  | { "Applied":    AppliedEffects }
  | { "Duplicate":  AppliedEffects }
  | { "OutOfOrder": { commandNumber: CommandNumber, current: CommandNumber } }
  | { "Rejected":   { commandNumber: CommandNumber, message: string } }

// Applied/Duplicate carry only the effects the answered command actually produces (correlate the
// command via commandNumber — see §1 for the per-command outcomes the current spec already lists):
//   RegisterDeposit        → { commandNumber }                            — successful registration, no effects
//   ApplyDepositDecisions  → { commandNumber, evacuationDiffs }           — no payouts
//   ApplyTransaction       → { commandNumber, evacuationDiffs, payouts }
type AppliedEffects = {
  commandNumber: CommandNumber,
  evacuationDiffs?: EvacuationDiff[],   // omitted for RegisterDeposit
  payouts?: TransactionOutput[],        // present only for ApplyTransaction
}

type CommandNumber = number   // u64, monotonic
```

- **`Applied`** — you applied command `T+1`; return the effects that command produces per
  `AppliedEffects`: nothing for `RegisterDeposit`, diffs for `ApplyDepositDecisions`, diffs + payouts
  for `ApplyTransaction`. (This matches the current spec's `Responses to Gummiworm`, just split out
  per command instead of one `Success` shape.)
- **`Duplicate`** — the incoming number equalled `T` **and you applied it the first time**; replay
  the *same* effects you returned then (from your window-of-one cache). Applied exactly once overall.
  If instead you *rejected* `T` the first time, replay that verdict as `Rejected(T, message)`, **not**
  `Duplicate` — the tip advances on a rejection too, so a lost-ack resend of a rejected command must
  come back as the same rejection; an empty `Duplicate` would be read as an apply and diverge the peer.
- **`OutOfOrder`** — the number was neither `T+1` nor `T`; report your current `T`.
- **`Rejected`** — a deterministic rejection (invalid tx, etc.) (== today's `Failure`). Gummiworm
  still ignores the message content; it just needs to know it was rejected.

**Every branch echoes the `commandNumber` it is answering** — the number from the command you're
responding to. Gummiworm uses it to match each response to the request it sent and hard-fails on a
mismatch (defense against a stray or duplicated frame). For `OutOfOrder`, `commandNumber` is the
number we sent and `current` is your tip.

`evacuationDiffs`, `payouts`, and the "invalid → no state transition" rule are unchanged from the
current spec.

**Reject a malformed deposit at *registration*, not at absorption.** A `RegisterDeposit` is invalid
(→ `Rejected`) when its payload is unparseable, when its spawned L2 outputs are individually invalid
(e.g. any output below min-ada), **or** when those outputs are not covered by `depositL2Value` — their
total exceeds the L1 value the treasury absorbs, which would mint L2 value from nothing. Validate all
of this when you register the deposit (the same validity you'd enforce when absorbing it), so **a
deposit that registers is guaranteed to absorb**. Both gates are authoritative at registration (every
peer), because a Gummiworm peer's own pre-screen is best-effort and, for a remote ledger, absent. Deferring output validity to `ApplyDepositDecisions` lets one user-crafted
deposit fail at absorption — and, if that is treated as fatal, wedge the block on every recovery
re-drive. This can't be prevented at the protocol/wire level (nothing stops a user submitting bad L2
metadata), so it is a ledger requirement. Gummiworm's reference EUTXO ledger enforces the same gate.
(Broader spam/DoS — many well-formed but junk deposits — is a separate mitigation, e.g. proof of
work, orthogonal to this validity gate.)

**A deposit decision has no verdict — it always applies.** Because deposits are validated at
registration, `ApplyDepositDecisions` only ever merges deposits you already registered and accepted,
so it **cannot be `Rejected`**. If you find you cannot apply one — it references a deposit you never
registered, or an absorbed output that fails a check registration should have caught — that is a
Gummiworm-side invariant violation, not a ledger verdict: **fail-stop** (error out / drop the
connection), do **not** fabricate a `Rejected`. A Gummiworm peer that emits such a command is a
failed ("mad") head, which Hydrozoa resolves through its L1 rule-based regime — so you never need to
degrade gracefully. Only `RegisterDeposit` and `ApplyTransaction` can be `Rejected`.

## 3. What you must persist to be restorable

Gummiworm records, per block, the command number your ledger reached, and on restart asks you to
`restoreTo(N)`. To answer that, persist, keyed by the command number:

- a **sparse log** `number → command`, for the commands you *applied* (rejected commands consume a
  number but are not logged — they changed nothing);
- periodic **snapshots** of your recoverable state, keyed by the command number (a replay
  accelerator);
- the **tip `T`** in its own right (a trailing rejected command moves `T` past your last logged
  command, so `T` is not recoverable from the log alone).

`restoreTo(N)` = load the latest snapshot `≤ N`, re-fold the logged commands in `(snapshot, N]`, set
your tip to `N`. `restoreTo(N)` where `N > T` should never happen (see below) — treat it as
corruption. You own your persistence entirely; Gummiworm only holds the number-per-block mapping.

**Ordering keeps the two sides consistent — no cross-system atomic commit needed.** Save each command
to disk *before you reply to it*. Gummiworm saves its per-block anchor only after you've replied to
every command in that block. So any number Gummiworm later asks you to `restoreTo` is one you've
already fully saved — you can never be asked to restore past what you have on disk.

## 4. Wire format {#wire}

Command — add the number around today's payloads (single-key tag, `commandNumber` + `command`):

```typescript
type GummiwormCommand =
  | { "RegisterDeposit":        { commandNumber: CommandNumber, command: RegisterDeposit } }
  | { "ApplyDepositDecisions":  { commandNumber: CommandNumber, command: ApplyDepositDecisions } }
  | { "ApplyTransaction":       { commandNumber: CommandNumber, command: ApplyTransaction } }
```

Transport: one long-lived WebSocket, one synchronous request/response at a time, Gummiworm reconnects
and resends on drop (which is why dedup matters).
