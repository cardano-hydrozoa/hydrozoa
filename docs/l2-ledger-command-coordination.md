# The Gummiworm ↔ L2 ledger protocol

How Hydrozoa drives a black-box L2 ledger (the built-in EUTXO reference ledger, or a remote sidecar
such as Sugar Rush) as an ordered command stream — both the **design** (who assigns command numbers,
how the transport survives connectivity loss without breaking consensus, how the two sides co-anchor
on crash recovery) **and the contract** a ledger must implement to serve as a Gummiworm ledger
backend.

Terminology: **spec** = the Gummiworm whitepaper. This doc *extends* the current spec
(`/whitepaper/sugar-rush/commands`); the delta is called out in [Wire protocol](#wire-protocol).
Where it says "the ledger must …", that is the normative contract for a ledger implementer.

## Screening vs. applying ledger commands

The `JointLedger` (JL) and `RequestSequencer` both talk to the L2 ledger, but with opposite
failure semantics, so they ride **separate traits and connections**:

- **Screening** (`L2Screener`, driven by `RequestSequencer`): stateless, order-independent,
  pre-`RequestId` checks. **Fail-soft** — a failure becomes `UserRequest.Rejected`; the user
  retries. A ledger is **recommended to run screening as a separate process**, not folded into the
  command API: it shares no state with the command stream, and keeping it off the command interface
  keeps that API's surface (and the command-number contract) small. Remote screening is a
  passthrough stub today (no socket) until a real screening endpoint lands.
- **Applying ledger commands** (`L2Ledger`, driven by `JointLedger`): the ordered, state-affecting
  command stream (`RegisterDeposit`, `ApplyTransaction`, `ApplyDepositDecisions`). Ordered,
  exactly-once, must-land. This doc is about the command stream.

## The command number is a coordination index, owned by JointLedger

JointLedger assigns every command a **monotonic command number** — a coordination /
sequencing / dedup key that identifies the command's *position in the stream*, **not** a
state-version. It is held in JL's actor state (`Done`/`Producing`), seeded at `zero`, and persisted
per block as the recovery anchor (`Cf.L2CommandNumber`).

The ledger does **not** invent its own number. It **validates and adopts** JL's number: no two
parallel counters means no drift into a wrong `restoreTo`, and a disagreement surfaces explicitly
(see [responses](#responses)) rather than silently.

### Advance on every command, not only on success

The index advances **once per command the ledger evaluates — applied *or* rejected** — because the
index is coordination, not state:

- The ledger **state** (utxos + deposit compartments) only moves for commands the ledger *applies*.
- The **index** moves for every command that gets a verdict.

So the number sequence is a dense stream of *attempts*, with the applied subset carrying the state.
Rejected commands consume a number and change nothing.

Precisely, against the ledger's current tip `T`:

| incoming number | meaning | ledger action |
|---|---|---|
| `== T + 1` | fresh | evaluate → apply (mutate + log) or reject (no mutation); **advance `T` either way** |
| `== T` | lost-ack resend of the last command | replay the **cached last response** verbatim (whatever it was); do not re-evaluate, do not advance |
| else (`> T+1` or `< T`) | out of order / desync | reply `OutOfOrder(…, expected = T+1)`; do not apply |

`T` does double duty: the dedup/validation cursor above, and the `restoreTo` guard below. It
advances on every command, so it is **not** derivable from the applied log (a trailing rejection
moves `T` past the last logged command) — it must be tracked/stored in its own right.

## Transport: retry through, never a verdict

Each peer drives its **own** replica of the ledger, so a peer-local transport failure that dropped
a command would diverge that peer's block from the others'. Therefore the command transport
**must not turn a transport failure into a per-request verdict.**

- A transport failure (connection loss, silent remote, timeout) is **retried through, forever**
  (bounded exponential backoff) over one persistent, shared-client connection. A request only
  returns once the remote gives a real verdict. There is **no "unavailable" error**.
- Blind resend is safe because of the command number: a re-sent command the ledger already evaluated
  replays its **cached last response** verbatim (an `Applied` with its effects, a `Rejected`, or a
  `LedgerFreeze`), so it is applied at most once and never silently re-evaluated.
- A permanently-unreachable ledger stalls this peer until the Cardano liaison's L1 fallback resolves
  the head. That is correct: a peer that cannot reach its ledger cannot make progress, and stalling
  beats diverging.

Consequently JL only ever sees a deterministic answer. A `Rejected` `RegisterDeposit` /
`ApplyTransaction` invalidates that user request, uniformly across peers. `ApplyDepositDecisions`
is **not** a user request, so a failure is never an ordinary verdict: a decision failure is a
coordination bug — a `CompartmentNotFound` (JL and the ledger disagree on which deposits exist)
or an `InternalLedgerError` — so JL **panics** on it, and on a `LedgerFreeze`, rather than continue
against a possibly corrupt L2 state (see [Responses](#responses)). Deposits are validated at
registration, so a decision should never fail on deposit *validity*.

## Responses

The response is a four-branch ADT. **Every branch echoes the command number it answers**, so
the client correlates each response to the request it sent and fail-stops on a mismatch. A resend
(`== T`) replays the ledger's **cached last response** verbatim.

```typescript
type GummiwormResponse =
  | { "Applied":      Applied }   // a concrete descendant per command, below
  | { "Rejected":     Rejected }  // a concrete descendant per command, below
  | { "OutOfOrder":   { commandNumber: CommandNumber, expected: CommandNumber } }
  | { "LedgerFreeze": { commandNumber: CommandNumber, wrongDecisionCommandNumber: CommandNumber } }

// `Applied` and `Rejected` each have a concrete descendant PER COMMAND — the command is the inner
// tag, and every descendant still echoes the command number:
type Applied =
  | { "RegisterDeposit":       { commandNumber: CommandNumber } }
  | { "ApplyDepositDecisions": { commandNumber: CommandNumber, evacuationDiffs: EvacuationDiff[] } }
  | { "ApplyTransaction":      { commandNumber: CommandNumber, evacuationDiffs: EvacuationDiff[], payouts: TransactionOutput[] } }

type Rejected =
  | { "RegisterDeposit":       { commandNumber: CommandNumber, reason: string } }
  | { "ApplyTransaction":      { commandNumber: CommandNumber, reason: string } }
  | { "ApplyDepositDecisions": { commandNumber: CommandNumber, reason: DepositDecisionRejectReason } }

// Only a rejected deposit decision is typed — it is always a coordination bug, never a user verdict.
// A rejected RegisterDeposit / ApplyTransaction carries a free-form message (the failure space is wide).
type DepositDecisionRejectReason =
  | { "CompartmentNotFound": { requestId: RequestId } }   // JL named an unregistered compartment — recoverable
  | { "InternalLedgerError": { message: string } }        // a ledger bug — terminal

type CommandNumber = number   // u64, monotonic
```

- **`Applied`** — the command applied; a concrete descendant **per command** (the command is the inner
  tag): nothing for `RegisterDeposit`, evacuation diffs for `ApplyDepositDecisions`, diffs + payouts
  for `ApplyTransaction`.
- **`Rejected`** — a deterministic rejection; also a descendant **per command**. A `RegisterDeposit` /
  `ApplyTransaction` carries a **free-form message** (unparseable, invalid, sub-min-ada, over-cover…);
  a rejected `ApplyDepositDecisions` carries a **typed** reason (`CompartmentNotFound` /
  `InternalLedgerError`, below), since it is always a coordination bug.
- **`OutOfOrder(commandNumber, expected = T+1)`** — the number is not fresh and not the cached last
  (`> T+1` or `< T`): a desync. Request-agnostic. `expected` is the number the ledger wanted next; JL
  derives the tip as `expected − 1`.
- **`LedgerFreeze(commandNumber, wrongDecisionCommandNumber)`** — the reply to every command that
  arrives *after* a decision the ledger could not apply: the ledger is **frozen**, and
  `wrongDecisionCommandNumber` is the `ApplyDepositDecisions` that broke it. Cleared only by JL
  rewinding past the freeze with `restoreTo`.

**Deposit decisions can fail, but not as a user verdict.** Deposits are validated at registration
(screening + the registration gate), so a decision should never fail on deposit *validity*. It can
still fail on a coordination bug — the failing decision gets `Rejected(ApplyDepositDecisions, reason)` with
`CompartmentNotFound` (JL named a compartment the ledger never registered — recoverable) or
`InternalLedgerError` (a ledger bug — terminal) — and the ledger then **freezes** (records a
`FreezeLedger` event; every *subsequent* command gets `LedgerFreeze`). JL **panics** on either for
now; later it may branch (retry the recoverable one, fall back to the L1 rule-based regime on the
terminal one). This reverts an earlier draft that made `ApplyDepositDecisions` infallible — it is
fallible; its failure is just handled by a panic, not by invalidating a request.

The client asserts each response's `commandNumber` equals the request's (mismatch → fail-stop), then
handles the branch: an `Applied`'s effects fold into JointLedger's accumulated state; a `Rejected`
`RegisterDeposit` / `ApplyTransaction` invalidates the request (advancing the number the ledger
consumed on the reject); and `OutOfOrder` / `LedgerFreeze` / a `Rejected` `ApplyDepositDecisions` / an
undecodable frame are a **hard fail-stop** — never a per-request verdict, which would diverge the
peer. A window of one suffices because JL is strictly single-in-flight. The consumer-side command
number lives on JointLedger; the ledger exposes no `currentCommandNumber` query (JL owns and persists
the authoritative number).

### Deposits are validated at registration, not at absorption

The ledger `Rejected`s a `RegisterDeposit` whose payload is unparseable, whose spawned L2 outputs are
individually invalid (e.g. any output below min-ada), or whose outputs are not covered by
`depositL2Value` — their total exceeding the L1 value the treasury absorbs would mint L2 value from
nothing. It validates all of this at *registration* (the same validity absorption would enforce), so
**a deposit that registers is guaranteed to absorb** — which is why an `ApplyDepositDecisions` should
never fail on deposit *validity*, only on the coordination bugs above. These gates are authoritative
at registration on every peer, because a peer's own pre-screen is best-effort and, for a remote
ledger, absent. (Broader spam/DoS is a separate mitigation, e.g. proof of work.)

## Recovery: mapping the coordination index to ledger state

Both sides persist. **JointLedger stores its full command stream and the responses it received**
(the authoritative record of what it drove); **the ledger** stores enough to be rewound and to
survive a crash:

- an **event log** — each applied command produces one or more events, stored keyed
  `(commandNumber, eventNumber) → event`, **sparse in the command number** (a rejected command
  produces no events and leaves a gap). A failed `ApplyDepositDecisions` records a **`FreezeLedger`
  event**, so the frozen state is just part of the reconstructed state (no separate flag) and survives
  a crash like any other state;
- the **cached last response** — so a lost-ack resend (including one right after a crash) replays
  byte-identically;
- the **tip `T`** (highest number seen, applied *or* rejected — a trailing rejection moves it past the
  last logged event, so it is not derivable from the log);
- *(optional)* periodic **snapshots** keyed by the command number, a replay accelerator.

**`restoreTo(N)` is not a command** — it carries no command number and does not advance the tip. It
reconstructs the ledger state as of command `N` (latest snapshot `≤ N`, then re-fold the events in
`(snapshot, N]`) and sets the tip to `N`. It **respects the freeze**: a `FreezeLedger` event in the
replayed range leaves the ledger frozen, so JL **unfreezes** by rewinding to *before* the offending
decision — it does not blindly clear the frozen state. After it, the ledger **trusts the suffix JL
re-drives** from `N+1` and does **not** re-check it against its stale post-`N` log: consensus is the
caller's, not the ledger's, so JL may legitimately re-drive a different suffix.

**Cross-store consistency comes from ordering, not a distributed transaction.** The ledger durably
records a command's events *and caches+persists its response* **before it replies**, and JL persists
its index *only at block completion* (after every command in the block got an answer, the transport
having retried through any connectivity loss). So the ledger's durable tip is **always ≥ JL's saved
index** — a transport failure just prevents the block from completing, saving nothing. If the ledger
is *ahead* of JL's saved index (crash mid-block), `restoreTo(saved)` rolls the extra back and
consensus re-drives the tail.

Scope: this is the *local EUTXO reference ledger's* recovery, and the *contract a remote sidecar must
meet* for its own recovery; JL saving the index is backend-agnostic. (The reference EUTXO ledger may
store applied *commands* and re-apply them instead of an event log — equivalent for restore.) Full
remote co-anchored recovery — a crash-time re-drive of `[anchor+1, head]` against a remote that stayed
*ahead* of the anchor — is out of scope here; the self-correlating wire (responses echo the number,
client asserts + fail-stops) is the safe interim.

## Wire protocol

The command envelope wraps the coordination number around each command; the response is the
[response ADT](#responses) above.

```typescript
type GummiwormCommand =
  | { "RegisterDeposit":        { commandNumber: CommandNumber, command: RegisterDeposit } }
  | { "ApplyDepositDecisions":  { commandNumber: CommandNumber, command: ApplyDepositDecisions } }
  | { "ApplyTransaction":       { commandNumber: CommandNumber, command: ApplyTransaction } }
```

Delta from the spec (`/whitepaper/sugar-rush/commands`):

- **Command** — spec `{ "RegisterDeposit": RegisterDeposit }`; ours wraps it with the number (above).
- **Response** — spec `Success | Failure`; ours is `Applied | Rejected | OutOfOrder | LedgerFreeze`
  (each a single-key tagged object; `Applied` and `Rejected` nest a further per-command tag).
  `Applied`/`Rejected` map onto the spec's `Success`/`Failure`; `OutOfOrder`/`LedgerFreeze` are new,
  required by the coordination contract. A resend replays the cached last response. `restoreTo` is a
  separate, un-numbered request.
- **Command payloads** — match the spec except **`userVk: ByteString`**: the contract omits it (the
  native L2 tx self-authenticates via its own witnesses, so the spec should drop it), and it omits
  the spec's `ProxyBlockConfirmation` / `ProxyRequestError`.
