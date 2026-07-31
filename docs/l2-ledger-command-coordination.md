# L2 ledger command coordination, transport, and recovery

How Hydrozoa drives a black-box L2 ledger (the built-in EUTXO reference ledger, or a remote
sidecar such as Sugar Rush) as an ordered command stream: who assigns command numbers, how the
transport survives connectivity loss without breaking consensus, and how the two sides co-anchor on
crash recovery.

Terminology: **spec** = the Gummiworm whitepaper. This is a *design doc*, not the spec. The wire
contract this doc defines *extends* the current spec (`/whitepaper/sugar-rush/commands`); the delta
is called out in [Wire protocol](#wire-protocol) and collected as the ledger's contract in
`docs/l2-ledger-contract.md`.

## The two operation classes

The `JointLedger` (JL) and `RequestSequencer` both talk to the L2 ledger, but with opposite
failure semantics, so they ride **separate traits and connections**:

- **Screening** (`L2Screener`, driven by `RequestSequencer`): stateless, order-independent,
  pre-`RequestId` checks. **Fail-soft** — a failure becomes `UserRequest.Rejected`; the user
  retries. Remote screening is a passthrough stub today (no socket) until a real screening endpoint
  lands.
- **Mutation** (`L2Ledger`, driven by `JointLedger`): the ordered, state-affecting command stream
  (`RegisterDeposit`, `ApplyTransaction`, `ApplyDepositDecisions`). Ordered, exactly-once,
  must-land. This doc is about the mutation stream.

## The command number is a coordination index, owned by JointLedger

JointLedger assigns every mutation command a **monotonic command number** — a coordination /
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
| `== T` | duplicate (lost-ack resend of the last command) | replay the cached outcome (`Duplicate` if you applied it, `Rejected` if you rejected it); do not advance |
| else (`> T+1` or `< T`) | out of order / desync | reject with `OutOfOrder(current = T)`; do not apply |

`T` does double duty: the dedup/validation cursor above, and the `restoreTo` guard below. It
advances on every command, so it is **not** derivable from the applied log (a trailing rejection
moves `T` past the last logged command) — it must be tracked/stored in its own right.

## Transport: retry through, never a verdict

Each peer drives its **own** replica of the ledger, so a peer-local transport failure that dropped
a command would diverge that peer's block from the others'. Therefore the mutation transport
**must not turn a transport failure into a per-request verdict.**

- A transport failure (connection loss, silent remote, timeout) is **retried through, forever**
  (bounded exponential backoff) over one persistent, shared-client connection. A request only
  returns once the remote gives a real verdict. There is **no "unavailable" error**.
- Blind resend is safe because of the command number: a re-sent command the ledger already evaluated
  replays its cached verdict — `Duplicate` (the original effects) if it was applied, `Rejected` (the
  same message) if it was rejected — so it is applied at most once and never silently re-evaluated.
- A permanently-unreachable ledger stalls this peer until the Cardano liaison's L1 fallback resolves
  the head. That is correct: a peer that cannot reach its ledger cannot make progress, and stalling
  beats diverging.

Consequently JL only ever sees a deterministic verdict, so its existing handling is correct as-is: a
`Rejected` verdict soft-invalidates the request, uniformly across peers. Only `RegisterDeposit` and
`ApplyTransaction` can be `Rejected`; `ApplyDepositDecisions` is infallible (it merges
already-validated deposits) and fail-stops on an invariant violation rather than rejecting.

## Responses

The mutation response is a small ADT (extending the spec's `Success | Failure`). **Every branch
echoes the command number it answers**, so the client correlates each response to the request it sent
and fail-stops on a mismatch (a stray/duplicated frame can't be mistaken for the current command's
verdict). The effects are only what the command produces:

- **`Applied(commandNumber, effects)`** — applied at the assigned number. Effects per command:
  nothing for `RegisterDeposit`, evacuation diffs for `ApplyDepositDecisions`, diffs + payouts for
  `ApplyTransaction`.
- **`Duplicate(commandNumber, effects)`** — a re-send of the *last applied* command (`== T`); replays
  the same per-command effects from a window-of-1 cache. Consumed exactly like `Applied` — applied
  once. (A re-send of the last *rejected* command replays as `Rejected`, not `Duplicate` — the tip
  advances on a rejection too.)
- **`OutOfOrder(commandNumber, current = T)`** — the number is not fresh and not the cached last
  (`> T+1` or `< T`): a desync. `commandNumber` is what we sent; `current` is the ledger's tip.
- **`Rejected(commandNumber, reason)`** — a deterministic ledger rejection (min-ada, invalid tx, …).
  Not a transport failure.

The remote client asserts each response's `commandNumber` equals the request's (mismatch →
fail-stop), then maps `Applied`/`Duplicate` → `Right(effects)`, `Rejected` → `Left`, and
`OutOfOrder`/undecodable → a **hard fail-stop** (never a `Left` — turning a desync into a per-request
verdict would diverge the peer). A window of one suffices because JL is strictly single-in-flight.
The consumer-side command number lives on JointLedger; the ledger trait exposes no
`currentCommandNumber` query (JL owns and persists the authoritative number).

## Recovery: mapping the coordination index to ledger state

The ledger persists its own durable record, keyed by the coordination index:

- a **sparse log** `index → command`, holding every command that returned a verdict of *apply*
  (valid commands only — invalid ones consume a number but are not logged);
- periodic **snapshots** keyed by index (a replay accelerator);
- the **tip `T`** (highest index seen, applied *or* rejected).

`restoreTo(N)` reconstructs the ledger state as of index `N`: load the latest snapshot `≤ N`, re-fold
the sparse log in `(snapshot, N]`, and set the tip to `N`. Because rejected commands change no state,
folding the applied subset `≤ N` yields exactly the state at `N`. `restoreTo(N > T)` is a corruption
tripwire (it should never happen — see below).

**Cross-store consistency comes from ordering, not a distributed transaction.** The ledger durably
logs a command *before it responds*, and JL persists its index *only at block completion* (after
every command in the block got a verdict, the transport having retried through any connectivity
loss). So:

- the ledger's durable tip is **always ≥ JL's saved index** — a transport failure just prevents the
  block from completing, saving nothing; `restoreTo(N > T)` is therefore a tripwire, not a live path;
- if the ledger is *ahead* of JL's saved index (crash after some commands, before block completion),
  `restoreTo(saved)` folds only `≤ saved` and rolls the extra back, then consensus re-drives the
  tail.

Scope: this is the *local EUTXO reference ledger's* recovery, and the *contract a remote sidecar must
meet* for its own recovery; JL saving the index is backend-agnostic. Full remote co-anchored
recovery — a crash-time re-drive of `[anchor+1, head]` against a remote that stayed *ahead* of the
anchor, where a window of one cannot replay a command `< T` — is out of scope here; the
self-correlating wire (responses echo the number, client asserts + fail-stops) is the safe interim.

## Wire protocol

The command envelope carries the coordination number alongside the command; the response is the ADT
above. Delta from the spec (`/whitepaper/sugar-rush/commands`):

- **Command** — spec `{ "RegisterDeposit": RegisterDeposit }`; ours adds the number:
  `{ "RegisterDeposit": { "commandNumber": N, "command": RegisterDeposit } }` (same for the other
  two).
- **Response** — spec `Success | Failure`; ours is `Applied | Duplicate | OutOfOrder | Rejected`
  (each a single-key tagged object). `Applied`/`Rejected` map onto the spec's `Success`/`Failure`;
  `Duplicate`/`OutOfOrder` are new, required by the coordination contract.
- **Command payloads** — match the spec except **`userVk: ByteString`**: the contract omits it (the
  native L2 tx self-authenticates via its own witnesses, so the spec should drop it), and it omits
  the spec's `ProxyBlockConfirmation` / `ProxyRequestError`.

## Screening split

`L2Screener{ screenTx, screenDeposit }` is split out of `L2Ledger` so screening rides its own trait
(and, for a remote, its own connection) with fail-soft semantics, keeping the command-number
contract on the mutation trait where it belongs. `EutxoL2Ledger` implements both; the remote screener
is a passthrough stub until a remote screening endpoint lands.
