# Hydrozoa Scala Style Guide

Conventions that are **not** enforced by scalafmt/scalafix and so must be applied by
hand. The mechanical settings (4-space indent, 100-col lines, import sorting, the
scalafix rule set) live in `CLAUDE.md` and the tool configs; this document covers the
judgement calls.

## Naming

### Functions are verbs

Name `def`s as verb phrases (an action), not nouns. `mk*` (make) counts as a verb.

```scala
def witnessMap(...)        // ✗ noun
def aggregateSignatures(...)  // ✓ verb phrase
```

Pure projection accessors (`regularPartitions`) are a gray area — a noun accessor is
fine; don't force an awkward `get*`. Use the IDE rename refactor so call sites and
backtick doc-links follow.

### No ad-hoc contractions

Prefer, in order: the codebase's **established term** for the concept, else the **full
word**. Never invent an abbreviation.

```scala
collectEvacSignatures   // ✗ "evac" is an ad-hoc contraction
collectSecSignatures    // ✓ SEC = Standalone Evacuation Commitment (established term)
addOpt                  // ✗
addOptional             // ✓ full word
```

Accepted universal exceptions: `tx` (transaction), `id` (identity/identifier). Kept as
established terms (not contractions to "fix"): the `mk*` prefix, `VKey`/`vk`, `SEC`.
Single-letter locals (`m`, `e`, `vk`) are out of scope — this targets named defs/types.

### Reserved domain words

- **"genesis"** — never use it for Hydrozoa's initial stack / stack 0 / initialization
  tx; it collides with **Cardano genesis**. Say "initial", "initialization", or
  "stack 0". Exempt: the real `Genesis` payout-obligation type and the L2 `Genesis` tx
  (`obligation/Genesis.scala`, `eutxol2/tx/L2Genesis.scala`, …) are defined domain terms.
- **"broadcast"** — use it only for messages that go **over the network to other
  peers**. For a message sent to *local* actors (even via a list of refs / `BroadcastOps`)
  say "fan out" / "announce" / "propagate". Example: `Block.SoftConfirmed.Next` is
  *fanned out* to local actors; an own soft-ack travelling via PeerLiaison's outbox to
  remote peers *is* broadcast.

## Code organization

### Public-first, commented; privates topological

1. **Every public definition has a doc comment** (`/** … */`). Private defs don't
   require one (keep any that aid understanding).
2. **Private definitions come after all public ones**, ordered **topologically —
   caller before callee**, so the file reads top-down: public API first, then helpers
   in call order, leaves last.

Example — `HardAckVerifier`: documented public `resolvePeerVKey` / `verify2PhaseRound1`
/ `verify2PhaseRound2` / `verifySole`, then private `verifyRound1Regular` →
`verifyRound2Initial` → `verifyPartition` → `verifySecOpt` → `verifyTxList` →
`verifyHeader` → `verifyTx`.

### Extensions live in objects

Every Scala 3 `extension` block must be declared **inside an `object`** — never bare in
a class body or at top level. Put it in the object that owns the extended type; callers
`import TheObject.*`. E.g. the `WitnessMap` `add`/`addOptional`/`addZip` extension lives
in `object WitnessMap` alongside the `type WitnessMap` alias.

### No inline fully-qualified names

Never spell out an FQN inline (in givens, `using`/`?=>` clauses, `summon[…]`, type
ascriptions, method bodies, return types, `Resource.eval(…)`). Add a top-of-file
`import` instead. A multi-segment path forces the reader to parse it before reaching
the symbol; one import line costs nothing.

```scala
given hydrozoa.config.head.network.CardanoNetwork.Section = …  // ✗
// import at top, then:
given Section = …                                              // ✓
```

### Rule of least knowledge for parameters

Give every function the **minimal data** it needs. Preference order: individual fields >
the narrowest config section / trait > the whole config object. Apply by default, not
only when asked.

```scala
def mkEffectsInitial(config: HeadConfig.Section)              // ✗ uses only two fields
def mkEffectsInitial(initializationTx, initialFallbackTx)     // ✓ caller projects them
```

"Minimal, not dogmatically tiny": if a function forwards the whole section onward (e.g.
`mkEffectsRegular` passing `config` to `SettlementTxSeq.Build`), the section *is* the
minimal set — leave it.

## Comments

- Describe the code **as it is now** — no historical perspective. No "formerly X",
  "moved to the slow side", "as of step N", "no longer a stub", "renamed from", "was
  pre-armed". History lives in Git and memory.
- Keep a comment **scoped to the local code's concern**. Don't explain downstream or
  other-actor internals from a method that doesn't do that work — a fast-side
  `JointLedger` path should not describe slow-side KZG/SEC/settlement mechanics; say only
  what's locally true.
- **Don't cite `design/` paths that may move, and never reference `.scratch/`** (it is
  gitignored). To point at a design doc, link a committed file under `design/`; if the
  note lives only in scratch, inline the relevant point instead of leaving a dangling
  pointer.

## Types and givens

### No phantom signing-stage parameter

For ADTs that come in signing-stage variants (Unsigned vs HardConfirmed), prefer an
explicit split into sibling subtypes — the `BlockEffects` / `StackEffects` pattern —
over a generic `+S` type parameter on a single ADT, **even when only one field differs**.

**Why:** with `+S`, code handling "a partition regardless of stage" must hold a
`PartitionEffects[?]`; pattern matches like `case m: Major[?] => m.sec` then surface the
SEC as `Option[?]` — opaque. Pattern matching is a primary code-organization tool here;
preserving it outweighs the one-field deduplication `+S` saves. Default to the explicit
`.Unsigned` / `.HardConfirmed` split. (`PartitionEffects[+S]` is the lone grandfathered
holdout; refactor it to the split shape next time it's touched.)

### Opaque type conversions to tuples

When an opaque type wraps a tuple whose elements are themselves opaque types, the
`Conversion` given **must** call `.convert` explicitly on each element. Using `identity`
or an implicit coercion in the lambda body causes either a stack overflow or an
infinite loop.

**Pattern 1 — `identity` body:**

```scala
opaque type Id = (HeadPeerNumber, RequestNumber)
given Conversion[Id, (Int, Long)] = identity  // BROKEN
```

1. Compiler elaborates `identity[Id]`, type `Id => Id`.
2. Return type `Id` must coerce to `(Int, Long)`.
3. Searches for `Conversion[Id, (Int, Long)]` — finds the given currently being
   elaborated (Scala 3 puts a given in scope during its own body elaboration).
4. Recurse to step 1 → stack overflow.

**Pattern 2 — implicit coercion in lambda body:**

```scala
given Conversion[Id, (Int, Long)] = id => id._1 -> id._2  // BROKEN
```

1. `id._1` gives `HeadPeerNumber`, which must coerce to `Int`.
2. Two candidates: `Conversion[HeadPeerNumber, Int]` (remote) and
   `Conversion[Id, (Int, Long)]` (local). **Local scope wins.**
3. Compiler rewrites `id._1: Int` as `given_Conversion.apply(id)._1` — applies the whole
   conversion to `id` first, then projects `._1`.
4. That calls the lambda again with the same `id` → infinite loop.

**Fix** — call `.convert` explicitly so the compiler resolves to each element's own
companion-scope conversion:

```scala
given Conversion[Id, (Int, Long)] = id => id._1.convert -> id._2.convert  // CORRECT
```

See `RequestId.scala` for the canonical working example.

**Safe exception:** `identity` is safe when the opaque type wraps a tuple of
**primitive** (non-opaque) types, because no element-level conversion search is needed:

```scala
opaque type Full = (Int, Int)
given Conversion[Full, (Int, Int)] = identity  // safe — underlying IS (Int, Int)
```

## Boolean predicates — `is*` / `has*` / `can*` naming

### Rule

A `def` or `val` that returns `Boolean` is named as a predicate that reads like a
yes/no question — prefix it with `is`, `has`, `can` (or another boolean-flavoured
verb) so call sites read as assertions:

```scala
private def isSaturated(present: Set[PeerId]): Boolean = ...
if isSaturated(cell.round1.keySet) then completeRound1(...)   // "if … is saturated"
```

Not:

```scala
private def saturated(present: Set[PeerId]): Boolean = ...
if saturated(cell.round1.keySet) then ...                     // reads as a noun, hides the test
```

### Why

`if isSaturated(x)` reads as a sentence; `if saturated(x)` reads as a noun and
hides that it is a test. The prefix also disambiguates a boolean check from a
value of the same root name (`val saturated: Set[…]` vs `def isSaturated:
Boolean`). `HeadPeerId.isLeader` / `isSlowLeader` are the in-repo examples.

### Scope

Pick the prefix that fits: `is*` for a state (`isSaturated`, `isLeader`), `has*`
for presence (`hasQuorum`), `can*` for capability (`canSubmit`). Don't force `is`
onto established collection-style predicates that already read as a test
(`contains`, `exists`, `startsWith`).

## Config holds pure data and pure functions — not behavior

### Rule

A config type and its `*.Section` traits (e.g. `OwnPeerPublic`, `HeadConfig`,
`TxTiming`) expose **identity / parameter data** and **pure functions**. Two
things must stay out:

- **IO actions** — anything returning `IO[_]` belongs in an actor, not config.
- **Single-use functions** — a helper consumed in exactly one place lives with
  that consumer, not in config. Domain-object *builders* are the usual offenders.

Pure functions are welcome, *especially* ones reused across many call sites:
time calculations in `TxTiming`, leadership predicates (`OwnPeerPublic.canLeadFast`
/ `canLeadSlow`), label/index helpers. A widely-shared pure function earns its
place on the config; a one-off does not.

### Example

`mkOwnSoftAck` built a `SoftAck` and was consumed only by `JointLedger`, so it
moved to `JointLedger` and dropped its `Option`. The head/coil decision is made
by the consumer matching on the pure identity `config.ownPeerId`; the builder is
a total function over a head peer number:

```scala
// JointLedger — the producer owns the builder
private def mkOwnSoftAck(peerNum: HeadPeerNumber, brief: BlockBrief.Next, …): SoftAck = …
_ <- config.ownPeerId match {
    case PeerId.Head(peerNum) => conn.fastConsensusActor ! mkOwnSoftAck(peerNum, brief, …)
    case PeerId.Coil(_)       => IO.unit
}
```

Not on the config seam:

```scala
// OwnPeerPrivate — DON'T: a builder, used in one place, returning Option to encode a role
def mkOwnSoftAck(brief: BlockBrief.Next, …): Option[SoftAck]
```

### Why

Config is read by nearly everything, so it grows without bound if behavior leaks
in. Keeping it to pure data + pure shared functions keeps the surface small and
honest: a reader knows config answers "what is true / who am I", not "do this".
Single-use behavior placed next to its sole consumer is easier to find and
change, and it avoids `Option`-returning factories that exist only to encode a
yes/no the caller already knows.

### Never `given x: T = summon`

When a `given` instance's body summons its own type, resolution picks the in-progress
given itself — Scala 3 keeps the symbol-being-defined in lexical scope during its own
body, and lexical scope outranks the companion's generative given. Result: silent
infinite recursion at first access, **not** a compile error.

```scala
given codec: StoreCodec[Value] = summon                  // ✗ resolves to codec → loop
given codec: StoreCodec[Value] = StoreCodec.fromCirce[Value]  // ✓ explicit factory
```

Always spell out the concrete factory (`StoreCodec.fromCirce[Value]`,
`Encoder.AsObject.derived`, …). Symptom of getting it wrong: a hang at first access with
no exception (a JVM-internal monitor wait), not a stack trace. Watch for it when
introducing the `type Value` + `given codec: StoreCodec[Value]` lower-bound pattern.

## Error handling — log before raising

Whenever you `IO.raiseError` (or otherwise throw), **emit a log message first**. A raised
exception carries only its message and stack trace — no logger name, no MDC/`Tracer`
context (peer number, block/stack, lane), and it may be caught, remapped, or swallowed
upstream before anything reaches the log. Logging at the raise site captures the context
*as it is now* and guarantees the failure is recorded even if the exception is later
absorbed.

```scala
// ✗ the cause is in the message, but no logger context and it may never reach the log
IO.raiseError(new IllegalStateException(s"$label cursor out of bounds — $detail"))

// ✓ log under the actor's logger first, then raise the same message
val message = s"$label cursor out of bounds — $detail"
logger.error(message) >> IO.raiseError(new IllegalStateException(message))
```

Use the component's existing logger / `Tracer` route (don't coin a new one just for this —
[Logging](../CLAUDE.md#logging)). Build the message once and share it between the log line
and the exception so they never drift. This applies to genuine error raises; it is not
needed for control-flow `raiseError`s that a caller is expected to handle as a normal
outcome (those should not be surprising in a log).
