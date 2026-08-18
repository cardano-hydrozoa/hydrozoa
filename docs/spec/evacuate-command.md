# `hydrozoa evacuate` — the standalone rule-based regime

`hydrozoa evacuate` starts a node **directly in the rule-based regime** from a database left by a
previous head run, plus the same two config files `serve` takes:

```
hydrozoa evacuate <head-config.json> <peer-private.json>
```

It boots only the L1 boundary — a [`RuleBasedRegimeManager`](../../src/main/scala/hydrozoa/rulebased/RuleBasedRegimeManager.scala)
owning a `CardanoLiaison` and a `RuleBasedActor` — and none of the consensus, mesh, HTTP, or L2
machinery `serve` wires. `CardanoLiaison` submits the fallback (driving the head into the rule-based
regime if it is not there yet) and finishes any in-flight rollouts; `RuleBasedActor` runs the
dispute → evacuation. The command runs **resident**: it keeps polling L1 — a rollback can
re-introduce work at any time — until the operator stops it.

The entry point is [`app/Evacuate.scala`](../../src/main/scala/hydrozoa/app/Evacuate.scala),
registered as a subcommand in [`app/Main.scala`](../../src/main/scala/hydrozoa/app/Main.scala).

## Why a separate command

The rule-based regime is normally reached by a *handoff*: the multisig `CardanoLiaison` submits a
fallback tx, observes the resulting rule-based treasury on L1, and tells
`HeadMultisigRegimeManager` to stop the multisig actors and spawn the `RuleBasedRegimeManager`
([`fast-consensus.md`](fast-consensus.md), [`coil-network.md`](coil-network.md) §5.2). That path
needs a full, live node.

`evacuate` needs only three things — the store, the head config, and the private config — because
everything the rule-based regime consumes is either on L1 or already in persistence. It is the tool
for acting on a head whose live process is gone: parse the database, find the rule-based utxos it
can act on, and drive them to completion.

## Actor topology

`RuleBasedRegimeManager` is shared by both entry paths and spawns its children accordingly, keyed on
its `ownLiaison` field:

| Child | Handoff path (`ownLiaison = None`) | `evacuate` path (`ownLiaison = Some`) |
|---|---|---|
| `RuleBasedActor` | spawned | spawned |
| `CardanoLiaison` | **not** spawned — `HeadMultisigRegimeManager` keeps its own alive across the handoff (it is the actor that reacts to an L1 rollback) | spawned here, since no multisig liaison pre-exists |
| `BlockWeaver` sink | — | a no-op actor filling the liaison's block-weaver slot |

Spawning a second `CardanoLiaison` on the handoff path would duplicate rollout submission and
rollback repair, so the manager spawns one **only** when none pre-exists. `RuleBasedRegimeManager.Config`
is therefore `RuleBasedActor.Config & CardanoLiaison.Config`; both are satisfied by `NodeConfig`.

### `RuleBasedActor`

Unchanged by this command. It holds no per-iteration state: each tick it re-queries L1 for the
treasury / regime / dispute utxos and re-reads what it cannot get from chain (SECs, evacuation-map
preimages, the fallback anchor) from `Persistence`, then dispatches on the treasury datum —
`Unresolved` → dispute (vote / ratchet / tally / resolve), `Resolved` → evacuation. It branches on
peer type internally (`PeerId.Head` vs `PeerId.Coil`), so `evacuate` needs no peer-type special
case. On a treasury that is missing (the fallback has not landed yet) it retries, so it can be
spawned before `CardanoLiaison` has submitted the fallback and simply picks the regime up once it
appears.

### `CardanoLiaison` in the rule-based regime

`CardanoLiaison` outlives the multisig regime because the rule-based regime does not subsume its
remaining L1 duties: it finishes the rollout chain of any already-settled major, and it re-applies
effects if an L1 rollback resurrects a multisig utxo. On the `evacuate` path it does one extra thing
first — walking its normal ladder to submit the fallback for whatever treasury utxo is currently on
L1 (silence-period-gated), which is how the head enters the rule-based regime at all.

Its "switch to rule-based" notification is injected as `onRuleBasedRegimeObserved`:

- under `HeadMultisigRegimeManager` it fires `HandoffToRuleBased` (the trigger that spawns the
  rule-based regime);
- under `RuleBasedRegimeManager` it is a **no-op** — there is no regime manager to hand off to, and
  the `RuleBasedActor` running alongside observes the regime off L1 itself, so re-announcing would
  just be spam.

The internal handed-off flag (`lastHandoffDispatchedFor`, which gates block-weaver forwarding and
init-tx resubmission) is set the same way in both cases. On boot the liaison restores its full
target state from persistence (`State.recover`) and re-samples L1, so a fresh liaison on the
`evacuate` path converges on the same target a live run held.

## Read-only persistence

The store is opened **read-only** (`RocksDbBackendStore.openReadOnly`). Both actors are read-only on
persistence in this mode:

- `RuleBasedActor` only reads (`persistence.get`, `Markers.derive`).
- `CardanoLiaison` only reads (`State.recover` folds the `HardConfirmation` CF). With no
  `SlowConsensusActor` feeding it new effects, its target state is fixed after boot — it never
  learns, and so never persists, a new effect.
- The startup arrival-stamp generation bump exists only so that *writers'* stamps sort after earlier
  boots; nothing here appends a journal entry, so the bump is unnecessary and off this path.

Read-only is therefore both correct and safer: it takes no exclusive lock, and a stray write throws
at the RocksDB layer. A read-only open refuses a missing or uninitialized store (it cannot create or
version-stamp one). The only caveat is that it sees a snapshot as of open — which is exactly right
for a database left by a stopped head.

## Not yet built

- **Integration coverage.** There is no `evacuate` integration test yet: seed a store to a
  hard-confirmed state, run the command against a mock / Yaci backend, and assert it submits the
  fallback, the liaison flips handed-off (with no outward notification), and `RuleBasedActor` drains
  the evacuation map — plus a rollout-in-flight case asserting the liaison completes the rollout
  chain. The rule-based behavior itself is covered by the `integration/.../rbr/` suites.
- **Recovery piggyback.** `RuleBasedRegimeManager` is the boundary-only boot that `serve`'s
  crash-recovery will spawn once it detects the head moved to the rule-based regime while it was
  down ([`persistence-and-crash-recovery.md`](persistence-and-crash-recovery.md) §8 step 7, itself
  unimplemented). The detection primitive — `CardanoLiaison`'s rule-based-treasury probe — is the
  shared piece to lift out when that lands.

## References

- Code: `app/Evacuate.scala`, `app/Main.scala`; `rulebased/RuleBasedRegimeManager.scala`,
  `rulebased/RuleBasedActor.scala`; `multisig/consensus/CardanoLiaison.scala`;
  `multisig/persistence/rocksdb/RocksDbBackendStore.scala`.
- Related: [`fast-consensus.md`](fast-consensus.md), [`coil-network.md`](coil-network.md) §5.2 (the
  shared rule-based regime), [`persistence-and-crash-recovery.md`](persistence-and-crash-recovery.md)
  (the R10 floor the rule-based regime reads, and the recovery handoff).
