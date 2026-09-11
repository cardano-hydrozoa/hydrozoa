# Crash-recovery testing

For whoever adds or changes a recovery test. It describes the one injection seam every such test
uses, the three decorators built over it, what each existing test proves, and what is not covered
yet.

The recovery contract itself is `persistence-and-crash-recovery.md`; this document is about how it
is exercised.

## One seam: a `Persistence` decorator

Every durable mutation in a node funnels through `Persistence[IO]`'s `put`, `delete` and `write`,
and every actor uses it — each `PeerLiaison*.persistInbound` (the CR8 write-before-advance point),
`StackComposer`, `FastConsensusActor`, `SlowConsensusActor`, `CoilAckSequencer`,
`RequestSequencer`, `JointLedger`. So a single decorator over one peer's `Persistence` observes
every durable op that peer makes, with no change to any actor and none to the backend.

Tests install one through the harness:

```scala
MultiPeerHeadHarness.Hooks.wrapPersistence: (PeerId, Persistence[IO]) => Persistence[IO]
```

Reads (`get`, `getOrFail`, `arrivalStamp`, `zeroTimes`, `wallClockOf`, `backend`) delegate
untouched and are never counted. Under the stage4 `TestControl` clock the op order is
deterministic, so "the N-th write" is reproducible.

## Three decorators, three questions

| decorator | asks | method |
|---|---|---|
| `CrashingPersistence` | does the peer come back from a process death at *this* write? | count ops, fire at the N-th |
| `DurabilityOrderOracle` | is the store ever in a state that would refuse to boot? | evaluate the boot gate after *every* write |
| `SlowPersistence` | does the write ordering survive a slow disk? | cede the fiber N times before each write |

They are deliberately separate. Crash injection samples one moment out of thousands; the other two
exist because the defects they catch are not reachable by sampling.

### `CrashingPersistence`

Two variants, because both barriers matter:

- **`Before`** — crash before the op's write reaches the backend. Nothing persisted; the sender
  re-derives or the user resubmits.
- **`After`** — let the write land, then crash before the handler's next step (advance cursor,
  send, dispatch). This is the CR4 / CR8 point.

**It signals; it does not block.** At the N-th op it completes `Plan.signal` and returns normally,
and the fixture races that signal to stop the peer's actor subtree. Blocking the op — `IO.never`
or raising and parking — would stall the actor's mailbox loop, so `stop`'s `Terminate` could never
be processed and system shutdown would hang.

That is sound because everything past the crash point is in-memory state a restart discards: the
durable store reflects exactly what landed, with the op's write included under `After` and skipped
under `Before`.

**It has one consequence every oracle must respect.** Between the signal and the fixture's stop,
the "crashed" peer is still live and still signing. So an assertion on the *rest of the head*
making progress can pass without the victim ever recovering — `CoilCrashRestartTest` records a
control run that skipped the restart entirely and still saw the head confirm. Assert on the
restarted peer's own anchor instead.

### `DurabilityOrderOracle`

`ReplayActor.validateInvariants` refuses to boot a peer whose store has `confirmed > acked` on
either arm — fast `softConfirmed ≤ fastBlockMark`, slow `hardConfirmed ≤ hardAckedStack`. That gate
reads the store once, at boot. The property it encodes is a property of the *write ordering*, so it
has to hold at every instant the store is observable: any moment it does not is a moment at which a
process death would brick that peer.

The oracle evaluates the gate after every durable write on every peer, so a violated ordering is
caught on the write that violates it. Violations are recorded rather than raised, so one run
reports every point at which the store was unrecoverable rather than only the first.

A window a handful of instructions wide is not something crash injection will find. This is how
such a window is found instead.

### `SlowPersistence`

Cedes the fiber a fixed number of times before each durable write, making one peer's store slow
relative to the rest of the node.

A race between two fibers of the *same* peer — one writing a confirmation, the other writing the
block result it must not outrun — does not reproduce under `TestControl`: the scheduler is
single-threaded, and the writing fiber runs straight through to its own write, so the losing
interleaving is never sampled. On a real multi-core runtime it is sampled constantly, which is how
such a bug reaches a fleet without the suite noticing.

Ceding hands every other eligible fiber a turn at exactly the point a slow disk would. Correct
ordering is unaffected — the write that must come first has already happened, and no amount of
yielding lets the other overtake it. Ordering that depends on winning a race fails on every run.

`IO.cede` rather than a sleep, so it costs no virtual time and leaves the scenario's timing intact.

## Restart primitives

`Hooks.restartHeadPeer` and `Hooks.restartCoilPeer` stop the peer's actor subtree and re-spawn it
against the same durable substrate. All peers share one `ActorSystem`; every consensus actor is a
child of that peer's regime-manager root, so stopping the root cascade-stops the subtree.
`HeadMultisigRegimeManager.preStartLocal` runs `ReplayActor.replay` before the connection barrier
opens, so a rebuilt manager against the same store replays inline.

| across a restart | |
|---|---|
| **persists** | the consensus `BackendStore`; **and the L2 ledger store** |
| **rebuilt fresh** | `PeerMetrics`, `Connections`, `SubmissionClient`, the CardanoLiaison tick fiber |

The L2 store is not optional. `JointLedger` recovery co-anchors L2 through
`l2Ledger.restoreTo(commandNumber)`, and against a fresh store that restore has nothing to fold.

Two constraints on callers:

- The initial `peers` map entry goes **stale** after a restart. Use the handle the restart returns.
- Direct transport only — a crash-restart under WebSocket transport is unsupported.

## What the tests cover today

| test | asserts |
|---|---|
| `CrashRestartTest` | a head peer crashed at an early durable write during bring-up recovers from its store and rejoins the running head |
| `CoilCrashRestartTest` | a coil crashed mid-run recovers and keeps applying blocks — asserted on the restarted coil's own `fastBlockMark`, which it writes once per block it applies |
| `DurabilityOrderTest` | no store outruns its own durability, across six topologies (1+0, 1+1, 1+2, 2+1, 2+2, 3+3) × three yield settings (0, 2, 8) |
| `RestartAfterBlocksTest` | a peer restarted *after producing blocks* resumes. **Both cases are `ignore`, not `test`** — see GUM-321 |

`DurabilityOrderTest`'s topology ladder is smallest-first, each rung adding one source of
concurrency, so a violation that appears at a given rung names the thing that rung introduced. The
zero-yield setting is the control: it must pass whatever the write ordering is.

## Not covered

Neither built nor scheduled here. The first two are gaps in what single-peer crash injection can
reach at all, and are the reason this list is worth keeping:

1. **The liaison-*link* recovery.** A link has two recovery endpoints, and crashing one peer at a
   time always pairs a recovered endpoint with a live one. Two specific paths therefore never run:
   - **cold↔cold reconnect.** Both peers restore cursors from disk, both outboxes come back empty,
     and each must `backfill` the other from its journal while the pull chain re-forms. The
     outbox-as-journal-view exists precisely for this. A single-peer crash always leaves one warm
     end with a full in-memory outbox.
   - **serve-below-floor.** Even on the victim-as-server side, `backfill` is reached only if the
     remote's cursor lags the victim's high-water at restart, and a live remote pulls continuously
     and stays converged. It has to be forced by engineering a delivery gap, not left to
     incidental timing.

   Rotating the victim does not fix either — every iteration still has a warm counterpart. What
   would: crash and restart **both** endpoints of one link, once per link shape (head↔head,
   hub↔coil, coil↔hub). That is in-contract; the contract forbids a peer acting as another's
   durability backstop, not two independently-recovered peers reconnecting.

2. **Role rotation.** Liaison serve and persist logic differs by role — a hub serves the whole
   population and runs `CoilAckSequencer`; a coil serves only its own hard-ack — so a victim should
   be rotated across head-leader, head-that-is-a-hub, and coil.

3. **An exhaustive sweep** over N × {`Before`, `After`} × victim role on one canonical workload,
   and a ScalaCheck breadth layer over workload and N. Cost is O(writes) per run across O(writes)
   runs, so it is a nightly shape rather than a per-commit one.

4. **Crash coupled to a fallback deadline.** A clock axis rather than a peer-count axis, and it
   needs a different oracle: assert clean evacuation, not equivalence to a crash-free run.

5. **Durability-backstop crashes** — crashing one peer while another is mid-recovery *and* depends
   on it to re-send lost data. Out of contract by design; if tested later, the assertion is that
   the head halts safely without equivocation, not that it recovers.

## Open questions

1. Does `stop` on a regime-manager root fully drain the child subtree's mailboxes before the
   rebuild, or can a zombie fiber still write to the retained store after the crash point?
2. Is the crash-free reference run stable across benign timing perturbation? Divergence is only
   evidence of a recovery bug if it is.
3. Should a recovered peer's store be compared against the reference peer's store at the end? It is
   a stronger oracle than the observational one, and it couples the test to internal layout.
