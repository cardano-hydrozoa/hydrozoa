# docs/

Implementation-facing documentation for Hydrozoa/Gummiworm: how the code is
organized and how its subsystems actually work, keyed to the current sources.

These describe the system **as built**, complementing `design/` (forward-looking
specs and decision points) and the
[Gummiworm whitepaper](https://gummiworm.net/whitepaper/introduction) (the
protocol spec). For project-wide conventions, start with the style guide.

| Doc | Summary |
|---|---|
| [codecs.md](codecs.md) | Conventions for wire/storage codecs (working notes). |
| [effect-tracking.md](effect-tracking.md) | Linking blocks and requests to their L1 effects: effect identity (`l1TxId`), the reverse-index CFs, per-block decomposition, `relatedEffects`, and consensus-safe withdrawal-effect tracking (the `payoutRequestIds` side-channel + forward-contiguous payout packing + the finalization payout fix). |
| [fast-consensus.md](fast-consensus.md) | The fast cycle: per-peer soft-confirmation of block headers, eager signature collection, BlockWeaver / JointLedger / FastConsensusActor roles. |
| [integration-stages.md](integration-stages.md) | The two integration test stages under `integration/`: which stage tests what, where to add a test, what each property checks. |
| [l2-isomorphism.md](l2-isomorphism.md) | L2 isomorphism: driving the EUTXO ledger with native Cardano txs — the headId pin, mandatory tx metadata, screening vs submission, how deposits pin their L2 payload, and the `cardano-eutxo` / `any-remote` backend selection. |
| [l2-query-endpoints.md](l2-query-endpoints.md) | The user-facing server's read-only L2 queries: `GET /l2/cardano-eutxo/utxos/{address}` (CIP-0116 utxos) and `GET /l2/cardano-eutxo/transactions` (recent activity); EUTXO-only, empty on a remote-ledger node. |
| [logging-tracing.md](logging-tracing.md) | Contextual logging and tracing: Tracer, IOLocal-carried context, routing keys, migration off SLF4J MDC. |
| [observability-endpoints.md](observability-endpoints.md) | The user-facing server's `/health` (liveness) and `/ready` (readiness) endpoints: semantics, status mapping, how `NodeStatus` is maintained. |
| [rate-limiter.md](rate-limiter.md) | A generic throttling actor that slows the fast/slow cycles (longer block/stack durations) without touching consensus logic. |
| [slow-consensus.md](slow-consensus.md) | The slow cycle: turning a run of soft-confirmed blocks into a multisigned, L1-submittable set of effect transactions; StackComposer / hard-acks. |
| [style-guide.md](style-guide.md) | Hydrozoa Scala conventions: opaque-tuple conversions, naming rules (verb functions, `is*`/`has*` predicates), and other house style. |
| [testcontrol-driver.md](testcontrol-driver.md) | How `ModelBasedSuite` drives the integration suites on a cats-effect `TestControl` virtual clock. |
