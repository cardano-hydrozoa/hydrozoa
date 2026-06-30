# Initialization-tx parsing (GUM-129)

Status: **Implementing** · Decisions locked 2026-06-30

The head must **read** its initialization transaction from the config — by *parsing* a bare
Cardano transaction — instead of **re-building** it from scratch through the tx builder. The
init-tx **builder** moves out of the head into a bootstrapping submodule that authors the config.

Progress: steps A and B done (metadata `totalEquity`; `Parse` reads equity from it). Steps C–G open.

## Why

The head config is produced by **external** bootstrapping software that knows nothing about
Hydrozoa's Scala types. So the only thing it can hand the head is a **bare Scalus/Cardano
`Transaction` (CBOR)**, with the Hydrozoa metadata riding *inside* the tx (auxiliary data). The
head reconstructs its rich `InitializationTx` by **parsing** that tx.

Re-building (today's `HeadConfig.headConfigDecoder`, `HeadConfig.scala:126-174`, which calls
`InitializationTxSeq.Build`) is wrong because:

- **It forbids arbitrarily-complex-but-compliant init txs.** Re-building only ever accepts the one
  canonical tx our builder emits; a real funding/coordination setup may have extra inputs, different
  change, additional outputs. The contract must be "parse the given tx and validate it complies,"
  not "regenerate it and hope it matches."
- **It leaks internal types into the wire format.** The config is encoded as
  `BlockEffects.Unsigned.Initial` (inside `Block.Unsigned.Initial` + `BlockBrief.Initial`), forcing
  an external producer to emit our block/effects/brief envelope — and a fallback tx it has no
  business constructing.
- **Divergence risk + waste.** The rebuilt tx is not guaranteed byte-identical to the stored /
  on-chain / peer-agreed tx, and nothing asserts byte-identity; and it re-runs the full builder on
  every config read.

Not to be confused with the **persistence** codecs (`InitializationTxCodec` / `FallbackTxCodec`),
which round-trip our *own* rich types for crash recovery — the node wrote those bytes and trusts
them. Config is externally authored and untrusted, so it carries a bare tx and the head **parses +
validates**. Persistence is untouched by this work.

## Target shape

- **Config wire format**: the init tx as bare CBOR `Transaction`, plus the resolved consumed inputs
  (`resolvedUtxos`, see below), plus plain config fields (`cardanoNetwork`, `headPeers`,
  `headParams`, `coilPeers`, `scriptReferenceUtxos`, `initialEvacuationMap`, per-peer
  `initialEquityContributions`, the block-0 end time). **No** fallback tx; **no**
  `BlockEffects`/`Block`/`BlockBrief` envelope.
- **Init-tx metadata** (`Metadata.Initialization`) carries `multisigTreasuryIx`, `multisigRegimeIx`,
  `seedIx`, **plus `totalEquity: Coin`** — so the init tx is *self-contained*: the head derives the
  treasury's `equity = totalEquity − fee` from the tx alone.
- **On read**, the head:
  1. deserializes the bare `Transaction` (+ the config's `resolvedUtxos`);
  2. **parses** it into a rich `InitializationTx` (via `InitializationTx.Parse`), **evaluating** it
     against the supplied `resolvedUtxos` to validate compliance;
  3. **derives** the fallback via `FallbackTx.Build` from the parsed init tx (the fallback is
     protocol-internal — never stored, never parsed).
- **Init-tx builder** (`InitializationTx.Build` + its funding/change/balance bookkeeping) lives in a
  new **bootstrapping submodule**, not the head runtime. The head keeps only
  `InitializationTx.Parse` (+ `FallbackTx.Build`).

### Why the config carries `resolvedUtxos`

The init tx is **not required to be immediately submittable** — not all of its inputs need exist on
L1 yet (funding that lands later, a coordination flow, etc.). So the head **cannot** resolve the
consumed inputs by querying L1. But whoever *built* the tx **knows** those inputs. So the config
carries `resolvedUtxos` (the resolved consumed inputs), and parsing uses them to **evaluate** the tx.
`CardanoLiaison` still owns actual L1 submission/observation — but resolution-for-parse comes from
the config, not L1.

### What the head no longer needs

- The `withInitTxDecoder` change-output extraction (`HeadConfig.scala:463-482`).
- The `changeUtxos` **field** on `InitializationTx` (`InitializationTx.scala:47`) and the build-only
  `InitializationParameters` fields (`initialChangeOutputs`, `initialFundingValue`): the change
  outputs are present in the tx body and go to the funders, not the head.
- The `isBalancedInitializationFunding` check — superseded by evaluating the tx on parse.

> Kept on `InitializationTx`: `resolvedUtxos` (now config-supplied, used for evaluation — the
> resolved input *values* are **not** in the tx body, only the outpoints are); `treasuryProduced` /
> `multisigRegimeProduced` (consumed by the fallback build; the `:37-38` TODO notes they're fishable
> from the tx — a separate cleanup). The `seedUtxo` / `additionalFundingUtxos` representation of the
> resolved inputs is kept for now (old names); it overlaps `resolvedUtxos` and may be consolidated
> later.

## Implementation plan

Steps map to the five tasks: A→T1, C+«codec»→T4, D→T3, E+F→T2, G→T5. Ordered so each compiles and is
testable (`just build-werror`; `sbtn "testOnly *InitializationTxSeq* *ConfigurationCodec* *Metadata*"`).

**A. Metadata: add `totalEquity`** (T1) — **done.** `Metadata.Initialization` gained `totalEquity: Coin`
(`asMap` + `parseInner`); `InitializationTx.Build` writes `config.initialEquityContributed`.

**B. `InitializationTx.Parse` takes equity from the metadata** — **done.** Reads `md.totalEquity − fee`
instead of `config.initialEquityContributed` (`InitializationTx.scala:510`).

**C. Wire config-read to parse + give the config its own codec** (T4). Replace the
`InitializationTxSeq.Build` call in `HeadConfig.headConfigDecoder` with a parse path (reuse
`InitializationTxSeq.Parse`'s parse-init→`FallbackTx.Build` flow; drop its body-compare against a
supplied fallback). Add a dedicated config encoder/decoder for the init tx as **bare CBOR** plus the
`resolvedUtxos` item — *not* `initialBlock.asJson` / `BlockEffects.Unsigned.Initial.deriveEncoder`
(`BlockEffects.scala:32`). Stop serializing the fallback. **Keep current names** (no
`BlockCreationEndTime` → `blockZeroEndTime` rename, no field renames, for now).

**D. Evaluate on parse** (T3). `Parse` evaluates the tx against the config-supplied `resolvedUtxos`;
it does **not** require those inputs to exist on L1 (the tx may not be submittable yet). On-chain
existence / submission stays `CardanoLiaison`'s job. Depth of phase-2 evaluation is an
implementation detail here.

**E. Remove the dead bits** (folds into T2). Delete `changeUtxos` from `InitializationTx`; remove the
build-only `InitializationParameters` fields (`initialChangeOutputs`, `initialFundingValue`) and the
`isBalancedInitializationFunding` check; reconcile the `headConfigBootstrap` projection
(`HeadConfig.scala:58-59`); update `InitializationTxCodec` (persistence). Keep the resolved-inputs
carriers.

**F. Move the builder out** (T2). Relocate `InitializationTx.Build` (and `InitializationTxSeq`'s
init-build half + the funding/change/balance bookkeeping) into a new **bootstrapping submodule** that
authors the config. The head runtime keeps `InitializationTx.Parse` + `FallbackTx.Build`. (Submodule
shape — sbt subproject vs package — to be decided when we reach this step.)

**G. Tests** (T5). Delete the stale commented-out "semantic parse == built" assertion
(`InitializationTxSeqTest:236-279`) — we are *not* keeping a parse-vs-build property. Add the
acceptance-criteria tests below.

## Decisions (locked 2026-06-30)

- **One metadata enrichment: `totalEquity`** → self-contained init tx. Nothing else added; the evac
  map is in the treasury datum, and the per-peer equity split stays plain config (the **fallback**
  needs it for `distributeEquity`). Validation may check the split sums to the metadata total.
- **Fallback is derived, never stored/parsed.** Built by the head via `FallbackTx.Build`; the config
  carries no fallback. No `FallbackTx.Parse`.
- **`resolvedUtxos` is carried in the config**, supplied by the bootstrap producer (who built the tx
  and knows its inputs). Parse uses it to evaluate the tx. It is **not** fetched from L1 — the init
  tx need not be immediately submittable, so its inputs may not exist on L1 yet.
- **Evaluate the init tx on parse**; don't require its input utxos to exist on L1 (that's
  `CardanoLiaison`'s concern).
- **Builder leaves the head** for the bootstrapping submodule.
- **Keep current names for now** — no `BlockCreationEndTime` → `blockZeroEndTime` rename, no field
  renames.

## Acceptance criteria

- Reading a `HeadConfig` *parses* the stored init tx; the in-memory `InitializationTx.tx` is
  **byte-identical** to the stored bytes (new test — today none exists).
- Parsing a known-good (bootstrap-produced) init tx + its `resolvedUtxos` reconstructs the expected
  rich `InitializationTx`, and the head-derived fallback is the expected one.
- A config whose stored init tx doesn't comply (under evaluation) is **rejected**, not silently
  accepted.
- The config JSON carries **no fallback tx**; the head derives it from the parsed init tx.
- No init-tx building in the head's config-read path: `InitializationTxSeq.Build` /
  `InitializationTx.Build` are gone from it (and ultimately from the head — step F), along with the
  `withInitTxDecoder` change-output extraction and the `isBalancedInitializationFunding` check.
- The stale commented-out "semantic parse == built" assertion (`InitializationTxSeqTest:236-279`) is
  **deleted**, not revived.
- Cross-module round-trip: the bootstrap submodule's output parses cleanly with the head's reader.

## Out of scope / dependencies

- **GUM-167** (CIP-67-compliant metadata asset names) — out of scope. (The `Metadata.scala:22`
  comment links gh #260, which is unrelated/stale.)
- **#338** ("Equity tracking in `MultisigTreasuryUtxo`") — **closed**; not a blocker. It added
  `equity: Coin` and threaded it through the builders (init fee now paid from equity).
- Metadata FIXMEs: non-negativity of indices (`Metadata.scala:171`), single-head assumption
  (`:314-316`) — fold in or defer.
- The `BlockCreationEndTime` → `blockZeroEndTime` rename — deferred (keep current names).
- How adversarial-safe the parse evaluation must be (vs today's crash-avoidance,
  `InitializationTxSeq.scala:99-101`) — an implementation detail of step D.
