# Initialization-tx parsing (GUM-129)

Status: **Implementing** · Decisions locked 2026-06-30

The head must **read** its initialization transaction from the config — by *parsing* a bare
Cardano transaction — instead of **re-building** it from scratch through the tx builder. The
init-tx **builder** moves out of the head into a bootstrapping submodule that authors the config.

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

- **Config wire format**: the init tx as bare CBOR `Transaction` + plain config fields
  (`cardanoNetwork`, `headPeers`, `headParams`, `coilPeers`, `scriptReferenceUtxos`,
  `initialEvacuationMap`, per-peer `initialEquityContributions`, `blockZeroEndTime`). **No** fallback
  tx; **no** `BlockEffects`/`Block`/`BlockBrief` envelope.
- **Init-tx metadata** (`Metadata.Initialization`) carries `multisigTreasuryIx`, `multisigRegimeIx`,
  `seedIx`, **plus a new `totalEquity: Coin`** — so the init tx is *self-contained*: the head derives
  the treasury's `equity = totalEquity − fee` from the tx alone.
- **On read**, the head:
  1. deserializes the bare `Transaction`;
  2. **parses** it into a rich `InitializationTx` (via `InitializationTx.Parse`), validating
     compliance;
  3. **derives** the fallback via `FallbackTx.Build` from the parsed init tx (the fallback is
     protocol-internal — never stored, never parsed);
  4. defers any check that the consumed input utxos *exist* on L1 to `CardanoLiaison`.
- **Init-tx builder** (`InitializationTx.Build` + its funding/change/balance bookkeeping) lives in a
  new **bootstrapping submodule**, not the head runtime. The head keeps only
  `InitializationTx.Parse` (+ `FallbackTx.Build`).

### What the head no longer needs

- The `withInitTxDecoder` change-output extraction (`HeadConfig.scala:463-482`).
- The `additionalFundingUtxos` / `changeUtxos` **fields** on `InitializationTx`
  (`InitializationTx.scala:46-47`) and the matching `InitializationParameters` build-fields
  (`additionalFundingUtxos` / `initialChangeOutputs`, `initialFundingUtxos`, `initialFundingValue`).
  Funding inputs are in `tx.body.value.inputs`; the seed is at the metadata `seedIx`; change goes to
  the funders, not the head.
- The `isBalancedInitializationFunding` check — a parsed tx is already a balanced Cardano tx.
- `resolvedUtxos` in the config — the init tx may spend Plutus-locked inputs, but resolution is an
  L1 concern handled by `CardanoLiaison`.

> Keep `treasuryProduced` / `multisigRegimeProduced` on `InitializationTx`: they *are* consumed (the
> fallback build takes them). The `:37-38` TODO notes they're fishable from the tx — a separate
> cleanup, out of scope here.

## Implementation plan

Ordered so each step compiles and is testable. (`just build-werror`; `sbtn "testOnly *InitializationTxSeq* *ConfigurationCodec* *Metadata*"`.)

**A. Metadata: add `totalEquity`** (Task 1). In `Metadata.Initialization` (`Metadata.scala:172-240`)
add `totalEquity: Coin`; extend `asMap` and the `parseInner` parser (mirror the existing index
fields; reuse the `Metadatum.Int` shape). `InitializationTx.Build` writes it
(`config.initialEquityContributed`); `InitializationTx.Parse` reads it from metadata instead of
config. Keep Build working for now.

**B. Make `InitializationTx.Parse` self-sufficient.** Stop reading `additionalFundingUtxos` /
`changeUtxos` / `initialEquityContributed` from config (`InitializationTx.scala:537-538`, `:510`):
take equity from the metadata `totalEquity`, leave funding inputs in the tx body, and drop the
change-output reconstruction. Result: `Parse` depends only on the tx + metadata + the plain config
(peers/params/network/script-refs/evac-map/equity-split/`blockZeroEndTime`).

**C. Wire config-read to parse + give the config its own codec** (Task 4). Replace the
`InitializationTxSeq.Build` call in `HeadConfig.headConfigDecoder` with a parse path (reuse
`InitializationTxSeq.Parse`'s parse-init→`FallbackTx.Build` flow; drop its body-compare against a
supplied fallback). Add a dedicated config encoder/decoder for the init tx as **bare CBOR** — not
`initialBlock.asJson` / `BlockEffects.Unsigned.Initial.deriveEncoder` (`BlockEffects.scala:32`). Stop
serializing the fallback. Rename `BlockCreationEndTime` / `brief.endTime` → **`blockZeroEndTime`**,
kept as an explicit plain config field.

**D. Evaluate on parse** (Task 3). Validate the parsed tx (don't trust the producer), but **don't
require the consumed utxos to exist** — defer existence/resolution to `CardanoLiaison`. Depth of
phase-2 evaluation without live inputs is an implementation detail here.

**E. Remove the dead fields** (folds into Task 2). Delete `additionalFundingUtxos` / `changeUtxos`
from `InitializationTx`; remove the `InitializationParameters` build-fields and the
`headConfigBootstrap` projection that fed them (`HeadConfig.scala:58-59`); update
`InitializationTxCodec` (persistence) to drop them (the full `tx` is still stored).

**F. Move the builder out** (Task 2). Relocate `InitializationTx.Build` (and `InitializationTxSeq`'s
init-build half + the funding/change/balance bookkeeping) into a new **bootstrapping submodule** that
authors the config. The head runtime keeps `InitializationTx.Parse` + `FallbackTx.Build`.

**G. Tests** (Task 5). Delete the stale commented-out "semantic parse == built" assertion
(`InitializationTxSeqTest:236-279`) — we are *not* keeping a parse-vs-build property. Add: byte-identity
(parsed `InitializationTx.tx` equals the stored bytes); parse of a known-good init tx reconstructs the
expected `InitializationTx` + the expected derived fallback; tamper-rejection; and a **cross-module
round-trip** — the bootstrap submodule's output parses cleanly with the head's reader.

## Decisions (locked 2026-06-30)

- **One metadata enrichment: `totalEquity`** → self-contained init tx. Nothing else added; the evac
  map is in the treasury datum, funding/change are in the tx body, and the per-peer equity split
  stays plain config (the **fallback** needs it for `distributeEquity`). Validation may check the
  split sums to the metadata total.
- **Fallback is derived, never stored/parsed.** Built by the head via `FallbackTx.Build`; the config
  carries no fallback. No `FallbackTx.Parse`.
- **Input resolution is an L1 concern** (`CardanoLiaison`), not carried in config/metadata.
- **Evaluate the init tx on parse**; don't require its input utxos to exist.
- **Builder leaves the head** for the bootstrapping submodule.
- **Keep `blockZeroEndTime` explicit** in config (don't derive from the TTL for now).

## Out of scope / dependencies

- **GUM-167** (CIP-67-compliant metadata asset names) — out of scope. (The `Metadata.scala:22`
  comment links gh #260, which is unrelated/stale.)
- **#338** ("Equity tracking in `MultisigTreasuryUtxo`") — **closed**; not a blocker. It added
  `equity: Coin` and threaded it through the builders (init fee now paid from equity).
- Metadata FIXMEs: non-negativity of indices (`Metadata.scala:171`), single-head assumption
  (`:314-316`) — fold in or defer.
- How adversarial-safe the parse evaluation must be (vs today's crash-avoidance,
  `InitializationTxSeq.scala:99-101`) — an implementation detail of step D.
