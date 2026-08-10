# Initialization-tx parsing

The head **reads** its initialization transaction from the config — by *parsing* a bare
Cardano transaction — instead of **re-building** it from scratch through the tx builder. The
init-tx **builder** lives outside the head, in a bootstrapping package (`hydrozoa.bootstrap`) that
authors the config.

## Why

The head config is produced by **external** bootstrapping software that knows nothing about
Hydrozoa's Scala types. So the only thing it can hand the head is a **bare Scalus/Cardano
`Transaction` (CBOR)**, with the Hydrozoa metadata riding *inside* the tx (auxiliary data). The
head reconstructs its rich `InitializationTx` by **parsing** that tx.

Re-building the init tx from scratch (the former `HeadConfig.headConfigDecoder` path, which called
`InitializationTxSeq.Build`) is the wrong contract because:

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
validates**. Persistence is unaffected: it is separate from config parsing.

## Shape

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
  2. **parses** it into a rich `InitializationTx` (via `InitializationTx.Parse`), reconstructing it
     from the supplied `resolvedUtxos`;
  3. **derives** the fallback via `FallbackTx.Build` from the parsed init tx (the fallback is
     protocol-internal — never stored, never parsed).
- **Init-tx builder** (`InitializationTx.Build` + its funding/change/balance bookkeeping) lives in
  the **`hydrozoa.bootstrap` submodule**, not the head runtime. The head keeps only
  `InitializationTx.Parse` (+ `FallbackTx.Build`).

### Why the config carries `resolvedUtxos`

The init tx is **not required to be immediately submittable** — not all of its inputs need exist on
L1 yet (funding that lands later, a coordination flow, etc.). So the head **cannot** resolve the
consumed inputs by querying L1. But whoever *built* the tx **knows** those inputs. So the config
carries `resolvedUtxos` (the resolved consumed inputs), and parsing uses them to **resolve** the tx's
consumed inputs. `CardanoLiaison` still owns actual L1 submission/observation — but
resolution-for-parse comes from the config, not L1.

### What the head no longer needs

- The `withInitTxDecoder` change-output extraction.
- The `changeUtxos` **field** on `InitializationTx` and the build-only `InitializationParameters`
  fields (`initialChangeOutputs`, `initialFundingValue`): the change outputs are present in the tx
  body and go to the funders, not the head.
- The `isBalancedInitializationFunding` check — superseded by evaluating the tx on parse.

> Kept on `InitializationTx`: `resolvedUtxos` (config-supplied, used for reconstruction — the
> resolved input *values* are **not** in the tx body, only the outpoints are); `treasuryProduced` /
> `multisigRegimeProduced` (consumed by the fallback build; a TODO notes they're fishable from the
> tx — a separate cleanup). The `seedUtxo` / `additionalFundingUtxos` representation of the resolved
> inputs is retained (its `seedUtxo` / `additionalFundingUtxos` names overlap `resolvedUtxos`).

## How config-read works

**A. Metadata carries `totalEquity`.** `Metadata.Initialization` holds `totalEquity: Coin`
(`asMap` + `parseInner`); `InitializationTx.Build` writes `config.initialEquityContributed`.

**B. `InitializationTx.Parse` takes equity from the metadata.** It reads `md.totalEquity − fee`
rather than `config.initialEquityContributed`.

**C. Config-read parses, and the config has its own codec.** `HeadConfig.headConfigDecoder` takes a
parse path (reusing `InitializationTxSeq.Parse`'s parse-init→`FallbackTx.Build` flow, without any
body-compare against a supplied fallback). A dedicated config encoder/decoder serializes the init tx
as **bare CBOR** plus the `resolvedUtxos` item — *not* `initialBlock.asJson` /
`BlockEffects.Unsigned.Initial.deriveEncoder`. The fallback is not serialized.

**D. Parse reconstructs the init tx.** `Parse` deserializes the bare tx and reconstructs a rich
`InitializationTx` from the config-supplied `resolvedUtxos`; it does **not** require those inputs to
exist on L1 (the tx may not be submittable yet). On-chain existence / submission stays
`CardanoLiaison`'s job.

> Known limitation: full phase-2 evaluation-on-parse (rejecting non-compliant txs) is not yet
> implemented.

**E. No dead build-time fields.** `additionalFundingUtxos` / `changeUtxos` are absent from
`InitializationTx` (they were build-time bookkeeping read by nothing at runtime). The bootstrap
builder and `InitializationTx.Parse` do not set them; `InitializationTxCodec` does not persist them
(older persisted records still decode — circe ignores the extra fields). `InitializationParameters`
still *requires* them for the bootstrap builder, so the `headConfigBootstrap` projection re-derives
them from the parsed `initTx`: funding inputs = `resolvedUtxos.utxos − seedInput`; change outputs =
tx outputs other than the treasury/multisig-regime outputs (indices taken from
`treasuryProduced`/`multisigRegimeProduced`).

**F. The builder lives outside the head.** `InitializationTx.Build` (and `InitializationTxSeq`'s
init-build half + the funding/change/balance bookkeeping) lives in the `hydrozoa.bootstrap` submodule
that authors the config. The head runtime keeps `InitializationTx.Parse` + `FallbackTx.Build`.

## Decisions

- **One metadata enrichment: `totalEquity`** → self-contained init tx. Nothing else added; the evac
  map is in the treasury datum, and the per-peer equity split stays plain config (the **fallback**
  needs it for `distributeEquity`). Validation may check the split sums to the metadata total.
- **Fallback is derived, never stored/parsed.** Built by the head via `FallbackTx.Build`; the config
  carries no fallback. No `FallbackTx.Parse`.
- **`resolvedUtxos` is carried in the config**, supplied by the bootstrap producer (who built the tx
  and knows its inputs). Parse uses it to resolve the tx's consumed inputs. It is **not** fetched
  from L1 — the init tx need not be immediately submittable, so its inputs may not exist on L1 yet.
- **Evaluate the init tx on parse**; don't require its input utxos to exist on L1 (that's
  `CardanoLiaison`'s concern).
- **Builder leaves the head** for the bootstrapping submodule.
- **Names retained** — `BlockCreationEndTime` is not renamed; field names are unchanged.

## Guarantees

- Reading a `HeadConfig` parses the stored init tx; `InitializationTx.tx` is byte-identical to the
  stored bytes.
- Parsing a bootstrap-produced init tx + its `resolvedUtxos` reconstructs the rich
  `InitializationTx`, and the head-derived fallback matches.
- The config JSON carries no fallback tx; the head derives it from the parsed init tx.
- The head's config-read path does no init-tx building: `InitializationTxSeq.Build` /
  `InitializationTx.Build` are absent from it (and from the head runtime), as are the
  `withInitTxDecoder` change-output extraction and the `isBalancedInitializationFunding` check.
- The bootstrap submodule's output parses cleanly with the head's reader (cross-module round-trip).

## Out of scope

- CIP-67-compliant metadata asset names are out of scope.
- `BlockCreationEndTime` naming is retained; the field is not renamed.
