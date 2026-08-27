# The head parameters hash

For anyone changing `HeadParameters`, `HeadConfig`, or the multisig treasury datum: this
document defines `headParamsHash` — the digest that pins a head's agreed configuration — and
where it is checked.

## What it is for

Head peers never exchange their configuration. Each node loads its own `head-config.json`
and starts; there is no handshake and no runtime key exchange. Some disagreements are caught
today as a side effect of construction — peer verification keys, their numbering, and
`coilQuorum` all feed `HeadMultisigScript`, so a peer with a different roster derives a
different policy id, a different head address, and rejects the initialization transaction.

Most of the configuration is not pinned by anything. Two peers can differ on
`depositMaturityDuration`, `maxRequestsPerBlock`, `votingDuration`, the fallback contingency
split, or which L2 ledger they run, and both will start, sign block zero, and diverge later —
at deposit absorption, at block packing, or at fallback, when the divergence is unrecoverable.

`headParamsHash` closes that gap at the only agreement gate that exists. It is carried in the
multisig treasury datum, which `InitializationTx.Parse` compares against the transaction it was
handed. **A peer whose configuration differs cannot sign the initialization transaction**, so
the head does not start rather than starting split.

The multisig treasury sits under a native script, so no on-chain validator reads this datum.
The enforcement is entirely off-chain. That is the right place: after initialization the
configuration cannot change, and after fallback there is no consensus left to agree.

## One digest, one nested leaf

```
multisig treasury datum
  └── headParamsHash = blake2b_256("gummiworm-head-params-v1" || <the whole agreed config>)
        └── l2ParamsHash — reported by the L2 ledger, 32 bytes, opaque to the head
```

`headParamsHash` covers the whole head config, not only the `HeadParameters` case class. The
name is the one the code and the whitepaper already use, and there is no second digest: a
nested digest earns its place only when something needs it standalone, and nothing compares,
reports, or transmits a parameters-only value.

`l2ParamsHash` is the one nesting that does earn it, on a different ground: **the head cannot
compute it.** The L2 ledger is the only party that knows its own parameters, so the value
arrives from outside, is compared against the ledger's report on its own (check 4), and is
folded in as an opaque leaf.

### Where it lives

The digest needs `initialEquityContributions`, `blockBrief`, `coilPeers`, and
`scriptReferenceUtxos` as well as `HeadParameters`, so it lives on `HeadConfig.Section` — a
`lazy val` on the `HeadConfig` case class, delegated to by the trait. The layout itself is
`config/head/HeadParamsHash.scala`; `HeadParamsHashTest` mutates every covered field one at a
time and asserts the digest moves, and asserts it does **not** move for `webSocketAddress`.

`InitializationTx.Parse` must **not** compute it. Its `Config` is a deliberately minimal
intersection —

```scala
type Config = CardanoNetwork.Section & HeadPeers.Section & FallbackContingency.Section &
    TxTiming.Section & InitializationParameters.Section
```

— and widening that to the whole config to hash it would work directly against the
rule of least knowledge. Compute the digest once in the `HeadConfig` decoder, where everything
is already in hand, and pass it into `Parse` as an opaque 32 bytes alongside
`blockCreationEndTime`. `Parse` compares bytes; it does not need to know what they cover.

## Encoding primitives

The digest is `blake2b_256` over a domain-tagged, explicitly framed byte string. The layout
below is normative — not a serialization of any JSON or CBOR encoder. Circe codecs for
`QuantizedFiniteDuration`, `Coin`, and `PositiveInt` each have their own quirks, and a codec
tweak that silently moved a hash already written into a treasury datum would leave a live head
unable to parse its own initialization transaction.

| notation | bytes |
|---|---|
| `u8(n)` | 1 byte |
| `u32(n)` | 4 bytes, big-endian unsigned |
| `u64(n)` | 8 bytes, big-endian; signed values two's-complement |
| `bool(b)` | 1 byte, `0x00` false / `0x01` true |
| `framed(b)` | `u32(length)` ‖ the bytes |
| `raw(b)` | the bytes, no framing — fixed-width fields only |

The domain tag is ASCII with no terminator; the fixed-width field that follows makes the
boundary unambiguous. Value types map as:

| type | as |
|---|---|
| `QuantizedFiniteDuration` | `u64` milliseconds (`.toMillis`) |
| `QuantizedInstant` | `u64` milliseconds since the Unix epoch (`.toEpochMilli`) |
| `Coin` | `u64` lovelace |
| `PositiveInt`, `Int`, `HeadPeerNumber`, `CoilPeerNumber` | `u32` |
| `Boolean` | `bool` |
| `Hash32`, `EvacuationMapHash` | `raw`, 32 bytes |
| `ScriptHash` | `raw`, 28 bytes |
| `TransactionInput` | `raw` 32-byte transaction id ‖ `u32(index)` |
| `L2LedgerKind` | `framed` config string — `cardano-eutxo` or `any-remote` |

`L2LedgerKind` folds in its own config string rather than an ordinal, so the digest reuses the
name already on the wire and in every config file instead of inventing a parallel numbering.

## The layout

```
headParamsHash = blake2b_256(
     "gummiworm-head-params-v1"

  -- HeadParameters.txTiming
  || u64(minSettlementDuration)      || u64(inactivityMarginDuration)
  || u64(silenceDuration)            || u64(depositSubmissionDuration)
  || u64(depositMaturityDuration)    || u64(depositAbsorptionDuration)

  -- HeadParameters.fallbackContingency.collectiveContingency
  || u64(publicVoteDeposit)          || u64(fallbackTxFee)
  || u64(minAdaForTreasury)          || u64(minAdaForRegime)

  -- HeadParameters.fallbackContingency.individualContingency
  || u64(collateralDeposit)          || u64(tallyTxFee)
  || u64(voteDeposit)                || u64(voteTxFee)

  -- HeadParameters: disputeResolutionConfig / settlementConfig / blockConfig
  || u64(votingDuration)
  || u32(maxDepositsAbsorbedPerBlock)
  || u32(maxRequestsPerBlock)        || u32(backpressureCoefficient)

  -- HeadParameters.rateLimits
  || u64(softBlockMinPeriod)         || u64(hardStackMinPeriod)

  -- HeadParameters: the rest
  || u32(coilQuorum)
  || raw(l2ParamsHash)
  || framed(l2Ledger)
  || bool(identityIsomorphism)

  -- cardanoNetwork
  || u64(protocolMagic) || u8(networkId)
  || u64(slotConfig.zeroTime) || u64(slotConfig.zeroSlot) || u64(slotConfig.slotLength)

  -- initialEquityContributions, ascending by HeadPeerNumber
  || u32(entryCount)
  || for each: u32(headPeerNumber) || u64(coin)

  -- scriptReferences
  || raw(HydrozoaBlueprint.treasuryScriptHash)
  || raw(HydrozoaBlueprint.disputeScriptHash)
  || raw(setupLadderAnchor.transactionId) || u32(setupLadderAnchor.index)

  -- initialBlockTiming, from blockBrief.header
  || u64(startTime) || u64(endTime) || u64(fallbackTxStartTime)
  || u64(forcedMajorBlockWakeupTime)
  || bool(hasDepositDecisionWakeupTime) [|| u64(mDepositDecisionWakeupTime)]

  -- coilHubTopology, ascending by CoilPeerNumber
  || u32(coilPeerCount)
  || for each: u32(hubHeadPeerNumber)
)
```

The `HeadParameters` fields come first, in declaration order, and the rest of the head config
follows. That grouping is for review, not for meaning — it is one flat preimage and no prefix
of it is separately checked.

### Notes on individual fields

`rateLimits` lives in `HeadParameters` rather than in the per-node
`NodeOperationMultisigConfig` because both knobs gate consensus cadence — `softBlockMinPeriod`
on the `FastConsensusActor → BlockWeaver` lane, `hardStackMinPeriod` on
`SlowConsensusActor → StackComposer` — and peers running different cadences produce different
blocks. It is its own section rather than a member of `txTiming`: `txTiming` holds L1
transaction validity windows, slot-quantized and consumed by transaction builders, while rate
limits are wall-clock gates that reach no transaction. See `docs/spec/rate-limiter.md`.

`coilQuorum` and the peer verification keys are already pinned by the native script.
`coilQuorum` is folded in anyway because it is a `HeadParameters` field and the section is
covered whole; the verification keys are not, because they are pinned by construction.

`networkId` is scalus's own `Network.networkId` — the Cardano network id byte, defined for every
case including `Network.Other`. `protocolMagic` alone does not determine it, because
`CardanoNetwork.Custom` pairs an arbitrary `CardanoInfo` with an arbitrary magic. `cardanoProtocolParams` is deliberately absent: it is fetched from the
chain and moves with hard forks, so it is not something peers agree on.

`initialEquityContributions` contributes its per-peer split, not just its total. Only the total
reaches the treasury value; the split decides who is paid what at finalization and is otherwise
unpinned.

The block-zero timing fields matter unevenly and are all included for that reason.
`startTime` and `endTime` reach the initialization transaction's validity end, but
`fallbackTxStartTime`, `forcedMajorBlockWakeupTime`, and `mDepositDecisionWakeupTime` reach no
transaction at all.

`hubHeadPeerNumber` decides which head peer relays a coil peer's hard acknowledgement and how
many `HubHardAck` journals a recovering coil peer must read. It is not in the native script.
Coil peer numbers are contiguous from zero, so their position in the sequence is their number
and is not repeated.

### Pin the outref where the chain pins the outref

The three script references are pinned three different ways, and the rule behind that is worth
stating: **pin an output reference exactly where the chain pins an output reference; pin the
hash everywhere else.**

| reference | pinned as | why |
|---|---|---|
| `rulebasedTreasuryScriptUtxo` | `HydrozoaBlueprint.treasuryScriptHash` | determines the rule-based treasury address the fallback transaction pays to |
| `disputeResolutionScriptUtxo` | `HydrozoaBlueprint.disputeScriptHash` | determines the dispute gate |
| `setupLadderUtxos` | rung 0's `TransactionInput` | verbatim what `RuleBasedRegimeOutput.datum` writes as `setupG2Ladder` |

The two Plutus script hashes are build constants, not configuration: `ScriptReferenceUtxos`
already rejects a reference utxo whose script hash is not the blueprint's. Folding them in
therefore catches nothing about a *config* mismatch — it catches a **build** mismatch, which
nothing else does. Two peers on hydrozoa builds with different compiled validators each decode
their own config happily and then build different fallback transactions.

Their output references are not pinned, and must not be: a reference input is chosen by
whoever builds the spending transaction, so the outref is deployment detail and redeploying a
reference script must not brick a live head.

The setup ladder inverts both halves. Its **content** is already verified offchain —
`SetupLadderUtxos` checks every rung's inline datum against the locally computed
`SetupLadder.rungDatum(i)`, so a peer with a different trusted setup cannot decode its config
at all — and a content hash would add nothing. Its **outref** is what is unpinned and
load-bearing: `RuleBasedRegimeOutput.datum` writes rung 0's outref into the regime datum as
`setupG2Ladder`, and Evacuate uses it on-chain to authenticate the setup reference input. Two
peers holding correct but differently deployed ladders build different fallback transactions,
and discover it at fallback. Only the anchor is folded in; `SetupLadderUtxos` already requires
the remaining rungs to be outputs `0..rungCount-1` of that one transaction.

## `l2ParamsHash`

The digest an L2 ledger reports over its own agreed parameters. 32 bytes, opaque to the head,
and **the same contract for every backend** — the built-in EUTXO ledger meets it exactly as a
remote sidecar does, through the same `L2Ledger` method, and nothing in the head branches on
`l2Ledger` to obtain or check it.

There is no head-side layout, deliberately: the L2 ledger is a black box, and the head's only
interest is that every peer runs a ledger reporting the same value. What goes into it is the
ledger's business — its rules version, its fee schedule, its asset policy, whatever it and its
operators agree matters.

**It covers parameters, never state.** No evacuation map goes into it. Parameters are fixed for
the head's lifetime; an evacuation map changes with every applied command, and a moving value
has no business inside a digest that the treasury datum pins forever. Keeping state out is also
what lets one definition serve both backends: the moment state enters, a ledger with a
different state model needs a different rule.

The initial evacuation map loses nothing by being kept out, because it is already committed
three times over: the initialization transaction's treasury **value** must back it, the same
transaction's treasury **datum** carries its KZG `commit`, and check 3 compares it against the
ledger's own at a cold anchor. A fourth commitment — a blake2b digest folded into the hash field
of the very datum whose neighbouring field is already the KZG of the same object — would be
redundant with something sitting inches away.

The built-in EUTXO ledger has no negotiable parameters yet: its rules are the hydrozoa code, and
its only agreed knobs — `identityIsomorphism` and the `headId` pin — already sit in
`HeadParameters`, so folding them in here would hash the configuration against itself. It
therefore reports a digest over an empty parameter set, which following `EvacuationMap.digest`'s
precedent hashes its domain tag to a **defined value rather than an absence**. That keeps
`l2ParamsHash` a plain `Hash32` — no `Option`, no special case in the layout or the checks — and
the constant becomes a real digest as soon as the ledger grows parameters worth agreeing on.

### What this does and does not prove

The head cannot verify that a ledger implements particular rules. It can only check that the
ledger reports a matching value, and a lying implementation defeats that entirely. What protects
the head is topological: every node runs its **own private** ledger instance
(`sugar-rush-ledger/DEPLOYMENT.md`), so a divergent ledger surfaces as consensus divergence
between peers rather than as a silent loss. This digest catches misconfiguration, which is the
failure mode that actually occurs.

Operators get the two sides to agree by generating the config from the ledger rather than by
hand, exactly as with the initial evacuation map: the ledger prints its `l2ParamsHash`
out-of-band and the bootstrap copies it in.

## What is deliberately excluded

| field | why |
|---|---|
| `initializationTx` | circular — it carries the datum that carries `headParamsHash` |
| `resolvedUtxos` | a cache so the head can parse the initialization transaction without a backend query; derived from that transaction's inputs, not negotiated |
| `headId` | derived from the initialization transaction's seed utxo and re-derived and checked by `InitializationTx.Parse` |
| `initialEvacuationMap` | already committed on-chain twice in the same transaction — as the treasury value it backs, and as the KZG `commit` in the same datum — and checked by check 1 |
| head and coil peer verification keys, and their numbering | pinned by the native script's ordered `IndexedSeq` → policy id → beacon token name → treasury address |
| `headPeers[*].webSocketAddress` | see below |
| `cardanoProtocolParams` | fetched from the chain, moves with hard forks |

### Why `webSocketAddress` is not in the hash

Folding the address in would prove that every peer holds the same string. It would not prove
that the node answering at that string is the intended peer, so it does not answer the question
an operator actually has. A typo pointing at a live but wrong node, a moved proxy, or a changed
DNS record all survive a matching hash.

It would also make every relocation a head re-initialization. The advertised address is
mutable infrastructure by design — see `peerBindHost` on `NodePrivateConfig`, which exists
precisely because the address the head is dialed at and the address a node binds are different
things.

What the head does have is payload authentication: consensus messages carry `HeaderSignature`
and `TxSignature`, verified against the statically configured verification keys, so a stranger
at the wrong address cannot forge a hard acknowledgement or a settlement signature. What it
does not have is connection authentication — `CoilFrame.Hello` carries a bare `coilNum` and
`HubWsTransport` accepts it on nothing more than "is this a coil peer I hub". Closing that is a
signed handshake over the already-pinned verification keys, and is tracked separately from this
document.

## The checks

Five checks, at four moments. Every one reuses a comparison point the code already has, and
**none of them branches on the backend.**

| # | when | who | compares | on mismatch |
|---|---|---|---|---|
| 1 | config decode, every boot | every head and coil peer | the initialization tx's treasury datum against the datum rebuilt from local config | refuse to decode the config |
| 2 | store open, every boot | every head and coil peer | the store's `Cf.Meta` identity stamp against `headParamsHash`, `headId`, and own `PeerId` | refuse to open the store |
| 3 | every `restoreTo` anchor | `JointLedger` | the ledger's reported `evacuationMapHash` against the head's map at that anchor | refuse to boot |
| 4 | every `restoreTo` anchor | `JointLedger` | the ledger's reported `l2ParamsHash` against the config's | refuse to boot |
| 5 | every major block | every head and coil peer | the settlement tx's treasury datum `headParamsHash` against the local one | refuse to sign the block |

Check 3 exists today. Checks 1, 2 and 5 are built — `InitializationTx.Parse`, `StoreIdentity`,
and `SettlementTx`. Check 4 is not yet.

### 1. The initialization transaction matches the hash

The load-bearing one. `InitializationTx.Parse` rebuilds the expected treasury datum from local
config and compares it **field by field** — a whole-datum equality would report one opaque
message for three unrelated operator problems:

```scala
expectedTreasuryDatum = MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum(
  config.initialEvacuationMap,
  ByteString.fromArray(headParamsHash.bytes)
)
```

`headParamsHash` in the datum makes that comparison cover the whole configuration. Everything folded into the preimage becomes self-verifying against a value committed
on-chain: a peer whose `depositMaturityDuration`, `maxRequestsPerBlock`, fallback contingency
split, hub topology, or setup-ladder anchor differs from the one the initialization transaction
was built for cannot parse that transaction, so it never signs block zero and the head does not
start split.

The three fields fail for three unrelated reasons — a wrong initial evacuation map (`commit`), a
stale version (`versionMajor`), and a configuration disagreement (`headParamsHash`) — and only
the third is something an operator can act on, so each carries its own message naming the two
digests.

`Parse` takes the digest as an already-computed `Hash32` rather than deriving it: computing it
needs nearly the whole head config, and `Parse` deliberately asks for only the five sections it
uses. `HeadConfig`'s decoder computes it, as does `Bootstrap.mkSharedHeadConfig` — which builds
block zero's header **before** the transactions for exactly this reason, since the header is part
of the preimage and the init tx's datum carries the result.

Two properties fall out of where this check sits, and both are worth relying on deliberately:

- **It re-runs on every boot, not just at initialization.** `NodeConfig.load` re-reads
  `head-config.json` and re-decodes it each time the node starts, and decoding runs
  `InitializationTx.Parse`. An operator who hand-edits a live head's config is caught at the
  next restart rather than at the next divergence.
- **It is the cross-peer check, and one instance of it suffices.** Peers never compare configs
  with each other, and do not need to: every peer compares against the *same* transaction, so
  agreeing with the transaction implies agreeing with each other. No handshake, no gossip, no
  quorum on config.

### 2. The store belongs to this config, and to this peer

A node's persistent store is built for one head, under one configuration, by one peer. Point it
at a different one and it does not fail — it proceeds on a store that means something else.

The check is a **stamp in `Cf.Meta`**, written when a fresh store is initialized and compared on
every subsequent open. Three keys, flat and name-keyed like the `store_version` key that is
already there:

| key | value | catches |
|---|---|---|
| `head_params_hash` | `raw(headParamsHash)` | a store built under a different configuration |
| `head_id` | the head's treasury token name | a store built for a different head — and makes the error message actionable |
| `own_peer_id` | `PeerId.toWireInt`, 4 bytes big-endian | a store built by a *different peer of the same head* |

`own_peer_id` is the one that cannot come from `headParamsHash`, and the one whose absence is
most dangerous. Which peer a node is comes from `NodePrivateConfig`, not the head config, so
every peer of a head has the identical `headParamsHash`. Opening head peer 0's store as head
peer 1 therefore passes every other check while adopting peer 0's own-author journals as this
peer's own — which is exactly the state equivocation avoidance exists to prevent. Reuse
`PeerId.toWireInt`, already the author discriminant in the `HardAck` CF name, rather than
inventing a second encoding.

`head_id` is redundant against `head_params_hash` and is stamped anyway, because a bare hash
mismatch tells an operator nothing they can act on. "This store belongs to head `0134…6b10`,
this config is head `8f2a…c401`" names the mistake.

**Where it runs, and what it costs.** `RocksDbBackendStore.openInternal` runs
`versionCheck` at open, and `StoreVersion.Check` already has the right three-way shape —
`Fresh` / `Compatible` / `Incompatible`. The identity stamp is a sibling of that, with the same
semantics: a writable open stamps a fresh store; a **read-only** open — the mode
`hydrozoa evacuate` uses — treats a missing stamp as a hard error, because it cannot stamp and
an unstamped store cannot be served; an incompatible stamp refuses the open, naming which of
the three fields differs. It runs **after** the version check, because a store whose schema this
build does not understand should not have its metadata interpreted at all, and **before** any
recovery read. The cost is one point lookup per key on an already-open handle at startup, the
same as the version check.

**What this catches that nothing catches today.** `Cf.mkAll` derives the column-family set from
head and coil membership, and `RocksDbBackendStore` opens RocksDB with
`setCreateMissingColumnFamilies(true)`. So pointing a node at a store built for a different
roster does not fail at open: the missing per-author CFs are **created empty**, and the node
proceeds as though it had no history. That is the shape of the worst persistence failure seen in
practice — a store that reads as empty re-bootstraps from stack 0, the peer can never rejoin the
head, and the symptom surfaces much later and far from the cause, as an out-of-bounds journal
cursor rather than as a refusal to open.

**Why this does not replace check 1.** The two run in opposite directions and neither implies
the other. Check 1 compares the config against the **chain** — is this configuration the one the
head was initialized with? Check 2 compares the config against the **store** — is this the store
that this configuration, run by this peer, has been writing? A config and a store can each be
individually valid and still belong to different heads; a config can be edited between restarts
while the store is untouched; a store can be copied between peers while the config is correct.

### 3, 4. The ledger is the one the config describes

`JointLedger` calls `restoreTo` on every boot including a cold one, and already compares the
ledger's reported `evacuationMapHash` against its own map at the anchor (check 3, defined in
`docs/spec/l2-ledger-command-coordination.md`). Check 4 adds `l2ParamsHash` to the `Restored`
reply and compares it against the config's, so `L2Ledger.restoreTo` returns the pair
`(EvacuationMapHash, Hash32)` rather than the map digest alone.

The two answer different questions, and the difference is why both exist:

- **`l2ParamsHash` never moves.** It is fixed for the head's lifetime, so it is comparable at
  every anchor, warm or cold, against a config value that is equally fixed.
- **`evacuationMapHash` moves with every applied command.** At a warm anchor it is compared
  against the head's *current* map, not the initial one — so nothing but check 4 keeps asking
  whether this is still the right *ledger* rather than merely a ledger holding the right state.

Both run against every backend, with no conditional. `L2Ledger.restoreTo` is deliberately
backend-agnostic — one signature, and check 3 already runs against the in-process ledger — so a
branch here would be the only one in that contract.

Check 4 is not vacuous for the built-in ledger, even while its parameter set is empty. The
config's value was written by whichever build's tooling produced it; the reported value comes
from the build now running. Once the ledger's domain tag carries a rules version, that
comparison is what stops two peers on hydrozoa builds with divergent L2 semantics from booting
against the same head.

**A mismatch on 3 or 4 is fatal: the node refuses to boot.** Not recoverable at runtime and not
safe to run through — either the on-chain commitment is already wrong, or the ledger is the
wrong one. Same rule the evacuation map digest already follows.

### 5. Every major block re-checks it

`SettlementTx` builds a fresh treasury datum for every major block:

```scala
datum = MultisigTreasuryUtxo.Datum(kzgCommitment, majorVersionProduced, config.headParamsHashBytes)
```

The digest comes from the builder's **own config**, not from the spent treasury's datum. Carrying
it forward would make every peer reproduce whatever was already there and check nothing; taking
it from config means a peer whose config diverged produces a datum the others reject. Every peer
verifies a settlement transaction before signing it. So the configuration agreement is re-checked once per
major block for the life of the head, by the machinery that already verifies settlements — a
peer whose configuration drifts after initialization stops being able to get blocks signed.

This is why `headParamsHash` belongs in the datum rather than in the initialization
transaction's metadata: metadata is written once, the datum is rewritten and re-verified
forever.

### What is deliberately not checked

- **Whether a ledger's `l2ParamsHash` honestly describes its rules.** The head cannot verify a
  black box, only that it reports a matching value.
- **Which node answers at a peer's `webSocketAddress`.** Not a hash problem; see above.
- **The initialization transaction's witnesses.** `Parse` establishes structure; signatures are
  collected and verified by initial-block consensus.

### Migration

Adding a field to `MultisigTreasuryUtxo.Datum` changes its `Data` arity, so
`Data.fromData[MultisigTreasuryUtxo.Datum]` fails on every datum written before the change —
on-chain and in persisted state alike. **A running head cannot be upgraded across this
change.** It applies to heads initialized afterwards; existing heads keep the two-field datum
and the build that understands it. This belongs in the release notes of the release that ships
it, with the configuration-change procedure below.

## Changing any of this

The layout is a wire break and an on-chain break at once. A head whose treasury datum holds a
`headParamsHash` computed under one layout cannot be parsed by a node computing the next.
Changing the layout means a new domain tag, and heads initialized under the old one keep the
old tag for life.

Adding a field anywhere the preimage covers — `HeadParameters` or the wider head config —
changes `headParamsHash`, and so changes the treasury datum of every head initialized
afterwards. Follow the configuration-change procedure: state which files change, whether
existing configs still decode, verify both decoders (`HeadParameters` and
`Bootstrap.BootstrapHeadParams`), and carry the migration into the release notes of the release
that ships it.
