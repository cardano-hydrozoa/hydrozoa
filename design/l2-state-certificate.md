# L2 state certificate

For whoever implements state commitments. It closes the one gap `design/block-hash.md` leaves
open — nothing commits to the L2 state a block produced — and it does so without a new digest,
a new message, or a new effect type: the effects a stack already produces become the certificate.

## The gap

`blockHash` commits to a block's content: which requests, in what order, with which flags and
absorption decisions. Two peers that agree on every one of those and reach **different ledger
states** still compare equal. Nothing anywhere names the state a block produced, so nothing
catches a ledger bug, a non-deterministic rule, or a genuinely different prior state.

The same absence is what makes seeding a peer an act of trust. A coil peer joining a head with
long history is handed a block number, the L2 ledger state at it, and the evacuation map at it.
The block is provable from signatures; both halves of the state rest on the donor. That is the
trust boundary seeding carries today.

## Certificates, not detectors

Two shapes close a gap like this, and they are not the same thing.

A **detector** is a digest every peer recomputes and compares. It tells peers they disagree, and
it is worth having, but it only works for a peer that has already applied the block. It says
nothing to a peer that was not there.

A **certificate** is a signed statement of what the state *is*. It can be handed to someone who
has no history, which is exactly what a joining peer needs. It is the sense in which the coil
handshake already uses the word.

This design takes the certificate. Detectors are cheap to add later on top of a certificate; the
reverse is not true.

## Effects already carry half of it

The insight that makes this cost almost nothing: **the effects a stack produces already commit to
the evacuation map**, and they are already multisigned N-of-N by every head peer through the
existing hard-ack flow. What is missing is the ledger state beside it.

| carrier | commits to today | signed | anchored |
|---|---|---|---|
| settlement's treasury datum — `Datum(commit, versionMajor, headParamsHash)` | the evacuation map, as `commit: KzgCommitment` | N-of-N hard-acks | **on L1**, at every major block |
| the SEC — `StandaloneEvacuationCommitmentOnchain(headId, versionMajor, versionMinor, commitment)` | the evacuation map at that block, as `commitment: KzgCommitment` | N-of-N hard-acks | under peer keys; reaches L1 only in a dispute |

So the change is one field in each, beside a commitment that is already there.

## Every stack carries at least one

`PartitionEffects` has three shapes, and a stack is a non-empty list of them:

| partition | carries | gets the certificate from |
|---|---|---|
| `Major(settlement, fallback, rollouts, refunds, sec: Option[S])` | a settlement | the settlement's treasury datum |
| `Minor(sec: S, refunds)` | a **mandatory** SEC | the SEC |
| `Final(finalization, rollouts)` | a finalization — no settlement, no SEC | nothing; see below |

A major partition snapshots state at the major; a minor partition's SEC covers the latest minor in
its run. So every stack that is not purely final produces at least one signed statement of state,
at partition granularity — coarser than per block, finer than per stack.

**The `Final` partition is the exception, and needs no certificate.** It finalizes the head: the
payouts are determined by the finalization itself, there is no next state to commit to, and no peer
joins at that anchor.

## Why not per block

Per-block state digests were the earlier design and were rejected on two grounds.

**Cost, on every backend.** A state digest is a hash over the elements that constitute the ledger
state, so producing one is work proportional to the state unless the construction is incremental.
That holds for the built-in ledger as much as for a remote one. `any-remote` adds a second cost on
top: the state never crosses the boundary, so the value comes back over the coordination protocol.
Per block that is a hash of everything, and a round trip besides, on the critical path of every
block cut. Per partition both land where the slow cycle already waits.

**It is the wrong artifact.** A per-block digest is a detector. It has to be recomputed by whoever
checks it, which a seeding peer cannot do — that is the whole reason it is seeding.

## Why not a stack hash

A `stackHash` over a stack's ordered `blockHash`es was the other candidate, and it is not needed.

What it would commit to — which blocks a stack covers — is **already enforced**, if emergently:
every peer derives effects from its own view of the range, and `HardAckSignatureVerifier` checks
each signature against locally-derived effect bodies, so a peer with a different range produces
effects whose signatures do not verify. Making that explicit would improve diagnosis, not safety,
and it would cost a wire break to do it.

The state commitment, which was the real reason to want a third layer, is served better here.

## What the digest covers

`l2StateHash` is an **efficient digest over the elements that constitute the ledger's state**. The
construction is the backend's to pick — a Merkle root, or anything else with the same property —
and the requirement on it is efficiency, because it is produced at every partition rather than once
at boot.

The backends do **not** have to agree on a construction. `l2Ledger: L2LedgerKind` is a head
parameter, pinned in `headParamsHash`, so every peer in one head drives the same backend and only
ever compares digests with peers computing them the same way. What has to be defined per backend is
what the digest ranges over:

| backend | ranges over | today |
|---|---|---|
| `EutxoL2Ledger` | active L2 UTxOs, transient tokens, pending deposits — the fields `L2Snapshot` persists | no digest; `EvacuationMap.digest` is the nearest precedent |
| `any-remote` | `[Sugar Rush defines what constitutes its state]` | nothing reported |

### Where the value comes from

**Piggyback on `restoreTo`.** `L2Ledger.Restored` already carries `evacuationMapHash` and
`l2ParamsHash`; it gains `l2StateHash` beside them, and so does the wire form —
`Restored { tip, evacuationMapHash }` in `docs/spec/l2-ledger-command-coordination.md`. One exchange
then reports every digest the head needs about the ledger, and the check the head already runs —
`JointLedger.State.recover` comparing `evacuationMapHash` against its own folded expectation — is
the pattern the new field follows.

The remote side owes two fields rather than one: that frame does not carry `l2ParamsHash` today
either, which is the gap GUM-327 tracks.

That carries one implication for `EutxoL2Ledger`: its `restoreTo` re-folds from the latest snapshot,
so a call at the anchor the ledger is already at is not free. Either that path gets a cheap
same-anchor case, or the digest is maintained incrementally — the efficiency requirement above, seen
from the other side.

## The two changes

### The settlement's treasury datum

```scala
MultisigTreasuryUtxo.Datum(commit, versionMajor, headParamsHash)   // today
```

Add `l2StateHash`. Two sites build it — `SettlementTx` (`:306` and `:355`) — plus
`mkInitMultisigTreasuryDatum` for the initialization transaction.

**It costs little.** The multisig treasury sits under a native script, so no validator reads this
datum; the enforcement is off-chain. The cost is a `Data` arity change and ~32 bytes per
settlement on L1.

**It earns its seat, where `headParamsHash` does not.** GUM-329 removes `headParamsHash` from this
datum precisely because copying an unchanging value onto every settlement is waste. `l2StateHash`
changes with every settlement, which is the same criterion reaching the opposite verdict. If the
two land together it is one arity change rather than two.

**The initialization transaction gets one too**, which yields a signed commitment to the opening
L2 state — the "l2 initial state" component GUM-332 wants, obtained for free here.

### The SEC

```scala
StandaloneEvacuationCommitmentOnchain(headId, versionMajor, versionMinor, commitment)   // today
```

Add `l2StateHash`. This is the one that covers minor-only stacks, which produce no settlement.

**This one is not free.** `StandaloneEvacuationCommitmentOnchain` lives in `cardano-onchain` and
**is read by the dispute validator**: `DisputeResolutionScript` verifies signatures over
`voteRedeemer.sec.toData |> serialiseData`. A field there changes the Plutus data shape, costs
script budget on every vote, tally and resolution, and recompiles the validator into new script
hashes — which changes head addresses.

Judged acceptable: one additional field is not a large change to that shape, and the alternative
is leaving minor-only stacks with no state commitment at all. It does mean this half wants review
from whoever owns the dispute path, and it wants measuring rather than assuming — see the open
questions.

## What a certificate is, concretely

Not a new type. A certificate is **an effect plus its hard-ack signatures**, both of which every
peer already stores:

- the settlement transaction (or the SEC) carrying `l2StateHash` beside the evacuation-map
  commitment, and
- the N-of-N signatures the slow cycle collected over it.

**A joining coil peer asks, and the hub answers with the latest stack as the start point.** The
peer takes the effect that stack carries, checks the signatures against the head peer verification
keys it holds from config, and reads the two commitments out of it. No replay, and no trust in the
donor beyond the signatures.

## Anchoring is uneven, deliberately

A settlement's datum lands **on L1**. A SEC is multisigned but submitted only in a dispute, so in
the happy path it lives under peer keys.

So a major stack yields an L1-anchored state commitment, and a minor-only stack yields a
peer-signed one. That asymmetry is real and worth stating: the strongest anchor the head has is
the settlement datum, and it is as sparse as the head's major cadence. Deployments that space
majors far apart get correspondingly sparse L1 anchoring, and everything between rests on peer
signatures.

## What this does not do

- **It does not detect execution divergence between peers at every block.** Two peers reaching
  different states diverge visibly only when their partition's effect is built and signed. A
  per-block detector on top of this would close that, and is a separate question.
- **It does not cover the rest of a snapshot.** The deposits map, request high-water, and the
  block and stack spines (§5.2 of `docs/spec/persistence-and-crash-recovery.md`) are committed to
  by nothing here.
- **It does not change `blockHash`.** The two work items are independent in both directions.

## Still to measure

**What the SEC change costs on-chain** — script budget on vote, tally and resolution, and the size
of the datum. It is the one part of this that touches Plutus, so measure it; the measurement is not
expected to change the decision.
