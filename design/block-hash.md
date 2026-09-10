# Block hash

For whoever implements content commitments on the fast side. It defines three digests —
`requestHash` over a user request, `blockHash` over what a leader *decides*, and `blockResultHash`
over what applying that decision *produces* — says when each is taken, and names what compares
them. The soft-ack signs both block digests, so the signature set a block already carries becomes
a proof a peer can seed from without replaying the head's history.

Stacks need the same treatment on the slow side. That is a separate work item; this document
fixes the approach both follow.

## The brief carries only what is known at the cut

The organising constraint, and the reason there are two block digests rather than one.

**A block brief must contain nothing that is knowable only after the block's requests have been
applied.** The leader chooses an ordered sequence of requests; that choice is complete the moment
it is made. Everything else about a block — whether each request turned out valid, which deposits
were absorbed, what state resulted — is a *consequence* of that choice, and computing it takes
work.

If the brief carries consequences, the leader must finish that work before it can announce, and
only then can followers begin theirs. Leader-apply and follower-apply run back to back. If the
brief carries only the decision, the leader announces at the cut and every peer applies in
parallel, the leader among them. Under light load this is invisible. Under heavy load it is the
difference between one application latency per block and two.

So the split is not a hashing detail. It is what lets the fast side pipeline at all.

The line falls between **decided at the cut** and **computed by applying**, and it does not land
where a first reading suggests:

| | decided at the cut | in `blockHash` | in `blockResultHash` |
|---|---|---|---|
| `blockNum`, versions, the four times | yes | ✓ | |
| the ordered `(RequestId, requestHash)` sequence | yes | ✓ | |
| `depositsAbsorbed` / `depositsRejected` | yes — the leader decides them | ✓ | |
| `ValidityFlag` per request | no — applying decides | | ✓ |
| `evacuationDiffHash`, `l2StateHash` | no | | ✓ |

Absorption decisions stay on the announced side, and belong there. A deposit decision rests on
what the leader observed on L1; peers observe L1 at different times, so the leader genuinely
*decides* rather than computes, and a follower has to be told. Telling it costs the leader
nothing it did not already know at the cut.

Validity flags are the opposite. Applying a known sequence to a known prior state is
deterministic, so every honest peer computes the same flags without being told. Carrying them in
the brief adds no information and costs exactly the latency this section is about.

Both eventually want to leave `BlockBody` — the flags because they are redundant, the absorption
lists because a deposit decision deserves to travel in a carrier of its own rather than riding a
block. Neither removal is in this document; see *Out of scope*. What this document fixes is that
**no digest a leader announces may depend on applying the block**, so that removing either later
changes what the brief carries and not what the design assumes.

## The gap

Three layers describe a block. None commits to its content, and none names the state it
produces.

| layer | holds | commits to |
|---|---|---|
| `RequestId` | `(HeadPeerNumber, RequestNumber)` | a position in one peer's sequence — nothing about the payload |
| `BlockBody.{Minor,Major,Final}` | `List[(RequestId, ValidityFlag)]`, `depositsAbsorbed`, `depositsRejected` | the same positions |
| `BlockHeader.{Minor,Major,Final}` | `blockNum`, `blockVersion`, `startTime`, `endTime`, `fallbackTxStartTime`, `forcedMajorBlockWakeupTime`, `mDepositDecisionWakeupTime` | itself — it carries no body field |
| `SoftAck.headerSignature` | Ed25519 over `BlockHeader.Section.signingBytes` | `SignedDigest.Onchain(blockNum, startTime, versionMajor, versionMinor)` |

So the signature set on a soft-confirmed block proves the peers agreed on **four scalars**: the
block number, the start time, and the two version components. It proves nothing about which
requests the block contains, in what order, or with which validity flags.

It does not even cover the whole header. `endTime`, `fallbackTxStartTime`,
`forcedMajorBlockWakeupTime` and `mDepositDecisionWakeupTime` are header fields outside the
signed digest.

**What holds today.** `JointLedger.panicOnMismatchWithExpectedBrief` compares the leader's brief
against the locally re-derived one with case-class `==`, which is a complete structural
comparison of header and body. That is a real check, and it is why divergence surfaces at all.
Its four limits are what this work item addresses:

1. It is local. A peer that skips it, or a peer type that never re-derives, is unconstrained.
2. It panics. The peer halts and hands over to the rule-based regime; it does not refuse the
   block and stay in the multisig regime.
3. It compares `RequestId`s. Two peers holding **different payloads under the same id** compare
   equal, because nothing anywhere ties an id to its bytes.
4. It compares a block. Two peers that agree on every request and reach **different ledger
   states** compare equal, because no layer names the state a block produces.

Points 3 and 4 are the ones that cannot be fixed by moving the comparison. Point 3 needs a
content hash; point 4 needs a state digest. They belong in *separate* preimages, because one is
known when the leader cuts the block and the other is not — which is the subject of the section
above.

## Three digests

| digest | over | taken by | when | carried on |
|---|---|---|---|---|
| `requestHash` | one user request as received | the submitter, and every peer that receives it | supplied on submission, re-derived and checked at `RequestId` assignment | the block body, beside its `RequestId` |
| `blockHash` | the header fields and the ordered request sequence | the block leader, and every peer that rebuilds the block | at block cut, **before applying anything** | the block brief |
| `blockResultHash` | the outcome of applying that sequence | every peer, independently | after applying the body | the soft-ack |

**Each digest travels on the message that is ready when it is.** The brief is announced at the
cut, so it carries what the cut decided; the ack is produced after applying, so it carries what
applying produced.

`blockHash` covers `requestHash`, not the request bytes: the body is a list of
`(RequestId, requestHash)` pairs, so the block commits to exactly which payload sits at each
position without carrying any payload.

That is the shape the whole design follows — **each layer commits to the one below by hash, and
nothing chains sideways within a layer.** The slow side extends it: a `stackHash` covers the
ordered `blockHash`es of the blocks a stack closed over, the same way a `blockHash` covers the
ordered `requestHash`es of its body. Stacks are a separate work item; the construction is fixed
here.

## The submitter supplies the hash, and the head checks it

`requestHash` is not something the head hands down to the user. **The submitter computes it and
sends it with the request**, and the head re-derives it from the body it received and refuses the
request if the two differ.

That inverts the obvious arrangement, and the inversion is the point. A hash the head computes and
returns tells the submitter what the head *thinks* it received; the submitter has nothing to check
it against except the same head's word. A hash the submitter computes and the head confirms is an
end-to-end check on the bytes: it fails exactly when the request the head holds is not the request
the user built, which is the failure a client-side encoding change, a truncated payload, or a
mangled `l1Payload` produces — silently, today.

**Refuse, do not correct.** A mismatch means the head and the submitter disagree about what was
submitted, so there is no version of the request it is safe to assign an id to. It is rejected
before assignment, through the existing `UserRequest.Rejected(reason)` channel that stateless
screening already uses, with a reason naming both digests. Nothing is persisted, no `RequestId` is
consumed, and the submitter retries with the request they meant.

**The check is `UserRequestBody.hash` itself**, run over the received body. There is one hash
function, used by the submitter to produce the value and by the head to verify it; a second
implementation would be a second thing to disagree about.

## When the head hashes: at assignment

In `RequestSequencer`, between `val newId = RequestId(ownHeadPeerNum, newNum)` and the CR1 persist
that follows it — the verification above happens on the way in, and the verified digest is what
the rest of this design carries.

Three reasons that is the right moment:

1. **It rides the barrier that already exists.** CR1 persists the assigned request to the
   `Request` journal *before* the user is told the id — durable before observable. The hash is
   part of the same write and inherits the same guarantee.
2. **It is taken over the request as received**, before screening verdicts, block packing, or
   any validity judgement. The hash describes what the user submitted, and nothing later can
   move it.
3. **Every other peer recomputes it.** `RequestSequencer` fans the same `UserRequestWithId` to
   `BlockWeaver`, the head-peer mesh and (on a hub) `CoilRelay`. Each recipient hashes the body
   it received rather than trusting a digest that travelled with it. A peer that computes a
   different one has different bytes, which is exactly the condition worth detecting.

Point 3 is not weakened by the submitter supplying the hash. The user's digest is verified once,
at the edge, and then discarded as an input: what flows between peers is the body, and every peer
derives the digest from the bytes in front of it. The head trusts no digest it did not compute —
including the user's.

**Rejected: hash at block packing.** Too late to reject a request the submitter mis-encoded, since
by then it holds an id. And only the leader would compute it, so a follower would be verifying the
leader's arithmetic rather than its own.

### The `RequestId` is not in the preimage

`requestHash` is a hash of the request, not of the assignment: the same bytes produce the same
digest regardless of which peer sequenced them or where they landed in that peer's sequence. That
is what makes the submission contract above possible at all: the submitter has no `RequestId` yet
when it computes the hash, and must not need one. It also lets the submitter recognize their own
request in a block without trusting anyone's arithmetic, and lets two peers that received the same
request agree on its hash without agreeing on anything else.

The uniqueness a per-assignment hash would add is not needed. The block body carries the
`RequestId` beside the hash, so the block's commitment names both which request and which
position.

## `UserRequestBody.hash` already exists

`UserRequest.scala` carries it: `blake2b_256`, with deposits hashed as
`blake2b_256(l1Payload) ++ blake2b_256(l2Payload)` before the outer hash, its comment explaining
this keeps the hash injective rather than collapsing `hash(abc + def) == hash(ab + cdef)`.
`UserRequestTest` pins two vectors. **Nothing calls it** — it is written, tested, and unreached,
which is why the gap above exists at all rather than because the function is missing.

It hashes the body and not the `RequestId`, which is exactly the shape decided above. It becomes
the verification function: the head runs it over the received body and compares against the digest
the submitter sent.

Two things to fix before it carries that weight:

- **No variant tag.** `TransactionRequestBody(l2Payload)` hashes `l2Payload` directly, while
  `DepositRequestBody` hashes a 64-byte concatenation of two digests. A transaction request
  whose `l2Payload` happens to be exactly that 64-byte string hashes identically to the deposit.
  Domain-separate the variants, the way `HeadParamsHash` domain-tags its preimage.
- **It is not specified anywhere a client can read.** Once a submitter has to reproduce this
  digest to get a request accepted, the construction is a public interface: the domain tag, the
  field order, and the deposit two-digest rule all have to be written down in
  `docs/user-guide/`, with the pinned vectors from `UserRequestTest` as worked examples. A hash
  a client cannot independently compute is a hash the client cannot supply.

Both changes move the pinned vectors in `UserRequestTest`, which is free now and is not free once
a client has shipped against them.

## What `blockHash` covers

The same construction as `EvacuationMap.digest` and `HeadParamsHash`: an ASCII domain tag,
fixed-width fields unframed, variable-length fields length-framed, `blake2b_256` over the whole
preimage.

```
blockHash = blake2b_256(
     "gummiworm-block-v1"
  || u8(blockType)                  -- Initial | Minor | Major | Final
  -- header
  || u32(blockNum)
  || u32(versionMajor)              || u32(versionMinor)
  || u64(startTime)                 || u64(endTime)
  || u64(fallbackTxStartTime)
  || u64(forcedMajorBlockWakeupTime)
  || bool(mDepositDecisionWakeupTime.isDefined)
  || u64(mDepositDecisionWakeupTime)          -- present only when the flag is true
  -- body: the leader's decision, and nothing derived from applying it
  || u32(requests.length)
  || for each, in list order:
       u32(peerNum) || u64(requestNum) || raw(requestHash)
  || u32(depositsAbsorbed.length)
  || for each, in list order: u32(peerNum) || u64(requestNum)
  || u32(depositsRejected.length)
  || for each, in list order: u32(peerNum) || u64(requestNum)
)
```

Every field above is available the instant the leader cuts the block, so `blockHash` is
computable at the cut and the brief can go out before a single request has been applied.

Notes on the layout:

- **`blockHash` is excluded from its own preimage.** It is a `BlockBrief` field (below), so every
  other field of the brief goes in and this one does not. Missing that makes the definition
  circular. It is the same exclusion `headParamsHash` makes for the initialization transaction,
  which carries the digest it is an input to.
- **The block type leads the block's own fields.** `BlockBody.Initial` has no fields, `Minor` and
  `Final` have no `depositsAbsorbed`, and `Major` has all three lists. Tagging the type first
  keeps the four shapes from colliding, and keeps the absent lists out of the preimage rather
  than encoding them as empty.
- **Order is the list's own order**, not sorted. The ordered request list is what the leader
  chose and what every follower must reproduce; sorting would hide a reordering, which is a real
  disagreement about block content.
- **`RequestNumber` is `u64`.** The `Request` journal key is 8 bytes (`Markers` decodes it with
  `getLong`), unlike the 4-byte soft/hard-ack indices.
- **The optional wakeup is flag-then-value**, so `None` and a present value can never produce
  the same bytes.
- **No `ValidityFlag` beside each request.** The flag is a result, and results live in
  `blockResultHash`. A leader that had to fill it in could not announce until it had applied the
  block.

**The initial block gets one too.** `BlockBrief.Initial` has an empty body and a header already
pinned by `headParamsHash` through the initialization transaction, so its hash proves nothing
new — but hashing it keeps all four block types uniform, keeps `blockHash` total on
`BlockBrief`, and removes a special case from every consumer. Its empty body encodes as three
zero-length lists under the `Initial` type tag.

## `blockResultHash`: what applying the block produced

`blockHash` commits to a block's **inputs**: which requests, in what order, and which deposits the
leader decided to absorb. A second digest commits to the **output** — what those inputs turned out
to mean, and the state they produced.

```
blockResultHash = blake2b_256(
     "gummiworm-block-result-v1"
  || raw(blockHash)                           -- 32 bytes; binds a result to its block
  || u32(requests.length)
  || for each, in blockHash's list order: u8(validityFlag)
  || raw(evacuationDiffHash)                  -- 32 bytes; this block's own diff, not the map
  || raw(l2StateHash)                         -- 32 bytes
)
```

- **It opens with `blockHash`.** A result is meaningless except about a specific block, and
  binding the two means a result digest cannot be lifted onto a different block that happens to
  produce the same state. It also means one comparison distinguishes the two failure modes
  below.
- **Flags ride positionally**, in `blockHash`'s order, so the sequence is stated once. The count
  is repeated only to length-frame the run.
- **Computed by every peer, never sent as an authority.** A follower applies the block it was
  announced and derives the digest itself. A peer that computes a different one has diverged, and
  that is precisely what should be caught.

**Two digests make two failures distinguishable**, which today's single structural comparison
conflates:

| | `blockHash` | `blockResultHash` | means |
|---|---|---|---|
| agree | ✓ | ✓ | fine |
| content divergence | ✗ | — | peers disagree about *which block this is* — mempool, ordering, or a payload mismatch under a shared `RequestId` |
| execution divergence | ✓ | ✗ | peers agree on the block and **compute different results** — a ledger bug, a non-deterministic rule, or a genuinely different prior state |

The second row has no detector at all today, and it is the more alarming of the two.

### The two state digests inside it

| field | over | why it alone is not enough |
|---|---|---|
| `evacuationDiffHash` | this block's `evacuationMapDiff`, in application order | a delta, not a state. It says what this block changed about who is owed what, and nothing about what the totals became |
| `l2StateHash` | the L2 ledger state after this block | says nothing about what each party is owed on exit, which is the thing L1 enforces |

Both, therefore. Neither implies the other, and a snapshot is only as trustworthy as the weaker
of the two commitments over it.

**The cumulative evacuation map is deliberately absent, because a block does not know it.**
`BlockResult` carries `evacuationMapDiff: Seq[EvacuationDiffGroup]` — the block's own contribution
— and nothing else about the map. The running map is folded on the **slow** side, in
`StackComposer`: `EvacuationMap.applyDiffs(runMap, result.flatEvacuationDiffs)`, walking a stack's
blocks in order from the previous stack's map, and persisted only at the blocks whose map backs an
effect. A digest over the map after block N therefore depends on every block before N, which is
information the fast side does not have at the cut and does not have when it applies the body
either.

Committing to the diff instead keeps the commitment at the layer that owns the value. The
cumulative map digest belongs one layer up, in the stack digest, where `StackComposer` already
computes the map it would cover — the same way `stackHash` covers the ordered `blockHash`es rather
than re-deriving their contents.

**`blake2b_256`, not KZG.** The evacuation map already carries a KZG commitment, and it stays
where it is: `EvacuationMap.kzgCommitment` goes into the treasury datum at major-block
settlement, where L1 needs a commitment it can open. Per-block digests are computed and verified
by every peer on every block, so they take the same `blake2b_256` construction as every other
digest here.

**They are not header fields.** An earlier draft of this design put them in the header and split
`BlockHeader` construction into a draft stage and a `finalize(draft, …)` stage. That is exactly
the coupling the opening section rules out: a header nobody can complete until the block has been
applied is a brief nobody can announce until then either.

Dropping them from the header removes the two-stage construction entirely. `nextHeaderMinor` and
friends keep deriving block N+1's header from N's header plus timing, in one step, as they do
today — there is no draft type, no `Option` field, and no finalizer.

**Every peer computes them, and none is told them.** A follower rebuilding block N applies the
same requests to its own ledger and derives both digests. That is what makes the execution-
divergence row above detectable: nothing today catches two peers reaching different states from
an identical request list.

## Why the preimage does not chain to the previous block

Considered and rejected. A `previousBlockHash` field would make each block's hash commit to its
whole history, the way a blockchain does. Here it buys almost nothing, because the reasons
blockchains chain are the reasons hydrozoa does not need to.

Chaining is a **substitute for identity**. It is what you build when no fixed set of signers can
vouch for a block: work accumulates along a chain so that rewriting block 5 costs redoing 5 to
the tip; "heaviest chain" is only a well-defined fork-choice rule if there is a chain; depth
means something only because reversal is expensive. Every one of those solves the absence of a
known signer set.

Hydrozoa has the thing chaining substitutes for. Membership is fixed in the head config and
pinned by the treasury address, and every block carries an all-peer soft-confirmation. There is
no work to accumulate — rewriting block 5 is not expensive, it is impossible without keys. There
is no fork to choose: a block either has every signature or is not confirmed. There is no
depth-based finality; finality is explicit, soft then hard.

The one benefit that survives is amortized validation — check one signature at a tip plus a
rehash, rather than every block's signature set. That is real but modest, and the store is
already checkpointed externally at a coarser grain: every major block's settlement writes the
evacuation map's KZG commitment into the treasury datum, so a store can be validated against L1,
which beats any self-referential chain.

### Chaining does not help a peer that skips history

The strongest case for chaining is seeding a new coil peer into a head with long history, where
replaying every block costs more than the coil can afford. Chaining does not serve it.

A chained `blockHash(N)` commits to every **input** from block 0 to N. A peer handed a snapshot
— block N and the ledger state at N — and a perfectly verified chained hash still cannot check
that state, because the only route from "these were the inputs" to "this is the resulting state"
is to apply them. That is the replay the snapshot exists to avoid. Chaining yields a commitment
whose sole use requires the work it was supposed to save.

The two state digests above are what serves the case, and they need no chain: one block, two
digests, and the signature set already collected on it.

**This is the shape Mithril uses**, and it is worth being precise about which thing Mithril
chains. A Mithril client bootstrapping a Cardano node does not walk the block chain; it verifies
a **digest of the state** carried by a certificate, under a threshold signature. It walks a
second, separate chain — of certificates — solely to learn that today's signers are legitimate,
because Cardano's SPO set and stake distribution turn over every epoch and there is no fixed
committee to name. That certificate chain authenticates *who may sign*. It never attests to what
happened.

Hydrozoa needs neither chain:

| Mithril builds | because | hydrozoa |
|---|---|---|
| a stake-based lottery over `m` indices, quorum `k` | thousands of permissionless signers, no nameable committee | membership is fixed in the head config and pinned by the treasury address |
| a certificate chain to a genesis certificate | the signer set rotates each epoch | it does not rotate |
| BLS aggregation with proof of possession | a certificate must stay small across many signers | a handful of peers; `N` Ed25519 signatures verify faster than one pairing |
| a snapshot digest under that signature | **the client must trust state it did not compute** | ← the one piece to take |

So the borrowed idea is the digest, not the machinery. The per-block signature set hydrozoa
already collects becomes the certificate as soon as what it signs covers the state.

What remains uncovered is a store whose blocks below `fastBlockMark` were altered: those blocks
are read back and believed, and verifying their stored signatures is what would catch it.
Nothing does that today.

## Seeding a peer from a snapshot

The reason the state digests are worth their bytes. A coil peer joining a head with long history
is handed a snapshot — a block number `N` and the L2 ledger state at `N` — and verifies it without
replaying anything:

```
1. recompute  l2StateHash        from the supplied ledger state
2. recompute  blockHash(N)       from block N's brief
3. recompute  blockResultHash(N) from (2), the supplied flags, block N's diff, and (1)
4. verify the peers' soft-ack signatures over (blockHash(N), blockResultHash(N))
```

Constant work, whatever `N` is. Step 4 is what makes steps 1–3 mean anything: the digests are
what the peers signed, so matching them is matching what the head agreed the state was. A donor
that fabricates the ledger state has to produce a signature set over the fabrication.

**The evacuation map is not verified here, and cannot be.** `blockResultHash` commits to block
`N`'s own diff, not to the map the diffs accumulate to, so the block layer offers nothing to check
a supplied map against. That check anchors one layer up, at the stack digest, where the cumulative
map is both computed and — at the blocks whose map backs an effect — persisted. Two consequences
worth stating rather than discovering later:

- **A snapshot anchors at a stack boundary, not at an arbitrary block.** `StackComposer` persists
  `StoreKey.EvacuationMap(blockNum)` only where the map backs an effect, so those are the blocks
  at which a map even exists to be handed over. Seeding at any other `N` means shipping a map no
  peer stored.
- **Full snapshot verification needs the stack digest**, which is a separate work item. Until it
  lands, a seeded peer can verify the L2 half of its snapshot against signatures and has to take
  the evacuation-map half on trust — which is the trust boundary `transplantStackNumber` declares
  today, narrowed rather than closed.

Splitting the digest does not weaken this. The state commitments sit in `blockResultHash` and the
ack signs it, so the seeding peer verifies the same claim from the ack. What it needs alongside
the state is the block's brief (for step 2) and its validity flags (for step 3); both are in the
`Block` journal beside the confirmation. Each ack also states the `blockResultHash` its signature
covers, so step 4 is a comparison before it is a verification, and a donor with a stale or
fabricated state is named as such rather than merely failing a signature check. Open question 5
asks whether a `bodyHash` reduces step 2 to the header alone.

**Every block, not only majors.** The KZG commitment in the treasury datum pins the evacuation
map at major-block settlement, which is a real anchor and a stronger one — it is on L1 rather
than under peer keys. It is also as sparse as the head's major cadence, which some deployments
space out arbitrarily far. Per-block digests make any block a seeding point.

**What a snapshot has to carry** is a separate question from what commits to it. The two digests
cover the ledger state and the evacuation map; the rest of the recovery base (§5.2 of
`persistence-and-crash-recovery.md` — the deposits map, request high-water, the block and stack
spines) is not covered by either, and a seeded peer either re-derives it or is handed it on
trust. Settling that is the work item this design unblocks rather than one it completes.

## Where the digests live

**`blockHash` is a `BlockBrief` field** — not a `BlockHeader` one, which cannot work.

`blockHash` is known at the cut, but a header still cannot *cover* a body it does not hold.

`BlockHeader` is used standalone in three places, none of which has a body:

| use | why no body |
|---|---|
| `nextHeaderMinor` / `nextHeaderMajor` / `nextHeaderIntermediate` / `nextHeaderFinal` | they derive block N+1's header from N's header plus timing, **before N+1's body exists** |
| `JointLedger.State.previousBlockHeader` | block chaining needs only the previous scalars |
| `StandaloneEvacuationCommitment.Onchain(headId, h, kzg)` | commits to the evacuation map, not the body |

The first is decisive. Those methods return `F[BlockHeader.Minor]` and friends from timing alone,
with no body to hash. The other two would carry a body commitment they have no use for.

**`blockResultHash` is not a brief field. It is a `SoftAck` field.** The brief is the
announcement and the announcement is made at the cut, so a brief field that exists only after
applying puts back, in the type, the ordering the opening section takes out. The result is also
not the leader's to state: every peer derives its own, and the leader's is one of `N` rather than
the one the others copy. So it rides the per-peer message that already travels after the work is
done — see *What the soft-ack signs*.

`BlockBrief` is where header and body meet, and `BlockBrief.Section` already extends both
`BlockHeader.Section` and `BlockBody.Section`, so the preimage needs no new plumbing. It is also
what actually travels and persists — the block lane carries briefs, and `JournalKey.Block` stores
one — so the storage and wire story is unchanged by the choice.

`signingBytes` moves from `BlockHeader.Section` to `BlockBrief.Section` with it, and stops being
a value on the section: the preimage names a result the brief does not hold, so it takes one —
`BlockBrief.Section.signingBytes(blockResultHash)`. Both call sites already hold a brief.
`JointLedger` (`:719`) passes the result it just computed, because it is signing its own ack.
`FastConsensusActor` (`:285`) passes the result each ack states, because it is verifying somebody
else's — see below.

**Stored, and never trusted.** The brief carries the hash on the wire and into the `Block`
journal, but a stored hash is a claim: every peer that rebuilds the block recomputes the digest
from header and body and compares. That holds in both directions:

- **On receipt.** A follower rebuilding block N from its own mempool computes `blockHash` and
  compares it against the leader's brief. That is the divergence
  `panicOnMismatchWithExpectedBrief` catches today, decided on one 32-byte value.
- **On replay.** `ReplayActor` feeds persisted briefs back into `BlockWeaver` and
  `FastConsensusActor`; where `JointLedger` re-derives the block it checks the stored hash
  against the recomputed one. Below `fastBlockMark` nothing re-derives — those blocks are read
  back and believed, and their stored signatures are the only thing that would catch a change.

**Coil peers check both digests.** A coil peer authors no soft-ack, so it signs neither — but it
rebuilds block bodies exactly as a head follower does, so it recomputes `blockHash` and compares
on the same path. It also *receives* the head peers' acks: `SoftAck` is a `CoilRelay.Artifact`,
relayed over `PeerLiaisonHubToCoil` alongside briefs and hard-acks. So a coil compares its own
`blockResultHash` against the value each ack states, exactly as a head peer does, and reaches the
same verdict without contributing a signature to it.

That extends both guarantees from head↔head to head↔coil, which is where they are most needed: a
coil peer's divergence is otherwise invisible until its hard-ack fails to verify.

## What the soft-ack signs

**Both block digests, with the two version components beside them.**

```scala
SignedDigest(versionMajor, versionMinor, blockHash, blockResultHash)
```

This is where the split pays for itself. The brief is announced at the cut carrying `blockHash`
alone; the ack is produced after applying and carries the result. So the ack is the natural home
for everything the announcement could not wait for — and the one message that already travels
*after* the work is done.

That the ack signs both is what preserves every guarantee of the single-digest design. A peer
attests to two things in one signature: *this is the block I was given* and *this is what I got
from applying it*. Neither statement is weaker than before, and their separation is what makes an
execution divergence nameable rather than merely visible.

**The ack carries `blockResultHash` as a field**, on the same terms the brief carries
`blockHash`: stored, and never trusted.

```scala
SoftAck(ackId, blockNum, blockResultHash, headerSignature, finalizationRequested)
```

Carrying the value costs 32 bytes per ack per block and buys the diagnosis. Left implicit in the
preimage, a peer that computed a different result is indistinguishable from one signing with the
wrong key or one whose message arrived mangled — three failures, one symptom. Stated, the result
is a claim that can be compared before any signature is checked.

**Verification becomes per-ack, and compares before it verifies.**
`FastConsensusActor.completeCell` (`:284`) computes one `msg` from the brief today and checks
every signature against it. Two things change:

1. **A preimage per ack**, built from the `blockResultHash` that ack states rather than from one
   value shared across all of them.
2. **An equality pass first** — every ack's `blockResultHash` against the peer's own — and only
   then the signature pass.

Building each preimage from the *verifier's* own result would catch a divergence too, as a
signature failure. Comparing first is what makes the divergence nameable: the comparison says
which peer computed what, and the verification then confirms that peer really claimed it rather
than being misquoted by whoever relayed the ack.

**The ordering this needs is already there.** The new preimage cannot be built before the
verifier has applied the block. `completeCell` runs only on a saturated cell, `isSaturated`
requires an ack from every head peer including the local one, and `JointLedger` (`:716`) authors
that ack only after applying. So no ack is verified before the local apply today, and none has to
start waiting.

`blockHash` is unaffected: it is checkable the moment the brief lands, which is the first of the
two stages below.

**The same shape lets deposit decisions follow later**, without redesigning any of this. If a
decision stops riding the brief, it needs a carrier that travels after the leader has observed
L1 — which is what the ack already is.

`blockNum` and `startTime` go. Both are inside the `blockHash` preimage, so dropping them
unbinds nothing — a signature made over block N still cannot be replayed as block M. `SoftAck`
already carries `blockNum` as a plain field, so anything wanting an ack's block number has it
without parsing signed bytes.

The versions stay, duplicated in the preimage on purpose: a ratchet must read them **without
recomputing a hash** — `versionMajor` for equality, `versionMinor` for the strict increase. A
digest gives an ordering on nothing; it can only say two things differ.

Be honest about what that is. **Nothing reads any field of `SignedDigest` today.** The type has
five references in the repository — constructed in `signingBytes`, its own declaration, an
unused `Serialized.Section`, and a doc comment in `PeerWallet` — and its derived `FromData`
decoder is never called. So the versions are kept for a ratchet that does not yet read them, at
a cost of eight bytes in an off-chain message, to keep that option open. That is a deliberate
choice, not a current requirement.

**Everything else collapses into the hash.** The check moves from a structural comparison to
signature verification, which is where it belongs — a follower that derives a different block
produces a different `blockHash`, and the leader's ack fails to verify against its own brief.
The domain tag inside the preimage keeps those signed bytes separable from any other digest the
protocol signs.

**This is what makes the signature set a state certificate.** `blockResultHash` covers the two
state digests and is bound to `blockHash`, so signing the pair attests to the state the block
produced and not only to the requests it contained. The `SoftConfirmation` record — the header
plus the aggregated soft-acks — is then a complete, self-contained proof that a given state
belongs to a given block, which is what a peer seeding from a snapshot verifies and what it
already finds in the store.

**What this does not touch: the rule-based ratchet.** It reads none of these fields.
`DisputeResolutionScript` compares `voteRedeemer.sec.versionMinor > prevVersionMinor` and
verifies signatures over `voteRedeemer.sec.toData |> serialiseData` — the standalone evacuation
commitment, whose `Onchain` shape carries `headId`, `versionMajor`, `versionMinor` and
`commitment` as its own fields. `StackEffectsBuilder.secOf` lifts `blockVersion` off the block
header into the SEC, so the version reaches the dispute through a shape the builder keeps
deliberately independent of the fast-cycle `signingBytes` path.

Two signposts in the code point the other way and are stale. `DisputeResolutionScript`'s comment
claims the multisig covers "the blockHeader field of voteRedeemer" when the code signs `sec`;
and `VoteTx`, `RatchetVoteTx` and `RuleBasedActor` type their SEC signatures as
`BlockHeader.Minor.HeaderSignature`, the aliasing the `BlockHeader.scala` TODO already wants
untied. Both are worth correcting; neither is a coupling.

**This costs no Plutus budget.** Despite the name, `SignedDigest.Onchain` is not consumed
on-chain. Its only readers are `PeerWallet.mkHeaderSignature`, `JointLedger` (signing, `:719`)
and `FastConsensusActor` (verification, `:285`), and `Onchain` is a misleading name worth
correcting alongside the shape change.

**`JointLedger` compares hashes.** `panicOnMismatchWithExpectedBrief` compares 32-byte values
instead of case-class trees, in two stages and against two different sources: `blockHash` against
the leader's brief on receipt, and `blockResultHash` against each peer's ack once the block has
been applied. The first is checkable before any work is done, which is worth having on its own —
a divergent block is rejected without being applied.

`briefMismatchSummary` stays: once the hashes differ, the field-level diff is what tells an
operator *which* part flipped, and it is the only thing that can — a hash says they disagree,
never how. What the split adds is that the operator is told *which kind* of disagreement it is
before reading the diff.

It explains a `blockHash` mismatch only, because a brief is all it holds. A `blockResultHash`
mismatch has no field-level diff behind it — the two peers hold different ledger states rather
than different messages — so what an operator gets is the ack's peer number and the two digests,
and the state comparison that would say more is a diagnostic tool this design does not build.

## Migration

**A running head cannot be upgraded across this change.** Three things move at once: the signed
bytes, so acks from a peer on the old preimage fail to verify on the new one and the reverse; the
brief, on the wire and as the `Block` journal value, which gains `blockHash`; and the ack, on the
wire and as the `SoftAck` journal value, which gains `blockResultHash`. It applies to heads
initialized afterwards, and belongs in the release notes of the release that ships it.

The block header is **unchanged** by this design. The state digests live in `blockResultHash`, not
on the header, which leaves `BlockHeader` and its `nextHeader*` constructors exactly as they are.

**A fourth thing moves if `l2StateHash` comes from the remote ledger** (open question 3): the
coordination protocol gains a per-block state digest, which lands in
`sugar-rush-ledger/types/src/types/coordination/` and `hydrozoa/multisig/ledger/remote/` in the
same work item, with the golden pins on both sides moved together. A head on the new preimage
cannot drive a ledger on the old one.

## Out of scope

- **The HTTP surface.** `requestHash` reaches the submitter through the existing synchronous
  reply, which is what makes the assignment-time choice above worth anything. Everything beyond
  that — whether `GET /head/requests/{id}` returns the hash, whether the hash becomes a lookup
  key in its own right, and the route and reverse index that would need — is a separate PR
  against the API.
- **Stack hashes.** The slow side needs the same commitment, built the same way: a `stackHash`
  over the stack brief's own fields and the ordered `blockHash`es of the blocks it closed over,
  exactly as a `blockHash` covers the ordered `requestHash`es of its body. Three layers, each
  committing to the one below by hash. Separate work item, and the same cut-time discipline
  applies: whatever a stack leader announces must not depend on closing the stack.

  It also inherits the **cumulative evacuation map digest** this design pushed up to it. That is
  the layer that folds the map (`EvacuationMap.applyDiffs` in `StackComposer`) and the layer that
  persists it, so it is the layer that can commit to it — and until it does, snapshot seeding
  verifies its L2 half only.
- **Removing `ValidityFlag` from `BlockBody`.** The flags are derivable, so carrying them in the
  brief is redundant rather than wrong, and this design already keeps them out of `blockHash`.
  Deleting the field is a change to the block type, the wire brief, the journal value and every
  consumer that reads a flag off a body instead of computing it — worth doing, not worth
  entangling here.
- **Moving deposit decisions out of the brief.** The larger of the two. A decision would need a
  carrier that travels after the leader has observed L1, which is the shape the ack already has,
  and a rule for when a block is complete without one. Until then absorption lists stay in
  `blockHash`, where the leader's decision belongs.
- **The two stale rule-based signposts** named above. Both live in `cardano-onchain` and neither
  blocks this work.

## Open questions

1. **What exactly does `evacuationDiffHash` cover?** `BlockResult.evacuationMapDiff` is
   `Seq[EvacuationDiffGroup]` — grouped, and `flatEvacuationDiffs` erases the boundaries for
   folding. Hashing the flattened sequence commits to the diffs in application order and nothing
   about the grouping; hashing the groups commits to both. The grouping exists because partitions
   need it on the slow side, so whether two peers must agree on it at the block layer decides
   which of the two the preimage takes.
2. **Should `blockResultHash` cover the rest of `BlockResult`?** It carries
   `payoutObligations`, `payoutRequestIds`, `postDatedRefundTxs`, `absorbedDeposits` and
   `competingFallbackTxTime` alongside the diffs — all of them produced by applying the block,
   all of them known at that moment, none of them currently committed to. If the digest's job is
   "what applying the block produced", the honest preimage is the whole result rather than two
   fields chosen from it. Against that: every field is derivable from the request list plus the
   ledger, so a mismatch would surface in `l2StateHash` anyway for anything that touches state,
   and the extra coverage buys a sharper error rather than a new detection.

3. **Can the remote L2 ledger produce `l2StateHash` on every block?** This design assumes it
   can. Hydrozoa cannot compute the digest itself — under `L2LedgerKind.AnyRemote` the ledger is
   a black box and its state never crosses the boundary — so the value has to come back over the
   coordination protocol, per block, cheaply enough to sit on the critical path of a block cut.
   The shape has precedent: `restoreTo` already returns an evacuation-map digest that
   `JointLedger.State.recover` checks against its own folded expectation
   (`RestoreError.EvacuationMapMismatch`). Extending that to a state root is the same kind of
   change, and the same kind of cost: a wire break landing in
   `sugar-rush-ledger/types/src/types/coordination/` and
   `hydrozoa/multisig/ledger/remote/` together, with golden pins moved on both sides. **Confirm
   with the Sugar Rush side what a RocksDB-backed CLOB can commit to per block before this
   design fixes an interface they have to implement.** [what is the per-block cost there?]
4. **What does `l2StateHash` cover on the built-in EUTXO ledger?** `EutxoL2Ledger` has no such
   digest today, and the two backends have to agree on what the field means even though neither
   sees the other's representation. Whether that is a root over the L2 UTxO set, or a digest
   defined the way `EvacuationMap.digest` is — over bytes both sides already exchange — decides
   how much of `l2-ledger-command-coordination.md` moves.
5. **Should the header carry a `bodyHash`, so a seeding peer needs headers only?** Add a digest
   over the ordered body to the header and `blockHash` becomes a hash of the header alone, so a
   peer seeding from a snapshot verifies a header plus signatures without fetching a single
   request list. A follower rebuilding a block checks `bodyHash` against the body it derived,
   which is the check `blockHash` performs today, one level down. That is the layered shape the
   rest of this design already follows.

   **The cut-time split makes this cheaper than it was.** A `bodyHash` is computable at the cut
   like everything else in `blockHash`, so adding it introduces no ordering constraint and no
   finalizer — the objection that killed the header-side state digests does not apply. It costs a
   second digest per block and a preimage rewrite in this document, and wants deciding before
   implementation starts.
6. **Memoize `blockHash` on the brief?** As a stored `BlockBrief` field the value is present
   without computation, but every rebuild recomputes it to compare. Whether that recomputed
   value is worth caching — a `lazy val` on `BlockBrief.Section`, once per brief rather than
   once per comparison — is a profiling question, not a design one.
7. ~~**Does `transplantStackNumber` come out in the same work item?**~~ **Settled: it comes out.**
   It declares a trust boundary — everything at or below the tag is taken from the donor and never
   verified — which is the hole the state digests close. The seeding path this design enables
   replaces it rather than sitting beside it. Tracked as GUM-320, decided 2026-09-08; the ordering
   between the two work items is the only thing left.
8. **Does the leader apply its own block on the same path as a follower?** The point of the split
   is that it can — announce at the cut, then apply alongside everyone else. Whether
   `BlockWeaver` and `JointLedger` actually allow that today, or whether the leader's apply is
   entangled with producing the brief, decides how much of the latency win is available without
   further restructuring. Worth checking before this design is used to justify a throughput
   claim.
