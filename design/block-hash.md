# Block hash

For whoever implements content commitments on the fast side. It defines two digests —
`requestHash` over a user request and `blockHash` over a block — says when each is taken, and
names what compares them. `blockHash` covers the block's content and the state it produces, so
the signature set a block already carries becomes a proof a peer can seed from without replaying
the head's history.

Stacks need the same treatment on the slow side. That is a separate work item; this document
fixes the approach both follow.

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
content hash; point 4 needs a state digest, and both belong in the same preimage.

## Two digests

| digest | over | taken by | when |
|---|---|---|---|
| `requestHash` | one user request as received | the peer that sequences it, and every peer that receives it | at `RequestId` assignment |
| `blockHash` | the header fields, the two state digests, and the ordered body | the block leader, and every peer that rebuilds the block | at block cut |

`blockHash` covers `requestHash`, not the request bytes: the body is a list of
`(RequestId, requestHash, ValidityFlag)` triples, so the block commits to exactly which payload
sits at each position without carrying any payload.

That is the shape the whole design follows — **each layer commits to the one below by hash, and
nothing chains sideways within a layer.** The slow side extends it: a `stackHash` covers the
ordered `blockHash`es of the blocks a stack closed over, the same way a `blockHash` covers the
ordered `requestHash`es of its body. Stacks are a separate work item; the construction is fixed
here.

## When a request is hashed: at assignment

In `RequestSequencer`, between `val newId = RequestId(ownHeadPeerNum, newNum)` and the CR1
persist that follows it.

Four reasons that is the right moment:

1. **The submitter gets it with the id, in the same reply.** `UserRequest` returns
   `Either[UserRequest.Rejected, RequestId]` today; it returns the hash alongside the id, from
   the one synchronous call, with no second round trip and nothing to poll.
2. **It rides the barrier that already exists.** CR1 persists the assigned request to the
   `Request` journal *before* the user is told the id — durable before observable. The hash is
   part of the same write and inherits the same guarantee.
3. **It is taken over the request as received**, before screening verdicts, block packing, or
   any validity judgement. The hash describes what the user submitted, and nothing later can
   move it.
4. **Every other peer recomputes it.** `RequestSequencer` fans the same `UserRequestWithId` to
   `BlockWeaver`, the head-peer mesh and (on a hub) `CoilRelay`. Each recipient hashes the body
   it received. The hash never travels — a peer that computes a different one has different
   bytes, which is exactly the condition worth detecting.

**Rejected: hash at block packing.** Too late for the submitter, who by then holds an id with no
way to check what it refers to. And only the leader would compute it, so a follower would be
verifying the leader's arithmetic rather than its own.

### The `RequestId` is not in the preimage

`requestHash` is a hash of the request, not of the assignment: the same bytes produce the same
digest regardless of which peer sequenced them or where they landed in that peer's sequence. So
a submitter can compute it before submitting and recognize their own request in a block without
trusting anyone's arithmetic, and two peers that received the same request agree on its hash
without agreeing on anything else.

The uniqueness a per-assignment hash would add is not needed. The block body carries the
`RequestId` beside the hash, so the block's commitment names both which request and which
position.

## `UserRequestBody.hash` already exists

`UserRequest.scala` carries it: `blake2b_256`, with deposits hashed as
`blake2b_256(l1Payload) ++ blake2b_256(l2Payload)` before the outer hash, its comment explaining
this keeps the hash injective rather than collapsing `hash(abc + def) == hash(ab + cdef)`.
`UserRequestTest` pins two vectors. Nothing in production calls it.

It hashes the body and not the `RequestId`, which is exactly the shape decided above. One thing
to fix before it becomes load-bearing:

- **No variant tag.** `TransactionRequestBody(l2Payload)` hashes `l2Payload` directly, while
  `DepositRequestBody` hashes a 64-byte concatenation of two digests. A transaction request
  whose `l2Payload` happens to be exactly that 64-byte string hashes identically to the deposit.
  Domain-separate the variants, the way `HeadParamsHash` domain-tags its preimage.

That change moves the pinned vectors in `UserRequestTest`, which is free now and is not free
once a hash has been handed to a user.

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
  -- state after the body is applied (brief fields, like `blockHash` itself)
  || raw(evacuationMapHash)                   -- 32 bytes
  || raw(l2StateHash)                         -- 32 bytes
  -- body
  || u32(requests.length)
  || for each, in list order:
       u32(peerNum) || u64(requestNum) || raw(requestHash) || u8(validityFlag)
  || u32(depositsAbsorbed.length)
  || for each, in list order: u32(peerNum) || u64(requestNum)
  || u32(depositsRejected.length)
  || for each, in list order: u32(peerNum) || u64(requestNum)
)
```

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
- **The two state digests are fixed-width and unframed**, like the scalars above them. They sit
  between header and body because they belong to the header (below).

**The initial block gets one too.** `BlockBrief.Initial` has an empty body and a header already
pinned by `headParamsHash` through the initialization transaction, so its hash proves nothing
new — but hashing it keeps all four block types uniform, keeps `blockHash` total on
`BlockBrief`, and removes a special case from every consumer. Its empty body encodes as three
zero-length lists under the `Initial` type tag.

## The two state digests

`blockHash` as described so far commits to a block's **inputs**: which requests, in what order,
with which validity flags. Two more `BlockBrief` fields make it commit to the **output** as
well — the state those inputs produce.

| field | over | why it alone is not enough |
|---|---|---|
| `evacuationMapHash` | the `EvacuationMap` after this block | a projection. Distinct L2 states share an evacuation map, transient tokens being the clearest case: they carry no payout obligation, so they leave the map untouched |
| `l2StateHash` | the L2 ledger state after this block | says nothing about what each party is owed on exit, which is the thing L1 enforces |

Both, therefore. Neither implies the other, and a snapshot is only as trustworthy as the weaker
of the two commitments over it.

**`blake2b_256`, not KZG.** The evacuation map already carries a KZG commitment, and it stays
where it is: `EvacuationMap.kzgCommitment` goes into the treasury datum at major-block
settlement, where L1 needs a commitment it can open. Per-block digests are computed and verified
by every peer on every block, so they take the same `blake2b_256` construction as every other
digest here — `EvacuationMap.digest` already produces exactly this value, defined over bytes the
head and a remote L2 ledger both already hold, and pinned to a shared golden with Sugar Rush.

**They sit on the brief, for the reason `blockHash` does.** Both describe state *after* the
block's requests are applied, so both are known only at block cut. `BlockHeader` cannot hold
them: `nextHeaderMinor` and friends derive block N+1's header from N's header plus timing,
before N+1 has a body to apply — a header carrying a post-application digest could not be
filled at the point a header is constructed. `BlockBrief` is where header, body and the state
they produce meet, which is the same argument that puts `blockHash` there.

They are ordered between the header fields and the body in the preimage, which is a layout
choice rather than a claim about where they live.

**Every peer recomputes them.** A follower rebuilding block N applies the same requests to its
own ledger and derives both digests, so a divergence in *state* is caught by the same comparison
that catches a divergence in *content* — one 32-byte value each, inside the one `blockHash` the
soft-ack signs. Nothing today catches a state divergence that produces an identical request
list.

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
is handed a snapshot — a block number `N`, the L2 ledger state at `N`, and the evacuation map at
`N` — and verifies it without replaying anything:

```
1. recompute  l2StateHash        from the supplied ledger state
2. recompute  evacuationMapHash  from the supplied map
3. compare both against block N's header
4. verify the peers' soft-ack signatures over blockHash(N)
```

Constant work, whatever `N` is. Step 4 is what makes steps 1–3 mean anything: the digests are
fields of a header the peers signed, so matching them is matching what the head agreed the state
was. A donor that fabricates either half has to produce a signature set over the fabrication.

**Every block, not only majors.** The KZG commitment in the treasury datum pins the evacuation
map at major-block settlement, which is a real anchor and a stronger one — it is on L1 rather
than under peer keys. It is also as sparse as the head's major cadence, which some deployments
space out arbitrarily far. Per-block digests make any block a seeding point.

**What a snapshot has to carry** is a separate question from what commits to it. The two digests
cover the ledger state and the evacuation map; the rest of the recovery base (§5.2 of
`persistence-and-crash-recovery.md` — the deposits map, request high-water, the block and stack
spines) is not covered by either, and a seeded peer either re-derives it or is handed it on
trust. Settling that is the work item this design unblocks rather than one it completes.

## Where `blockHash` lives

**A `BlockBrief` field** — not a `BlockHeader` one, which cannot work. The same holds for
`evacuationMapHash` and `l2StateHash`, and for the same reason: all three are known only once a
body has been applied.

`BlockHeader` is used standalone in three places, none of which has a body:

| use | why no body |
|---|---|
| `nextHeaderMinor` / `nextHeaderMajor` / `nextHeaderIntermediate` / `nextHeaderFinal` | they derive block N+1's header from N's header plus timing, **before N+1's body exists** |
| `JointLedger.State.previousBlockHeader` | block chaining needs only the previous scalars |
| `StandaloneEvacuationCommitment.Onchain(headId, h, kzg)` | commits to the evacuation map, not the body |

The first is decisive. Those methods return `F[BlockHeader.Minor]` and friends from timing alone;
a header field covering the body could not be filled at the point a header is constructed. The
other two would carry a body commitment they have no use for.

`BlockBrief` is where header and body meet, and `BlockBrief.Section` already extends both
`BlockHeader.Section` and `BlockBody.Section`, so the preimage needs no new plumbing. It is also
what actually travels and persists — the block lane carries briefs, and `JournalKey.Block` stores
one — so the storage and wire story is unchanged by the choice.

`signingBytes` moves from `BlockHeader.Section` to `BlockBrief.Section` with it. Both call sites
already hold a brief: `JointLedger` (`:719`) and `FastConsensusActor` (`:285`) each write
`brief.header.signingBytes` today and become `brief.signingBytes`.

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

**Coil peers check it.** A coil peer authors no soft-ack, so it never signs a `blockHash` — but
it rebuilds block bodies exactly as a head follower does, so it recomputes and compares on the
same path. That extends the guarantee from head↔head to head↔coil, which is where it is most
needed: a coil peer's divergence is otherwise invisible until its hard-ack fails to verify.

## What the soft-ack signs

**The block hash, with the two version components beside it.**

```scala
SignedDigest(versionMajor, versionMinor, blockHash)
```

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

**This is what makes the signature set a state certificate.** `blockHash` covers the two state
digests, so signing it attests to the state the block produced and not only to the requests it
contained. The `SoftConfirmation` record — the header plus the aggregated soft-acks — is then a
complete, self-contained proof that a given state belongs to a given block, which is what a
peer seeding from a snapshot verifies and what it already finds in the store.

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

**`JointLedger` compares hashes.** `panicOnMismatchWithExpectedBrief` compares one 32-byte value
instead of two case-class trees. `briefMismatchSummary` stays: once the hashes differ, the
field-level diff is what tells an operator *which* part flipped, and it is the only thing that
can — a hash says they disagree, never how.

## Migration

**A running head cannot be upgraded across this change.** Three things move at once: the signed
bytes, so acks from a peer on the old preimage fail to verify on the new one and the reverse;
the `Block` journal value and wire brief, which gain the `blockHash` field; and the block header,
which gains `evacuationMapHash` and `l2StateHash`. It applies to heads initialized afterwards,
and belongs in the release notes of the release that ships it.

**A fourth thing moves if `l2StateHash` comes from the remote ledger** (open question 1): the
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
  committing to the one below by hash. Separate work item.
- **The two stale rule-based signposts** named above. Both live in `cardano-onchain` and neither
  blocks this work.

## Open questions

1. **Can the remote L2 ledger produce `l2StateHash` on every block?** This design assumes it
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
2. **What does `l2StateHash` cover on the built-in EUTXO ledger?** `EutxoL2Ledger` has no such
   digest today, and the two backends have to agree on what the field means even though neither
   sees the other's representation. Whether that is a root over the L2 UTxO set, or a digest
   defined the way `EvacuationMap.digest` is — over bytes both sides already exchange — decides
   how much of `l2-ledger-command-coordination.md` moves.
3. **Memoize `blockHash` on the brief?** As a stored `BlockBrief` field the value is present
   without computation, but every rebuild recomputes it to compare. Whether that recomputed
   value is worth caching — a `lazy val` on `BlockBrief.Section`, once per brief rather than
   once per comparison — is a profiling question, not a design one.
4. **Does `transplantStackNumber` come out in the same work item?** It declares a trust boundary
   — everything at or below the tag is taken from the donor and never verified — which is the
   hole the two state digests close. The seeding path this design enables replaces it rather
   than sitting beside it, and shipping both leaves two ways to seed a peer, one of them
   unverified.
