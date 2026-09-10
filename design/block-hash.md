# Block hash

For whoever implements content commitments on the fast side. It defines `requestHash` over a user
request and `blockHash` over the block that carries it, says when each is taken, and names what
compares them. Committing to the *state* a block produces is a further question this document
scopes but does not settle.

Stacks need the same treatment on the slow side. That is a separate work item; this document
fixes the approach both follow.

## Scope

This document describes the whole shape. **Cycle 3 implements the request layer and one block
digest**, and nothing else:

1. **`requestHash` on the request.** A hash field on the request, with a variant tag so a
   transaction request and a deposit request cannot collide.
2. **Verification in `RequestSequencer`.** Re-derive the hash from the received body and reject
   on mismatch, before an id is assigned.
3. **Persistence in `RequestSequencer`.** The verified hash is stored with the assigned request.
4. **One `blockHash`, over the brief as it stands.** Validity flags stay in `BlockBody` and stay
   in the preimage. Moving them out is a large refactor of the block type, the wire brief, the
   journal value and every consumer that reads a flag off a body — out of scope here.
5. **Followers re-hash every request in `JointLedger`.** A follower rebuilding a block hashes
   each request body it holds. A peer that received different bytes under the same `RequestId`
   computes a different `requestHash`, so its `blockHash` differs and the mismatch surfaces at
   the block comparison instead of never.

Deferred, with the reasoning kept below because it is what the increment is aiming at:

- **`blockResultHash` and the cut-time split.** Announcing a brief before applying requires the
  flags to leave it, which is item 4's refactor. Until then the brief carries consequences and is
  announced after applying, as it is today.
- **State commitments.** Committing to the L2 state and the evacuation map is a separate
  question, and the leading answer is no longer a per-block digest — see *State commitments are
  a stack question* below.
- **Stack hashes.** The slow side's layer, unchanged in scope.

The five items above are self-contained: they close the "different payloads under one id" hole
(point 3 of *The gap*) without touching block or stack structure. The state hole (point 4) stays
open, deliberately.

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

| | decided at the cut | in `blockHash` (cycle 3) |
|---|---|---|
| `blockNum`, versions, the four times | yes | ✓ |
| the ordered `(RequestId, requestHash)` sequence | yes | ✓ |
| `depositsAbsorbed` / `depositsRejected` | yes — the leader decides them | ✓ |
| `ValidityFlag` per request | no — applying decides | ✓, because the flags stay in the brief |
| the L2 state and the evacuation map | no | not committed to at the block layer at all |

The fourth row is where cycle 3 departs from the target shape. Flags belong on the applied side
by this argument, but moving them out of `BlockBody` is a refactor of the block type, the wire
brief, the journal value and every consumer that reads a flag off a body. Until that happens the
brief carries them, `blockHash` covers them, and the brief is announced after applying — the
behaviour today. The digest is correct either way; only the pipelining is deferred.

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
known when the leader cuts the block and the other is not. Cycle 3 takes the content hash; the
state digest is scoped out above.

## Two digests, and where the third lives

| digest | over | taken by | when | carried on |
|---|---|---|---|---|
| `requestHash` | one user request as received | the submitter, and every peer that receives it | supplied on submission, re-derived and checked at `RequestId` assignment | the block body, beside its `RequestId` |
| `blockHash` | the brief — header fields, the ordered request sequence, flags and absorption decisions | the block leader, and every peer that rebuilds the block | after the block is applied, as briefs are produced today | the block brief |

A third layer commits to the state, and it is not a block digest: see *State commitments are a
stack question*.

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
- **`ValidityFlag` beside each request.** The flags are a result, not a decision, so the target
  shape keeps them out — but they stay in `BlockBody` for now (see *Scope*), and the preimage
  covers the brief as it stands. Taking them out later changes the preimage and therefore the
  domain tag.

**The initial block gets one too.** `BlockBrief.Initial` has an empty body and a header already
pinned by `headParamsHash` through the initialization transaction, so its hash proves nothing
new — but hashing it keeps all four block types uniform, keeps `blockHash` total on
`BlockBrief`, and removes a special case from every consumer. Its empty body encodes as three
zero-length lists under the `Initial` type tag.

## State commitments are a stack question

`blockHash` commits to a block's **content**: which requests, in what order, with which flags and
which absorption decisions. It does not commit to the **state** that applying them produced, which
is point 4 of *The gap* — two peers that agree on every request and reach different ledger states
still compare equal.

An earlier draft closed that with a second per-block digest, `blockResultHash`, carrying an
`l2StateHash` and an evacuation-map digest and signed by the soft-ack. **That is not the direction
being taken.** The leading answer is a **certificate on the stack**: a signed statement of the L2
ledger state and the evacuation map at a stack boundary, rather than a digest recomputed and
compared on every block.

Two reasons it is better:

1. **The state hash is not computed every block.** Under `L2LedgerKind.AnyRemote` the head cannot
   compute it at all — the ledger is a black box and the value has to come back over the
   coordination protocol. Asking for that per block puts a round trip on the critical path of
   every block cut; asking for it per stack puts it where the slow cycle already waits.
2. **It is a real certificate.** A digest that every peer recomputes and compares is a divergence
   *detector*: it tells peers they disagree. A signed statement of the state is something a peer
   can be *handed* — it confirms the state and the evacuation map to someone who was not there,
   which is what a joining coil peer actually needs, and what "certificate" already means in this
   codebase's vocabulary for the coil handshake.

The evacuation map fits the stack layer for a second, independent reason: **a block does not know
it.** `BlockResult` carries `evacuationMapDiff: Seq[EvacuationDiffGroup]` — the block's own
contribution — and nothing else. The running map is folded on the slow side, in `StackComposer`
(`EvacuationMap.applyDiffs(runMap, result.flatEvacuationDiffs)`), walking a stack's blocks in order
from the previous stack's map, and persisted only at the blocks whose map backs an effect. A digest
over the map after block N depends on every block before it — information the fast side does not
have at the cut and does not have when it applies the body either.

**Not settled.** The certificate shape has to be designed against the slow cycle: what it covers,
who signs it, at which boundary, how it is stored, and how a joining peer asks for one. That is
the stack work item's problem, and this document does not prejudge it beyond saying the state
commitment belongs there rather than here.

**What stays true meanwhile.** `JointLedger.panicOnMismatchWithExpectedBrief` still compares the
locally re-derived brief structurally, so a state divergence that changes any block content is
still caught — just not one that changes only state. Nothing this document adds makes that worse,
and `blockHash` makes the content half of it verifiable by a peer that did not re-derive.

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

What `blockHash` gives a joining peer, and what it does not.

A coil peer joining a head with long history is handed a snapshot — a block number `N`, the L2
ledger state at `N`, and the evacuation map at `N`. Two halves, verified very differently:

- **The block's content is verifiable from signatures.** Recompute `blockHash(N)` from block
  `N`'s brief and check the head peers' soft-ack signatures over it. Constant work, whatever `N`
  is, and a donor that fabricates the brief has to produce a signature set over the fabrication.
- **The state is not.** Nothing signed covers the ledger state or the evacuation map, so both
  halves of the snapshot rest on the donor. That is the trust boundary `transplantStackNumber`
  declares today, and `blockHash` narrows it — the block a peer is seeded at is now provably the
  block the head agreed on — without closing it.

**Closing it is what the stack certificate is for.** A signed statement of the L2 state and the
evacuation map at a stack boundary is exactly the thing a seeding peer needs and cannot get from
a per-block digest it would have to recompute for itself. Note the anchor that implies:
`StackComposer` persists `StoreKey.EvacuationMap(blockNum)` only where the map backs an effect,
so those blocks are the ones at which a map exists to hand over at all. A snapshot anchors there,
not at an arbitrary `N`.

**What a snapshot has to carry** is a separate question from what commits to it. Beyond the
ledger state and the evacuation map, the rest of the recovery base (§5.2 of
`persistence-and-crash-recovery.md` — the deposits map, request high-water, the block and stack
spines) is covered by nothing here, and a seeded peer either re-derives it or is handed it on
trust. Settling that is a work item this design unblocks rather than one it completes.

## Where `blockHash` lives

**A `BlockBrief` field** — not a `BlockHeader` one, which cannot work. A header cannot *cover* a
body it does not hold.

`BlockHeader` is used standalone in three places, none of which has a body:

| use | why no body |
|---|---|
| `nextHeaderMinor` / `nextHeaderMajor` / `nextHeaderIntermediate` / `nextHeaderFinal` | they derive block N+1's header from N's header plus timing, **before N+1's body exists** |
| `JointLedger.State.previousBlockHeader` | block chaining needs only the previous scalars |
| `StandaloneEvacuationCommitment.Onchain(headId, h, kzg)` | commits to the evacuation map, not the body |

The first is decisive. Those methods return `F[BlockHeader.Minor]` and friends from timing alone,
with no body to hash. The other two would carry a body commitment they have no use for.

`BlockBrief` is where header and body meet, and `BlockBrief.Section` already extends both
`BlockHeader.Section` and `BlockBody.Section`, so the preimage needs no new plumbing. It is also
what actually travels and persists — the block lane carries briefs, and `JournalKey.Block` stores
one — so the storage and wire story is unchanged by the choice.

`signingBytes` moves from `BlockHeader.Section` to `BlockBrief.Section` with it. Both call sites
already hold a brief: `JointLedger` (`:719`) signing its own ack, and `FastConsensusActor`
(`:285`) verifying somebody else's.

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

**Coil peers check it too.** A coil peer authors no soft-ack, so it signs nothing — but it
rebuilds block bodies exactly as a head follower does, so it recomputes `blockHash` and compares
on the same path. That extends the guarantee from head↔head to head↔coil, which is where it is
most needed: a coil peer's divergence is otherwise invisible until its hard-ack fails to verify.

### Followers hash every request they hold

The check above is only as good as the request hashes feeding it, so a follower does not take
`requestHash` from the brief. **`JointLedger` hashes each request body it holds** as it rebuilds
the block, and builds its `blockHash` from those digests.

That is what closes point 3 of *The gap*. Two peers holding different payloads under the same
`RequestId` compare equal today, because nothing ties an id to its bytes. Once the follower hashes
its own copy, the difference lands in `requestHash`, which lands in `blockHash`, which the
follower is already comparing against the leader's brief. No new comparison site is needed — the
existing one gets something worth comparing.

It also means the hash on the brief is never load-bearing for a peer that has the request. It is
load-bearing only for a peer that does not: a submitter checking that their request made it into a
block, or a peer seeded from a snapshot.

## What the soft-ack signs

**The block digest, with the two version components beside it.**

```scala
SignedDigest(versionMajor, versionMinor, blockHash)
```

`blockNum` and `startTime` go. Both are inside the `blockHash` preimage, so dropping them unbinds
nothing — a signature made over block N still cannot be replayed as block M. `SoftAck` already
carries `blockNum` as a plain field, so anything wanting an ack's block number has it without
parsing signed bytes.

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

**What the signature set then proves.** A soft-confirmed block's aggregated acks attest that
every head peer saw the same block: the same requests, in the same order, with the same flags and
the same absorption decisions. They do **not** attest to the state that block produced — that is
the stack certificate's job, and until it exists the signature set is a content proof and nothing
more. A snapshot's state half rests on the donor, not on signatures.

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
instead of case-class trees, against the leader's brief on receipt — before the block is applied,
so a divergent block is rejected without being applied.

`briefMismatchSummary` stays: once the hashes differ, the field-level diff is what tells an
operator *which* part flipped, and it is the only thing that can — a hash says they disagree,
never how.

## Migration

**A running head cannot be upgraded across this change.** Three things move at once: the request,
which gains a hash field; the signed bytes, so acks from a peer on the old preimage fail to verify
on the new one and the reverse; and the brief, on the wire and as the `Block` journal value, which
gains `blockHash`. It applies to heads initialized afterwards, and belongs in the release notes of
the release that ships it.

The block header is **unchanged** by this design, and so is `SoftAck` beyond what it signs.

**The L2 coordination protocol is untouched by cycle 3.** It moves only when the state commitment
does (open questions 1-3): the protocol would gain a state digest landing in
`sugar-rush-ledger/types/src/types/coordination/` and `hydrozoa/multisig/ledger/remote/` in the
same work item, with the golden pins on both sides moved together. Nothing here obliges the Sugar
Rush side to do anything yet.

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

1. **What does a stack certificate look like?** The leading answer to the state question, and
   undesigned: what it covers (the L2 state digest and the evacuation map, presumably as
   digests), who signs it, at which boundary, how it is stored, and how a joining peer asks for
   one. It belongs to the stack work item, but it is the thing that closes point 4 of *The gap*,
   so the two are coupled in sequence even though they are separate in scope.
2. **What can the remote L2 ledger commit to, and how often?** Hydrozoa cannot compute an L2
   state digest itself — under `L2LedgerKind.AnyRemote` the ledger is a black box and its state
   never crosses the boundary — so the value has to come back over the coordination protocol.
   Per stack is the point of the certificate shape: it takes the round trip off the block-cut
   path and puts it where the slow cycle already waits. The shape has precedent: `restoreTo`
   already returns an evacuation-map digest that `JointLedger.State.recover` checks against its
   own folded expectation (`RestoreError.EvacuationMapMismatch`). **Confirm with the Sugar Rush
   side what a RocksDB-backed CLOB can commit to, and at what cadence, before a design fixes an
   interface they have to implement.**
3. **What does an L2 state digest cover on the built-in EUTXO ledger?** `EutxoL2Ledger` has no
   such digest today, and the two backends have to agree on what the field means even though
   neither sees the other's representation. Whether that is a root over the L2 UTxO set, or a
   digest defined the way `EvacuationMap.digest` is — over bytes both sides already exchange —
   decides how much of `l2-ledger-command-coordination.md` moves.
4. **Should the header carry a `bodyHash`, so a seeding peer needs headers only?** Add a digest
   over the ordered body to the header and `blockHash` becomes a hash of the header alone, so a
   peer seeding from a snapshot verifies a header plus signatures without fetching a single
   request list. A follower rebuilding a block checks `bodyHash` against the body it derived,
   which is the check `blockHash` performs today, one level down. That is the layered shape the
   rest of this design already follows.

   It costs a second digest per block and a preimage rewrite in this document, and wants deciding
   before implementation starts — a peer seeded on headers alone is the case it buys.
5. **Memoize `blockHash` on the brief?** As a stored `BlockBrief` field the value is present
   without computation, but every rebuild recomputes it to compare. Whether that recomputed
   value is worth caching — a `lazy val` on `BlockBrief.Section`, once per brief rather than
   once per comparison — is a profiling question, not a design one.
6. ~~**Does `transplantStackNumber` come out in the same work item?**~~ **Settled: it comes out.**
   It declares a trust boundary — everything at or below the tag is taken from the donor and never
   verified — which is the hole the state digests close. The seeding path this design enables
   replaces it rather than sitting beside it. Tracked as GUM-320, decided 2026-09-08; the ordering
   between the two work items is the only thing left.
7. **Does the leader apply its own block on the same path as a follower?** The point of the split
   is that it can — announce at the cut, then apply alongside everyone else. Whether
   `BlockWeaver` and `JointLedger` actually allow that today, or whether the leader's apply is
   entangled with producing the brief, decides how much of the latency win is available without
   further restructuring. Worth checking before this design is used to justify a throughput
   claim.
