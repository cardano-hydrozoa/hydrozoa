# Block hash

For whoever implements content commitments on the fast side. It defines `requestHash` over a user
request and `blockHash` over the block that carries it, says when each is taken, and names what
compares them. Committing to the *state* a block produces is a separate question, answered in
`docs/spec/l2-state-certificate.md`.

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
5. **`JointLedger` re-hashes every alien request.** A peer rebuilding a block hashes the body of
   each request another peer assigned. A peer that received different bytes under the same
   `RequestId` computes a different `requestHash`, so its `blockHash` differs and the mismatch
   surfaces at the block comparison instead of never. Its own requests it does not hash again:
   their digest is the one its `RequestSequencer` already verified.

Deferred, with the reasoning kept below because it is what the increment is aiming at:

- **`blockResultHash` and the cut-time split.** Announcing a brief before applying requires the
  flags to leave it, which is item 4's refactor. Until then the brief carries consequences and is
  announced after applying, as it is today.
- **State commitments.** Committing to the L2 state and the evacuation map is a separate work
  item with its own design — `docs/spec/l2-state-certificate.md`. It is not a block digest.

The five items above are self-contained: they close the "different payloads under one id" hole
(point 3 of *The gap*) without changing the shape of a block. The state hole (point 4) is closed
elsewhere.

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

## Two digests

| digest | over | taken by | when | carried on |
|---|---|---|---|---|
| `requestHash` | one user request as received | the submitter, and every peer that receives it | supplied on submission, checked before a `RequestId` is drawn, persisted with it | the block body, beside its `RequestId` |
| `blockHash` | the brief — header fields, the ordered request sequence, flags and absorption decisions | the block leader, and every peer that rebuilds the block | after the block is applied, as briefs are produced today | the block brief |

Committing to the state those requests produced is not a block digest at all — see
`docs/spec/l2-state-certificate.md`.

`blockHash` covers `requestHash`, not the request bytes: the body is a list of
`(RequestId, requestHash)` pairs, so the block commits to exactly which payload sits at each
position without carrying any payload.

That is the shape the design follows — **each layer commits to the one below by hash, and nothing
chains sideways within a layer.**

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

**The check is `UserRequestBody.mkHash` itself**, run over the received body. There is one hash
function, used by the submitter to produce the value and by the head to verify it; a second
implementation would be a second thing to disagree about. `UserRequest.checkRequestHash` runs it
and names both digests on a mismatch, and `RequestSequencer` counts the refusal as a screening
rejection (`RejectionKind.Screening`): it is a stateless admission check like the rest, and the
reason string tells the two apart.

**The reply is unchanged.** `UserRequest` keeps returning
`Either[UserRequest.Rejected, RequestId]`. There is nothing to add to it: the submitter computed
the hash, so returning it would hand back a value they already hold. Only the request grows a
field, and a mismatch travels as a reason string in the `Rejected` the channel already carries.

## When the head hashes: before assignment, persisted with it

In `RequestSequencer`, **first** — before stateless screening, and before `tryNextRequestNum`
draws a request number. That ordering is what makes *Refuse, do not correct* hold: a check placed
after the number is drawn would consume a `RequestId` for a request it then refuses. The digest
mismatch is the cheapest verdict available and the one that says the two sides disagree about
what was submitted, so nothing else runs on a request that fails it.

The verified digest is then **persisted by the CR1 write**, as the `Request` record's
`request_hash` field (`request_record.proto`, field 5).

Three reasons that is the right shape:

1. **Persistence rides the barrier that already exists.** CR1 persists the assigned request to
   the `Request` journal *before* the user is told the id — durable before observable. The hash
   is part of the same write and inherits the same guarantee.
2. **It is taken over the request as received**, before screening verdicts, block packing, or
   any validity judgement. The hash describes what the user submitted, and nothing later can
   move it.
3. **Every other peer recomputes it.** `RequestSequencer` fans the same `UserRequestWithId` to
   `BlockWeaver`, the head-peer mesh and (on a hub) `CoilRelay`. Each recipient hashes the body
   it received rather than trusting a digest that travelled with it. A peer that computes a
   different one has different bytes, which is exactly the condition worth detecting.

Point 3 is not weakened by the submitter supplying the hash. The user's digest is verified once,
at the edge, by the peer that received it — which then builds its own blocks from that verified
value rather than hashing the body a second time. Every other peer derives the digest from the
bytes in front of it. No peer trusts a digest nobody on it checked — including the user's.

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

## A request digest already exists

`UserRequest.scala` carries one, as `UserRequestBody.hash`: `blake2b_256`, with deposits hashed as
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
  `docs/user-guide/REQUEST-HASH.md`, with the pinned vectors from `UserRequestTest` as worked
  examples. A hash a client cannot independently compute is a hash the client cannot supply.

With the tag mixed in, the construction is:

```
requestHash = blake2b_256(
     "gummiworm-request-v1"
  || u8(variant)                                    -- 0 deposit | 1 transaction
  || deposit:     blake2b_256(l1Payload) || blake2b_256(l2Payload)
  || transaction: l2Payload
)
```

The variant tag leads the payload, so neither arm needs length framing: a deposit contributes two
fixed-width digests and a transaction one trailing payload, and the tag already separates them.
The two-digest rule stays for the reason its comment gives — hashing the deposit's payloads raw
would collapse `hash(abc + def) == hash(ab + cdef)`.

Both changes move the pinned vectors in `UserRequestTest`, which is free now and is not free once
a client has shipped against them. The new vectors are `58828159…` for the transaction and
`ac596c7f…` for the deposit; `docs/user-guide/REQUEST-HASH.md` gives them in full, with the
deposit's two intermediate digests.

**Where it lives.** The digest is its own type, `RequestHash`, opaque over `Hash32` and placed in
`ledger.event` beside `RequestId` — it is the content counterpart of the id, and what a block body
carries next to it. Its object holds the domain tag, the variant tags and the preimage, as
`hashDeposit(l1Payload, l2Payload)` and `hashTransaction(l2Payload)`. They take payloads rather
than a `UserRequestBody`, so `ledger.event` gains no dependency on `consensus`, and
`UserRequestBody.mkHash` stays the single entry point by dispatching to them.

## What `blockHash` covers

The same construction as `EvacuationMap.digest` and `HeadParamsHash`: an ASCII domain tag,
fixed-width fields unframed, lists behind a `u32` count, `blake2b_256` over the whole preimage.

```
blockHash = blake2b_256(
     "gummiworm-block-v1"
  || u8(blockType)                  -- 0 Initial | 1 Minor | 2 Major | 3 Final
  -- header
  || u32(blockNum)
  || u32(versionMajor)              || u32(versionMinor)
  || u64(startTime)                 || u64(endTime)
  || non-final block types only:               -- a final header holds none of these
       u64(fallbackTxStartTime)
    || u64(forcedMajorBlockWakeupTime)
    || bool(mDepositDecisionWakeupTime.isDefined)
    || u64(mDepositDecisionWakeupTime)         -- present only when the flag is true
  -- body: every block type writes all three lists, absent ones as length zero
  || u32(requests.length)
  || for each, in list order:
       u32(peerNum) || u64(requestNum) || raw(requestHash) || u8(validityFlag)
  || u32(depositsAbsorbed.length)
  || for each, in list order: u32(peerNum) || u64(requestNum)
  || u32(depositsRejected.length)
  || for each, in list order: u32(peerNum) || u64(requestNum)
)
```

Every field above except the validity flags is available the instant the leader cuts the block,
and the flags are the whole of what stands between this preimage and a brief that can go out
before a single request has been applied. Take them out of `BlockBody` and `blockHash` becomes
computable at the cut, with no other change to the layout. That removal is *Out of scope* here,
so the brief is announced after applying, as it is today.

Notes on the layout:

- **`blockHash` is excluded from its own preimage.** It is a `BlockBrief` field (below), so every
  other field of the brief goes in and this one does not. Missing that makes the definition
  circular. It is the same exclusion `headParamsHash` makes for the initialization transaction,
  which carries the digest it is an input to.
- **The block type leads the block's own fields**, and it is what keeps the four shapes from
  colliding. They differ on both sides: `BlockHeader.Final` carries none of the forward times —
  a final block schedules no fallback, no forced major and no deposit decision — while
  `BlockBody.Initial` has no fields, `Minor` and `Final` have no `depositsAbsorbed`, and `Major`
  has all three lists.
- **Absent header times are absent; absent body lists encode as length zero.** A final header has
  no field to read, while every body exposes all three lists through `BlockBody.Section` and an
  absent one reads as empty — so writing the lists uniformly costs nothing and keeps the body
  branch-free. Neither choice is what makes the encoding injective: the type tag has already
  separated the shapes.
- **Order is the list's own order**, not sorted. The ordered request list is what the leader
  chose and what every follower must reproduce; sorting would hide a reordering, which is a real
  disagreement about block content.
- **`RequestNumber` is `u64`.** The `Request` journal key is 8 bytes (`Markers` decodes it with
  `getLong`), unlike the 4-byte soft/hard-ack indices.
- **The optional wakeup is flag-then-value**, so `None` and a present value can never produce
  the same bytes.
- **`ValidityFlag` rides beside each request**, one byte after its digest: `0` valid, `1`
  invalid. The flags are a result, not a decision, so the target shape keeps them out — but they
  stay in `BlockBody` for now (see *Scope*), and the preimage covers the brief as it stands.
  Taking them out later changes the preimage and therefore the domain tag.

**The initial block gets one too.** `BlockBrief.Initial` has an empty body and a header already
pinned by `headParamsHash` through the initialization transaction, so its hash proves nothing
new — but hashing it keeps all four block types uniform, keeps `blockHash` total on
`BlockBrief`, and removes a special case from every consumer. Its empty body encodes as three
zero-length lists under the `Initial` type tag.

## The state hole stays open here

`blockHash` commits to a block's **content**: which requests, in what order, with which flags and
which absorption decisions. It does not commit to the **state** that applying them produced, which
is point 4 of *The gap* — two peers that agree on every request and reach different ledger states
still compare equal.

That hole is closed by a signed L2 state certificate, designed in
`docs/spec/l2-state-certificate.md`. It is not a block digest and nothing in this document depends on
it; the two work items are independent.

**What holds meanwhile.** `JointLedger.panicOnMismatchWithExpectedBrief` still compares the
locally re-derived brief structurally, so a state divergence that changes any block content is
still caught — just not one that changes only state. Nothing here makes that worse, and `blockHash`
makes the content half of it verifiable by a peer that did not re-derive.

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
  halves of the snapshot rest on the donor. `blockHash` narrows that — the block a peer is seeded
  at is now provably the block the head agreed on — without closing it.

**Closing it is what the L2 state certificate is for** — a signed statement of the L2 state and
the evacuation map, designed in `docs/spec/l2-state-certificate.md`. That is exactly what a seeding
peer needs and cannot get from a per-block digest it would have to recompute for itself.

**What a snapshot has to carry** is a separate question from what commits to it. Beyond the
ledger state and the evacuation map, the rest of the recovery base (§5.2 of
`persistence-and-crash-recovery.md`) is covered by nothing here, and a seeded peer either
re-derives it or is handed it on trust. Settling that is a work item this design unblocks rather
than one it completes.

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

The soft-ack signs `blockHash` itself, so there is no separate signing-bytes accessor to move. Both
call sites already hold a brief: `JointLedger` signing its own ack with
`PeerWallet.mkSoftAckSignature(brief.blockHash)`, and `FastConsensusActor` verifying somebody
else's against `brief.blockHash`.

The digest is typed `BlockHash`, opaque over `Hash32` like `RequestHash`, so the two can never be
passed for each other. `BlockBrief`'s companion builds a brief two ways: `apply(header, body)`
derives the digest, which is what a producer does, and the generated full-arity constructor keeps
one that arrived, which is what a decoder does.

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

### Every peer re-hashes the requests it did not assign

The check above is only as good as the request hashes feeding it, so a peer does not take
`requestHash` from the brief. As it rebuilds a block, `JointLedger` picks each request's digest
with `mkHashOf`, and builds its `blockHash` from those. `BlockBody.requests` is
`List[(RequestId, RequestHash, ValidityFlag)]`.

| request | digest used | why |
|---|---|---|
| **own** — this peer assigned it | the one it carries | this peer's `RequestSequencer` verified it against the body before assigning the id; hashing again would repeat that work |
| **alien** — any other peer assigned it | recomputed from the body this peer holds | nobody on this peer checked the digest it carries, and trusting it would compare two copies of one claim |

A coil peer assigns nothing, so every request is alien to it.

"Own" is read off the id, and peer liaisons stay transport: they check nothing, so a byzantine
peer can send a request under another peer's id. Its carried digest is then reused by that one
peer while every honest peer recomputes it. If it does not describe the body, that peer's
`blockHash` disagrees with theirs, and no block soft-confirms without every head peer's ack. The
shortcut can cost liveness under a byzantine peer — which withholding an ack already costs — and
never lets a wrong block confirm.

The digests in the store follow the same line. The CR1 write persists an own request's verified
digest; a liaison persists an alien request's digest as it arrived. So only records under a peer's
own author number carry a verified digest in that peer's store (`request_record.proto`, field 5).

That is what closes point 3 of *The gap*. Two peers holding different payloads under the same
`RequestId` compare equal today, because nothing ties an id to its bytes. Once each peer hashes
its own copy of every alien request, the difference lands in `requestHash`, which lands in `blockHash`, which the
follower is already comparing against the leader's brief. No new comparison site is needed — the
existing one gets something worth comparing.

It also means the hash on the brief is never load-bearing for a peer that has the request. It is
load-bearing only for a peer that does not: a submitter checking that their request made it into a
block, or a peer seeded from a snapshot.

## What the soft-ack signs

**The block digest, and nothing beside it.** A soft-ack is an Ed25519 signature over the 32 bytes
of `blockHash`.

Everything a signed statement about a block needs is inside that preimage already. `blockNum` and
`startTime` are there, so a signature made over block N cannot be replayed as block M, and
`SoftAck` carries `blockNum` as a plain field for anything that wants it without re-deriving a
hash. The versions are there too, and nothing needs them beside the digest: **ratcheting reads the
SEC's versions, on the slow side, never a soft-ack's.** A digest gives an ordering on nothing, but
no consumer of a soft-ack ever has to order two of them.

The one thing versions beside the digest would buy is a self-contained equivocation proof — two
signed `(versions, hash)` pairs that agree on the versions and disagree on the hash, provable
without either brief. Nothing consumes such a proof. With fixed membership and every head peer
signing every block, an equivocating peer stalls the head, and the dispute that follows is settled
by SECs.

**Everything collapses into the hash.** The check moves from a structural comparison to signature
verification, which is where it belongs — a follower that derives a different block produces a
different `blockHash`, and the leader's ack fails to verify against its own brief. The domain tag
inside the preimage keeps these signed bytes separable from any other digest the protocol signs,
and an SEC signature covers CBOR-encoded `Data`, not a bare 32-byte digest, so the two can never be
confused.

**What the signature set then proves.** A soft-confirmed block's aggregated acks attest that
every head peer saw the same block: the same requests, in the same order, with the same flags and
the same absorption decisions. They do **not** attest to the state that block produced — that is
the L2 state certificate's job, and until it exists the signature set is a content proof and nothing
more. A snapshot's state half rests on the donor, not on signatures.

**What this does not touch: the rule-based ratchet.** It reads no soft-ack at all.
`DisputeResolutionScript` compares `voteRedeemer.sec.versionMinor > prevVersionMinor` and
verifies signatures over `voteRedeemer.sec.toData |> serialiseData` — the standalone evacuation
commitment, whose `Onchain` shape carries `headId`, `versionMajor`, `versionMinor` and
`commitment` as its own fields. `StackEffectsBuilder.secOf` lifts `blockVersion` off the block
header into the SEC, so the version reaches the dispute through the slow side alone.

**Each signature gets its own type.** One opaque, `BlockHeader.Minor.HeaderSignature`, used to
carry both — nested under `Minor` for no reason the code gave, and named for a header neither of
them signs. It splits along fast and slow:

| side | signs | type | made by |
|---|---|---|---|
| fast | a block's `blockHash` | `SoftAck.Signature` | `PeerWallet.mkSoftAckSignature` |
| slow | an SEC's serialized `Data` | `StandaloneEvacuationCommitment.Signature` | `PeerWallet.mkSecSignature` |

Signatures over L1 effect transactions were already a separate type, `TxSignature`. The rule-based
users — `VoteTx`, `RatchetVoteTx`, `RuleBasedActor` — take the slow type: they carry the same SEC
signatures into vote redeemers. Both types are `IArray[Byte]` with the same hex codec, so the
split moves no bytes on the wire, in the store, or on-chain.

**This costs no Plutus budget.** Soft-acks never reach a script; the dispute consumes only SEC
signatures, and those are byte-identical to what it consumed before.

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

The block header is **unchanged** by this design. `SoftAck` gains no field: its one signature is
named `signature` and typed `SoftAck.Signature`, and `Block.SoftConfirmed`'s list of them is
`softAckSignatures`. Both codecs are derived, so those names are the JSON keys on the wire and in
the `SoftConfirmation` store — which the brief's new field already breaks.

The SEC side moves no bytes. Its signatures are byte-identical, and its persisted
`headerMultiSigned` key is kept; renaming it would add a store break this work item does not
otherwise cause.

**What Sugar Rush has to do.** Two of the contracts that move are ones it reads:

| contract | what moves | Sugar Rush |
|---|---|---|
| `POST /head/requests` | `requestHash` becomes a **required** field | **must change.** The DEX submits deposits and transactions through the unified API's proxy (`useHandleDepositRequest`, `useHandleTransactionRequest`, `useReDelegateKey`), which forwards the body byte-for-byte. Without the digest every submission is refused. The DEX computes it per `docs/user-guide/REQUEST-HASH.md`, and the change lands together with hydrozoa's. |
| `Request` journal record | gains `request_hash` (proto field 5) | nothing required. `hydrozoa-store` vendors the record without field 5, and prost skips unknown fields; it can adopt the field whenever it wants the digest. |

**The L2 coordination protocol is untouched by cycle 3.** It moves only when the state commitment
does (`docs/spec/l2-state-certificate.md`): the protocol would gain a state digest landing in
`sugar-rush-ledger/types/src/types/coordination/` and `hydrozoa/multisig/ledger/remote/` in the
same work item, with the golden pins on both sides moved together.

## Out of scope

- **L2 state certificates.** The state commitment, with its own design in
  `docs/spec/l2-state-certificate.md`. Independent of this work item in both directions.
- **Removing `ValidityFlag` from `BlockBody`.** The flags are derivable, so carrying them in the
  brief is redundant rather than wrong. Deleting the field is a change to the block type, the wire
  brief, the journal value and every consumer that reads a flag off a body instead of computing
  it — worth doing, not worth entangling here. Until it happens the flags stay in `blockHash`.
- **Moving deposit decisions out of the brief.** The larger of the two. A decision would need a
  carrier that travels after the leader has observed L1, which is the shape the ack already has,
  and a rule for when a block is complete without one. Until then absorption lists stay in
  `blockHash`, where the leader's decision belongs.
- **Whether the leader can apply its own block on the follower path.** It matters only for the
  deferred cut-time split, which is what would let the leader announce and then apply alongside
  everyone else. Whether `BlockWeaver` and `JointLedger` allow that today decides how much of the
  latency win is available without further restructuring — a question for whoever takes the
  split, and one to answer before it is used to justify a throughput claim.

## Settled, and why

**No `bodyHash` on the header.** It was considered as a way to let a seeding peer verify headers
alone, and it fails twice. The mechanism cannot work, for the same reason `blockHash` is not a
header field: `nextHeaderMinor` and friends derive block N+1's header from N's header plus timing,
*before N+1's body exists*, so a header digest over the body could never be filled. And the
benefit is not there either — briefs are what travel on the block lane and what `JournalKey.Block`
stores, so a seeding peer has briefs, not bare headers. There is no headers-only path to optimize.

**Memoizing `blockHash` is not a design question.** The stored value is already a field; what
gets recomputed on every rebuild has to be recomputed, because a stored hash is a claim (see
*Where `blockHash` lives*). Whether the recomputation is cached anywhere is an implementation
call.

**No versions beside the digest.** Settled in review of the implementation (#734): a soft-ack signs
`blockHash` alone, because ratcheting reads the SEC's own versions on the slow side. The reasoning
is in *What the soft-ack signs*.

**Each digest and each signature has its own type.** `RequestHash` and `BlockHash` are opaque over
`Hash32`; `SoftAck.Signature` and `StandaloneEvacuationCommitment.Signature` replace the single
minor-scoped `BlockHeader.Minor.HeaderSignature`. None of the four changes a byte — they exist so
that a request digest cannot stand in for a block digest, or a soft-ack signature for an SEC's.

**A digest mismatch is a screening rejection.** It counts under `RejectionKind.Screening` rather
than a counter of its own: it is a stateless admission check like the rest, and its reason string
tells the two apart. A dedicated counter would add a `PeerStats` field, a Prometheus line and a spec
change for a distinction the reason already carries.

**One preimage buffer.** `BlockHash` and `HeadParamsHash` write their preimages through the same
`lib.crypto.Preimage` — ASCII domain tag, fixed-width fields unframed, variable-width ones
length-framed — rather than two copies of the same encoder. Its byte layout is pinned by its own
test, which the digests' property tests could not do: a change shifting every writer the same way
still passes "the digest moves when a field moves".
