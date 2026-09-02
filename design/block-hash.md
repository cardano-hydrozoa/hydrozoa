# Block hash

For whoever implements content commitments on the fast side. It defines two digests —
`requestHash` over a user request and `blockHash` over a block — says when each is taken, and
names what compares them.

Stacks need the same treatment on the slow side. That is a separate work item; this document
fixes the approach both follow.

## The gap

Three layers describe a block, and none of them commits to its content.

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
Its three limits are what this work item addresses:

1. It is local. A peer that skips it, or a peer type that never re-derives, is unconstrained.
2. It panics. The peer halts and hands over to the rule-based regime; it does not refuse the
   block and stay in the multisig regime.
3. It compares `RequestId`s. Two peers holding **different payloads under the same id** compare
   equal, because nothing anywhere ties an id to its bytes.

Point 3 is the one that cannot be fixed by moving the comparison. It needs a content hash.

## Two digests

| digest | over | taken by | when |
|---|---|---|---|
| `requestHash` | one user request as received | the peer that sequences it, and every peer that receives it | at `RequestId` assignment |
| `blockHash` | the header fields and the ordered body | the block leader, and every peer that rebuilds the block | at block cut |

`blockHash` covers `requestHash`, not the request bytes: the body is a list of
`(RequestId, requestHash, ValidityFlag)` triples, so the block commits to exactly which payload
sits at each position without carrying any payload.

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
  || raw(previousBlockHash)         -- headParamsHash for block 0
  || u8(blockType)                  -- Initial | Minor | Major | Final
  -- header
  || u32(blockNum)
  || u32(versionMajor)              || u32(versionMinor)
  || u64(startTime)                 || u64(endTime)
  || u64(fallbackTxStartTime)
  || u64(forcedMajorBlockWakeupTime)
  || bool(mDepositDecisionWakeupTime.isDefined)
  || u64(mDepositDecisionWakeupTime)          -- present only when the flag is true
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

**The initial block gets one too.** `BlockBrief.Initial` has an empty body and a header already
pinned by `headParamsHash` through the initialization transaction, so its hash proves nothing
new — but hashing it keeps all four block types uniform, keeps `blockHash` total on
`BlockBrief`, and removes a special case from every consumer. Its empty body encodes as three
zero-length lists under the `Initial` type tag.

## Chaining to the previous block

`previousBlockHash` is in the preimage, so each block's hash commits to its whole history, and
block 0's predecessor is `headParamsHash` — the chain roots in the agreed configuration rather
than in a zero constant.

**Be clear about what it does not buy here.** Hydrozoa is not a fork-choice system. Every block
carries an all-peer soft-confirmation, so there is no competing history to arbitrate and no work
saved at verification time: checking a chained hash still means recomputing every block from its
content, which a peer rebuilding ledger state does anyway. And divergence is already caught at
the *first* differing block — `panicOnMismatchWithExpectedBrief` fires on block N+1 the moment
two peers' states differ at block N, so chaining detects nothing earlier on the live path.

**What it does buy is integrity where blocks are trusted rather than rebuilt** — and that case
exists, in two places that matter:

- **Replay below the fast anchor.** `ReplayActor` routes a brief below `fastBlockMark` to
  `FastConsensusActor` only: *"already accepted, so JointLedger never re-handles it."* Those
  blocks are read back from the store and believed. Nothing recomputes them.
- **A transplanted store.** Seeding a peer from another peer's database means adopting its whole
  block history on the strength of what is written there.

Without chaining, each block's signature attests only to that block. Splice a different block 5
into a store and the signatures on blocks 6 through 50 remain valid, because none of them
mentions block 5 — and if the splice sits below `fastBlockMark`, nothing ever re-derives it. With
chaining, block 5's substitution changes every subsequent hash, so the tip no longer matches the
signatures over it, and one comparison at the tip detects it.

That is the whole argument: the signature stops being evidence about one block and becomes
evidence about a history. It costs 32 bytes in the preimage, one field on the brief, and one
more field in `JointLedger.State`, which already carries `previousBlockHeader` for exactly this
kind of chaining and would carry `previousBlockHash` beside it.

## Where `blockHash` lives

**A `BlockBrief` field** — not a `BlockHeader` one, which cannot work.

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
  against the recomputed one. Below `fastBlockMark` nothing re-derives, which is what the chained
  `previousBlockHash` above is there to cover.

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

**A running head cannot be upgraded across this change.** Two things move at once: the signed
bytes, so acks from a peer on the old preimage fail to verify on the new one and the reverse;
and the `Block` journal value and wire brief, which gain the `blockHash` field. It applies to
heads initialized afterwards, and belongs in the release notes of the release that ships it.

Chaining makes that boundary sharper rather than softer. A head's block hashes root in its
`headParamsHash`, so a store written before the change carries no chain at all and one written
after cannot be continued by a build that computes the preimage differently. There is no partial
adoption: the first block a peer hashes fixes the chain every later block extends.

## Out of scope

- **The HTTP surface.** `requestHash` reaches the submitter through the existing synchronous
  reply, which is what makes the assignment-time choice above worth anything. Everything beyond
  that — whether `GET /head/requests/{id}` returns the hash, whether the hash becomes a lookup
  key in its own right, and the route and reverse index that would need — is a separate PR
  against the API.
- **Stack hashes.** The slow side needs the same commitment, following the same construction —
  chaining included, where a stack's predecessor is the stack it follows. Separate work item.
- **The two stale rule-based signposts** named above. Both live in `cardano-onchain` and neither
  blocks this work.

## Open questions

1. **Memoize `blockHash` on the brief?** As a stored `BlockBrief` field the value is present
   without computation, but every rebuild recomputes it to compare. Whether that recomputed
   value is worth caching — a `lazy val` on `BlockBrief.Section`, once per brief rather than
   once per comparison — is a profiling question, not a design one.
