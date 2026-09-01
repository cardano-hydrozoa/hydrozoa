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
| `blockHash` | the header fields and the ordered body | the block leader, and every follower re-deriving | at block cut |

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

## `UserRequestBody.hash` already exists

`UserRequest.scala` carries it: `blake2b_256`, with deposits hashed as
`blake2b_256(l1Payload) ++ blake2b_256(l2Payload)` before the outer hash, its comment explaining
this keeps the hash injective rather than collapsing `hash(abc + def) == hash(ab + cdef)`.
`UserRequestTest` pins two vectors. Nothing in production calls it.

Two things to settle before it becomes load-bearing:

- **No variant tag.** `TransactionRequestBody(l2Payload)` hashes `l2Payload` directly, while
  `DepositRequestBody` hashes a 64-byte concatenation of two digests. A transaction request
  whose `l2Payload` happens to be exactly that 64-byte string hashes identically to the deposit.
  Domain-separate the variants, the way `HeadParamsHash` domain-tags its preimage.
- **The `RequestId` is not in the preimage.** See open question 1.

Both changes move the pinned vectors in `UserRequestTest`, which is free now and is not free
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

- **The block type leads.** `BlockBody.Initial` has no fields, `Minor` and `Final` have no
  `depositsAbsorbed`, and `Major` has all three lists. Tagging the type first keeps the four
  shapes from colliding, and keeps the absent lists out of the preimage rather than encoding
  them as empty.
- **Order is the list's own order**, not sorted. The ordered request list is what the leader
  chose and what every follower must reproduce; sorting would hide a reordering, which is a real
  disagreement about block content.
- **`RequestNumber` is `u64`.** The `Request` journal key is 8 bytes (`Markers` decodes it with
  `getLong`), unlike the 4-byte soft/hard-ack indices.
- **The optional wakeup is flag-then-value**, so `None` and a present value can never produce
  the same bytes.

## Where it lives and what compares it

**The soft-ack signs the block hash, with `blockNum` and the versions beside it.**

```scala
SignedDigest(blockNum, versionMajor, versionMinor, blockHash)
```

Only `startTime` goes. It is inside the `blockHash` preimage and nothing reads it out of the
signed bytes.

The other three stay, each duplicated in the preimage on purpose:

- `versionMajor` and `versionMinor`, because a ratchet must read them without recomputing a
  hash — major for equality, minor for the strict increase. A digest gives an ordering on
  nothing; it can only say two things differ.
- `blockNum`, because it names what the signature is about. A signed blob that says only "some
  block, version 3.7" is worse to hold, log and diagnose than one that names the block, and the
  cost is four bytes in an off-chain message.

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

**A running head cannot be upgraded across this change.** The signed bytes move, so acks from a
peer on the old preimage fail to verify on the new one and the reverse. It applies to heads
initialized afterwards, and belongs in the release notes of the release that ships it.

**`JointLedger` compares hashes.** `panicOnMismatchWithExpectedBrief` compares one 32-byte value
instead of two case-class trees. `briefMismatchSummary` stays: once the hashes differ, the
field-level diff is what tells an operator *which* part flipped, and it is the only thing that
can — a hash says they disagree, never how.

## Open questions

1. **Does `requestHash` cover the `RequestId`?** Excluding it means the same L2 transaction
   submitted twice hashes identically, which makes the hash a content identifier a user can
   compute independently before submitting. Including it makes every assignment distinct and ties
   the hash to one position in the log. The block body carries the id next to the hash either
   way, so the block's commitment is unambiguous under both. I lean **excluding** it — a content
   hash the submitter can compute themselves is worth more than a uniqueness it does not need.
2. **Is `blockHash` a stored field or a derived one?** Making the soft-ack preimage *be* the
   hash mostly settles this: a verifier cannot take a claimed hash on trust, so it must recompute
   from header and body to check any signature at all. A stored field would then be a claim every
   peer recomputes anyway. Derived is the default, matching `EvacuationMap.digest`;
   `headParamsHash` is stored only because it must reach a datum, and this one reaches no
   transaction. What is left open is whether to memoize it on the brief — a `lazy val` on
   `BlockBrief.Section`, computed once per brief rather than once per comparison.
3. **What does the API return, and does the existing surface move?**
   `GET /head/requests/{id}` resolves by id today. Returning `requestHash` from the submit path
   is additive, but if the hash is also to be a lookup key that is a new route and a new index.
4. **Does the initial block get a hash?** `BlockBrief.Initial` has an empty body and its header
   is pinned by `headParamsHash` through the initialization transaction. Hashing it costs
   nothing and keeps the four block types uniform; skipping it means one fewer field to define
   for a block that is already pinned twice over.
5. **Does this change what a coil peer verifies?** A coil peer authors no soft-ack, so it never
   signs a `blockHash`. Whether it should *check* one — it re-derives block bodies, so it can —
   decides whether this closes the head↔coil gap as well as the head↔head one.
