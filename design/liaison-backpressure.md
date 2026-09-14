# Liaison backpressure

For whoever implements GUM-310. Its single job: for **every** lane on **every** liaison
link, fix what it carries, how wide a reply is, and what ceiling bounds it — in one
place. Graduates into [`docs/spec/coil-network.md`](../docs/spec/coil-network.md) §5.5
once the hub->coil ceilings land; until then only the mesh `request` ceiling exists.

## What a ceiling is here

A `GetMsgBatch` carries a **cursor** per lane — the next number the puller expects. A
**ceiling** is the separate bound the puller sends saying how far past its own
*confirmed* state it will be served. `LaneOutbound.reply` takes it as a predicate
(`servable: T => Boolean`), so a lane can be bounded in a dimension other than its own
numbering.

Anchor a ceiling to **confirmed** progress, never to the cursor: `cursor + k` moves
whenever the puller consumes, so it can refuse nothing — that is a batch-size cap, not
backpressure.

Truncation is always `takeWhile`, never `filter`. A contiguous lane tolerates a short
serve, not a hole, because the remote's `LaneInbound.verify` requires consecutive
numbers.

`maxPerReply` is load-bearing twice over: it sets the reply width **and**, since
GUM-310's first commit, the outbox size (`capacity = peerLiaisonOutboxDepth *
maxPerReply`). Raising a lane's reply width grows its cache in step.

## Mesh (head <-> head) — `Mesh.Get` / `Mesh.New`

Six lanes, symmetric. Each side serves only its **own** production; `LaneBidirectional`,
so every lane has both an outbox and a cursor.

| # | lane | item | numbered by | shape | `maxPerReply` | ceiling |
|---|---|---|---|---|---|---|
| 1 | `blockLane` | `BlockBrief.Next` | `BlockNumber` | sparse (own leader schedule) | 1 | none |
| 2 | `stackLane` | `StackBrief` | `StackNumber` | sparse | 1 | none |
| 3 | `requestLane` | `UserRequestWithId` | `RequestNumber` | contiguous | `peerLiaisonMaxRequestsPerBatch` | **`requestCeiling`** — confirmed + `backpressureCoefficient * maxRequestsPerBlock` |
| 4 | `softAckLane` | `SoftAck` | `SoftAckNumber` | contiguous | 1 | none |
| 5 | `hardAckLane` | `HardAck` | `HardAckNumber` | contiguous | 1 | none |
| 6 | `hubHardAckLane` | `HardAckWithId` | `HubHardAckNumber` | contiguous | 1 | none |

**Unchanged by GUM-310.** N-of-N confirmation gates production, so a leader cannot run
ahead: blocks and stacks are self-limiting and the ack lanes track them. Only user
requests have an unbounded producer, which is why lane 3 is the only one ceilinged.

Lane 6 carries the same re-sequenced coil acks as the hub->coil link's lane 6, so it has
the same non-monotonic `stackNum` — it simply is not ceilinged, so that never arises.

## Hub -> coil — `Population.Get` / `Population.New`

The hub serves the **full population**. All `LaneOutbound`, all contiguous.

| # | lane | count | item | numbered by | `maxPerReply` | ceiling dimension | anchor + window | ordering agrees? |
|---|---|---|---|---|---|---|---|---|
| 1 | `blockLane` | 1 | `BlockBrief.Next` | `BlockNumber` | 1 | `BlockNumber` | soft-confirmed block **+ `backpressureCoefficient`** | yes |
| 2 | `stackLane` | 1 | `StackBrief` | `StackNumber` | 1 | `StackNumber` | hard-confirmed stack **+ 1** | yes |
| 3 | `requestLanes` | `nHeadPeers` | `UserRequestWithId` | `RequestNumber` | `peerLiaisonMaxRequestsPerBatch` | `RequestNumber` | per-author confirmed **+ `backpressureCoefficient * maxRequestsPerBlock`** | yes |
| 4 | `softAckLanes` | `nHeadPeers` | `SoftAck` | `SoftAckNumber` | 1 | same — `SoftAckNumber` *is* the block number | soft-confirmed block **+ `backpressureCoefficient`** | yes |
| 5 | `headHardAckLanes` | `nHeadPeers` | `HardAck` | `HardAckNumber` | 1 | **`ack.stackNum`** | hard-confirmed stack **+ 1** | yes — round-1 or sole precedes round-2, stacks close in order |
| 6 | `coilHardAckLanes` | `nHubs` | `HardAckWithId` | `HubHardAckNumber` | 1 | **`ack.stackNum`** | hard-confirmed stack **+ 20** | **no** — `CoilAckSequencer` stamps arrivals from many coils |

Lanes share anchors, so `Population.Get` carries four values, not six:

| field | serves |
|---|---|
| `blockCeiling: BlockNumber` | lanes 1, 4 |
| `stackCeiling: StackNumber` | lanes 2, 5 |
| `coilHardAckCeiling: StackNumber` | lane 6 |
| `requestCeilings: Map[HeadPeerNumber, RequestNumber]` | lane 3 |

`blockCeiling` and a `softAckLanes` cursor are the same value in different types, so
lane 4 converts rather than taking a field of its own.

## Coil -> hub — `OwnHardAck.Get` / `OwnHardAck.New`

| # | lane | item | numbered by | shape | `maxPerReply` | ceiling |
|---|---|---|---|---|---|---|
| 7 | `ownHardAckLane` | `HardAck` | `HardAckNumber` | contiguous | 1 | none |

The hub pulls one coil's own acks, needs them for quorum, and is never the constrained
side — there is nothing to refuse.

## The rule: a window must exceed the lane's reordering bound

Three hub->coil ceilings read a dimension that is not the lane's own number (lane 4
coincidentally, lanes 5 and 6 genuinely). That is safe only where the lane's ordering
agrees with the ceiling's dimension — then the next item the puller needs is always the
one at its cursor, and any window >= 1 admits it.

`headHardAckLanes` agree. `coilHardAckLanes` do not: `CoilAckSequencer` stamps a
`HubHardAckNumber` on arrival across all of a hub's coil peers — the number is transport
ordering only — so a straggler's ack for an earlier stack lands among a later stack's.

### Why lane 6 still cannot deadlock

The apparent danger: a coil at hard-confirmed `S` with window `w` refuses an ack for
stack `S + w + 1` sitting at its cursor. If the acks it still needs were behind that
one it could never reach `coilQuorum` (`SlowConsensusActor`:
`allHeadPeers.subsetOf(present) && coilPeerCount(present) >= coilQuorum`), so its
hard-confirmed stack would never advance and the ceiling would never rise.

It cannot happen, because:

> **Before an ack for stack `N+1` exists anywhere, a full quorum for stack `N` has
> already been stamped.**

A coil emits an ack for `N+1` only once its own `StackComposer.tryProgress` passes
`previousStackHardConfirmed` — it has locally hard-confirmed `N`, so it observed
all-head plus `coilQuorum` acks for `N`, and those were stamped on their hubs' lanes
first.

So a coil at `S` blocks only on an ack above `S + w`, and by the invariant the quorums
for `S + 1` ... `S + w` were stamped below that point and are consumed first. It
advances to at least `S + w` before the ceiling can bind. **This holds for any
`w >= 1`**, independently of how far the head has run ahead, and it never depends on a
straggler: the acks needed are the ones that already formed the head's quorum. It also
survives the quorum being spread across hubs — each hub's lane carries its share below
the blocking point, and the coil needs quorum in aggregate, not per lane.

**+20 is margin over that proof, not a substitute for it.**

One stall does remain, and it is the ceiling working as intended: a coil behind on
*blocks* rather than acks stops advancing its hard-confirmed stack, so the ack lanes
reach the ceiling and park until block catch-up moves it.

When the hub refuses the item **at the coil's cursor** on a `coilHardAckLane`, that is
the deadlock precondition exactly. Trace it, so a wrong reordering bound surfaces as a
diagnosable stall rather than a silent permanent one.

## Anchors the coil liaison needs

The ceilings are computed by `PeerLiaisonCoilToHub`, which today holds none of the three
inputs.

| anchor | source | reaches the liaison? |
|---|---|---|
| per-author confirmed request high-water | `FastConsensusActor`, as `SoftConfirmedHighWater` | computed on a coil and **dropped** — `requestSequencer` and `headPeerLiaisons` are both empty there |
| soft-confirmed block | `FastConsensusActor` | no — its `Connections` has no path to the coil liaison |
| hard-confirmed stack | `SlowConsensusActor` | not sent, but it already holds `coilUplink` |

So `FastConsensusActor.Connections` needs a `coilUplink` — `None` on a head peer,
`Some(hubLiaison)` on a coil — mirroring `SlowConsensusActor`, which already has one.

## Not doing

- **A floor on `coilHardAckLanes`.** A coil cannot compute which acks it may skip: that
  needs the `stackNum` of acks it has not received. A hub could advertise one, but the
  cursor is restored from `backend.lastKey(Cf.HubHardAck(hub))`, so a jump would need
  its own durable mark (the shape `StoreKey.CoilStampMark` already has) plus a jump path
  through `LaneInbound.verify`. Raising that lane's `maxPerReply` gets most of the
  catch-up win for none of that cost.
- **Ceilings on the mesh.** See above.
- **A ceiling on lane 7.** See above.
