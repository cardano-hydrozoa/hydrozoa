# Liaison backpressure

For whoever works on the liaison lanes next. Its single job: for **every** lane on
**every** link, record what it carries, how wide a reply is, and what ceiling bounds it
— in one place.

## What a ceiling is

A `GetMsgBatch` carries a **cursor** per lane — the next number the puller expects. A
**ceiling** is the separate bound the puller sends saying how far past its own
*confirmed* state it will be served. `LaneOutbound.reply` takes it as a predicate
(`servable: T => Boolean`), so a lane can be bounded in a dimension other than its own
numbering.

A ceiling anchors to **confirmed** progress, never to a cursor. A cursor moves whenever
the puller consumes, so a cursor-relative bound refuses nothing — that is a batch-size
cap, not backpressure.

Truncation is `takeWhile`, never `filter`. A contiguous lane tolerates a short serve, not
a hole, because the remote's `LaneInbound.verify` requires consecutive numbers.

`maxPerReply` is load-bearing twice: it sets the reply width **and** the outbox size
(`capacity = peerLiaisonOutboxDepth * maxPerReply`). Raising a lane's reply width grows
its cache in step.

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

Five lanes need no ceiling: N-of-N confirmation gates production, so a leader cannot run
ahead — blocks and stacks are self-limiting and the ack lanes track them. Only user
requests have an unbounded producer, which is why lane 3 is the only one bounded.

Lane 6 carries the same re-sequenced coil acks as the hub->coil link's lane 6, so it has
the same non-monotonic `stackNum` — it simply is not ceilinged, so that never arises.

## Hub -> coil — `Population.Get` / `Population.New`

The hub serves the **full population** and can run arbitrarily far ahead of one coil
peer, so every lane is bounded. All `LaneOutbound`, all contiguous.

| # | lane | count | item | numbered by | `maxPerReply` | ceiling dimension | anchor + window | ordering agrees? |
|---|---|---|---|---|---|---|---|---|
| 1 | `blockLane` | 1 | `BlockBrief.Next` | `BlockNumber` | 1 | `BlockNumber` | soft-confirmed block **+ `backpressureCoefficient`** | yes |
| 2 | `stackLane` | 1 | `StackBrief` | `StackNumber` | 1 | `StackNumber` | hard-confirmed stack **+ 1** | yes |
| 3 | `requestLanes` | `nHeadPeers` | `UserRequestWithId` | `RequestNumber` | `peerLiaisonMaxRequestsPerBatch` | `RequestNumber` | per-author confirmed **+ `backpressureCoefficient * maxRequestsPerBlock`** | yes |
| 4 | `softAckLanes` | `nHeadPeers` | `SoftAck` | `SoftAckNumber` | 1 | `blockNum` — a `SoftAckNumber` *is* the block number | soft-confirmed block **+ `backpressureCoefficient`** | yes |
| 5 | `headHardAckLanes` | `nHeadPeers` | `HardAck` | `HardAckNumber` | 1 | **`ack.stackNum`** | hard-confirmed stack **+ 1** | yes — round-1 or sole precedes round-2, stacks close in order |
| 6 | `coilHardAckLanes` | `nHubs` | `HardAckWithId` | `HubHardAckNumber` | 1 | **`ack.stackNum`** | hard-confirmed stack **+ `coilHardAckStackWindow`** (20) | **no** — `CoilAckSequencer` stamps arrivals from many coils |

Lanes share anchors, so `Population.Get` carries four ceilings for six lane families:

| field | serves |
|---|---|
| `blockCeiling: BlockNumber` | lanes 1, 4 |
| `stackCeiling: StackNumber` | lanes 2, 5 |
| `requestCeilings: Map[HeadPeerNumber, RequestNumber]` | lane 3 |
| `coilHardAckCeiling: StackNumber` | lane 6 |

`blockCeiling` and a `softAckLanes` cursor are the same value in different types, so lane
4 converts rather than taking a field of its own.

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

### Why lane 6 cannot deadlock

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

`coilHardAckStackWindow` is margin over that proof, not a substitute for it. It is a
constant on `PeerLiaisonCoilToHub` rather than a config field: a correctness margin, not
an operating knob.

Refusing the item **at the coil's cursor** on a `coilHardAckLane` is that proof's
precondition exactly, so the hub traces it (`PeerLiaisonEvent.CoilHardAckHeadRefused`,
via `LaneOutbound.heldAt` — outbox-only, diagnostics-only). A few are normal: a coil
behind on *blocks* rather than acks stops advancing its hard-confirmed stack, so its ack
lanes reach the ceiling and park until block catch-up moves it, which is the ceiling
working. A lane that never resumes is not.

## Where the anchors come from

`PeerLiaisonCoilToHub` measures every ceiling from this peer's own confirmed progress,
held in three `Ref`s and merged by max.

| anchor | arrives as | from |
|---|---|---|
| soft-confirmed block | `SoftConfirmedHighWater.blockNum` | `FastConsensusActor`, on every soft-confirmation |
| per-author confirmed request high-water | `SoftConfirmedHighWater.highWater` | the same message |
| hard-confirmed stack | `HardConfirmedHighWater.stackNum` | `SlowConsensusActor`, on every hard-confirmation |

Both travel `coilUplink`, which `FastConsensusActor` and `SlowConsensusActor` each hold
as `Option` — `None` on a head peer, whose mesh lanes need no ceiling beyond lane 3's.

`SoftConfirmedHighWater` is sent on **every** soft-confirmation, empty request map
included: `blockNum` advances whether or not the block carried requests, and a coil peer
anchors two lanes on it. `HardConfirmedHighWater` carries the number alone rather than
the `Stack.HardConfirmed` it came from — a liaison bounds pulls, it does not inspect
stacks.

Cold values mean "nothing confirmed yet", which is the tightest correct ceiling on a
fresh boot.

## Out of scope

- **A floor on `coilHardAckLanes`.** A coil cannot compute which acks it may skip: that
  needs the `stackNum` of acks it has not received. A hub could advertise one, but the
  cursor is restored from `backend.lastKey(Cf.HubHardAck(hub))`, so a jump would need
  its own durable mark (the shape `StoreKey.CoilStampMark` already has) plus a jump path
  through `LaneInbound.verify`. Raising that lane's `maxPerReply` gets most of the
  catch-up win for none of that cost.
- **Ceilings on the mesh**, and **a ceiling on lane 7**. See above.

## Outstanding

**Lift `maxPerReply` above 1 on the brief and ack lanes.** At 1, a coil peer that has
fallen behind walks every ack one per round trip, and no ceiling touches that — catch-up
is round-trip-bound, not buffer-bound. Both `capacity` and the ceilings are already
written to rescale with it, so it is the reply width and its cache that move, not this
design.
