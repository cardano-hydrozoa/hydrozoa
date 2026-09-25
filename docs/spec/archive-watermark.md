# Archive watermark

For whoever attaches an archiver to a node, or writes the retention that deletes on its word. Its
single job: say how a node learns that an archiver exists, how far that archiver got, and what the
node does with the answer.

The node **accepts and holds** a watermark. Deleting on the strength of one is retention's job and
is not built — see *What is not here*.

## Two signals, two questions

An archiver reads a node's store as a RocksDB secondary and is never dialed, so the one thing a
node ever hears from it is a watermark. That makes the **absence** of a watermark ambiguous between
two situations that call for opposite behaviour: no archiver exists, so delete as soon as consensus
allows; or an archiver exists and is down, so delete nothing.

Nothing a node can measure separates those, so which one holds is declared rather than discovered.

| signal | answers | absence means |
|---|---|---|
| `archiver` in `NodePrivateConfig` | *should* there be one? | no archiver — delete on the local rule |
| the reported watermark + its arrival instant | how far has it got, is it alive? | attached but not reporting — **stall** |

Self-registration would not do. An archiver announcing itself at startup needs no config, but a
node restart forgets the announcement, and in the window before the archiver reconnects the node
concludes "no archiver" and deletes data the archive never copied — the unsafe reading, on every
restart. Registration can say an archiver is *alive*; only configuration can say one is *expected*.

## The declaration

`ArchiverConfig`, as `archiver: Option[ArchiverConfig]` on `NodePrivateConfig`, beside
`remoteLedgerUri: Option[String]`. It carries one field:

| field | default | meaning |
|---|---|---|
| `staleAfter` | 15 minutes | how long a watermark may go unrefreshed before the archiver counts as not running |

The decoder is lenient in one respect: an empty object means "an archiver is attached, with the
default staleness window", so declaring one does not require knowing the knob exists. The field is
additive with a `None` default, so a config written before it decodes unchanged.

**Deliberately outside `headParamsHash`.** Whether this peer runs an archiver bounds what it does
to its own disk, and no follower's validation depends on it — the test
[`head-params-hash.md`](head-params-hash.md) applies to `rateLimits` for the same reason. Covering
it would make attaching an archiver a head re-initialization, and would force every peer to agree
on one operator's deployment choice.

## The endpoint

```
POST /api/admin/archive/watermark
```

Request — the highest index durably in the archive, per column family:

```json
{ "watermarks": { "Block": 472021, "Request:0": 510941, "HardAck:1": 2949 } }
```

Response — the floor the node actually adopted:

```json
{ "effectiveFloor": { "Block": 471000, "Request:0": 510941 } }
```

camelCase, like the rest of hydrozoa's API. The mismatch is worth guarding because it fails
**silently**: a renamed field decodes as an empty floor rather than an error, so an archiver would
keep reporting and simply never learn what the node adopted.

**Per family, not one number.** The journals are independent streams advancing at independent
rates, so a scalar could only ever carry the minimum and would hold retention back to the slowest
lane.

**The response is not an echo.** It reports what the node will allow after taking the minimum with
what consensus still needs, so an archiver can tell whether it is the binding constraint or the
mesh is. Until retention exists there is nothing to take a minimum against and the effective floor
is what was accepted — the field carries the distinction from the start, because adding it later
would leave an archiver unable to tell "I am ahead of the head" from "I am holding it back".

**Mounted when `archiver.isDefined`**, on any node type — `archiveWatermarkEndpoints` is empty
otherwise, following the idiom that an absent capability removes its routes rather than mounting
one that fails at request time. Not gated on head-versus-coil: a coil peer with an archiver gets
the endpoint, a head peer without one does not.

### Auth

Copies `/api/admin/finalize` exactly: tapir `auth.basic[Option[UsernamePassword]]` against
`serverConfig.adminUsername` / `adminPassword`, with `WWWAuthenticateChallenge.basic` re-advertised
on a 401, and the optional-credentials idiom so the security logic runs and traces
`UnauthorizedAdmin` even when the header is absent.

That gives an archiver the same credentials as "finalize the head". Separating admin capabilities
is its own work item.

### Family names are resolved against this node's own set

`familiesByName` is built from `Cf.mkAll` over the head's own membership — the same call the store's
family set comes from — so a name resolves only if this node actually has that family. A report
naming a family this node does not have is refused with a 400 naming it, and a report mixing a
known family with an unknown one is refused **whole** rather than half-recorded: an archiver reading
a different node's store should not get a 200 and a watermark nobody will ever read.

## What the node holds

`ArchiveWatermarks`, in `multisig/persistence/`, following `PeerMetrics`: one `AtomicReference` over
an immutable `Map[Cf, Long]` plus the arrival `Instant`, built once per node and injected into
`HydrozoaRoutes`.

**Monotone, never lowered.** `record` keeps the higher index per family. A rebuilt or truncated
archive reporting below what was already seen would be meaningless to adopt — the node cannot
un-delete what it removed on the strength of the earlier report — so the earlier figure stands and
the regression is reported back as `Report.regressed`.

`record` returns a `Report` separating `advanced` from `regressed` rather than folding them
together, because the two mean opposite things to an operator: one is an archiver making progress,
the other an archive that has lost ground.

`reportedAt` is recorded even when nothing advanced. A report that repeats the previous watermarks
still proves the archiver is running, and liveness is the other half of what retention needs.

**Not persisted, deliberately.** A restart forgets every watermark and the node retains everything
until the archiver reports again — seconds, at the archiver's tail cadence. The failure direction
is "retain more", and persisting would cost either new `Cf.Meta` keys or a column family plus a
`StoreVersion` bump to buy nothing.

### Freshness

`isFresh(staleAfter, now)` is `false` before the first report as well as after a silence. Both mean
the same thing to retention — an archiver was declared and is not currently accounted for, so
nothing may be deleted on its behalf. Telling "not yet" from "not any more" is a matter for the
operator's alert, not for the decision.

## What an operator sees

| event | when |
|---|---|
| `ArchiveWatermarkRecorded(advanced, regressed)` | every accepted report, with the two counts |
| `ArchiveWatermarkRegressed(family, reported, held)` | once per family that reported below the held figure |
| `RequestRejected` | a report naming a column family this node does not have |
| `UnauthorizedAdmin` | missing or wrong admin credentials |

### The cost of failing safe

An operator who decommissions the archiver and leaves `archiver` set stalls deletion **forever**
and fills the disk, silently, because everything else looks healthy. That is the price of the safe
default, and it has to be loud: a stale watermark is an alert condition, not merely a stalled
floor. `ArchiveWatermarks.isFresh` exposes it; surfacing it is retention's open question.

## What is not here

- **Deleting anything.** Taking the minimum of the watermark and what consensus still needs, and
  trimming on the result, is store retention (GUM-345) and is not built. Until it is, the endpoint
  records and nothing acts on the record.
- **Checking rather than trusting the archive.** A digest over the archived range alongside the
  watermark would let the node verify instead of trust. Not built; the request shape is easier to
  widen before there is a client in the field than after.
