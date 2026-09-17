# Versioning

For anyone changing a wire codec, a consensus rule or a store layout. This document defines the
three versions a build carries — software, protocol and store — what each one covers, where it is
checked, when it moves, and what deploying a move costs.

## Scope

GUM-322's signed handshake carries items 1–3, built in hydrozoa#744 and #749. The rest ride along
because they touch the same values.

1. **`ProtocolVersion.current`**: one integer, `1`.
2. **The version travels first.** It sits in the first frame the dialer sends, at a position every
   version can decode — see *Carrying it in the handshake*.
3. **Equality on both lanes.** A mismatch closes the link with a reason naming both versions. The
   node keeps running.
4. **Reporting.** `hydrozoa version`, `GET /version` and the boot log report the protocol version
   and the store version beside the software version.
5. **A store version or identity mismatch is a `StartupRefusal`** (exit 2).
6. **Clean-up** of the contradictory text around `StoreVersion`, listed in *Present state*.
7. **Golden fixtures** (GUM-348) for every frame, payload codec, signing preimage and digest
   layout the protocol version covers, so a wire change cannot land without showing up as one.

Deferred:

- **The L2 store's version.** GUM-324 item 3 owns it; *Store version* gives the shape it follows.
- **Head migration's mechanism.** *Head migration* states what versioning relies on and what rides
  one; the transition transaction itself is GUM-323's own sketch.

## Three versions

| | software | protocol | store |
|---|---|---|---|
| owned by | each implementation | the Gummiworm protocol, shared by every implementation | each implementation, one per store |
| answers | which build is this? | can this peer talk to that one? | can this binary read this directory? |
| value today | `0.1.14` (`build.sbt`, via `BuildInfo`) | `1` (`ProtocolVersion.current`) | consensus store `5`; L2 store none |
| compared | never | equality, at connect | equality, at open |
| on mismatch | — | the link is refused; the node waits | the node refuses to start |

They are separate because **Gummiworm may have more than one implementation, each with its own
software policy.** The protocol is the only thing implementations share, so its version belongs
to the protocol. A release number and a store layout belong to one implementation, which moves
them on its own schedule. A number owned by one implementation cannot tell another
implementation whether the two can talk.

Humans still quote one number: the software version. Each release states the protocol version and
store versions it carries (*Software version*).

## What the protocol version covers

**Everything two peers of one head must do identically that the head configuration does not
pin.** Two builds that report the same protocol version — of one implementation or of two — can be
peers of the same head.

| covered | where it lives today |
|---|---|
| frame envelopes | `HeadFrame`, `CoilFrame` |
| payload codecs | `transport/Codecs.scala`: `Mesh.Get` / `Mesh.New`, `Population.*`, `OwnHardAck.*`, `UserRequestWithId` |
| message semantics | `docs/spec/fast-consensus.md`, `slow-consensus.md`, `coil-network.md` |
| signing preimages | `blockHash.bytes` for soft acks (`PeerWallet.mkSoftAckSignature`); effect transaction bodies for hard acks; `HandshakeProof.preimage` for the handshake |
| effect transaction construction | settlement, finalization, fallback, rollout and refund — each peer derives the body it signs, so peers that derive different bodies cannot combine signatures |
| digest layouts | the six domain tags — `"gummiworm-block-v1"`, `"gummiworm-request-v1"`, `"gummiworm-head-params-v1"`, `"gummiworm-evacuation-map-v1"`, `"gummiworm-l2-params-cardano-eutxo-v1"`, `"gummiworm-l2-state-cardano-eutxo-v1"` |

What it does not cover, and what pins each instead:

| not covered | pinned by |
|---|---|
| head configuration: timings, contingencies, rosters, script reference utxos | `headParamsHash`, in the multisig regime datum, for the head's life |
| L2 ledger rules | `l2ParamsHash`, a field of `HeadParameters` and so of `headParamsHash`; the cardano-eutxo ledger derives it from `"gummiworm-l2-params-cardano-eutxo-v1"` |
| L2 payload encoding | the ledger; opaque to Gummiworm (whitepaper, *Wire format*) |
| store layouts | the store version |
| the black-box L2 ledger socket | its own contract — see *Out of scope* |
| the user-facing HTTP API | `apiVersion` in the OpenAPI document |

The `-v1` domain tags stay. They keep one digest from being read as another. A layout change moves
its tag and the protocol version together. For `headParamsHash` this is already the rule:
`head-params-hash.md` says a head keeps the tag it was initialized with for life, and *Datum
compatibility* says "a running head cannot be upgraded across such a change". Head migration is
the upgrade path for exactly that case.

`ProtocolVersion.current` is where the number is written down — a constant in the code, and
nothing else. It moves to the whitepaper's *Wire format* section when a second implementation
needs to read it.

## When it moves

**Bump the protocol version when a peer on an earlier build at the same version could fail to
decode what this build sends, or could act differently on the same input.**

A build speaks exactly one protocol version, so peers at different versions never share a mesh. A
bump is therefore deployed by head migration. A change that does not bump deploys in place: every
earlier build at the same version already interoperates with it, so peers restart onto it one at a
time.

Two consequences:

1. **Tolerance code does not survive a bump.** A decoder that accepts an old and a new form exists
   so that builds at one version can meet. Once the new form ships in a bump, no peer at the new
   version meets a peer emitting the old form, and the old branch is removed with the bump.
2. **Golden encodings make the wire half mechanical.** Once every frame, payload codec, signing
   preimage and digest layout has a golden fixture, a regenerated fixture is a bump. Today only
   the stored request record has one (`src/test/resources/golden/request-record/`); adding the
   rest is scope item 7 (GUM-348). Behaviour changes have no fixture and need judgment — open
   question 1.

The protobuf request form shows both. `Codecs.scala` already decodes either form ("Accept both
forms. The two are told apart by JSON shape, not by a tag or a version field") while
`userRequestWithIdCodec` still emits the JSON object. Flipping the encoder (GUM-315 C1) is a bump,
because a peer on a build before the tolerant decoder cannot read the new form. Flipped before the
testnet head initializes, it is part of version `1` and costs no migration; the tolerant branch
then goes.

## Carrying it in the handshake

Both liaison links open `Challenge(nonce)` → `Handshake` → `Refused` (GUM-322,
`docs/spec/coil-network.md` §4.3). The version rides in the `Handshake` under three rules.

1. **The version is the one field every version can read.** `HeadFrame.Handshake` and
   `CoilFrame.Handshake` carry a top-level `protocolVersion`, and `ProtocolVersion.check` runs on
   it before the rest of the frame is used, because another version may lay out or sign the rest
   differently. The field is `Option[Int]`, so a counterpart too old to announce one is refused
   with a legible reason rather than failing to decode.
2. **The dialer learns both versions too.** `Challenge` carries only the nonce, so the dialer
   learns of a mismatch from the refusal rather than from the opening frame:
   `Refused(ProtocolVersionMismatch(found, expected))`, rendered `protocol version <found>, this
   node speaks <expected>`.
3. **It is inside the signed bytes.** `HandshakeProof` signs a domain-tagged preimage of
   `link || claimant || protocolVersion || headParamsHash || nonce`, so the announced version
   cannot be edited in flight.

On a mismatch:

| side | does |
|---|---|
| server | sends `Refused`, traces `ServerRefusedHandshake`, and closes the socket with the same reason in the close frame |
| dialer | traces `DialerRefused` and keeps redialing on its existing backoff |

The dialer waits instead of exiting. `StartupRefusal` draws the line: a node whose peers are not
ready "waits — indefinitely, visibly, and without exiting — because the world may yet become
ready". A peer on another version is fixed by its operator upgrading, so a mismatch is that case.

The rule is the same on both lanes — `PeerTransport` on `/head`, `HubWsTransport` on `/hub`. On
the mesh a mismatch is a deployment error, since every head peer of one head runs one version by
construction. On hub↔coil it is the same by construction, because a migration moves the coils with
the head (*Head migration*). The in-process transports have no handshake and no check: both ends
are one build.

## Head migration

Upgrading a running mesh in place is an outage: head peers sign N-of-N, so one peer that cannot
talk halts the head, and restarting every peer at the same instant is not feasible. Head migration
moves the head instead — operators bring up a new set of peers on the new build beside the running
head, and a transition transaction moves the head to them.

Four properties versioning relies on:

1. **The `headId` is preserved.** A migrated head keeps its identity; the two generations are told
   apart by `headParamsHash` in the multisig regime utxo's datum, not by a new head id. The
   transfer-transaction fixture on `ilia/membership-change` mints a new head id instead — take its
   transaction shape as the model, not its identity handling.
2. **Coils migrate with the head.** A coil peer speaks the same wire protocol and keeps the same
   store layout as a head peer, so it moves on the same schedule. That is what makes the hub↔coil
   version check an equality like the mesh's.
3. **Old and new peers never share a mesh.** `headParamsHash` is in the signed handshake, so a
   peer of the old head is refused by the new head's mesh whatever version either one speaks, and
   `StoreIdentity` stamps it, so no store of the old head opens under the new head's config.
4. **The new peers start from an empty store and a non-empty L2 state.** The transition
   transaction carries the state, not the store — see *Store version*.

**What rides a migration: any change to a format — store, wire or protocol.** A change that leaves
all three alone deploys in place, peers restarting onto it one at a time.

That is deliberately broader than "every protocol bump". A store-only bump could in principle be
deployed by migrating each store where it lies, but that needs in-place migration code per bump,
and on a live head the cheap answer — rebuild the store — is not available, because a cold store
re-bootstraps stack 0 and never rejoins (GUM-312). Routing store-format changes through a
migration keeps one deployment path for all three versions.

## Store version

One integer per store, owned by the implementation.

| store | version | checked |
|---|---|---|
| consensus store | `StoreVersion.current = 5` on main (v0.1.14 ships `2`); key `store_version` in `Cf.Meta` | `RocksDbBackendStore.versionCheck` at every open, then `identityCheck` |
| L2 store (`RocksDbL2Store`) | none | none — GUM-324 item 3 |

Rules:

1. **Stamp, match or refuse.** A writable open of an unstamped store stamps `current`; a matching
   value opens; any other value refuses. A read-only open of an unstamped store refuses. This is
   `versionCheck` today.
2. **Bump on any change to the column-family set, the key layout or a value codec.** A
   regenerated fixture under `src/test/resources/golden/request-record/` is a store-format change
   (`RequestRecordCodecTest`).
3. **A mismatch is a `StartupRefusal`.** Nothing about the world changes what is on disk, so a
   restart re-derives the same verdict. Today `Serve` converts only `RocksDBException` into a
   refusal; a version or identity mismatch exits 1, and `Restart=on-failure` restarts it forever.
4. **A bump deploys by head migration**, like a protocol bump, and for the reason given there.
5. **The L2 store takes the same shape** (GUM-324).

The store version still moves independently of the protocol version — one release may move either,
both or neither. `StoreVersion` 1→2 moved the Request journal to the protobuf record, shipped in
v0.1.9, while the wire kept the JSON object form. What rule 4 settles is the deployment path, not
the numbering.

**Stores and head migration: empty store, non-empty state.** `StoreIdentity` stamps
`headParamsHash`. A migration changes `headParamsHash`, so no store of the old head opens under
the new head's config and every peer of the new head starts from a fresh store. That is the
intended shape, not a gap to close: a store bump shipped in a migration needs no in-place store
migration, and no peer ever reads a store written under another schema.

The new head's L2 state does not come from a store. It comes from the transition transaction, the
way any head's opening state comes from its initialization transaction: a transfer transaction is
a valid initialization transaction for the new head, so the old head's final L2 state arrives as
the new head's `initialL2State`, projected into `initialEvacuationMap`, committed on-chain by the
init tx's datum through `initialL2StateHash`, and checked at cold boot. The bootstrap author
supplies the values; the migrated head verifies them exactly as a fresh head does.

[Carrying the transient-token overlay (`EutxoL2Ledger.State.transientTokens`, branch
`fund14/transient-l2-tokens`) through the same field looks possible and is not settled.
`initialL2State` is `List[L2Output]` — main-compartment only — so the overlay would need its own
declaration alongside it.]

## Software version

- `inThisBuild(version := "0.1.14")` in `build.sbt`, bumped per `RELEASE.md` and tagged
  `v<version>`. `BuildInfo` also carries `gitCommit` and `buildTime`.
- Never compared.
- `hydrozoa version`, `GET /version` (`VersionResponse`) and the boot log in `Serve` gain the
  protocol version and the consensus store version [and the L2 store version, once GUM-324 lands].
  `VersionResponse` is in `docs/api/openapi.yaml`, so `OpenApiSchemaTest`'s golden regenerates.
- `RELEASE.md` gains a step: state the protocol and store versions the release carries and whether
  either moved. A moved protocol version means the release deploys by head migration.

## Present state

The protocol version is defined, signed and checked on both lanes in hydrozoa#744 and #749; on main
nothing on the wire carries a version. Scope items 4–7 are not built anywhere: `hydrozoa version`,
`GET /version` and the boot log report only `BuildInfo`; a store version or identity mismatch
raises `IllegalStateException` and exits 1; and only the stored request record has a golden
fixture.

Text that contradicts the code, fixed in this work item:

1. `StoreVersion.scala` says bumps wait "until the layout stabilizes"; the file lists four bumps
   and `current` is `5`.
2. The same scaladoc says "a format change just rebuilds the store". That holds in development and
   fails on a live head, where a cold store never rejoins (GUM-312) — see *Head migration*.
3. The same scaladoc cites "CR6 / §7 versioning note"; CR6 in
   `persistence-and-crash-recovery.md` is write atomicity.
4. `StoreVersion.Check` has no callers — `versionCheck` open-codes the same three-way decision —
   while `head-params-hash.md` says it "already has the right three-way shape". Use it or delete
   it.
5. `persistence-and-crash-recovery.md` §7 says "the store version is **held at 1**" and repeats
   the rebuild answer.

## Out of scope

- **The black-box L2 ledger socket.** `RemoteL2Ledger` and sugar-rush's `/ws` exchange no version
  and no handshake. It is a node-local link with its own contract
  (`docs/spec/l2-ledger-command-coordination.md`); L2 rule compatibility goes through
  `l2ParamsHash`: `JointLedger.checkL2Params` refuses a mismatch, and only traces
  `L2ParamsHashUnreported` when the ledger reports no hash.
- **The HTTP API's `apiVersion`.**

## Settled, and why

1. **Separate store and protocol versions; neither is the software version.** Implementations
   share the protocol and nothing else.
2. **One protocol version per build; no negotiation.** Head peers sign N-of-N, so upgrading a
   running mesh across a protocol change is an outage. Head migration moves the head to peers
   already on the new version, so no mesh ever mixes versions, and there is no window to prune and
   no mixed-version test matrix.
3. **Equality on both lanes.** A stale coil costs the head nothing and gets a legible refusal.
4. **A migration keeps the `headId` and moves the coils with the head.** One head keeps one
   identity across an upgrade, and `headParamsHash` tells the generations apart.
5. **Any store, wire or protocol format change rides a migration.** One deployment path for all
   three versions, and no per-bump in-place store migration code.
6. **The protocol version is a constant in the code.** It moves to the whitepaper when a second
   implementation needs to read it.
7. **A migrated head starts from an empty store and a non-empty L2 state.** The transition
   transaction carries the state, so a store-format bump rides a migration for free.

## Open questions

1. **Where the bump line falls for behaviour changes.** Golden fixtures decide wire and preimage
   changes. A consensus fix that changes which blocks, briefs or effect bodies a peer produces has
   no fixture. Proposal: any behaviour change a peer on the earlier build would disagree with is a
   bump. Who signs off on "would not disagree"?
2. **Version `1` = what the testnet head initializes with.** That makes every wire change ready
   before initialization free — the protobuf flip (GUM-315 C1) among them. Agreed?
