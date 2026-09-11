# Versioning

For whoever implements the signed `Hello` (GUM-322), and for anyone changing a wire codec, a
consensus rule or a store layout. This document defines the three versions a build carries —
software, protocol and store — what each one covers, where it is checked, and when it moves.

## Scope

The signed `Hello` needs items 1–3. The rest ride along because they touch the same values.

1. **`ProtocolVersion.current`**: one integer. Version `1` is the protocol the public testnet head
   initializes with (open question 2).
2. **The version travels first.** Both sides put it in the first frame they send, at a position
   every version can decode — see *Carrying it in `Hello`*.
3. **Equality on both lanes.** A mismatch closes the link with a reason naming both versions. The
   node keeps running.
4. **Reporting.** `hydrozoa version`, `GET /version` and the boot log report the protocol version
   and the store version beside the software version.
5. **A store version or identity mismatch is a `StartupRefusal`** (exit 2).
6. **Clean-up** of the contradictory text around `StoreVersion`, listed in *Present state*.
7. **Golden fixtures** for every frame, payload codec, signing preimage and digest layout the
   protocol version covers, so a wire change cannot land without showing up as one.

Deferred:

- **The L2 store's version.** GUM-324 item 3 owns it; *Store version* gives the shape it follows.
- **Head migration.** This document states only the two properties versioning relies on; see
  *Out of scope*.
- **A store change on a live head** that does not ride a head migration — open question 4.

## Three versions

| | software | protocol | store |
|---|---|---|---|
| owned by | each implementation | the Gummiworm protocol, shared by every implementation | each implementation, one per store |
| answers | which build is this? | can this peer talk to that one? | can this binary read this directory? |
| value today | `0.1.14` (`build.sbt`, via `BuildInfo`) | none | consensus store `3`; L2 store none |
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
| signing preimages | `BlockHeader.Section.signingBytes` for soft acks; effect transaction bodies for hard acks |
| effect transaction construction | settlement, finalization, fallback, rollout and refund — each peer derives the body it signs, so peers that derive different bodies cannot combine signatures |
| digest layouts | `"gummiworm-head-params-v1"`, `"gummiworm-evacuation-map-v1"`, and `requestHash` / `blockHash` once `design/block-hash.md` lands |

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

[Where the number is written down for other implementations is GUM-323 open question 4. The
natural home is the whitepaper's *Wire format* section.]

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
   rest is scope item 7. Behaviour changes have no fixture and need judgment — open question 1.

The protobuf request form shows both. `Codecs.scala` already decodes either form ("Accept both
forms. The two are told apart by JSON shape, not by a tag or a version field") while
`userRequestWithIdCodec` still emits the JSON object. Flipping the encoder (GUM-315 C1) is a bump,
because a peer on a build before the tolerant decoder cannot read the new form. Flipped before the
testnet head initializes, it is part of version `1` and costs no migration; the tolerant branch
then goes.

## Carrying it in `Hello`

GUM-322 replaces `HeadFrame.Hello(peerNum: Int)` and `CoilFrame.Hello(coilNum: Int)` with a
signed, nonce-bound frame. The version rides in it under three rules.

1. **The version is the one field every version can read.** It sits at a fixed position in the
   first frame each side sends, independent of everything else in that frame [a top-level
   `protocolVersion` field, in whatever envelope GUM-322 settles]. A peer checks it **before**
   parsing the rest, because another version may lay out or sign the rest differently.
2. **Both sides send it.** GUM-322's server opens with a nonce frame. That frame carries the
   server's version, so the dialer learns of a mismatch as well and can name it. sugar-rush's
   user streaming API uses the same order: a `welcome` frame carrying `PROTOCOL_VERSION` before
   the client's `Hello` (`sugar-rush-ledger/api/src/ws.rs`).
3. **It is inside the signed bytes.** The whole `Hello` is signed, so this costs nothing.

On a mismatch:

| side | does |
|---|---|
| server | closes the socket with a reason naming both versions (GUM-322 fix step 5), and traces a distinct event [name — e.g. `ServerRejectedProtocolVersion(theirs, ours)`] |
| dialer | traces both versions and keeps redialing on its existing backoff |

The dialer waits instead of exiting. `StartupRefusal` draws the line: a node whose peers are not
ready "waits — indefinitely, visibly, and without exiting — because the world may yet become
ready". A peer on another version is fixed by its operator upgrading, so a mismatch is that case.

The rule is the same on both lanes. On the mesh a mismatch is a deployment error, since every head
peer of one head runs one version by construction. On hub↔coil, a coil on another version is
refused and told which version it needs; with coil quorum 1 on the testnet head, that costs the
head nothing. The in-process transports have no `Hello` and no check: both ends are one build.

## Store version

One integer per store, owned by the implementation.

| store | version | checked |
|---|---|---|
| consensus store | `StoreVersion.current = 3` on main (v0.1.14 ships `2`); key `store_version` in `Cf.Meta` | `RocksDbBackendStore.versionCheck` at every open, then `identityCheck` |
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
4. **The L2 store takes the same shape** (GUM-324).

The store version moves independently of the protocol version. `StoreVersion` 1→2 moved the
Request journal to the protobuf record, shipped in v0.1.9, while the wire kept the JSON object
form.

**Stores and head migration.** `StoreIdentity` stamps `headParamsHash`. If a migration changes
`headParamsHash` (GUM-323), no store of the old head opens under the new head's config, so every
peer of the new head starts from a fresh store. A store bump shipped in a migration therefore
needs no store migration.

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

Nothing on the wire carries a version. `HeadFrame.Hello` and `CoilFrame.Hello` carry a
self-asserted peer number, unsigned; a rejected `Hello` is traced and the socket stays open
(GUM-322).

Text that contradicts the code, fixed in this work item:

1. `StoreVersion.scala` says "Current on-disk schema version — **2**"; `current` is `3`.
2. The same scaladoc says bumps wait "until the layout stabilizes"; the file lists two bumps.
3. The same scaladoc cites "CR6 / §7 versioning note"; CR6 in
   `persistence-and-crash-recovery.md` is write atomicity.
4. `StoreVersion.Check` has no callers — `versionCheck` does not use it — while
   `head-params-hash.md` says it "already has the right three-way shape". Use it or delete it.
5. `persistence-and-crash-recovery.md` §7 says "the store version is **held at 1**".

## Out of scope

- **Head migration.** GUM-323 holds the sketch. Versioning relies on two properties of it: peers
  of the old and the new head never share a mesh, and the new head's peers start from fresh stores
  when `headParamsHash` changes.
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

## Open questions

1. **Where the bump line falls for behaviour changes.** Golden fixtures decide wire and preimage
   changes. A consensus fix that changes which blocks, briefs or effect bodies a peer produces has
   no fixture. Proposal: any behaviour change a peer on the earlier build would disagree with is a
   bump. Who signs off on "would not disagree"?
2. **Version `1` = what the testnet head initializes with.** That makes every wire change ready
   before initialization free — the protobuf flip (GUM-315 C1) and `blockHash` (#734) among them.
   Agreed?
3. **Coils in a migration** (GUM-323 open question 3). Equality on hub↔coil assumes the coils move
   to the new version with the head.
4. **A store change on a live head, outside a migration.** `StoreVersion`'s scaladoc answers with a
   rebuild; on a live head a cold store is unrecoverable (GUM-312). In-place migration code,
   re-join from snapshot, or always ride a head migration?
