# Hydrozoa Deployment Guide — Multi-Peer Head (Head + Coil Peers)

> ## ⚠️ Never use this deployment in production
>
> What this guide sets up is a **demo**. It cuts corners that are outright disqualifying for
> anything holding real value (a later revision will split out a hardened default bootstrap; the
> restrictions below are the gap it has to close):
>
> - **Single-operator key custody.** `keygen-fleet` generates *every* peer's signing keys in one
>   pass on one machine — there is no multi-party negotiation, and whoever runs it holds the whole
>   head. Key generation itself is insecure (keys are written unencrypted to disk).
> - **Multi-peer head on a single host.** One `docker-compose.yml` runs all peers; there are no
>   separate per-peer configs or hosts yet, so the "multi-party" head has a single point of
>   failure and a single administrator.
> - **Only the built-in `cardano-eutxo` L2 ledger** (and `l2Ledger` / `identityIsomorphism` are
>   pinned in code, not operator-configurable).
> - **Only head peer 0 funds the head** — seed, funding utxos, and (by default) all equity come
>   from head peer 0's address.
> - **Plaintext secrets, default credentials, no TLS.** Private configs carry signing keys and the
>   Blockfrost key in clear JSON; the template ships `admin`/`welcome` admin credentials; the HTTP
>   API and the WS mesh are unencrypted.
> - **Ephemeral state, single-use head config.** No volumes are mounted — any restart means
>   re-initializing a fresh head on L1 (§5b) — and `head-config.json` embeds real utxos +
>   wall-clock anchors, so it cannot be reused.
> - **Preview testnet via Blockfrost only** — a trusted third party between every node and L1.

Deploys a multi-party Hydrozoa head running the **built-in EUTXO L2 ledger** (`l2Ledger =
cardano-eutxo`): the ledger runs in-process inside every node, so a node is a single container —
no ledger sidecar, no external database, no separate API service or UI. (The `any-remote` backend
that drives an external ledger over `remoteLedgerUri` is a separate deployment shape, not covered
here.) Sources are cited as `path:line` in the hydrozoa repo.

---

## 1. System overview

A running head is N **head peers** and M **coil peers** (optional). Each peer is one `hydrozoa`
process talking to Cardano L1 (Preview by default) via Blockfrost:

```
                       Cardano L1 (Preview) via Blockfrost
                     ▲                 ▲                  ▲
                     │                 │                  │
             ┌───────┴──────┐   ┌──────┴───────┐   ┌──────┴───────┐
   /head ◄──►│ hydrozoa     │◄─►│ hydrozoa     │   │ hydrozoa     │
   (WS mesh, │ head node 0  │   │ head node 1  │   │ coil node 0  │
   full mesh)│ (hub)        │   │              │   │              │
             └──────┬───────┘   └──────────────┘   └──────┬───────┘
                    │                                     │
                    └────────── /hub (WS star: ───────────┘
                                coil dials its hub)
```

- **hydrozoa** (Scala 3 / sbt) — the L2 head protocol node. One binary, `hydrozoa.app.Main`, runs
  either a head or a coil node depending on the identity in its private config
  (`src/main/scala/hydrozoa/app/Main.scala:136-168`). The built-in EUTXO L2 ledger is instantiated
  in-process (`Main.scala:191-199`) — no external ledger process.
- **Roles.** Head peers: full consensus participants (lead blocks/stacks, sign soft- and
  hard-acks, serve the user HTTP API). Coil peers: constant followers that sign hard-acks only;
  they run no HTTP server and no WS server — each dials its hub head peer's `/hub` route
  (`design/coil-network.md` §2, §4.3). The L1 multisig is
  `AllOf(headVKeys) ∧ AtLeast(coilQuorum, coilVKeys)`.
- **State.** Each node keeps two RocksDB stores under its data dir: the consensus store
  (`peer-<label>/rocksdb`, `Main.scala:118-119`) and the EUTXO ledger store
  (`peer-<label>/l2-rocksdb`, `Main.scala:195-196`). Default data dir `.hydrozoa-data` relative to
  cwd; give each node its own.
- **L2 query API** (head peers only, EUTXO only): `GET /api/l2/utxos/{address}` and
  `GET /api/l2/transactions` are mounted only when the node runs the EUTXO ledger — it is the sole
  `EutxoL2LedgerReader`, and a remote-ledger node is handed `None` and mounts neither
  (`Main.scala:181-186,417-419`; `HydrozoaRoutes.scala:259-261`).

### Network matrix (per node)

| Port | Service | Who connects | Configured in |
|---|---|---|---|
| 4001 (example) | hydrozoa mesh WS server: `/head` (mesh), `/hub` (hub→coil) | other head peers; hubbed coil peers | `webSocketAddress` in the shared head config — **bind address == dialed address** (`Main.scala:236-258`) |
| `httpPort` (8080 in docker) | hydrozoa user/admin HTTP API (**head peers only**) | users, admins | `peer-private.json` (`httpHost`/`httpPort`) |
| — | Blockfrost (outbound HTTPS) | every hydrozoa node | `blockfrostApiKey` in `peer-private.json` |

Head-mesh dialing convention: lower-numbered peer dials higher (`design/coil-network.md` §4.3).
Coil peers dial out only; they need no inbound port at all.

---

## 2. Current multi-peer support status (read first)

| Capability | Status |
|---|---|
| Multiple **head peers** via the CLI config path | **Supported.** `BuildHeadConfig` accepts N head peers; `Main` runs any of them. Proven in-process/WS by the integration harness, not by a committed multi-host script. |
| **Coil peers** in the head config / multisig script | **Supported.** The bootstrap config takes `coilPeers` + `coilQuorum`; the threshold script and hub/coil transports are implemented. |
| Starting a **coil node from JSON configs** | **Supported.** `ownPeerPrivate` dispatches on `ownHeadWallet` / `ownCoilWallet`; `Main` starts a coil node from the standard two files. Keygen writes the coil shape (`--role coil`). |
| Multi-node orchestration | **Supported for the local docker demo** (`docker-compose.yml`, §6): 2 head + 4 coil single-container nodes on one mesh network. No multi-host automation yet. |
| Coil-peer **persistence** | **Implemented.** |
| Coil-peer **crash recovery** | **Implemented, not yet tested.** |

---

## 3. Building

Toolchain: Nix flake devshell (JDK 25, sbt, just — `flake.nix`); Scala 3.3.7. JDK 21+ if not using
Nix.

```bash
nix develop            # or direnv (.envrc = use flake .)
sbt compile            # sbtn for the resident client
just test              # unit tests
just integration-fast  # multi-peer integration subset
```

Docker image (used by the composition in §6):

```bash
sbt docker:publishLocal
# -> cardano-hydrozoa/hydrozoa:0.1.0-SNAPSHOT
#    base eclipse-temurin:21-jre-jammy, EXPOSE 8080 (build.sbt; sbt-native-packager)
```

There is no publish/deploy CI — GitHub Actions runs checks only.

---

## 4. Configuration model

### 4.1 Two JSON files per node, no environment variables

`Main` reads **no env vars** (`Main.scala:40-43`); everything comes from two files:

**(a) Shared `head-config.json`** — identical on every node, head and coil alike (coil peers reuse
`HeadConfig` wholesale). Produced once by `BuildHeadConfig` from the bootstrap directory (§5). It embeds
the peer topology, the L1 network, `scriptReferenceUtxos`, the peer-agreed `HeadParameters`
(including `l2Ledger` and `identityIsomorphism`, hashed into the treasury datum), the head id, and
the **pre-built initialization + fallback transactions** with their wall-clock timing anchors — so
it is single-use per head (§5b).

**(b) Per-node `peer-private.json`** — `NodePrivateConfig`
(`src/main/scala/hydrozoa/config/node/NodePrivateConfig.scala`): `ownPeerPrivate` (identity +
Ed25519 signing wallet — `ownHeadWallet` for a head peer / `ownCoilWallet` for a coil peer, matched
against the head config's vkeys), `nodeOperationEvacuationConfig` (incl. a separate
`ruleBasedWallet` keypair), `nodeOperationMultisigConfig` (rate limits, Cardano polling period),
`blockfrostApiKey`, `adminUsername`, `adminPassword`, `httpHost`, `httpPort`. `remoteLedgerUri` is
**optional and unused for the EUTXO ledger** — it is read only on the `any-remote` path
(`NodePrivateConfig.scala:42-44`; `Main.scala:200-206`), so an EUTXO node may omit it.

Producer: **`just keygen-fleet`** (§5) fills the committed `peer-private.template.json` with
freshly generated keys for every peer.

### 4.2 Ledger + network facts

- **L2 ledger.** `HeadParameters.l2Ledger` is `cardano-eutxo` | `any-remote`
  (`config/head/parameters/L2LedgerKind.scala`). The bootstrap tooling currently **pins it to
  `cardano-eutxo`** in `mkSharedHeadConfig` (`TODO: surface via a --l2-ledger flag`).
- **Isomorphism.** `HeadParameters.identityIsomorphism` defaults to `false` — *format* isomorphism:
  the EUTXO ledger enforces the `headId` metadata pin on every L2 tx (pin check
  `eutxol2/HeadIdPin.scala`). `true` (*identity* isomorphism) runs the exact L1 tx unchanged and
  drops the pin; the tooling pins it `false`.
- **Head parameters + equity.** `bootstrap/defaults.json` carries the full `headParams` (`txTiming`,
  `fallbackContingency`, `disputeResolutionConfig`, `settlementConfig`, `coilQuorum`) and the
  per-peer `initialEquityContributions` (indexed by head peer number). `InitBootstrapFiles` seeds
  demo defaults; operators edit them. Equity is therefore read from the config — `build-head-config`
  no longer takes an `--equity` argument.
- **Network.** Set in `bootstrap/defaults.json` (`cardanoNetwork`, default `preview`) and folded
  into the head config by `BuildHeadConfig` — no longer hard-coded. `BuildHeadConfig` /
  `deploy-reference-scripts` build a Blockfrost backend for that network, so only the Blockfrost
  networks (mainnet / preprod / preview) are supported from the CLI.
- **Block-zero timing.** Optional. `bootstrap/defaults.json` omits `blockZeroStartTime` /
  `blockZeroEndTime` by default, so `BuildHeadConfig` anchors the initial block to wall-clock at
  build time (fresh, since it runs right before bring-up). Operators who need a pinned negotiation
  baseline add both as epoch-millis.
- **Opening L2 state.** The head's opening ledger state is `bootstrap/l2-cardano-eutxo.json` — a
  human-readable list of CIP-0116 outputs (address + value). `BuildHeadConfig` keys it into the
  initialization tx's evacuation map once the seed utxo is resolved (each output's input reference
  is the seed tx id + its index); the init tx commits to it on-chain (treasury value + datum KZG)
  and every node verifies that (`design/l2-isomorphism-design-note.md` §6.1).

---

## 5. Generating a head's configuration

All commands run inside `nix develop`. The walkthrough uses the docker topology: **2 head peers,
4 coil peers, coil quorum 2**. The pipeline turns operator-authored files into the two runtime
files each node needs:

```
   keygen-fleet (one sbt run)                      the bootstrap directory (operator-authored)
   ├─ bootstrap/roster.json ──────────┐  peer topology (head/coil vkeys, ws addresses, hubs)
   ├─ bootstrap/defaults.json ────────┤  network + head params + per-peer equity  (editable)
   ├─ bootstrap/l2-cardano-eutxo.json ┤  opening L2 outputs (one per head peer, editable)
   ├─ private/head-N/private.json     │  per-peer private configs (not part of the bootstrap dir)
   └─ private/coil-N/private.json     │
                                      │
   deploy-reference-scripts           │
   (head-0 wallet, Blockfrost)        │
   └─ bootstrap/script-refs.json ─────┤  the two on-chain reference-script UTxOs
                                      │
                                      │  + head-0 UTxOs + protocol params   (Blockfrost)
   build-head-config                  ▼
   BuildHeadConfig ───► head-config/head-config.json   assembles the four bootstrap files, adds
                                      │                headId + the pre-built initTx/fallback + timing
                                      ▼
   distribute head-config.json (shared) + each node's private.json  →  run the nodes (§6)
```

**Step 1 — Adjust the template.** `peer-private.template.json` is the per-peer private config every
generated peer starts from; keygen replaces `ownPeerPrivate` and `ruleBasedWallet` with fresh keys
and leaves the rest as-is. Before generating, set `blockfrostApiKey` (placeholder
`REPLACE_WITH_BLOCKFROST_API_KEY`) and, if not using the docker defaults, `httpHost` / `httpPort` /
admin creds. Same ports work for every peer in docker, so one template serves all. The template is
read at generation time — edit it after generating and you must re-run keygen (fresh keys, so
re-fund).

**Step 2 — Generate keys, roster, and defaults in one run:**

```bash
just keygen-fleet 2 4 2            # HEADS COILS QUORUM, → config/demo/
# or: just keygen-fleet 2 4 2 mydir           # custom output dir
```

One sbt invocation running keygen once per peer, then `InitBootstrapFiles`. Output layout:

```
config/demo/
├── bootstrap/                     # the operator-facing bootstrap directory (build-head-config input)
│   ├── roster.json                #   peer topology
│   ├── defaults.json              #   network + head params (coilQuorum, timing…) + per-peer equity
│   ├── l2-cardano-eutxo.json      #   opening L2 outputs, one 5-ADA output per head peer — edit to taste
│   └── script-refs.json           #   written later by deploy-reference-scripts (step 4)
├── head-config/
│   └── head-config.json           # written later by build-head-config (step 5)
└── private/
    ├── head-0/private.json        # ownHeadWallet identity
    ├── head-1/…
    └── coil-0/… coil-3/…          # ownCoilWallet identities, hubs assigned round-robin (0,1,0,1)
```

Peer numbering is positional in the roster: `private/head-N` ↔ head peer N; likewise coils.

**Step 2b — Adjust the defaults and opening state.** `bootstrap/defaults.json` carries demo defaults
for `cardanoNetwork` (`preview`), the full `headParams` (`coilQuorum` = a simple majority, plus
timing / fallback contingency / dispute / settlement), and the per-peer `initialEquityContributions`
(head peer 0 funds everything, the rest contribute zero); edit any of them. Add `blockZeroStartTime`
/ `blockZeroEndTime` (epoch millis) only to pin the block-zero window — omitted, it is wall-clock at
build. `bootstrap/l2-cardano-eutxo.json` is the opening L2 ledger — a list of `{ "address", "value" }`
outputs (value in CIP-0116). The template seeds one 5-ADA output per head peer; replace it with the
real opening distribution (or leave it empty `[]` for an empty head).

**Step 3 — Fund head peer 0** (the sole funder): print its address and send Preview tADA to it
(e.g. from the [Preview faucet](https://docs.cardano.org/cardano-testnets/tools/faucet); one
10k-tADA drip is plenty):

```bash
just head-zero-address       # derives the address from bootstrap/{roster,defaults}.json on demand
```

There are no address files lying around to grab a stale copy from — the target derives the address
from the roster every time. The funding must cover equity + the whole head's fallback contingency +
the opening L2 value + tx fee; step 5 logs the exact lovelace required and fails with the shortfall
if underfunded.

**Step 4 — Deploy the reference scripts** (once per network per script version):

```bash
export BLOCKFROST_API_KEY=preview...
just deploy-reference-scripts config/demo/private/head-0/private.json
# -> config/demo/bootstrap/script-refs.json
```

The rule-based regime (evacuation/dispute) txs resolve the treasury + dispute validators as
**reference scripts** from two L1 UTxOs at startup. This target deploys the currently compiled
scripts: two chained txs funded from head-0's wallet (change returns), each locking one script at
the unspendable burn address, then writes the reference inputs to `script-refs.json`. Because the
burn address can never be spent from, **one deployment serves every head and every restart** —
redeploy only when the compiled scripts change (symptom: `InvalidTreasuryScriptUtxo` /
`InvalidDisputeScriptUtxo` at step 5 or node start).

**Step 5 — Build the shared head config:**

```bash
export BLOCKFROST_API_KEY=preview...
just build-head-config       # reads config/demo/bootstrap/, writes config/demo/head-config/head-config.json
```

`BuildHeadConfig` assembles the bootstrap directory's four files (roster, defaults, opening L2
state, script refs) and talks to L1: it fetches head peer 0's UTxOs (to select funding inputs and
verify the balance) and the protocol parameters via Blockfrost, then pre-builds the initialization
+ fallback txs into the config. The Blockfrost key comes from `$BLOCKFROST_API_KEY` (the bootstrap
directory carries no credentials; the CLI also accepts `--blockfrost-key`). Equity and the head
parameters come from `defaults.json`, so there is no equity argument. Distribute the **same**
`head-config.json` to every node. It embeds real UTxO references **and wall-clock timing anchors**,
so build it **after** funding, **right before** bring-up, and treat it as single-use per head (§5b).

At this point every node has its two files, and the composition (§6) mounts
`head-config/head-config.json` + that node's `private/<peer>/private.json`.

**Step 6 — Bring up the head** (see §6):

```bash
sbt docker:publishLocal        # hydrozoa image (once, after code changes)
docker compose up -d
```

Stack 0 initializes once both head peers + any `coilQuorum` coil peers are signing.

**Step 7 — Verify.**
- Pods: `docker compose ps` — every `hydrozoa-*` Up (not Restarting).
- Head peers: `GET /health`, `GET /api/head-info` on `localhost:8080` / `:8081` (head-0/1); watch
  `docker compose logs -f hydrozoa-head-0` for the init tx hash and L1 confirmation on Preview
  (`preview.cexplorer.io`).
- L2 reads: `GET /api/l2/utxos/{address}`, `GET /api/l2/transactions` (EUTXO only).
- User traffic: `POST /api/l2/submit`, `POST /api/deposit/register`; admin:
  `POST /api/admin/finalize` (basic auth) (`docs/openapi.yaml`).
- If a `hydrozoa-*` service crash-loops: `docker compose logs hydrozoa-<peer>` — most likely a
  stale/reused head-config (rebuild step 5) or the Blockfrost placeholder still in the private
  configs (step 1 skipped).

**Teardown / recovery of funds:** to get the head's funds back on L1, **finalize before tearing
down** — the head address is a multisig (`AllOf(heads) ∧ AtLeast(coilQuorum, coils)`), so once the
peers are gone nothing can spend from it except the pre-signed fallback/evacuation path. Finalize on
any head peer (basic auth, the template's default credentials shown):

```bash
curl -u admin:welcome -X POST http://localhost:8080/api/admin/finalize
```

The finalization tx pays the L2 state and equity back out on L1 (watch `preview.cexplorer.io`).
Then `docker compose down`. Leftover change at head peer 0's own L1 address (not head-locked) can
be swept with its single key at any time.

---

## 5b. Restarting the head

The demo nodes keep no durable state across `docker compose down` (no volumes), so restarting means
**re-initializing a fresh head on L1** — and `head-config.json` is a one-off artifact that must be
rebuilt each time. It embeds:

- **head-0's actual funding UTxOs**, spent by the initialization tx — consumed the moment a head
  initializes; and
- **wall-clock timing anchors** (block-creation start/end, the fallback-tx start time), computed
  when `BuildHeadConfig` runs. These go stale even if the head never initialized (e.g. bring-up
  failed): start a head long after building the config and the pre-signed fallback deadline is
  already looming.

So the restart cycle is:

```bash
# 0. previous head initialized and holds funds? finalize FIRST (§5 teardown), or they stay
#    locked until the fallback/evacuation path matures.
docker compose down
# re-fund head peer 0 if the previous head consumed the funding — `just head-zero-address`
# prints the address; check it on preview.cexplorer.io
just build-head-config
docker compose up -d           # right after the build — the config is freshest now
```

**Reusable across restarts:** the whole `config/demo/bootstrap/` directory and every
`config/demo/private/` config — identities and keys are not time- or UTxO-bound, and the
reference-script UTxOs sit at an unspendable burn address. Only `head-config/head-config.json` is
single-use. Regenerate the fleet (`just keygen-fleet`) only for fresh identities; that changes head
peer 0's address, so re-fund it. The hydrozoa docker image only needs rebuilding after hydrozoa
code changes.

---

## 6. The docker composition (2 head + 4 coil)

`docker-compose.yml` — one `hydrozoa` container per node on a single user-defined bridge network,
`mesh`. Because the EUTXO ledger is in-process there are no sidecars: each service is a full
container with exactly one network interface, so hydrozoa's mesh WS server binds cleanly to its own
service name (`bind == dialed address`, `ws://head-N:4001`, `Main.scala:236-258`) and coil peers
dial their hub's `/hub` route over the mesh.

- **head-N** publish their hydrozoa HTTP API: head-0 → `8080`, head-1 → `8081`. **coil-N** publish
  nothing (they dial out only). All nodes reach Blockfrost outbound.
- Config mounts come from `${HYDROZOA_CONFIG:-./config/demo}`: the shared `head-config.json` plus
  `head-N/private.json` or `coil-N/private.json` per node. `${HYDROZOA_IMAGE}` overrides the
  hydrozoa image (default `cardano-hydrozoa/hydrozoa:0.1.0-SNAPSHOT`).

Caveats:
- **State is ephemeral.** No volumes are mounted, so both RocksDB stores are lost on `docker compose
  down` — consistent with the re-init-per-restart model (§5b). A durable deployment mounts a
  per-node volume at `/opt/docker/.hydrozoa-data`.
- **The head initializes only when all head peers + at least `coilQuorum` coil peers are up.** Start
  order doesn't matter (dialers retry); stack 0 hard-confirms with all head signatures +
  `coilQuorum` coil signatures (`design/coil-network.md` §5.7).
- **Rootless docker (NixOS):** the mesh network pins `com.docker.network.driver.mtu: 1400` and each
  node sets an upstream `dns:` — with slirp4netns's default MTU / resolver, outbound TLS to
  Blockfrost can fail. If containers have no outbound connectivity at all, restart the daemon
  (`systemctl --user restart docker`) — stale rootlesskit state produces exactly that.

