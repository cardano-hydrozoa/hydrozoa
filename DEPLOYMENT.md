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
> - **Ephemeral state, single-use head config.** No data volumes are mounted (config comes in via
>   read-only bind mounts), so any restart means re-initializing a fresh head on L1 (§4) — and
>   `head-config.json` embeds real utxos + wall-clock anchors, so it cannot be reused.
> - **Preview testnet via Blockfrost only** — a trusted third party between every node and L1.

Deploys a multi-party Hydrozoa head running the **built-in EUTXO L2 ledger** (`l2Ledger =
cardano-eutxo`): the ledger runs in-process inside every node, so a node is a single container —
no ledger sidecar, no external database, no separate API service or UI. (The `any-remote` backend
that drives an external ledger over `remoteLedgerUri` is a separate deployment shape, not covered
here.)

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
             └──┬─────┬─────┘   └─────┬────────┘   └──────┬───────┘
                │     │               │                   │
                │     └───────────────┼─── /hub (WS star: ┘
                │                     │    coil dials its hub)
                └──────────┬──────────┘
                           │  user/admin HTTP API (head peers only; host :8080 / :8081)
                       ┌───┴──┐
                       │ user │
                       └──────┘
```

- **hydrozoa** (Scala 3 / sbt) — the L2 head protocol node. One binary runs either a head or a
  coil node, depending on the identity in its private config. The built-in EUTXO L2 ledger is
  instantiated in-process — no external ledger process.
- **Roles.** Head peers: full consensus participants (lead blocks/stacks, sign soft- and
  hard-acks, serve the user HTTP API). Coil peers: constant followers that sign hard-acks only;
  they run no HTTP server and no WS server — each dials its hub head peer's `/hub` route
  (`design/coil-network.md` §2, §4.3). The L1 multisig is
  `AllOf(headVKeys) ∧ AtLeast(coilQuorum, coilVKeys)`.
- **State.** Each node keeps two RocksDB stores under its data dir: the consensus store
  (`peer-<label>/rocksdb`) and the EUTXO ledger store
  (`peer-<label>/l2-rocksdb`). Default data dir `.hydrozoa-data` relative to
  cwd; give each node its own.
- **L2 query API** (head peers only, EUTXO only): `GET /api/l2/utxos/{address}` and
  `GET /api/l2/transactions` are served only when the node runs the EUTXO ledger; a remote-ledger
  node serves neither.

### Network matrix

| Port | Service | Who connects | Configured in |
|---|---|---|---|
| 8080 | head-0 user/admin HTTP API | users, admins | `peer-private.json` (`httpHost`/`httpPort`) — in-container `8080`, published to host `8080` |
| 8081 | head-1 user/admin HTTP API | users, admins | `peer-private.json` (`httpHost`/`httpPort`) — in-container `8080`, published to host `8081` |
| 4001 | hydrozoa mesh WS server: `/head` (mesh), `/hub` (hub→coil) | other head peers; hubbed coil peers | `webSocketAddress` in the shared head config — **bind address == dialed address** |

Head-mesh dialing convention: lower-numbered peer dials higher (`design/coil-network.md` §4.3).
Coil peers dial out only; they need no inbound port at all. Every node also needs outbound HTTPS
to Blockfrost (`blockfrostApiKey` in `peer-private.json`).

---

## 2. Building

Toolchain: Nix flake devshell (JDK 25, sbt, just — `flake.nix`); Scala 3.3.7. JDK 21+ if not using
Nix.

```bash
nix develop            # or direnv (.envrc = use flake .)
sbt compile            # sbtn for the resident client
just test              # unit tests
just integration-fast  # multi-peer integration subset
```

Docker image (used by the composition in §4):

```bash
sbt docker:publishLocal
# -> cardano-hydrozoa/hydrozoa:0.1.0-SNAPSHOT
#    base eclipse-temurin:21-jre-jammy, EXPOSE 8080
```

There is no publish/deploy CI — GitHub Actions runs checks only.

---

## 3. Configuration

Everything a node reads comes from two files:

**(a) Shared `head-config.json`** — identical on every node. It embeds
the peer topology, the L1 network, `scriptReferenceUtxos`, the peer-agreed head parameters
(including `l2Ledger` and `identityIsomorphism`), the head id, and
the **pre-built initialization transaction** with its wall-clock timing anchors — so it is
single-use per head (§4). Each node derives the fallback transaction from the initialization tx
when it reads the config.

**(b) Per-node `peer-private.json`** — `ownPeerPrivate` (identity +
Ed25519 signing wallet — `ownHeadWallet` for a head peer / `ownCoilWallet` for a coil peer, matched
against the head config's vkeys), `nodeOperationEvacuationConfig` (incl. a separate
`ruleBasedWallet` keypair), `nodeOperationMultisigConfig` (rate limits, Cardano polling period),
`blockfrostApiKey`, `adminUsername`, `adminPassword`, `httpHost`, `httpPort`. `remoteLedgerUri` is
**optional and unused for the EUTXO ledger** — it is read only on the `any-remote` path, so an
EUTXO node may omit it.

### Generating a head's configuration

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
   └─ bootstrap/script-refs.json ─────┤  the two on-chain reference-script UTxOs (optional —
                                      │  falls back to config/script-refs/<network>.json)
                                      │
                                      │  + head-0 UTxOs + protocol params   (Blockfrost)
                                      ▼
   build-head-config ───► head-config/head-config.json  assembles the four bootstrap files, adds
                                      │                 headId + the pre-built init tx + timing
                                      ▼
   distribute head-config.json (shared) + each node's private.json  →  run the nodes (§4)
```

**Step 1 — Set the Blockfrost key.** Copy `config/template/peer-private.template.json` to
`config/template/peer-private.template.json.local` (gitignored) and set `blockfrostApiKey` there.
keygen-fleet reads only the `.local` file and refuses to run without it, so the real key never
lands in a committed file. The template is read at generation time — regenerating means fresh
keys, so re-funding.

**Step 2 — Generate keys, roster, and defaults in one run:**

```bash
just keygen-fleet 2 4 2            # HEADS COILS QUORUM, → config/demo/
# or: just keygen-fleet 2 4 2 mydir           # custom output dir
```

One sbt invocation: keygen once per peer, then the bootstrap files. Output layout:

```
config/demo/
├── bootstrap/                     # the operator-facing bootstrap directory (build-head-config input)
│   ├── roster.json                #   peer topology
│   ├── defaults.json              #   network + head params (coilQuorum, timing…) + per-peer equity
│   ├── l2-cardano-eutxo.json      #   opening L2 outputs, one 5-ADA output per head peer — edit to taste
│   └── script-refs.json           #   written by deploy-reference-scripts (step 4; optional —
│                                  #   committed per-network defaults are the fallback)
├── head-config/
│   └── head-config.json           # written later by build-head-config (step 5)
└── private/
    ├── head-0/private.json        # ownHeadWallet identity
    ├── head-1/…
    └── coil-0/… coil-3/…          # ownCoilWallet identities, hubs assigned round-robin (0,1,0,1)
```

Peer numbering is positional in the roster: `private/head-N` ↔ head peer N; likewise coils.

**Step 2b — Set the head parameters.** `bootstrap/defaults.json` carries demo defaults; edit any
of them:

- `cardanoNetwork` (default `preview`) — `build-head-config` talks to Blockfrost for this
  network, so only the Blockfrost networks (mainnet / preprod / preview) are supported. Keep it
  consistent with the Blockfrost key from step 1 (`deploy-reference-scripts` derives its target
  network from that key).
- `headParams` — `coilQuorum` (the QUORUM argument passed to keygen-fleet; 2 here), timing,
  fallback contingency, dispute resolution, settlement.
- `initialEquityContributions` — per head peer; the demo default is head peer 0 funds
  everything, the rest contribute zero.

The initial block's timing is anchored to wall-clock automatically when the head config is built
— nothing to set here.

**Step 2c — Specify the initial utxo set.** `bootstrap/l2-cardano-eutxo.json` is the head's
opening L2 ledger — a list of `{ "address", "value" }` outputs (value in CIP-0116). keygen-fleet
seeds one 5-ADA output per head peer; replace it with the real opening distribution, or leave it
empty (`[]`) for an empty head.

**Step 3 — Fund head peer 0** (the sole funder): print its address and send Preview tADA to it
(e.g. from the [Preview faucet](https://docs.cardano.org/cardano-testnets/tools/faucet); one
10k-tADA drip is plenty):

```bash
just head-zero-address       # derives the address from bootstrap/{roster,defaults}.json on demand
```

The funding must cover equity + the whole head's fallback contingency + the opening L2 value +
tx fee; step 5 logs the exact lovelace required and fails with the shortfall if underfunded.

**Step 4 — Deploy the reference scripts** (once per network per script version; the target
network comes from the Blockfrost key):

```bash
export BLOCKFROST_API_KEY=preview...
just deploy-reference-scripts config/demo/private/head-0/private.json
# -> config/demo/bootstrap/script-refs.json
```

**This step is optional when a committed default exists for the network**: with no
`bootstrap/script-refs.json`, `build-head-config` falls back to
`config/script-refs/<network>.json` (Preview's is committed). Deploy yourself only for a network
without a default, or after the compiled scripts change.

The rule-based regime (evacuation/dispute) txs resolve the treasury + dispute validators as
**reference scripts** from two L1 UTxOs at startup. This target deploys the currently compiled
scripts: two chained txs funded from head-0's wallet (change returns), each locking one script at
the unspendable burn address, then writes the reference inputs to `script-refs.json`. Because the
burn address can never be spent from, **one deployment serves every head and every restart** —
redeploy only when the compiled scripts change (symptom: step 5 or node start fails complaining
about invalid treasury/dispute script utxos).

**Step 5 — Build the shared head config:**

```bash
export BLOCKFROST_API_KEY=preview...
just build-head-config       # reads config/demo/bootstrap/, writes config/demo/head-config/head-config.json
```

The build assembles the bootstrap directory's four files (roster, defaults, opening L2 state,
script refs) and talks to L1: it fetches head peer 0's UTxOs (to select funding inputs and verify
the balance) and the protocol parameters via Blockfrost, then pre-builds the initialization tx
into the config (each node derives the fallback tx from it when reading the config). The
Blockfrost key comes from `$BLOCKFROST_API_KEY` (the bootstrap directory carries no credentials;
the CLI also accepts `--blockfrost-key`).

At this point every node has its two files, and the composition (§4) mounts
`head-config/head-config.json` + that node's `private/<peer>/private.json`.

---

## 4. Running Hydrozoa L2

### Docker composition (2 head + 4 coil)

`docker-compose.yml` — one `hydrozoa` container per node on a single user-defined bridge network,
`mesh`.

- Config mounts come from `${HYDROZOA_CONFIG:-./config/demo}`: the shared `head-config.json` plus
  `head-N/private.json` or `coil-N/private.json` per node. `${HYDROZOA_IMAGE}` overrides the
  hydrozoa image (default `cardano-hydrozoa/hydrozoa:0.1.0-SNAPSHOT`).

Caveats:
- **State is ephemeral.** No data volumes are mounted (only read-only config bind mounts), so both
  RocksDB stores are lost on `docker compose down` — consistent with the re-init-per-restart model
  (Restarting, below). A durable deployment mounts a per-node volume at
  `/opt/docker/.hydrozoa-data`.
- **The head initializes only when all head peers + at least `coilQuorum` coil peers are up.** Start
  order doesn't matter (dialers retry); stack 0 hard-confirms with all head signatures +
  `coilQuorum` coil signatures (`design/coil-network.md` §5.7).
- **Rootless docker (NixOS):** the mesh network pins `com.docker.network.driver.mtu: 1400` and each
  node sets an upstream `dns:` — with slirp4netns's default MTU / resolver, outbound TLS to
  Blockfrost can fail. If containers have no outbound connectivity at all, restart the daemon
  (`systemctl --user restart docker`) — stale rootlesskit state produces exactly that.

### Bringing up the head

```bash
sbt docker:publishLocal        # hydrozoa image (once, after code changes)
docker compose up -d
```

Stack 0 initializes once both head peers + any `coilQuorum` coil peers are signing.

### Restarting the head

**Teardown first:** if the previous head initialized and still holds funds, finalize before
anything else (Teardown / recovery of funds, §5) — otherwise the funds stay locked until the
fallback/evacuation path matures.

The demo nodes keep no durable state across `docker compose down` (no volumes), so restarting means
**re-initializing a fresh head on L1** — and `head-config.json` is a one-off artifact that must be
rebuilt each time. It embeds:

- **head-0's actual funding UTxOs**, spent by the initialization tx — consumed the moment a head
  initializes; and
- **wall-clock timing anchors** (block-creation start/end, the fallback-tx start time), computed
  when `just build-head-config` runs. These go stale even if the head never initialized (e.g.
  bring-up failed): start a head long after building the config and the fallback deadline is
  already looming.

So the restart cycle is:

```bash
# 0. head still holds funds? finalize first (see above)
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

## 5. Demo: drive the running head

### Submit an L2 transaction

```bash
just submit-l2-tx        # or: just submit-l2-tx config/demo http://localhost:8081
```

Pick a peer (its key signs), pick one of its L2 utxos (fetched from `GET /api/l2/utxos/{address}`
— the opening `l2-cardano-eutxo.json` outputs sit at the head peers' addresses), enter a
destination (bech32, or a peer name like `head-1`) and a value. The tool builds the zero-fee
native tx (with the CIP-67 output designations and the headId pin in the metadata), signs it with
the peer wallet, and posts it to `POST /api/l2/submit`. An example session — send 2 of head-0's
opening 5 ADA to head-1:

```
Select a peer (its key signs everything below):
    1) coil-0
    ...
    5) head-0
    6) head-1
Enter 1..6: 5

Peer head-0, L2 address: addr_test1...

L2 utxos at head-0:
    1) 63f37fc38e5b3652…#0  5.000000 ADA
Enter 1..1: 1
Destination (bech32 address, or a peer name like head-1): head-1
Value to send (whole ADA, available 5.000000 ADA): 2
Built + signed L2 tx 8de4...
Accepted: requestId=... . Watch GET http://localhost:8080/api/l2/utxos/... for the result.
```

Verify with `curl http://localhost:8080/api/l2/utxos/<address>` for both peers — head-1 gains a
2-ADA utxo, head-0 keeps 3 ADA change.

### Deposit into the head

```bash
just deposit
```

Pick a peer, pick one of its **L1** utxos (via the peer's Blockfrost backend — for the demo that
is head peer 0, the funded one), and enter the L2 outputs the deposit should spawn. The tool
serializes the L2 payload, COSE-signs its hash with the peer wallet, registers the deposit with
`POST /api/deposit/register`, then signs the deposit tx and submits it to L1 via Blockfrost,
polling until the utxo lands. An example session — deposit 3 ADA from head-0's L1 funds to
coil-0's L2 address:

```
Select a peer (its key signs everything below):
Enter 1..6: 5

Peer head-0, L1 address: addr_test1...

L1 utxos at head-0:
    1) 09d34beadf03c8b7…#1  9737.297159 ADA
Enter 1..1: 1

L2 outputs the deposit spawns on absorption:
Destination (bech32 address, or a peer name like head-1): coil-0
Value (whole ADA): 3
Add another output? [y/N]: n
Built deposit TransactionInput(...#0) (3.000000 ADA to L2, accept-by ...)
Registered with the head: requestId=...
Submitted deposit tx ... to L1; waiting for the utxo…
Deposit is on L1. The head absorbs it after maturity — watch GET .../api/l2/utxos/{destination}
```

The head absorbs the deposit after maturity (a few minutes with the demo timing) — then
`curl http://localhost:8080/api/l2/utxos/<coil-0 address>` shows the spawned 3-ADA output.

### Querying the head

Any head peer answers (`:8080` for head-0, `:8081` for head-1):

```bash
curl http://localhost:8080/ready                               # 200 once the head is up and active
curl "http://localhost:8080/api/l2/transactions?count=10"      # recent applied L2 activity, newest first
curl "http://localhost:8080/api/l2/utxos/<bech32-address>"     # current L2 utxos at an address
```

`/api/l2/transactions` covers plain L2 transactions plus deposit registrations, absorptions, and
refunds (the `kind` field tells them apart); `/api/l2/utxos` returns the utxos as CIP-0116 JSON.
Both are the quickest way to watch a submitted tx or deposit land.

### Teardown / recovery of funds

To get the head's funds back on L1, **finalize before tearing down** — the head address is a
multisig (`AllOf(headVKeys) ∧ AtLeast(coilQuorum, coilVKeys)`), so once the peers are gone nothing
can spend from it except the pre-signed fallback/evacuation path.

**Known bug:** a head that has processed no L2 traffic is stuck on block 1 (the first block waits
for a user request), and finalization waits with it. Submit any L2 tx first as a kick — e.g.
`just submit-l2-tx` (above) — then finalize.

Finalize on any head peer (basic auth, the template's default credentials shown):

```bash
curl -u admin:welcome -X POST http://localhost:8080/api/admin/finalize
```

The finalization tx pays the L2 state and equity back out on L1 (watch `preview.cexplorer.io`).
Then `docker compose down`. Leftover change at head peer 0's own L1 address (not head-locked) can
be swept with its single key at any time.

