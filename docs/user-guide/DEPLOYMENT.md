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
> - **Durable state, single-use head config.** Each peer's RocksDB store lives on a named volume, so
>   it survives a restart (crash recovery and evacuation read it); `just head-reset` wipes it
>   for a fresh head. `head-config.json` still embeds real utxos + wall-clock anchors, so it cannot be
>   reused — a fresh head needs a rebuilt config.
> - **Public testnet (Preview / Preprod) via Blockfrost only** — a trusted third party between
>   every node and L1.

Deploys a multi-party Hydrozoa head running the **built-in EUTXO L2 ledger** (`l2Ledger =
cardano-eutxo`): the ledger runs in-process inside every node, so a node is a single container —
no ledger sidecar, no external database, no separate API service or UI. (The `any-remote` backend
that drives an external ledger over `remoteLedgerUri` is a separate deployment shape, not covered
here.)

---

## 1. System overview

A running head is N **head peers** and M **coil peers** (optional). Each peer is one `hydrozoa`
process talking to Cardano L1 (a public testnet) via Blockfrost:

```
                       Cardano L1 (testnet) via Blockfrost
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
  (`docs/spec/coil-network.md` §2, §4.3). The L1 multisig is
  `AllOf(headVKeys) ∧ AtLeast(coilQuorum, coilVKeys)`.
- **State.** Each node keeps two RocksDB stores under its data dir: the consensus store
  (`peer-<label>/rocksdb`) and the EUTXO ledger store
  (`peer-<label>/l2-rocksdb`). Default data dir `.hydrozoa-data` relative to
  cwd; give each node its own.
- **User HTTP API** (head peers only, port 8080). One REST surface:
  - **submit** — `POST /head/requests` (deposits and L2 transactions, distinguished by the body's
    `type` field — `{ "type": "deposit", … }` or `{ "type": "transaction", … }`);
  - **queries** — `GET /head/blocks[/{n}[/body]]`, `GET /head/requests[/{id}]` (lifecycle status per
    request), `GET /head/effects/{l1TxId}` (L1 effect by tx id);
  - **observability** — `GET /health` (liveness), `GET /ready` (readiness), `GET /version` (the
    baked version, git commit, and build time);
  - **admin** — `POST /api/admin/finalize` (basic-auth);
  - **L2 EUTXO queries** (EUTXO ledger only — a remote-ledger node serves neither):
    `GET /l2/cardano-eutxo/utxos/{address}`, `GET /l2/cardano-eutxo/transactions`.

  Interactive **Swagger UI at `/docs`**; the authoritative machine-readable contract is published as
  the [API reference](../api/) — Redoc over the OpenAPI documents, regenerated from the endpoint
  definitions.

### Network matrix

| Port | Service | Who connects | Configured in |
|---|---|---|---|
| 8080 | head-0 user/admin HTTP API | users, admins | `peer-private.json` (`httpHost`/`httpPort`) — in-container `8080`, published to host `8080` |
| 8081 | head-1 user/admin HTTP API | users, admins | `peer-private.json` (`httpHost`/`httpPort`) — in-container `8080`, published to host `8081` |
| 4001 | hydrozoa mesh WS server: `/head` (mesh), `/hub` (hub→coil) | other head peers; hubbed coil peers | `webSocketAddress` in the shared head config — **bind address == dialed address** |

Head-mesh dialing convention: lower-numbered peer dials higher (`docs/spec/coil-network.md` §4.3).
Coil peers dial out only; they need no inbound port at all. Every node also needs outbound HTTPS
to Blockfrost (`blockfrostApiKey` in `peer-private.json`).

---

## 2. Getting Hydrozoa

> Assumes **rootless Docker** (the tested setup): the alias runs the container as root, which under
> rootless maps to *your* host user, so it can write the mounted head directory. On rootful Docker,
> add `-u "$(id -u):$(id -g)"` to the alias instead.

**You do not need to clone this repo.** The published image carries the whole CLI plus the workspace
files (`docker-compose.yml`, `hydrozoa.sh`, config template), so pull it and scaffold a head
workspace from it:

```bash
mkdir myhead && cd myhead
docker pull ghcr.io/cardano-hydrozoa/hydrozoa:latest
docker run --rm -v "$PWD:/work" -w /work --user root \
  ghcr.io/cardano-hydrozoa/hydrozoa:latest scaffold .
# writes docker-compose.yml, hydrozoa.sh, template/peer-private.template.json.local
```

The scaffold dir is itself the head directory — everything the CLI reads and writes lives directly
under it. Then load the CLI alias and check you are on the version you expect:

```bash
source ./hydrozoa.sh   # sets the `hydrozoa` alias + HYDROZOA_HOME (this folder, an absolute path)

hydrozoa version       # verify the image you are running
#   hydrozoa 0.1.8
#   git:   v0.1.8
#   built: 2026-07-28 14:00:37.834-0600
```

(The `git:` line is `git describe` provenance; a published image built from the `v0.1.8` tag reads a
clean `v0.1.8`. A locally built image between releases shows the distance from the newest tag, e.g.
`v0.1.0-10-gaa9d7c69`.)

Need a version that isn't in the registry? Build it from this repo — see §3.

---

## 3. Building

Cloning this repo is only needed to run a version other than the published image, or to hack on
Hydrozoa. Toolchain: Nix flake devshell (JDK 25, sbt, just — `flake.nix`); Scala 3.3.7. JDK 23+ if
not using Nix (the runtime passes `--sun-misc-unsafe-memory-access=allow`).

```bash
nix develop            # or direnv (.envrc = use flake .)
sbt compile            # compile the project
just test              # unit tests
just integration-fast  # multi-peer integration subset
```

Two ways to use your build:

**A local Docker image** — the same image as the published one, from your sources. It is tagged
`cardano-hydrozoa/hydrozoa:<version>`, `ghcr.io/cardano-hydrozoa/hydrozoa:<version>`, and
`ghcr.io/cardano-hydrozoa/hydrozoa:latest` — the last matches `docker compose`'s default image, so
the scaffolded head (§5) picks up your local build with no `HYDROZOA_IMAGE` override:

```bash
just docker-image      # -> cardano-hydrozoa/hydrozoa:<version> + ghcr.io/…:<version> + ghcr.io/…:latest
```

(That local `…:latest` tag shadows the published one on this machine; `docker compose pull` restores
the released image.)

**Locally-compiled code (development)** — `just stage` builds the `hydrozoa` launcher from the
current sources, and the `just` recipes invoke it directly, with no Docker and no sbt startup per
command. `just scaffold` then populates the head directory ($HYDROZOA_HOME, default `head/demo`)
with the config template the §4 recipes fill in:

```bash
just stage             # -> target/universal/stage/bin/hydrozoa
just scaffold          # -> $HYDROZOA_HOME/ (default head/demo): config template + docker-compose.yml + hydrozoa.sh
```

`$HYDROZOA_HOME` selects which head directory every recipe reads and writes — default `head/demo`;
export it (e.g. `export HYDROZOA_HOME=./head/release/preview`) to keep separate per-network heads.

Every command in §4–§6 is shown **both ways**: a `# Docker` line (`hydrozoa <cmd>`, via the image and
the alias from §2) and the `just` equivalent (`just <cmd>`, which runs the staged launcher). Use
whichever matches the build you made.

---

## 4. Configuration

> **L1 backend.** The flow below targets a **public testnet via Blockfrost** (Preview / Preprod).
> To run against a **local Yaci DevKit** devnet instead — no Blockfrost key, no funded wallet,
> hermetic and repeatable — see [§7](#7-alternative-l1-a-local-yaci-devkit-devtesting); only a few
> steps differ.

Everything a node reads comes from two files:

**(a) Shared `head-config.json`** — identical on every node. It embeds
the peer topology, the L1 network, `scriptReferenceUtxos`, the peer-agreed head parameters
(including `l2Ledger` and `identityIsomorphism`), the head id, and
the **pre-built initialization transaction** with its wall-clock timing anchors — so it is
single-use per head (§5). Each node derives the fallback transaction from the initialization tx
when it reads the config.

**(b) Per-node `peer-private.json`** — `ownPeerPrivate` (identity +
Ed25519 signing wallet — `ownHeadWallet` for a head peer / `ownCoilWallet` for a coil peer, matched
against the head config's vkeys), `nodeOperationEvacuationConfig` (the evacuation-bot polling
period), `nodeOperationMultisigConfig` (rate limits, Cardano polling period),
`blockfrostApiKey`, `adminUsername`, `adminPassword`, `httpHost`, `httpPort`. `remoteLedgerUri` is
**optional and unused for the EUTXO ledger** — it is read only on the `any-remote` path, so an
EUTXO node may omit it.

### Generating a head's configuration

Each step below covers both the Docker (`hydrozoa <cmd>`, via the §2 alias) and local (`just <cmd>`,
after `just stage`) versions of the command.

The walkthrough uses the docker topology: **2 head peers, 4 coil peers, coil quorum 2**. The
pipeline turns operator-authored files into the two runtime files each node needs:

```
   keygen-fleet (keygen per peer, then init-bootstrap-files)   the bootstrap directory (operator-authored)
   ├─ bootstrap/roster.json ──────────┐  peer topology (head/coil vkeys, ws addresses, hubs)
   ├─ bootstrap/defaults.json ────────┤  network + head params + per-peer equity  (editable)
   ├─ bootstrap/l2-cardano-eutxo.json ┤  opening L2 outputs (one per head peer, editable)
   ├─ private/head-N/private.json     │  per-peer private configs (not part of the bootstrap dir)
   └─ private/coil-N/private.json     │
                                      │
   deploy-scripts-and-g2-setup        │
   (head-0 wallet, Blockfrost)        │
   └─ bootstrap/ref-utxos.json ───────┤  the on-chain reference UTxOs: treasury + dispute
                                      │  validators and the G2 setup ladder (falls back to the
                                      │  per-network default baked into the image)
                                      │
                                      │  + head-0 UTxOs + protocol params   (Blockfrost)
                                      ▼
   build-head-config ───► head-config/head-config.json  assembles the four bootstrap files, adds
                                      │                 headId + the pre-built init tx + timing
                                      ▼
   distribute head-config.json (shared) + each node's private.json  →  run the nodes (§5)
```

### Step 1 — Edit the template

Set `blockfrostApiKey` in `$HYDROZOA_HOME/template/peer-private.template.json.local` — i.e.
`template/peer-private.template.json.local` in the Docker workspace, or `head/demo/template/…` for a
local `just` run (scaffolded in §2; Nix users create it with `just scaffold`).

- **Blockfrost key** — keygen-fleet reads only the `.local` file and refuses to run without it,
  so the real key never lands in a committed file. The build steps that query the chain
  (`build-head-config`, `deploy-scripts-and-g2-setup`) fall back to this same `blockfrostApiKey`
  unless you pass `--blockfrost-key` / `$BLOCKFROST_API_KEY`, so you set the key once, here.
- **Cardano network** — the key's prefix (`preview…` / `preprod…` / `mainnet…`) selects the
  network for everything downstream: the `cardanoNetwork` seeded into `defaults.json` and the
  target of `deploy-scripts-and-g2-setup`.

The template is read at generation time — regenerating means fresh keys, so re-funding.

### Step 2 — Generate keys, roster, and defaults

```bash
hydrozoa keygen-fleet 2 4 2        # Docker; HEADS COILS QUORUM, → $HYDROZOA_HOME/ (the workspace)
just keygen-fleet 2 4 2            # local; → head/demo/ (custom dir: HYDROZOA_HOME=head/mydir just keygen-fleet 2 4 2)
```

One command generates a key pair per peer (registered in the roster, with a filled private config),
then the shared `defaults.json` + `l2-cardano-eutxo.json`; the network comes from the template's
Blockfrost-key prefix. Output layout:

```
$HYDROZOA_HOME/                    # Docker: your scaffold dir; local just: head/demo/
├── bootstrap/                     # the operator-facing bootstrap directory (build-head-config input)
│   ├── roster.json                #   peer topology
│   ├── defaults.json              #   network + head params (coilQuorum, timing…) + per-peer equity
│   ├── l2-cardano-eutxo.json      #   opening L2 outputs, one 5-ADA output per head peer — edit to taste
│   └── ref-utxos.json           #   written by deploy-scripts-and-g2-setup (step 4; optional —
│                                  #   committed per-network defaults are the fallback)
├── head-config/
│   └── head-config.json           # written later by build-head-config (step 5)
└── private/
    ├── head-0/private.json        # ownHeadWallet identity
    ├── head-1/…
    └── coil-0/… coil-3/…          # ownCoilWallet identities, hubs assigned round-robin (0,1,0,1)
```

Peer numbering is positional in the roster: `private/head-N` ↔ head peer N; likewise coils.

### Step 2b — Set the head parameters

`bootstrap/defaults.json` carries demo defaults; edit any
of them:

- `headParams` — `coilQuorum` (the QUORUM argument passed to keygen-fleet; 2 here), timing,
  fallback contingency, dispute resolution, settlement.
- `initialEquityContributions` — per head peer; the demo default is head peer 0 funds
  everything, the rest contribute zero.

The initial block's timing is anchored to wall-clock automatically when the head config is built
— nothing to set here.

### Step 2c — Specify the initial utxo set

`bootstrap/l2-cardano-eutxo.json` is the head's
opening L2 ledger — a list of `{ "address", "value" }` outputs (value in CIP-0116). keygen-fleet
seeds one 5-ADA output per head peer; replace it with the real opening distribution, or leave it
empty (`[]`) — a head can also be funded later by depositing into it (§6).

### Step 3 — Fund head peer 0

The sole funder: print its address and send testnet tADA to it
(e.g. from the [testnet faucet](https://docs.cardano.org/cardano-testnets/tools/faucet) — pick
your network there; one 10k-tADA drip is plenty):

```bash
hydrozoa head-zero-address   # Docker; derives the address from bootstrap/{roster,defaults}.json
just head-zero-address       # local
```

The funding must cover equity + the whole head's fallback contingency + the opening L2 value +
tx fee; step 5 logs the exact lovelace required and fails with the shortfall if underfunded.

### Step 4 — Deploy the reference scripts

> **You can likely skip this.** With no `bootstrap/ref-utxos.json`, `build-head-config` falls back
> to the per-network default **baked into the image** — Preview and Preprod are included — so on
> those networks skip to Step 5. Run this only for a network without a baked default, or after the
> compiled scripts change (then update `src/main/resources/scaffold/ref-utxos/` so the next head
> can skip it too).

```bash
hydrozoa deploy-scripts-and-g2-setup   # Docker
just deploy-scripts-and-g2-setup        # local
# -> $HYDROZOA_HOME/bootstrap/ref-utxos.json (funded by head-0's wallet under the head dir)
```

The rule-based regime (evacuation/dispute) txs resolve the treasury + dispute validators — and
the G2 setup ladder — as **reference UTxOs** at startup. This target deploys the currently
compiled scripts, funded from head-0's wallet (change returns): one chained tx per validator
plus, unless an existing ladder is reused (the `LADDER_REFS` argument), one tx carrying the seven
setup-ladder rungs — all locked at the unspendable burn address — then writes the reference
inputs to `ref-utxos.json`; the bootstrap directory's own refs take precedence over the
committed defaults. Because the burn address can never be spent from, **one deployment serves
every head and every restart** — redeploy the validators only when the compiled scripts change
(symptom: step 5 or node start fails complaining about invalid treasury/dispute script utxos);
the ladder never changes.

### Step 5 — Build the shared head config

```bash
hydrozoa build-head-config   # Docker; reads $HYDROZOA_HOME/bootstrap/, writes $HYDROZOA_HOME/head-config/head-config.json
just build-head-config       # local
```

The build assembles the bootstrap directory's four files (roster, defaults, opening L2 state,
script refs) and talks to L1: it fetches head peer 0's UTxOs (to select funding inputs and verify
the balance) and the protocol parameters via Blockfrost, then pre-builds the initialization tx
into the config (each node derives the fallback tx from it when reading the config). The
Blockfrost key comes from the `.local` template (step 1); the build fails fast if the key's
network does not match the bootstrap config's `cardanoNetwork`.

At this point every node has its two files, and the composition (§5) mounts
`head-config/head-config.json` + that node's `private/<peer>/private.json`.

---

## 5. Running Hydrozoa L2

### Docker composition (2 head + 4 coil)

`docker-compose.yml` — one `hydrozoa` container per node on a single user-defined bridge network,
`mesh`.

- Config mounts resolve against `.` — the head directory the compose file was scaffolded into — so
  running compose from there mounts that dir's shared `head-config.json` plus each node's
  `head-N/private.json` or `coil-N/private.json`. The image defaults to the published
  `ghcr.io/cardano-hydrozoa/hydrozoa:latest` (pulled on first run); pin a release with
  `${HYDROZOA_VERSION}`, or override the whole ref with `${HYDROZOA_IMAGE}` (e.g. a locally built one).

Caveats:
- **State is durable.** Each node's consensus + L2 RocksDB stores live on a named volume mounted at
  `/opt/docker/.hydrozoa-data`, so they survive `just head-down` / restart (crash recovery and
  evacuation read them). Reset to a fresh head with `just head-reset`, which wipes the volumes.
- **The head initializes only when all head peers + at least `coilQuorum` coil peers are up.** Start
  order doesn't matter (dialers retry); stack 0 hard-confirms with all head signatures +
  `coilQuorum` coil signatures (`docs/spec/coil-network.md` §5.7).
- **Rootless docker (NixOS):** the mesh network pins `com.docker.network.driver.mtu: 1400` and each
  node sets an upstream `dns:` — with slirp4netns's default MTU / resolver, outbound TLS to
  Blockfrost can fail. If containers have no outbound connectivity at all, restart the daemon
  (`systemctl --user restart docker`) — stale rootlesskit state produces exactly that.

### Bringing up the head

The `just` recipes below run compose in the scaffolded head directory for you (`$HYDROZOA_HOME`, set
when you source `hydrozoa.sh`), so config mounts resolve against it.

Default — pull the published image and run the scaffolded head:

```bash
just head-up          # pulls ghcr.io/cardano-hydrozoa/hydrozoa:latest on first run
```

Stack 0 initializes once both head peers + any `coilQuorum` coil peers are signing.

To watch the head land on L1, run `docker logs <any service>` (e.g. `docker logs head-0`) and find
this section:

```
23:18:05.107 TRACE CardanoLiaison.0
[peer=0] current time=1784071085000 utxoIds= state=State(
  targetState: Active(treasuryUtxoId=TransactionInput("c8bac56e7be7438bd766760a277ed0518fcf390e90cdb30e76eb9cff72254ba2",0))
  effectInputs (0 entries):

  happyPathEffects (1 entries):
  (0,0) -> txHash="c8bac56e7be7438bd766760a277ed0518fcf390e90cdb30e76eb9cff72254ba2"
  fallbackEffects (1 entries):
  0 -> txHash="a9fe26149502e8c88ba13440976cc8a938bcace5fc33bcd2e4ab06f301e28675"
)
```

The `(0,0)` entry under `happyPathEffects` is the initialization tx — open its hash in the
network's explorer to check it out. Every following happy-path effect (settlements, the
finalization) appears in the same section as the head progresses.

**Another image** — pin a published release with `HYDROZOA_VERSION`, or override the whole ref with
`HYDROZOA_IMAGE` (default `ghcr.io/cardano-hydrozoa/hydrozoa:latest`). A local `just docker-image`
build (§3) is already tagged `…:latest`, so it is picked up without either:

```bash
HYDROZOA_VERSION=0.1.8 just head-up                          # a specific published release
HYDROZOA_IMAGE=cardano-hydrozoa/hydrozoa:0.1.8 just head-up  # a specific/other image name
```

**Another head** — each head directory carries its own `docker-compose.yml`, so switch heads by
pointing `HYDROZOA_HOME` at a different one (e.g. a per-network `head/release/preview`) before
`just head-up`.

### Restarting the head

**Teardown first:** if the previous head initialized and still holds funds, finalize before
anything else (Teardown / recovery of funds, §6) — otherwise the funds stay locked until the
fallback/evacuation path matures.

Nodes keep durable state on named volumes, so a plain restart recovers the **same** head from its
store. To start a **fresh** head instead, wipe the volumes with `just head-reset` — and
rebuild `head-config.json`, a one-off artifact that must be rebuilt each time. It embeds:

- **head-0's actual funding UTxOs**, spent by the initialization tx — consumed the moment a head
  initializes; and
- **wall-clock timing anchors** (block-creation start/end, the fallback-tx start time), computed
  when `just build-head-config` runs. These go stale even if the head never initialized (e.g.
  bring-up failed): start a head long after building the config and the fallback deadline is
  already looming.

So the restart cycle is:

```bash
# 0. head still holds funds? finalize first (see above)
just head-down
# re-fund head peer 0 if the previous head consumed the funding — `just head-zero-address`
# prints the address; check it in the network's explorer
just build-head-config
just head-up           # right after the build — the config is freshest now
```

**Reusable across restarts:** the whole `$HYDROZOA_HOME/bootstrap/` directory and every
`$HYDROZOA_HOME/private/` config — identities and keys are not time- or UTxO-bound, and the
reference-script UTxOs sit at an unspendable burn address. Only `head-config/head-config.json` is
single-use. Regenerate the fleet (`just keygen-fleet`) only for fresh identities; that changes head
peer 0's address, so re-fund it. The hydrozoa docker image only needs rebuilding after hydrozoa
code changes.

---

## 6. Demo: drive the running head

These two commands are **interactive** (they prompt on the console) and reach the head over HTTP —
the `hydrozoa` alias from §2 already enables the TTY and host networking they need.

### Submit an L2 transaction

```bash
hydrozoa submit-l2-tx    # Docker; add --head-uri http://localhost:8081 to hit head-1
just submit-l2-tx        # local; or: HYDROZOA_HOME=head/demo just submit-l2-tx http://localhost:8081
```

Pick a peer (its key signs), pick one of its L2 utxos (fetched from `GET /l2/cardano-eutxo/utxos/{address}`
— the opening `l2-cardano-eutxo.json` outputs sit at the head peers' addresses), enter a
destination (bech32, or a peer name like `head-1`) and a value. The tool builds the zero-fee
native tx (with the output designations and the headId pin in the metadata), signs it with
the peer wallet, and posts it to `POST /head/requests`. An example session — send 2 of head-0's
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
Accepted: requestId=... . Watch GET http://localhost:8080/l2/cardano-eutxo/utxos/... for the result.
```

Verify with `curl http://localhost:8080/l2/cardano-eutxo/utxos/<address>` for both peers — head-1 gains a
2-ADA utxo, head-0 keeps 3 ADA change.

### Deposit into the head

```bash
hydrozoa submit-deposit  # Docker
just submit-deposit      # local
```

Pick a peer, pick one of its **L1** utxos (via the peer's Blockfrost backend — for the demo that
is head peer 0, the funded one), and enter the L2 outputs the deposit should spawn. The tool
serializes the L2 payload (its hash rides in the deposit tx metadata), registers the deposit with
`POST /head/requests`, then signs the deposit tx and submits it to L1 via Blockfrost,
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
Deposit is on L1. The head absorbs it after maturity — watch GET .../l2/cardano-eutxo/utxos/{destination}
```

The head absorbs the deposit after maturity (a few minutes with the demo timing) — then
`curl http://localhost:8080/l2/cardano-eutxo/utxos/<coil-0 address>` shows the spawned 3-ADA output.

### Querying the head

Any head peer answers (`:8080` for head-0, `:8081` for head-1):

```bash
curl http://localhost:8080/ready                               # 200 once the head is up and active
curl "http://localhost:8080/l2/cardano-eutxo/transactions?count=10"      # recent applied L2 activity, newest first
curl "http://localhost:8080/l2/cardano-eutxo/utxos/<bech32-address>"     # current L2 utxos at an address
```

`/l2/cardano-eutxo/transactions` covers plain L2 transactions plus deposit registrations, absorptions, and
refunds (the `kind` field tells them apart); `/l2/cardano-eutxo/utxos` returns the utxos as CIP-0116 JSON.
Both are the quickest way to watch a submitted tx or deposit land.

Point a browser at `http://localhost:8080/docs` for the interactive Swagger UI over the full API
(blocks, requests, effects, health/readiness, and the L2 queries above).

### Teardown / recovery of funds

To get the head's funds back on L1, **finalize before tearing down** — the head address is a
multisig (`AllOf(headVKeys) ∧ AtLeast(coilQuorum, coilVKeys)`), so once the peers are gone nothing
can spend from it except the pre-signed fallback/evacuation path.

**Known bug:** the finalize request does not push a block out by itself — it rides the next
block. After calling finalize, submit any L2 tx as a kick (e.g. `just submit-l2-tx`, above) or
wait for the head to force a block on its own; the finalization follows with that block.

Finalize on any head peer (basic auth, the template's default credentials shown):

```bash
curl -u admin:welcome -X POST http://localhost:8080/api/admin/finalize
```

The finalization tx pays the L2 state and equity back out on L1 (watch the network's explorer,
e.g. `preview.cexplorer.io` / `preprod.cexplorer.io`).
Then `just head-down`. Leftover change at head peer 0's own L1 address (not head-locked) can
be swept with its single key at any time.

### Evacuation

When the happy-path finalize is not available — peers gone, the head wedged — funds come back via
the **rule-based regime**: a peer submits the pre-signed fallback tx, the on-chain scripts resolve a
dispute, and every L2 utxo is paid back out on L1. The `hydrozoa evacuate` command runs that regime
standalone from a peer's leftover database plus its head + private config
([`docs/spec/evacuate-command.md`](../spec/evacuate-command.md)):

```
hydrozoa evacuate <head-config.json> <peer-private.json>
```

It reads the store read-only and keeps polling L1 until stopped, so it is safe to run and re-run.

The composition already persists each peer's store (named volumes) and carries an `evacuate`
profile, so the state `evacuate` reads is always there:

```bash
# Drive the head's evacuation from one peer ad-hoc (uses that peer's store):
just evacuate-peer head-0

# Drive the head's evacuation from every peer: stop the serve peers, then bring up the profile:
just evacuate
```

Evacuation acts on the whole head — resolving the on-chain dispute and paying every L2 utxo back out
on L1; one or more peers can drive it (they converge on-chain). Stop a peer's serve container before
starting its evac service — the read-only store open takes no lock, but you do not want `serve`
pushing the happy path while `evacuate` submits the fallback. Wipe the persisted stores with
`just head-reset` once recovery is complete.

> The composition defaults to the `:latest` image; `evacuate` works once a release that includes it
> is `:latest`. Pin a specific build any time with `HYDROZOA_VERSION` (or a full `HYDROZOA_IMAGE`).
---

## 7. Alternative L1: a local Yaci DevKit (dev/testing)

The flow in §4–§6 targets a **public testnet via Blockfrost**. For local development and testing you
can instead run the head against a **local [Yaci DevKit](https://devkit.yaci.xyz/) devnet** — no
Blockfrost key, no funded testnet wallet, hermetic and repeatable. `docker-compose.yml` ships a
`yaci` **profile** for exactly this (a `bloxbean/yaci-cli` container on the `mesh` network serving a
Blockfrost-compatible store API plus the admin API). This is the same custom-network path the e2e
propagation suite drives
(`integration/src/test/scala/hydrozoa/integration/e2e/DockerPropagationTest.scala`).

Only the steps that **differ** from §4–§5 are shown; `keygen-fleet`, the demo (§6), and teardown are
otherwise identical.

### URL split — read this first

The nodes and the host reach the same Yaci at **different addresses**:

| Who | Store / Blockfrost API | Admin API |
|---|---|---|
| **containers** (in-mesh) | `http://yaci:8080/api/v1` | — |
| **host** (CLI generation steps) | `http://localhost:18080/api/v1` | `http://localhost:10000/local-cluster/api` |

So the peers' `cardanoBackendUrl` is the **in-mesh** URL, while the host-side `deploy-scripts` and
`build-head-config` are given the **host** URLs via `--blockfrost-url` / `--yaci-admin-url` (which
override the in-mesh URL that `keygen-fleet` wrote into `defaults.json`).

### Step 1 — start Yaci and create the devnet

```bash
docker compose --profile yaci up -d yaci
# create + start a fast (1s-block) devnet; the compose sets yaci_store_enabled so create-node also
# brings up the Blockfrost API on 8080:
docker compose --profile yaci exec -d yaci /app/yaci-cli.sh create-node -o --start --block-time 1 --slot-length 1
# wait until the store serves protocol params:
until curl -sf http://localhost:18080/api/v1/epochs/latest/parameters >/dev/null; do sleep 2; done
echo "yaci ready"
```

### Step 2 — template: point at Yaci (replaces the Blockfrost key)

In `head/template/peer-private.template.json.local` (§4 Step 1) set the **in-mesh** backend URL — its
presence marks the network **Custom**; leave the placeholder `blockfrostApiKey` as-is (it is ignored
for a custom network):

```json
"cardanoBackendUrl": "http://yaci:8080/api/v1",
```

Then run `keygen-fleet` as in §4 Step 2 (`just keygen-fleet 2 4 2`) — it writes a pending-custom
marker into `defaults.json` and the in-mesh URL into every `private.json`.

### Step 3 — fund head peer 0 via Yaci topup (no faucet)

Yaci has no faucet; top up head-0's address straight through the admin API:

```bash
ADDR=$(just head-zero-address)                          # head peer 0's bech32 address
curl -X POST http://localhost:10000/local-cluster/api/addresses/topup \
     -H 'Content-Type: application/json' \
     -d "{\"address\":\"$ADDR\",\"adaAmount\":100000}"  # bump if build-head-config reports a shortfall
```

### Step 4 — deploy scripts + build the head config, pointed at Yaci

Run these **host-side** (the staged launcher, or the `hydrozoa` docker alias with host networking) so
`localhost:…` reaches Yaci's host-mapped ports, passing both URLs:

```bash
hydrozoa deploy-scripts-and-g2-setup \
    --blockfrost-url http://localhost:18080/api/v1 \
    --yaci-admin-url http://localhost:10000/local-cluster/api
hydrozoa build-head-config \
    --blockfrost-url http://localhost:18080/api/v1 \
    --yaci-admin-url http://localhost:10000/local-cluster/api
```

(`--blockfrost-url` / `--yaci-admin-url` on `build-head-config` mirror `deploy-scripts` and override
the in-mesh marker so the host-side build reaches Yaci at its host-mapped port; the nodes keep the
in-mesh URL from Step 2.)

### Step 5 — run the head against Yaci

```bash
docker compose --profile yaci up -d           # keeps the yaci container in the project
```

The head initializes on the devnet; §6's submit / deposit / query commands work unchanged. Tear down
with `docker compose --profile yaci down -v`.
