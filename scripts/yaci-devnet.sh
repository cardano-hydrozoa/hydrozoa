#!/usr/bin/env bash
#
# Drive a local Yaci DevKit devnet as the head's L1, for local development and the Docker
# smoke-test. This is the only place in the repo that knows anything about Yaci: `hydrozoa` itself
# targets any Blockfrost-compatible backend and has no devnet-specific code path.
#
# Run `scripts/yaci-devnet.sh` with no arguments for the commands, or reach them as
# `just yaci-devnet <command>`. Intended as the one implementation of the devnet path — for a human
# following DEPLOYMENT.md and for the Docker smoke-test alike, so the suite ends up exercising the
# documented path instead of a lookalike.
#
# The devnet is added to the shipped `docker-compose.yml` by the `docker-compose.yaci.yml` overlay,
# never in place of it. Set COMPOSE_PROJECT_NAME to run more than one project side by side.
#
# See DEPLOYMENT.md.
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

# The devnet's host-mapped ports (docker-compose.yaci.yml). The peers reach the same devnet in-mesh
# at http://yaci:8080/api/v1 instead — see the overlay's port-split note.
blockfrost_url="http://localhost:18080/api/v1"
admin_url="http://localhost:10000/local-cluster/api"

# The packaged launcher `just stage` builds; only `network` needs it.
hydrozoa="${HYDROZOA_BIN:-$repo_root/target/universal/stage/bin/hydrozoa}"

# A cold `create-node` pulls no images (the container is already up) but does have to produce a
# genesis and start the node; the store API follows a little later.
container_timeout=120
up_timeout=300
topup_timeout=180

main() {
    local command=${1:-}
    shift || true
    case "$command" in
        up) cmd_up "$@" ;;
        network) cmd_network "$@" ;;
        topup) cmd_topup "$@" ;;
        down) cmd_down "$@" ;;
        "" | -h | --help | help) usage ;;
        *)
            echo "unknown command: $command" >&2
            usage >&2
            exit 2
            ;;
    esac
}

usage() {
    cat <<'EOF'
usage: scripts/yaci-devnet.sh <command> [args]

  up                    create a fresh devnet and wait until both its APIs answer
                        (replaces the devnet already there, if any)
  network [OUT]         write its chain description, for --cardano-network-file
                        (default: network.json)
  topup ADDRESS [ADA]   fund an address, and wait until the funds are indexed
                        (default: 100000 ADA)
  down                  remove the devnet and its state (the peers are left alone)

Set COMPOSE_PROJECT_NAME to run more than one project side by side.
EOF
}

# Start the container, create the devnet inside it, and return only once both APIs answer.
cmd_up() {
    say "starting the devnet container…"
    compose up -d yaci
    # A cold `up` returns once the container is created, which is not quite the same as being able
    # to exec into it; without this wait a cold start fails the exec below and only shows up five
    # minutes later as an API timeout, pointing at the wrong problem.
    wait_until "the devnet container" "$container_timeout" container_running

    # `create-node --start` runs the node in the foreground, so exec it detached (-d): a blocking
    # exec would never return, which also makes its exit status uninformative.
    say "creating the devnet (1s blocks, for a wall-clock test; replaces any existing one)…"
    if ! compose exec -d yaci /app/yaci-cli.sh \
        create-node -o --start --block-time 1 --slot-length 1; then
        # Expected for a devnet that already exists, so the readiness polls arbitrate — but say it,
        # rather than swallowing the one clue for why they might be about to time out.
        say "warning: create-node exited non-zero; if the wait below times out, start there"
    fi
    wait_until "the devnet admin API" "$up_timeout" admin_ready
    wait_until "the Blockfrost-compatible store API" "$up_timeout" store_ready
}

# Write the running devnet's chain description, in the shape `--cardano-network-file` reads:
#
#   scripts/yaci-devnet.sh network network.json
#   hydrozoa keygen-fleet 2 4 2 --cardano-network-file network.json
#
# The protocol parameters come off the Blockfrost API like any other backend's. The chain's slot
# geometry cannot: Yaci's store answers 404 for `/genesis`, so the four numbers that would come
# from there are read from the admin API and handed to `discover-network` as arguments — where they
# are typed, and validated with everything else. Nothing edits the file it writes.
cmd_network() {
    local out=${1:-network.json}
    need jq
    [[ -x $hydrozoa ]] ||
        die "the 'hydrozoa' launcher isn't built at $hydrozoa — run 'just stage' first"

    local devnet
    devnet=$(probe "$admin_url/admin/devnet") ||
        die "the devnet admin API isn't answering at $admin_url — run 'up' first"

    local start slot epoch magic
    read -r start slot epoch magic < <(
        jq -r '[.startTime, .slotLength, .epochLength, .protocolMagic] | @tsv' <<<"$devnet"
    )
    [[ -n ${magic:-} ]] || die "the admin API did not report the devnet's geometry: $devnet"

    say "describing the devnet chain (magic $magic, ${slot}s slots)…"
    "$hydrozoa" discover-network \
        --blockfrost-url "$blockfrost_url" \
        --system-start "$start" \
        --slot-length "$slot" \
        --epoch-length "$epoch" \
        --protocol-magic "$magic" \
        --out "$out" ||
        die "could not describe the chain at $blockfrost_url"
}

# Fund an address from the admin API — a devnet has no faucet. Returns once the funds are spendable,
# not merely accepted.
cmd_topup() {
    local address=${1:-} ada=${2:-100000}
    [[ -n $address ]] || die "usage: topup ADDRESS [ADA]"
    need jq

    say "topping up $address with $ada ADA…"
    curl -fsS --max-time 30 -X POST "$admin_url/addresses/topup" \
        -H 'Content-Type: application/json' \
        -d "{\"address\":\"$address\",\"adaAmount\":$ada}" >/dev/null
    # The store indexes the topup a block or two later, and `deploy-scripts-and-g2-setup` fetches
    # head peer 0's utxos exactly once — it fails outright against an address that still looks empty.
    wait_until "the funds to be indexed" "$topup_timeout" address_funded "$address"
}

# Remove the devnet container, and with it the chain state it holds (the service declares no
# volumes). Deliberately *only* the devnet: a project-wide `compose down` here would take the
# operator's head — all six peers — with it, which is not what a command called `yaci-devnet down`
# should do. Stop the peers with `docker compose down` as usual.
cmd_down() {
    say "removing the devnet container and its state…"
    compose rm --stop --force --volumes yaci
}

# ---- probes ------------------------------------------------------------------------------------

# One HTTP GET, capped: without a cap a black-holed port hangs for curl's multi-minute default and
# `wait_until`'s deadline — which is only consulted between attempts — never gets a look in.
probe() { curl -sf --max-time 10 "$1"; }

container_running() { [[ -n $(compose ps --quiet --status running yaci) ]]; }

admin_ready() { probe "$admin_url/admin/devnet" >/dev/null; }

# The store is up once it serves protocol parameters — the first query the host-side generation
# steps (`deploy-scripts-and-g2-setup`, `build-head-config`) make.
store_ready() { probe "$blockfrost_url/epochs/latest/parameters" >/dev/null; }

# 404 while the address is still unseen, then `[]` until the topup is indexed: both count as unfunded.
address_funded() {
    local count
    count=$(probe "$blockfrost_url/addresses/$1/utxos" | jq 'length' 2>/dev/null || true)
    [[ ${count:-0} -gt 0 ]]
}

# ---- plumbing ----------------------------------------------------------------------------------

# The shipped deployment file plus the devnet overlay, in that order — the same pair DEPLOYMENT.md
# gives an operator. `-p` keeps a local devnet from colliding with a run against a public testnet.
compose() {
    docker compose -p "${COMPOSE_PROJECT_NAME:-hydrozoa-local}" \
        -f "$repo_root/docker-compose.yml" \
        -f "$repo_root/docker-compose.yaci.yml" \
        "$@"
}

# Poll `predicate args…` until it succeeds, or give up after `timeout` seconds.
wait_until() {
    local what=$1 timeout=$2
    shift 2
    local deadline=$((SECONDS + timeout))
    # Braced: bash folds the following multibyte character into the name otherwise, and `set -u`
    # then dies on the variable that does not exist.
    say "waiting for ${what}…"
    until "$@"; do
        ((SECONDS < deadline)) || die "timed out after ${timeout}s waiting for $what"
        sleep 2
    done
}

need() { command -v "$1" >/dev/null || die "$1 is required but not on PATH"; }

say() { echo "[yaci-devnet] $*"; }

die() {
    echo "[yaci-devnet] error: $*" >&2
    exit 1
}

main "$@"
