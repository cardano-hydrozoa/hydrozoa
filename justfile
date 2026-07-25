#!/usr/bin/env just --justfile

# The packaged `hydrozoa` launcher the deployment recipes invoke. Built by `just stage` (no sbt in
# the hot path, so each command starts fast and prints clean output). Run from the repo root so
# relative config paths (config/demo) resolve.
hydrozoa := "target/universal/stage/bin/hydrozoa"

# This justfile is configured to send notifications when commands complete.
# To enable this, add a `./.just/notify` file.
#
# Each recipe uses a bash shebang + `trap '... EXIT'` so that `just notify`
# fires whether the command succeeds or fails, while still propagating the
# original exit status to just (so CI and pre-push hooks see failures).

fmt:
  #!/usr/bin/env bash
  trap 'just notify "fmt"' EXIT
  sbt fmtAll

fmt-check:
  #!/usr/bin/env bash
  trap 'just notify "fmt-check"' EXIT
  sbt fmtCheckAll

lint:
  #!/usr/bin/env bash
  trap 'just notify "lint"' EXIT
  sbt lintAll

lint-check:
  #!/usr/bin/env bash
  trap 'just notify "lint-check"' EXIT
  sbt lintCheckAll

nixfmt:
  #!/usr/bin/env bash
  trap 'just notify "nixfmt"' EXIT
  nixfmt flake.nix

nixfmt-check:
  #!/usr/bin/env bash
  trap 'just notify "nixfmt-check"' EXIT
  nixfmt flake.nix --check

test:
  #!/usr/bin/env bash
  trap 'just notify "test"' EXIT
  sbt test

# Compile all sources (main + test) with -Werror, mirroring CI.
build-werror:
  #!/usr/bin/env bash
  trap 'just notify "build-werror"' EXIT
  CI=true sbt Test/compile integration/Test/compile

# Build the packaged `hydrozoa` launcher (target/universal/stage/bin/hydrozoa). Run this once after
# changing code; the deployment recipes below then invoke it directly, with no sbt startup cost.
stage:
  #!/usr/bin/env bash
  trap 'just notify "stage"' EXIT
  sbt stage

# Fail fast with a clear hint if the launcher hasn't been built yet.
_require-launcher:
  #!/usr/bin/env bash
  if [ ! -x "{{hydrozoa}}" ]; then
    echo "error: the 'hydrozoa' launcher isn't built — run 'just stage' first" >&2
    exit 1
  fi

# Build the hydrozoa Docker image locally (cardano-hydrozoa/hydrozoa:<version>); publish via RELEASE.md.
docker-image:
  #!/usr/bin/env bash
  trap 'just notify "docker-image"' EXIT
  sbt Docker/publishLocal

# Write the Docker build context to target/docker/stage without building it (what the release workflow builds).
docker-stage:
  #!/usr/bin/env bash
  trap 'just notify "docker-stage"' EXIT
  sbt Docker/stage

# Generate a whole head's keys + configs into the config layout:
#   OUTDIR/bootstrap/{roster.json, defaults.json, l2-cardano-eutxo.json}
#   OUTDIR/private/{head,coil}-N/private.json
# Coil peers are hubbed round-robin across the head peers. Next: `just head-zero-address` and fund
# it, edit bootstrap/l2-cardano-eutxo.json, run `just deploy-scripts-and-g2-setup`, then
# `just build-head-config`.
keygen-fleet HEADS COILS QUORUM OUTDIR="config/demo": _require-launcher
  #!/usr/bin/env bash
  trap 'just notify "keygen-fleet"' EXIT
  {{hydrozoa}} keygen-fleet {{HEADS}} {{COILS}} {{QUORUM}} --out-dir {{OUTDIR}}

# Print head peer 0's L1 funding address (derived from the roster + defaults on demand — no
# address files to go stale).
head-zero-address BOOTSTRAP_DIR="config/demo/bootstrap": _require-launcher
  #!/usr/bin/env bash
  trap 'just notify "head-zero-address"' EXIT
  {{hydrozoa}} head-zero-address --bootstrap-dir {{BOOTSTRAP_DIR}}

# Deploy the treasury + dispute validators (and, unless reused, the G2 setup ladder), funded by
# WALLET (a keygen private config, e.g. config/demo/private/head-0/private.json; change returns
# to it). The network is derived from the Blockfrost key (read from the .local template, else
# $BLOCKFROST_API_KEY). Writes the reference inputs to OUT for build-head-config. Pass
# LADDER_REFS (an existing script-refs.json) to reuse the already-deployed ladder and redeploy
# only the validators.
deploy-scripts-and-g2-setup WALLET OUT="config/demo/bootstrap/script-refs.json" LADDER_REFS="": _require-launcher
  #!/usr/bin/env bash
  set -euo pipefail
  trap 'just notify "deploy-scripts-and-g2-setup"' EXIT
  template="config/template/peer-private.template.json.local"
  key="${BLOCKFROST_API_KEY:-}"
  if [ -f "$template" ]; then key=$(sed -n 's/.*"blockfrostApiKey"[^"]*"\([^"]*\)".*/\1/p' "$template"); fi
  if [ -z "$key" ]; then echo "error: no Blockfrost key — create $template (deployment guide step 1) or export BLOCKFROST_API_KEY" >&2; exit 1; fi
  args=(--wallet {{WALLET}} --blockfrost-key "$key" --out {{OUT}})
  if [ -n "{{LADDER_REFS}}" ]; then args+=(--ladder-refs {{LADDER_REFS}}); fi
  {{hydrozoa}} deploy-scripts-and-g2-setup "${args[@]}"

# Build the shared head-config.json from the bootstrap directory's four files (roster, defaults,
# l2-cardano-eutxo, script-refs). Reads the Blockfrost key from the .local template (else
# $BLOCKFROST_API_KEY); head peer 0's address must be funded on the target network first (the
# tool logs the exact lovelace required and fails with the shortfall if not).
build-head-config BOOTSTRAP_DIR="config/demo/bootstrap" OUT="config/demo/head-config/head-config.json": _require-launcher
  #!/usr/bin/env bash
  set -euo pipefail
  trap 'just notify "build-head-config"' EXIT
  template="config/template/peer-private.template.json.local"
  key="${BLOCKFROST_API_KEY:-}"
  if [ -f "$template" ]; then key=$(sed -n 's/.*"blockfrostApiKey"[^"]*"\([^"]*\)".*/\1/p' "$template"); fi
  if [ -z "$key" ]; then echo "error: no Blockfrost key — create $template (deployment guide step 1) or export BLOCKFROST_API_KEY" >&2; exit 1; fi
  mkdir -p "$(dirname {{OUT}})"
  {{hydrozoa}} build-head-config {{BOOTSTRAP_DIR}} --blockfrost-key "$key" --out {{OUT}}

# Run a head node in the foreground from a generated head-config + a peer's private config.
serve HEAD_CONFIG PRIVATE_CONFIG: _require-launcher
  {{hydrozoa}} serve {{HEAD_CONFIG}} {{PRIVATE_CONFIG}}

# Interactively build, sign, and submit an L2 transaction to a running head: pick a peer key,
# pick one of its L2 utxos, enter destination + value.
submit-l2-tx CONFIG_DIR="config/demo" HEAD_URI="http://localhost:8080": _require-launcher
  {{hydrozoa}} submit-l2-tx --config-dir {{CONFIG_DIR}} --head-uri {{HEAD_URI}}

# Interactively deposit into a running head: pick a peer key, pick one of its L1 utxos, enter the
# L2 outputs to spawn; registers with the head, then submits the deposit tx to L1 via Blockfrost.
submit-deposit CONFIG_DIR="config/demo" HEAD_URI="http://localhost:8080": _require-launcher
  {{hydrozoa}} submit-deposit --config-dir {{CONFIG_DIR}} --head-uri {{HEAD_URI}}

export:
  #!/usr/bin/env bash
  trap 'just notify "export"' EXIT
  sbt "runMain hydrozoa.rulebased.ledger.l1.script.plutus.Export"

export-test:
  #!/usr/bin/env bash
  trap 'just notify "export-test"' EXIT
  sbt "testOnly *ExportTest*"

migrate ADDRESS: _require-launcher
  #!/usr/bin/env bash
  trap 'just notify "migrate"' EXIT
  {{hydrozoa}} migrate {{ADDRESS}}

integration-fast:
  #!/usr/bin/env bash
  trap 'just notify "integration-fast"' EXIT
  sbt "integration/testOnly * -- -s 10 -f ^(?!.*\(extended\))"

integration:
  #!/usr/bin/env bash
  trap 'just notify "integration"' EXIT
  sbt "integration/test"

integration-yaci:
  #!/usr/bin/env bash
  trap 'just notify "integration-yaci"' EXIT
  sbt "integration/testOnly hydrozoa.integration.stage1.Stage1PropertiesYaci"

precommit: lint-check fmt-check nixfmt-check
  just notify "precommit"

# Like precommit, but cleans first — matches CI's fresh-target behaviour so
# stale SemanticDB can't hide unused-import / lint failures.
ci-check:
  #!/usr/bin/env bash
  trap 'just notify "ci-check"' EXIT
  CI=true sbt "clean; fmtCheckAll; lintCheckAll"
  just nixfmt-check

prepush: precommit test integration-fast build-werror
  just notify "prepush"

notify name:
  if [ -f .just/notify ]; \
  then notify-send -i "{{justfile_dir()}}/.just/notify-icon.jpg" -a "Hydrozoa Justfile" "{{name}} finished"; \
  fi
