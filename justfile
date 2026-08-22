#!/usr/bin/env just --justfile

# The packaged `hydrozoa` launcher the deployment recipes invoke. Built by `just stage` (no sbt in
# the hot path, so each command starts fast and prints clean output). Run from the repo root so
# relative config paths (head/demo) resolve.
hydrozoa := "target/universal/stage/bin/hydrozoa"

# The head workspace directory the deployment recipes read from. Read from the environment, default
# head/demo. To target another head — e.g. per-network release homes — export it once, or prefix
# the command:
#   export HYDROZOA_HOME=./head/release/preview
#   just scaffold && just keygen-fleet 2 4 2      # both act on ./head/release/preview
# or inline:
#   HYDROZOA_HOME=./head/release/preprod just deploy-scripts-and-g2-setup
export HYDROZOA_HOME := env_var_or_default("HYDROZOA_HOME", "head/demo")

# This justfile is configured to send notifications when commands complete.
# To enable this, add a `./.just/notify` file.
#
# Each recipe uses a bash shebang + `trap '... EXIT'` so that `just notify`
# fires whether the command succeeds or fails, while still propagating the
# original exit status to just (so CI and pre-push hooks see failures).

# ================================ Development ================================

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
  CI=true sbt "Test/compile; integration/Test/compile"

integration-fast:
  #!/usr/bin/env bash
  trap 'just notify "integration-fast"' EXIT
  # The -s 10 / (extended) filter now live in build.sbt, scoped to the ScalaCheck framework —
  # passing them here handed them to ScalaTest too, which rejects -s and fails the run.
  sbt "integration/testOnly *"

integration:
  #!/usr/bin/env bash
  trap 'just notify "integration"' EXIT
  sbt "integration/test"

integration-yaci:
  #!/usr/bin/env bash
  trap 'just notify "integration-yaci"' EXIT
  HYDROZOA_INCLUDE_HEAVY_TESTS=1 sbt "integration/testOnly hydrozoa.integration.stage1.Stage1PropertiesYaci"

# Yaci suites that spin up their own devnet via Testcontainers (require Docker).
# Bypasses the build.sbt Tests.Exclude that keeps these out of `just integration`.
#
# The probes and the RBR MBT run in SEPARATE sbt/JVM invocations on purpose: they share a JVM-wide
# singleton devnet container (the probes via `YaciDevnet.resource`, the MBT via `acquireShared`
# without a reset). In one JVM the probes' script deploy spends head-peer-0's genesis inputs before
# the MBT's own deploy, which then fails "all inputs are spent". A fresh JVM per group gives each a
# pristine container.
integration-yaci-docker:
  #!/usr/bin/env bash
  set -eo pipefail
  trap 'just notify "integration-yaci-docker"' EXIT
  sbt "; set integration/Test/testOptions := Seq() ; integration/testOnly hydrozoa.integration.yaci.*"
  sbt "; set integration/Test/testOptions := Seq() ; integration/testOnly hydrozoa.integration.rbr.mbt.RbrMbtPropertiesYaci"

# Heavy Docker smoke-test on the shipped topology — 2 head + 4 coil peers against a local Yaci
# devnet: builds the image, stages the launcher, then runs the CI-excluded DockerSmokeTest. Needs a
# running Docker; minutes-long. See the E2E section of docs/spec/integration-stages.md.
integration-e2e-docker:
  #!/usr/bin/env bash
  trap 'just notify "integration-e2e-docker"' EXIT
  HYDROZOA_INCLUDE_HEAVY_TESTS=1 sbt "Docker/publishLocal; stage; integration/testOnly hydrozoa.integration.e2e.DockerSmokeTest"

# Recompile and export the on-chain script blueprint to src/main/resources/hydrozoa/scripts/plutus.json.
export:
  #!/usr/bin/env bash
  trap 'just notify "export"' EXIT
  sbt "runMain hydrozoa.rulebased.ledger.l1.script.plutus.Export"

export-test:
  #!/usr/bin/env bash
  trap 'just notify "export-test"' EXIT
  sbt "testOnly *ExportTest*"

# Render the RBR HLPN net to an SVG and open it in a browser. Runs the DOT visualizer test
# (writes target/rbr-net.dot), renders it with graphviz, then opens it via $BROWSER (else xdg-open).
graphviz:
  #!/usr/bin/env bash
  set -euo pipefail
  trap 'just notify "graphviz"' EXIT
  sbt "integration/testOnly hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNetDotTest"
  # one SVG per transition, gathered into a single scrollable index page
  for f in target/rbr-net/*.dot; do dot -Tsvg "$f" -o "${f%.dot}.svg"; done
  {
    echo "<html><body style='font-family:monospace'><h1>RBR net — per transition</h1>"
    for f in target/rbr-net/*.svg; do
      echo "<h3>$(basename "${f%.svg}")</h3><img src='$(basename "$f")'>"
    done
    echo "</body></html>"
  } > target/rbr-net/index.html
  echo "wrote target/rbr-net/index.html"
  "${BROWSER:-xdg-open}" target/rbr-net/index.html
# RBR MBT against the public Preview testnet (real Blockfrost). Requires a valid Blockfrost key
# (hardcoded in the Runner) and a funded master wallet: generate one with `hydrozoa keygen`, fund its
# printed address from the Preview faucet, then export RBR_MBT_PREVIEW_MASTER_SIGNING_KEY first.
# Bypasses the build.sbt Tests.Exclude that keeps it out of `just integration`.
integration-rbr-preview:
  #!/usr/bin/env bash
  set -eo pipefail
  trap 'just notify "integration-rbr-preview"' EXIT
  sbt "; set integration/Test/testOptions := Seq() ; integration/testOnly hydrozoa.integration.rbr.mbt.RbrMbtPropertiesPublic"

# Fail if any project registers ScalaCheck's own sbt framework instead of the wrapper that keeps
# a suite's property events in one batch. See `ScalaCheckFrameworkFixed` for why that matters.
scalacheck-framework-check:
  sbt checkScalaCheckFramework

precommit: lint-check fmt-check nixfmt-check scalacheck-framework-check
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

# Build the packaged `hydrozoa` launcher (target/universal/stage/bin/hydrozoa). Run this once after
# changing code; the deployment recipes below then invoke it directly, with no sbt startup cost.
stage:
  #!/usr/bin/env bash
  trap 'just notify "stage"' EXIT
  # native-packager's `stage` doesn't rewrite bin/hydrozoa when its content is unchanged, so its
  # mtime would keep pointing at the first-ever stage. Touch it so `_require-launcher`'s staleness
  # check sees a real "last staged" time.
  sbt stage && touch "{{hydrozoa}}"

# Build the hydrozoa Docker image locally — tagged cardano-hydrozoa/hydrozoa:<version> plus
# ghcr.io/cardano-hydrozoa/hydrozoa at :<version> and :latest, so the composition's default image
# (…:latest) resolves to this local build without a HYDROZOA_IMAGE override. Publish via RELEASE.md.
docker-image:
  #!/usr/bin/env bash
  trap 'just notify "docker-image"' EXIT
  sbt Docker/publishLocal

# Write the Docker build context to target/docker/stage without building it (what the release workflow builds).
docker-stage:
  #!/usr/bin/env bash
  trap 'just notify "docker-stage"' EXIT
  sbt Docker/stage

# ================================ Deployment ================================
#
# Every recipe below runs the staged `hydrozoa` launcher against $HYDROZOA_HOME (see the variable at
# the top). They depend on `_require-launcher`, which errors if the launcher is missing and warns if
# it looks stale relative to the sources.

# Fail if the launcher isn't built; warn if sources changed since it was staged (it may be stale).
_require-launcher:
  #!/usr/bin/env bash
  if [ ! -x "{{hydrozoa}}" ]; then
    echo "error: the 'hydrozoa' launcher isn't built — run 'just stage' first" >&2
    exit 1
  fi
  stale=$(find src cardano-onchain/src build.sbt project/plugins.sbt project/build.properties \
            -type f \( -name '*.scala' -o -name '*.sbt' -o -name '*.properties' \) \
            -newer "{{hydrozoa}}" 2>/dev/null | head -n1)
  if [ -n "$stale" ]; then
    echo "warning: sources changed since the last 'just stage' (e.g. $stale);" \
         "the launcher may be stale — re-run 'just stage'" >&2
  fi

# Scaffold the head workspace files into $HYDROZOA_HOME (default head/demo): docker-compose.yml,
# hydrozoa.sh, and template/peer-private.template.json.local (fill in the Blockfrost key). The
# scaffold dir is itself the head home the later recipes act on, so a per-network deploy is just
# `HYDROZOA_HOME=./head/release/preview just scaffold` (or export it once). Existing files are skipped.
scaffold: _require-launcher
  {{hydrozoa}} scaffold {{HYDROZOA_HOME}}

# Extra ARGS go straight to the launcher — e.g. `--cardano-network-file` for a chain with no
# baked-in description.
# Generate a whole head's keys + configs into $HYDROZOA_HOME (default head/demo):
#   $HYDROZOA_HOME/bootstrap/{roster.json, defaults.json, l2-cardano-eutxo.json}
#   $HYDROZOA_HOME/private/{head,coil}-N/private.json
# Coil peers are hubbed round-robin across the head peers. Next: `just head-zero-address` and fund
# it, edit bootstrap/l2-cardano-eutxo.json, run `just deploy-scripts-and-g2-setup`, then
# `just build-head-config`.
keygen-fleet HEADS COILS QUORUM *ARGS: _require-launcher
  #!/usr/bin/env bash
  {{hydrozoa}} keygen-fleet {{HEADS}} {{COILS}} {{QUORUM}} --home {{HYDROZOA_HOME}} {{ARGS}}

# Print head peer 0's L1 funding address (derived from the roster + defaults on demand — no
# address files to go stale).
head-zero-address: _require-launcher
  #!/usr/bin/env bash
  {{hydrozoa}} head-zero-address --home {{HYDROZOA_HOME}}

# Extra ARGS go straight to the launcher — e.g. `--blockfrost-url` to reach a devnet at its
# host-mapped port. LADDER_REFS comes first, so pass "" for it when adding ARGS.
# Deploy the treasury + dispute validators (and, unless reused, the G2 setup ladder), funded by
# head peer 0's wallet under $HYDROZOA_HOME (change returns to it). The network is derived from the Blockfrost
# key (read from the .local template, else $BLOCKFROST_API_KEY). Writes $HYDROZOA_HOME/bootstrap/ref-utxos.json
# for build-head-config. Pass LADDER_REFS (an existing ref-utxos.json) to reuse the already-deployed
# ladder and redeploy only the validators.
deploy-scripts-and-g2-setup LADDER_REFS="" *ARGS: _require-launcher
  #!/usr/bin/env bash
  set -euo pipefail
  trap 'just notify "deploy-scripts-and-g2-setup"' EXIT
  template="{{HYDROZOA_HOME}}/template/peer-private.template.json.local"
  key="${BLOCKFROST_API_KEY:-}"
  if [ -f "$template" ]; then key=$(sed -n 's/.*"blockfrostApiKey"[^"]*"\([^"]*\)".*/\1/p' "$template"); fi
  if [ -z "$key" ]; then echo "error: no Blockfrost key — create $template (deployment guide step 1) or export BLOCKFROST_API_KEY" >&2; exit 1; fi
  args=(--home {{HYDROZOA_HOME}} --blockfrost-key "$key")
  if [ -n "{{LADDER_REFS}}" ]; then args+=(--ladder-refs {{LADDER_REFS}}); fi
  {{hydrozoa}} deploy-scripts-and-g2-setup "${args[@]}" {{ARGS}}

# Extra ARGS go straight to the launcher — e.g. `--blockfrost-url` to reach a devnet at its
# host-mapped port.
# Build the shared head-config.json from $HYDROZOA_HOME's bootstrap files (roster, defaults, l2-cardano-eutxo,
# ref-utxos), writing $HYDROZOA_HOME/head-config/head-config.json. Reads the Blockfrost key from the .local
# template (else $BLOCKFROST_API_KEY); head peer 0's address must be funded on the target network
# first (the tool logs the exact lovelace required and fails with the shortfall if not).
build-head-config *ARGS: _require-launcher
  #!/usr/bin/env bash
  set -euo pipefail
  trap 'just notify "build-head-config"' EXIT
  template="{{HYDROZOA_HOME}}/template/peer-private.template.json.local"
  key="${BLOCKFROST_API_KEY:-}"
  if [ -f "$template" ]; then key=$(sed -n 's/.*"blockfrostApiKey"[^"]*"\([^"]*\)".*/\1/p' "$template"); fi
  if [ -z "$key" ]; then echo "error: no Blockfrost key — create $template (deployment guide step 1) or export BLOCKFROST_API_KEY" >&2; exit 1; fi
  {{hydrozoa}} build-head-config --home {{HYDROZOA_HOME}} --blockfrost-key "$key" {{ARGS}}

# Drive a local Yaci DevKit devnet as the head's L1 (dev/testing — no Blockfrost key, no funded
# testnet wallet). Commands: `up`, `network [OUT]`, `topup ADDRESS [ADA]`, `down`; run without
# arguments for the full usage. The Docker smoke-test calls the same script.
yaci-devnet *ARGS:
  scripts/yaci-devnet.sh {{ARGS}}

# Run a head node in the foreground from a generated head-config + a peer's private config.
serve HEAD_CONFIG PRIVATE_CONFIG: _require-launcher
  {{hydrozoa}} serve {{HEAD_CONFIG}} {{PRIVATE_CONFIG}}

# The Docker composition under $HYDROZOA_HOME. Each peer gets a named-volume RocksDB store (durable
# across restarts — what `evacuate` reads), and the `evacuate` profile adds one `*-evac` service per
# peer. Needs head-config.json + the private/ configs generated first. Set HYDROZOA_HOME to the head
# dir (e.g. `HYDROZOA_HOME=./head/evacuate`).

# Bring the head up (detached) with durable per-peer stores.
head-up:
  #!/usr/bin/env bash
  set -euo pipefail
  cd {{HYDROZOA_HOME}}
  docker compose up -d

# Stop and remove the head containers, keeping the persisted stores (add `-v` to the command to wipe).
head-down:
  #!/usr/bin/env bash
  set -euo pipefail
  cd {{HYDROZOA_HOME}}
  docker compose down

# Fresh head: remove ALL containers (serve + evacuate profile) and orphans AND wipe the volume stores.
head-reset:
  #!/usr/bin/env bash
  set -euo pipefail
  cd {{HYDROZOA_HOME}}
  docker compose --profile evacuate down -v --remove-orphans

# Evacuate the whole head: stop the serve peers, then bring up the evacuate profile (all peers).
evacuate:
  #!/usr/bin/env bash
  set -euo pipefail
  cd {{HYDROZOA_HOME}}
  docker compose stop
  docker compose --profile evacuate up

# Drive the head's evacuation from a single peer ad-hoc (default head-0), reusing its store.
evacuate-peer PEER="head-0":
  #!/usr/bin/env bash
  set -euo pipefail
  cd {{HYDROZOA_HOME}}
  docker compose run --rm {{PEER}} evacuate /configs/head-config.json /configs/private.json

# Interactively build, sign, and submit an L2 transaction to a running head: pick a peer key,
# pick one of its L2 utxos, enter destination + value.
submit-l2-tx HEAD_URI="http://localhost:8080": _require-launcher
  {{hydrozoa}} submit-l2-tx --home {{HYDROZOA_HOME}} --head-uri {{HEAD_URI}}

# Interactively deposit into a running head: pick a peer key, pick one of its L1 utxos, enter the
# L2 outputs to spawn; registers with the head, then submits the deposit tx to L1 via Blockfrost.
submit-deposit HEAD_URI="http://localhost:8080": _require-launcher
  {{hydrozoa}} submit-deposit --home {{HYDROZOA_HOME}} --head-uri {{HEAD_URI}}

# Move the treasury to a new peer set / head id via a transfer tx submitted to the given address.
migrate ADDRESS: _require-launcher
  #!/usr/bin/env bash
  trap 'just notify "migrate"' EXIT
  {{hydrozoa}} migrate {{ADDRESS}}

# ================================== Internal =================================

notify name:
  @if [ -f .just/notify ]; \
  then notify-send -i "{{justfile_dir()}}/.just/notify-icon.jpg" -a "Hydrozoa Justfile" "{{name}} finished"; \
  fi
