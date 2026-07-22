#!/usr/bin/env just --justfile

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
  CI=true sbt Test/compile integration/Test/compile examples/Test/compile

# Generate a whole head's keys + configs in one sbt run, into the config layout:
#   OUTDIR/bootstrap/{roster.json, defaults.json, l2-cardano-eutxo.json}
#   OUTDIR/private/{head,coil}-N/private.json
# Coil peers are hubbed round-robin across the head peers. Next: `just head-zero-address` and fund
# it, edit bootstrap/l2-cardano-eutxo.json, run `just deploy-scripts-and-g2-setup`, then
# `just build-head-config`.
keygen-fleet HEADS COILS QUORUM OUTDIR="config/demo":
  #!/usr/bin/env bash
  set -euo pipefail
  trap 'just notify "keygen-fleet"' EXIT
  outdir="{{OUTDIR}}"
  if [ -e "$outdir/bootstrap/roster.json" ]; then
    echo "error: $outdir/bootstrap/roster.json already exists; move it away or pick another OUTDIR" >&2
    exit 1
  fi
  template="config/template/peer-private.template.json.local"
  if [ ! -f "$template" ]; then
    echo "error: $template not found — copy config/template/peer-private.template.json to it and set blockfrostApiKey" >&2
    exit 1
  fi
  key=$(sed -n 's/.*"blockfrostApiKey"[^"]*"\([^"]*\)".*/\1/p' "$template")
  case "$key" in
    preview*) network=preview;;
    preprod*) network=preprod;;
    mainnet*) network=mainnet;;
    *) echo "error: cannot derive the network from blockfrostApiKey in $template (expected a preview…/preprod…/mainnet… key)" >&2; exit 1;;
  esac
  echo "network (from the Blockfrost key): $network"
  cmds=()
  for i in $(seq 0 $(( {{HEADS}} - 1 ))); do
    cmds+=("runMain hydrozoa.bootstrap.GenerateKeyPair --roster $outdir/bootstrap/roster.json --role head --ws-address ws://head-$i:4001 --template $template --out $outdir/private/head-$i/private.json")
  done
  for i in $(seq 0 $(( {{COILS}} - 1 ))); do
    cmds+=("runMain hydrozoa.bootstrap.GenerateKeyPair --roster $outdir/bootstrap/roster.json --role coil --hub $(( i % {{HEADS}} )) --template $template --out $outdir/private/coil-$i/private.json")
  done
  cmds+=("runMain hydrozoa.bootstrap.InitBootstrapFiles $outdir/bootstrap/roster.json --out-dir $outdir/bootstrap --coil-quorum {{QUORUM}} --cardano-network $network")
  sbt "${cmds[@]}"

# Print head peer 0's L1 funding address (derived from the roster + defaults on demand — no
# address files to go stale).
head-zero-address BOOTSTRAP_DIR="config/demo/bootstrap":
  #!/usr/bin/env bash
  trap 'just notify "head-zero-address"' EXIT
  sbt "runMain hydrozoa.bootstrap.PrintHeadZeroAddress --bootstrap-dir {{BOOTSTRAP_DIR}}"

# Deploy the treasury + dispute validators (and, unless reused, the G2 setup ladder), funded by
# WALLET (a keygen private config, e.g. config/demo/private/head-0/private.json; change returns
# to it). The network is derived from the Blockfrost key (read from the .local template, else
# $BLOCKFROST_API_KEY). Writes the reference inputs to OUT for build-head-config. Pass
# LADDER_REFS (an existing script-refs.json) to reuse the already-deployed ladder and redeploy
# only the validators.
deploy-scripts-and-g2-setup WALLET OUT="config/demo/bootstrap/script-refs.json" LADDER_REFS="":
  #!/usr/bin/env bash
  set -euo pipefail
  trap 'just notify "deploy-scripts-and-g2-setup"' EXIT
  template="config/template/peer-private.template.json.local"
  key="${BLOCKFROST_API_KEY:-}"
  if [ -f "$template" ]; then key=$(sed -n 's/.*"blockfrostApiKey"[^"]*"\([^"]*\)".*/\1/p' "$template"); fi
  if [ -z "$key" ]; then echo "error: no Blockfrost key — create $template (deployment guide step 1) or export BLOCKFROST_API_KEY" >&2; exit 1; fi
  args=(--wallet {{WALLET}} --blockfrost-key "$key" --out {{OUT}})
  if [ -n "{{LADDER_REFS}}" ]; then args+=(--ladder-refs {{LADDER_REFS}}); fi
  sbt "runMain hydrozoa.app.DeployScriptsAndG2Setup ${args[*]}"

# Build the shared head-config.json from the bootstrap directory's four files (roster, defaults,
# l2-cardano-eutxo, script-refs). Reads the Blockfrost key from the .local template (else
# $BLOCKFROST_API_KEY); head peer 0's address must be funded on the target network first (the
# tool logs the exact lovelace required and fails with the shortfall if not).
build-head-config BOOTSTRAP_DIR="config/demo/bootstrap" OUT="config/demo/head-config/head-config.json":
  #!/usr/bin/env bash
  set -euo pipefail
  trap 'just notify "build-head-config"' EXIT
  template="config/template/peer-private.template.json.local"
  key="${BLOCKFROST_API_KEY:-}"
  if [ -f "$template" ]; then key=$(sed -n 's/.*"blockfrostApiKey"[^"]*"\([^"]*\)".*/\1/p' "$template"); fi
  if [ -z "$key" ]; then echo "error: no Blockfrost key — create $template (deployment guide step 1) or export BLOCKFROST_API_KEY" >&2; exit 1; fi
  mkdir -p "$(dirname {{OUT}})"
  sbt "runMain hydrozoa.bootstrap.BuildHeadConfig {{BOOTSTRAP_DIR}} --blockfrost-key $key --out {{OUT}}"

# Interactively build, sign, and submit an L2 transaction to a running head: pick a peer key,
# pick one of its L2 utxos, enter destination + value.
submit-l2-tx CONFIG_DIR="config/demo" HEAD_URI="http://localhost:8080":
  sbt "examples/runMain hydrozoa.examples.demo.SubmitL2Transaction --config-dir {{CONFIG_DIR}} --head-uri {{HEAD_URI}}"

# Interactively deposit into a running head: pick a peer key, pick one of its L1 utxos, enter the
# L2 outputs to spawn; registers with the head, then submits the deposit tx to L1 via Blockfrost.
deposit CONFIG_DIR="config/demo" HEAD_URI="http://localhost:8080":
  sbt "examples/runMain hydrozoa.examples.demo.SubmitDeposit --config-dir {{CONFIG_DIR}} --head-uri {{HEAD_URI}}"

export:
  #!/usr/bin/env bash
  trap 'just notify "export"' EXIT
  sbt "runMain hydrozoa.rulebased.ledger.l1.script.plutus.Export"

export-test:
  #!/usr/bin/env bash
  trap 'just notify "export-test"' EXIT
  sbt "testOnly *ExportTest*"

migrate ADDRESS:
  #!/usr/bin/env bash
  trap 'just notify "migrate"' EXIT
  sbt "runMain hydrozoa.bootstrap.Migrate {{ADDRESS}}"

# Render the RBR HLPN net to an SVG and open it in a browser. Runs the DOT visualizer test
# (writes target/rbr-net.dot), renders it with graphviz, then opens it via $BROWSER (else xdg-open).
graphviz:
  #!/usr/bin/env bash
  set -euo pipefail
  trap 'just notify "graphviz"' EXIT
  sbt "integration/testOnly hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNetDotTest"
  dot -Tsvg target/rbr-net.dot -o target/rbr-net.svg
  echo "wrote target/rbr-net.svg"
  "${BROWSER:-xdg-open}" target/rbr-net.svg

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
