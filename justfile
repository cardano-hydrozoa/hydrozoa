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
  CI=true sbt Test/compile integration/Test/compile

keygen *ARGS:
  #!/usr/bin/env bash
  trap 'just notify "keygen"' EXIT
  sbt "runMain hydrozoa.bootstrap.GenerateKeyPair {{ARGS}}"

# Generate a whole head's keys + configs in one sbt run: the peer-topology roster (roster.json), a
# private config + L1 address per peer under OUTDIR/{head,coil}-N/, plus bootstrap-defaults.json
# (network + coil quorum) and an l2-state.json template. Coil peers are hubbed round-robin across
# the head peers. Next: fund OUTDIR/head-0/address.txt, edit l2-state.json, run
# `just deploy-reference-scripts`, then `just build-bootstrap` and `just build-head-config`.
keygen-fleet HEADS COILS QUORUM OUTDIR="config/demo":
  #!/usr/bin/env bash
  set -euo pipefail
  trap 'just notify "keygen-fleet"' EXIT
  outdir="{{OUTDIR}}"
  if [ -e "$outdir/roster.json" ]; then
    echo "error: $outdir/roster.json already exists; move it away or pick another OUTDIR" >&2
    exit 1
  fi
  cmds=()
  for i in $(seq 0 $(( {{HEADS}} - 1 ))); do
    cmds+=("runMain hydrozoa.bootstrap.GenerateKeyPair --roster $outdir/roster.json --role head --ws-address ws://head-$i:4001 --template peer-private.template.json --out $outdir/head-$i/private.json")
  done
  for i in $(seq 0 $(( {{COILS}} - 1 ))); do
    cmds+=("runMain hydrozoa.bootstrap.GenerateKeyPair --roster $outdir/roster.json --role coil --hub $(( i % {{HEADS}} )) --template peer-private.template.json --out $outdir/coil-$i/private.json")
  done
  cmds+=("runMain hydrozoa.bootstrap.InitBootstrapFiles $outdir/roster.json --out-dir $outdir --coil-quorum {{QUORUM}}")
  sbt "${cmds[@]}"

# Deploy the treasury + dispute validators as reference scripts on Preview, funded by
# WALLET (a keygen private config, e.g. config/demo/head-0/private.json; change returns
# to it). Writes the reference inputs to OUT for build-head-config. One deployment
# serves every head until the compiled scripts change. Reads $BLOCKFROST_API_KEY.
deploy-reference-scripts WALLET OUT="config/demo/script-refs.json":
  #!/usr/bin/env bash
  trap 'just notify "deploy-reference-scripts"' EXIT
  sbt "runMain hydrozoa.app.DeployReferenceScripts --wallet {{WALLET}} --out {{OUT}}"

# Assemble bootstrap.json (pure, no backend) from the keygen-fleet roster + defaults + l2-state and
# the deployed script refs. Defaults to the config/demo artifacts.
build-bootstrap ROSTER="config/demo/roster.json" SCRIPT_REFS="config/demo/script-refs.json" DEFAULTS="config/demo/bootstrap-defaults.json" L2_STATE="config/demo/l2-state.json" OUT="config/demo/bootstrap.json":
  #!/usr/bin/env bash
  trap 'just notify "build-bootstrap"' EXIT
  sbt "runMain hydrozoa.bootstrap.BuildBootstrapConfig {{ROSTER}} --script-refs {{SCRIPT_REFS}} --defaults {{DEFAULTS}} --l2-state {{L2_STATE}} --out {{OUT}}"

# Build the shared head-config.json from a complete bootstrap.json (from build-bootstrap). Reads the
# Blockfrost key from $BLOCKFROST_API_KEY; head peer 0's address must be funded on Preview first (the
# tool logs the exact lovelace required and fails with the shortfall if not).
build-head-config BOOTSTRAP OUT EQUITY:
  #!/usr/bin/env bash
  trap 'just notify "build-head-config"' EXIT
  sbt "runMain hydrozoa.bootstrap.BuildHeadConfig {{BOOTSTRAP}} --equity {{EQUITY}} --out {{OUT}}"

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
