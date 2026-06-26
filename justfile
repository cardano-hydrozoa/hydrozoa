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
  CI=true sbt Test/compile integration/Test/compile timingViz/Test/compile

# One-command demo of the timing-rules visualizer. Starts the Scala backend on :8765, loads
# the sample command sequence, and brings up the Vite dev server on :5173. Ctrl-C tears
# everything down.
timing-viz-demo:
  #!/usr/bin/env bash
  set -e
  pids=()
  cleanup() {
    echo ""
    echo "Stopping demo..."
    for pid in "${pids[@]}"; do kill "$pid" 2>/dev/null || true; done
    wait 2>/dev/null || true
  }
  trap cleanup EXIT INT TERM

  echo "==> Installing frontend deps (if needed)..."
  (cd timing-viz/frontend && npm install --silent)

  echo "==> Starting backend on :8765..."
  sbt "timingViz/runMain hydrozoa.timingviz.Server" > /tmp/timing-viz-server.log 2>&1 &
  pids+=($!)

  echo -n "==> Waiting for backend..."
  until curl -s http://localhost:8765/frame > /dev/null 2>&1; do
    sleep 1
    echo -n "."
  done
  echo " up."

  echo "==> Loading sample commands..."
  curl -s -X POST http://localhost:8765/replay \
       -H 'Content-Type: application/json' \
       -d @timing-viz/examples/commands.json > /dev/null

  echo "==> Starting frontend dev server on :5173..."
  (cd timing-viz/frontend && npm run dev) &
  pids+=($!)

  echo ""
  echo "  Demo running."
  echo "    Frontend:  http://localhost:5173"
  echo "    Backend:   http://localhost:8765/frame"
  echo "    Logs:      /tmp/timing-viz-server.log"
  echo "  Ctrl-C to stop."
  wait

keygen:
  #!/usr/bin/env bash
  trap 'just notify "keygen"' EXIT
  sbt "runMain hydrozoa.app.GenerateKeyPair"

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
  sbt "runMain hydrozoa.app.Migrate {{ADDRESS}}"

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
