#!/usr/bin/env just --justfile

# This justfile is configured to send notifications when commands complete.
# To enable this, add a `./.just/notify` file.

fmt:
  - sbt fmtAll
  just notify "fmt"

fmt-check:
  - sbt fmtCheckAll
  just notify "fmt-check"

lint:
  - sbt lintAll
  just notify "lint"

lint-check:
  - sbt lintCheckAll
  just notify "lint-check"

nixfmt:
  - nixfmt flake.nix
  just notify "nixfmt"

nixfmt-check:
  - nixfmt flake.nix --check
  just notify "nixfmt-check"

test:
  - sbt test
  just notify "test"

integration-fast:
  - sbt "integration/testOnly * -- -s 10"
  just notify "integration-fast"

integration:
  - sbt "integration/test"
  just notify "integration"

integration-yaci:
  - sbt "integration/testOnly hydrozoa.integration.stage1.Stage1PropertiesYaci"
  just notify "integration-yaci"

[parallel]
precommit: lint-check fmt-check nixfmt-check
  just notify "precommit"

[parallel]
prepush: precommit test integration-fast
  just notify "prepush"

notify name:
  if [ -f .just/notify ]; \
  then notify-send -i "{{justfile_dir()}}/.just/notify-icon.jpg" -a "Hydrozoa Justfile" "{{name}} finished"; \
  fi
