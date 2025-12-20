#!/usr/bin/env just --justfile

# This justfile is configured to send notifications when commands complete.
# To enable this, add a `./.just/notify` file.

fmt:
  - sbt scalafmtAll
  just notify "fmt"

fmt-check:
  - sbt scalafmtCheck
  just notify "fmt-check"

lint:
  - sbt scalafixAll
  just notify "lint"

lint-check:
  - sbt "scalafixAll --check"
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

[parallel]
precommit: lint-check fmt-check nixfmt-check test
  just notify "precommit"

notify name:
  if [ -f .just/notify ]; \
  then notify-send -i "{{justfile_dir()}}/.just/notify-icon.jpg" -a "Hydrozoa Justfile" "{{name}} finished"; \
  fi
