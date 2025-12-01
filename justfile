#!/usr/bin/env just --justfile

fmt:
  sbt scalafmtAll

fmt-check:
  sbt scalafmtCheck

lint:
  sbt scalafixAll

lint-check:
  sbt "scalafixAll --check"

nixfmt:
  nixfmt flake.nix

nixfmt-check:
  nixfmt flake.nix --check	

precommit:
  just lint-check
  just fmt-check  
  just nixfmt-check
