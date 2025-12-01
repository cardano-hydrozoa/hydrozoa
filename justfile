#!/usr/bin/env just --justfile

fmt:
  sbt scalafmtAll

fmt-check:
  sbt scalafmtCheck

lint:
  sbt scalafixAll

lint-check:
  sbt "scalafixAll --check"

lint-fmt-check:
  just lint-check
  just fmt-check  
