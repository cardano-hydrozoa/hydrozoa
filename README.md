# Hydrozoa

[![spec: hydrozoa.pdf](https://img.shields.io/badge/spec-hydrozoa.pdf-blue)](https://cardano-hydrozoa.github.io/hydrozoa/hydrozoa.pdf)
[![code license: Apache-2.0](https://img.shields.io/badge/code%20license-Apache--2.0-seagreen)](https://www.apache.org/licenses/LICENSE-2.0)
[![docs + spec license: CC BY-SA 4.0](https://img.shields.io/badge/docs%2Fspec%20license-CC%20BY--SA%204.0-seagreen)](https://creativecommons.org/licenses/by-sa/4.0/)

Hydrozoa is a lightweight multi-party state channel implementation for Cardano,
written in Scala 3.

> [!WARNING]
>
> Hydrozoa is **pre-alpha** and under active development. It is not
> production-ready, internal and public APIs are unstable, and some
> instructions below may lag the code. Expect breaking changes.

## What is Hydrozoa?

Hydrozoa is a multi-party state channel protocol that decouples *execution*
from *custody*.

* **L2 state channel.** A small set of head peers run a fast off-chain ledger,
  rapidly sequencing user transactions while the L1 holds the funds. The
  channel admits continuous **deposits and withdrawals while open**; if
  consensus stalls, a pre-signed dead-man's-switch transaction matures and
  shifts custody into a rules-based regime where every participant can
  individually evacuate against the last KZG-committed state.

* **Pluggable L2 ledger, EUTXO built in.** Hydrozoa treats the L2 ledger as a
  black-box state machine. An **EUTXO-based L2 ledger** is provided out of the
  box (transactions are CBOR-serialized Cardano txs; outputs are either
  L2-bound or L1-bound via settlement / rollout effects), and
  application-specific ledgers can be slotted in — for example, Sugar Rush, a
  privacy-enhancing CLOB DEX, runs as its own domain-specific L2.

* **Cardano-first, multi-L1 by design.** Cardano is the initial and currently
  the only supported L1. The architecture is intentionally agnostic about the
  custody chain; **multi-L1 custody** (one head holding funds from several
  L1s) and **inter-head transactions** are planned future work.

This repository contains the L1 interface, the consensus actors (fast and
slow), the rules-based fallback regime, and the EUTXO L2 ledger — i.e. the
operational substrate of a Hydrozoa head.

## Getting Started

* Read the latest specification — see [Specification](#specification) below
  (pre-built PDF, build it locally, or browse the in-repo design specs).
* Join the discussion =>
  goto [discussions](https://github.com/cardano-hydrozoa/hydrozoa/discussions)
* Start hacking by picking up an issue =>
  goto [issues: help wanted](https://github.com/cardano-hydrozoa/hydrozoa/issues?q=is%3Aissue%20state%3Aopen%20label%3A%22help%20wanted%22)

## Specification

Design documentation is currently in flux. There are two sources, at different
stages of maturity:

* **Original Hydrozoa specification (deprecated)** — the LaTeX source under
  `specification/`, published as a PDF:
  [hydrozoa.pdf](https://cardano-hydrozoa.github.io/hydrozoa/hydrozoa.pdf).
  This document predates the current protocol direction and is kept for
  historical reference; treat it as superseded rather than authoritative. You
  can still build it locally (requires [Nix](https://nixos.org/download/#download-nix)
  with [flakes enabled](https://nixos.wiki/wiki/Flakes)):

  ```bash
  make spec        # build the PDF
  make spec-clean  # remove generated files
  ```

  See `specification/README.md` for contributor guidelines and more build
  options.

* **New Hydrozoa whitepaper (work in progress)** — the new authoritative
  protocol description, succeeding the original spec. It is being authored in a
  separate repository and is not yet ready for public release.

## Developer's Guide

### Prerequisites

The preferred and supported setup is [Nix](https://nixos.org/download/). Install
a recent Nix and run `nix develop` to drop into a dev shell with every
dependency pinned (JDK, `sbt`, `scalafmt`, `scalafix`, `mermaid-cli`, and more).
If you use [direnv](https://direnv.net/), `.envrc` already runs `use flake .`
so the shell loads automatically.

Without Nix you'll need a Java JDK (21+; the Nix shell pins JDK 25) and
[sbt](https://www.scala-sbt.org/). The project targets Scala 3.3.6 (LTS).

### Setting up IntelliJ IDEA

`File` -> `New` -> `Project from Existing Sources` -> `select the project directory`
-> `Import project from external model` -> `BSP` -> `Sbt`

Launch IDEA from within the Nix shell so `sbt` and the other tools are on `PATH`.

### Building and testing

Enter the sbt shell with `sbt` (or `sbtn` for a faster, resident server inside
the Nix shell):

```shell
sbt
> compile
> test
```

Formatting and linting are exposed as sbt aliases:

```shell
sbt fmtAll        # scalafmt — format all sources
sbt fmtCheckAll   # scalafmt — check only
sbt lintAll       # scalafix — apply fixes
sbt lintCheckAll  # scalafix — check only
```

Always run linting and formatting before committing.

### Integration tests

The `integration` subproject contains model-based test suites built on
ScalaCheck commands-inspired modified `ModelBasedSuite` harness. "Stages" here refer
to **levels of testing** — each stage exercises the system
at a different layer of abstraction:

* **`stage1` — single-peer head, low-level commands.** Drives the node with
  primitives like `StartBlock` / `CompleteBlock` and checks that the produced
  block briefs match the model and that timing rules are satisfied. Useful for
  pinning down block-production and scheduling invariants in isolation. The
  same suite runs against three L1 backends: an in-process **mock L1**, a
  local **[Yaci DevKit](https://devkit.yaci.xyz/docker)**, and a **public
  Cardano testnet** (Preview, via Blockfrost).

* **`stage4` — multi-peer head, real user commands.** Runs several peers in a
  single suite, communicating either in-process or over the WebSocket transport,
  and drives them with user-level commands — L2 transactions and deposits —
  exercising the full consensus path end-to-end.

#### Running the suites

Run the full integration suite (mock + public Preview):

```shell
sbt "integration/test"
```

Or run a single stage1 backend in isolation:

```shell
sbt "integration/testOnly hydrozoa.integration.stage1.Stage1PropertiesL1Mock"  # mock L1 (default)
sbt "integration/testOnly hydrozoa.integration.stage1.Stage1PropertiesYaci"    # local Yaci DevKit
sbt "integration/testOnly hydrozoa.integration.stage1.Stage1PropertiesPublic"  # public Preview testnet
```

The Yaci suite requires a running Yaci DevKit instance and is excluded from
the default integration run. The public-testnet suite runs by default and
needs network access.

## License

Code is licensed under [Apache-2.0](https://www.apache.org/licenses/LICENSE-2.0);
documentation and the specification under
[CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/). See `LICENSE`
and `NOTICE` for details.
