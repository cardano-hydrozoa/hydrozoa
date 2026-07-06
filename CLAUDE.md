# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Hydrozoa is a lightweight multi-party state channels implementation for Cardano, written in Scala 3.

## Development Commands

### Building and Testing
```bash
# Enter sbt shell (use sbtn for faster execution)
sbtn

# Compile the project
sbtn compile

# Run tests
sbtn test

# Run specific test suites
sbtn "testOnly *SpecificTestSuite*"
```

**Note**: IDEA (and Claude Code) are launched from within the Nix shell, so `sbtn` and other tools are available directly — no `nix develop --command` prefix needed.

### Code Quality
```bash
# Format code
just fmt
# or
sbtn scalafmtAll

# Check formatting
just fmt-check
# or
sbtn scalafmtCheck

# Apply linting fixes
just lint
# or
sbtn scalafixAll

# Check linting
just lint-check
# or
sbtn scalafixAll --check
```

### Benchmarks
```bash
# Run JMH benchmarks
sbtn "benchmark / Jmh / run -i 5 -wi 5 -f1 -t1"
```

### Specification
```bash
# Build PDF specification
make spec

# Clean specification files
make spec-clean
```

## Architecture

### Key Technologies
- **Scala 3.3.6** with modern language features
- **Scalus**: Cardano Plutus integration and on-chain script compilation
- **Cats Effect**: Functional effect system for IO operations
- **Cats Actors**: Actor-based concurrency model
- **Bloxbean Cardano Client**: Off-chain Cardano interaction
- **MUnit + ScalaCheck**: Testing framework with property-based testing

## Design specifications

Durable design specifications live in `design/` (a top-level directory). Specs are added here as
they are written — none are committed on this branch yet.

## Documentation

Working design and reference docs live in `docs/`:

**Consensus & protocol**
- [`fast-consensus.md`](docs/fast-consensus.md) — fast cycle: per-block soft-confirmation via
  per-peer header signatures.
- [`slow-consensus.md`](docs/slow-consensus.md) — slow cycle: turning a run of soft-confirmed
  blocks into a multisigned, L1-submittable set of effect transactions over stacks.
- [`rate-limiter.md`](docs/rate-limiter.md) — generic throttling actor that slows the consensus
  cycles without changing consensus logic.

**API**
- [`l2-query-endpoints.md`](docs/l2-query-endpoints.md) — the user-facing server's read-only L2
  queries: `GET /api/l2/utxos/{address}` and `GET /api/l2/transactions` (EUTXO-only).

**Testing**
- [`integration-stages.md`](docs/integration-stages.md) — the stage1/stage4 integration test
  levels: what each exercises and where to add a test.
- [`testcontrol-driver.md`](docs/testcontrol-driver.md) — how `ModelBasedSuite` drives tests on a
  cats-effect `TestControl` virtual clock.

**Reference**
- [`style-guide.md`](docs/style-guide.md) — hand-applied Scala conventions (see [Code
  Style](#code-style)).
- [`logging-tracing.md`](docs/logging-tracing.md) — contextual logging and Tracer design.
- [`Codecs.md`](docs/Codecs.md) — codec conventions (WIP notes).

## Development Environment

### Using Nix
The project uses a Nix flake for a reproducible dev environment. Launch IDEA (and Claude Code) from within the Nix shell so all tools (`sbtn`, `just`, etc.) are on `PATH`:
```bash
nix develop
# then launch your editor from here
```

## Code Style

Mechanical settings (auto-enforced by scalafmt/scalafix):

- **Indentation**: 4 spaces
- **Max line length**: 100 characters
- **Import sorting**: scalastyle format
- **Scalafix rules**: ExplicitResultTypes, OrganizeImports, RemoveUnused, etc.

Hand-applied conventions — full rules and worked examples in
[`docs/style-guide.md`](docs/style-guide.md):

- **Naming**: functions are verb phrases (`mk*` counts); no ad-hoc contractions (use the
  established term or full word; `tx`/`id`/`VKey`/`SEC` exempt); never "genesis" for stack 0;
  "broadcast" = cross-peer network sends only (local fan-out is "announce"/"fan out").
  — [Naming](docs/style-guide.md#naming)
- **Organization**: every public def has a doc comment; privates come after publics, ordered
  caller-before-callee; every `extension` lives inside an `object`; no inline FQNs (always
  import); give functions the minimal data they need (fields > section > whole config).
  — [Code organization](docs/style-guide.md#code-organization)
- **Types/givens**: prefer an explicit `Unsigned`/`HardConfirmed` split over a phantom `+S`
  type parameter; opaque-tuple `Conversion` givens must `.convert` each element; never write
  `given x: T = summon` (resolves to itself → infinite loop).
  — [Types and givens](docs/style-guide.md#types-and-givens)

### Terminology

- **Reuse the project's existing names; don't coin new ones.** When a concept
  already has a concrete name — an actor (`JointLedger`, `StackComposer`,
  `FastConsensusActor`/`SCA`), a type, a spec term — use it in code, comments, and
  docs. Don't invent a fresh descriptive phrase for something already named (e.g.
  write "JointLedger", not "the signer / ledger side"; "fast-side" / "slow-side",
  not "block-side" / "stack-side"). New synonyms fragment the vocabulary and make
  the code harder to search and read.

### Comments

See [Comments](docs/style-guide.md#comments) for the full rule. In short:

- Describe the code **as it is now** — no historical perspective ("formerly X", "moved to the
  slow side", "as of step N", "renamed from"). History lives in Git and memory.
- Keep a comment **scoped to the local code's concern** — don't explain downstream or
  other-actor internals from a method that doesn't do that work.
- Don't reference `.scratch/` (gitignored); link committed `design/` docs or inline the point.

### Logging

- When you add a new named logger / tracer route, add a matching `<logger name="…">` line to
  **every** `logback.xml` in the same subproject — keep them in sync. Configs by subproject:
  - root: `src/main/resources/logback.xml` **and** `src/test/resources/logback.xml`
  - `integration`: `integration/src/test/resources/logback.xml`

### Warnings

- CI compiles with `-Werror` (gated on `$CI` in `build.sbt`); reproduce locally with
  `just build-werror`.
- Monocle's `Focus[T](_.field)` macro emits synthetic field-named locals that trip
  `-Wunused:all`. Annotate the enclosing def with
  `@nowarn("msg=unused local definition")` (import `scala.annotation.nowarn`).

## Testing

- **Unit tests**: Located in `src/test/scala/hydrozoa/`
- **Integration tests**: Separate `integration` project (currently needs updates - see issue #111)
- **Property-based testing**: Using ScalaCheck for testing protocol invariants
- **Benchmarks**: JMH-based performance testing in `benchmark/` subproject

### ScalaCheck rules

- **Never use `.sample` or `.sample.get`** on a `Gen[_]` in tests. It bypasses ScalaCheck's seeding
  mechanism, breaking reproducibility (a failing seed cannot be replayed), and throws
  `NoSuchElementException` if the generator returns `None`. Always drive generation through
  `forAll`, `pick`, or another ScalaCheck-managed entry point.

## Transaction Builder Implementation

## Important Notes

- The project is currently undergoing refactoring (expected completion: October 2025)
- Some README instructions may not work during the refactor period
- Use implicit conversions carefully when working with opaque types
- Always run linting and formatting before committing changes
- Always run linting and formatting before committing changes
