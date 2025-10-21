# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Hydrozoa is a lightweight multi-party state channels implementation for Cardano, written in Scala 3. The project provides two regime types:
- **Multisig regime**: Multi-party consensus-based state channels
- **Rule-based regime**: Used when consensus is no longer held (transition from multisig regime)

## Development Commands

### Building and Testing
```bash
# Enter sbt shell (use sbtn for faster execution)
nix develop --command sbtn

# Compile the project - ALWAYS use this exact command for compilation checks
nix develop --command sbtn compile

# Run tests
nix develop --command sbtn test

# Run specific test suites
nix develop --command sbtn "testOnly *SpecificTestSuite*"
```

**Important**: When using Claude Code, always run sbtn commands within the nix shell using `nix develop --command sbtn <command>`. Do not use `sbt` or `sbtn` directly, as they may not work correctly in this environment.

### Code Quality
```bash
# Format code
nix develop --command just fmt
# or
nix develop --command sbtn scalafmtAll

# Check formatting
nix develop --command just fmt-check
# or
nix develop --command sbtn scalafmtCheck

# Apply linting fixes
nix develop --command just lint
# or
nix develop --command sbtn scalafixAll

# Check linting
nix develop --command just lint-check
# or
nix develop --command sbtn scalafixAll --check
```

### Benchmarks
```bash
# Run JMH benchmarks
nix develop --command sbtn "benchmark / Jmh / run -i 5 -wi 5 -f1 -t1"
```

### Specification
```bash
# Build PDF specification
nix develop --command make spec

# Clean specification files
nix develop --command make spec-clean
```

## Architecture

### Core Structure
- **`src/main/scala/hydrozoa/`**: Main source directory
  - **`Types.scala`**: Fundamental type definitions using opaque types for L1/L2 layer distinction
  - **`Transitionary.scala`**: Bridging types between hydrozoa, scalus, and bloxbean libraries
  - **`multisig/`**: Multi-party consensus implementation
    - **`MultisigRegimeManager.scala`**: Actor-based regime coordinator
    - **`consensus/`**: Consensus protocol implementation
    - **`ledger/`**: L2 ledger logic and transaction builders
    - **`protocol/`**: Communication protocols between components
  - **`rulebased/`**: Rule-based regime implementation
    - **`RuleBasedRegimeManager.scala`**: Simplified regime manager
  - **`node/`**: Network and consensus node components
  - **`lib/`**: Shared utilities and extensions

### Key Technologies
- **Scala 3.3.6** with modern language features
- **Scalus**: Cardano Plutus integration and on-chain script compilation
- **Cats Effect**: Functional effect system for IO operations
- **Cats Actors**: Actor-based concurrency model
- **Bloxbean Cardano Client**: Off-chain Cardano interaction
- **MUnit + ScalaCheck**: Testing framework with property-based testing

### Type Safety Design
The codebase uses opaque types to distinguish between L1 (Cardano mainnet) and L2 (Hydrozoa state channel) layers:
- `Address[L1]` vs `Address[L2]`
- `UtxoId[L1]` vs `UtxoId[L2]`
- `Transaction[L1]` vs `Transaction[L2]`

This prevents accidental mixing of layer-specific values while maintaining zero runtime overhead.

### Performance Considerations
- KZG commitments are a critical performance bottleneck for L2 state
- Current naive approach supports up to 32k UTXOs using Ethereum's trusted setup
- Benchmarks show polynomial building (step 7) as the main bottleneck for large UTXO sets
- Future optimizations planned: incremental commitments, hot-spot optimization, diff-based algorithms

## Development Environment

### Prerequisites
- Java JDK 21+
- Scala 3.3.6
- sbt or scala-cli
- **Recommended**: Nix package manager for reproducible development environment

### Using Nix
```bash
# Enter development shell with all dependencies
nix develop
```

### IntelliJ IDEA Setup
1. File → New → Project from Existing Sources
2. Select project directory
3. Import project from external model → BSP → Sbt

### Running Hydrozoa Network
**Main class**: `hydrozoa.HydrozoaNode`

## Code Style

- **Indentation**: 4 spaces
- **Max line length**: 100 characters
- **Import sorting**: scalastyle format
- **Scalafix rules**: ExplicitResultTypes, OrganizeImports, RemoveUnused, etc.

## Testing

- **Unit tests**: Located in `src/test/scala/hydrozoa/`
- **Integration tests**: Separate `integration` project (currently needs updates - see issue #111)
- **Property-based testing**: Using ScalaCheck for testing protocol invariants
- **Benchmarks**: JMH-based performance testing in `benchmark/` subproject

## Transaction Builder Implementation

### Declarative Transaction Building
The project includes a declarative transaction builder ported from `purescript-cardano-transaction-builder`, located in `src/main/scala/hydrozoa/lib/tx/`:

#### Core Components
- **`TxBuilder.scala`**: Main transaction building types and operations
  - `TransactionBuilderStep`: ADT for declarative transaction operations (SpendOutput, Pay, MintAsset, etc.)
  - `OutputWitness`, `CredentialWitness`, `ScriptWitness`: Type-safe witness management
  
- **`TxEditor.scala`**: Automatic redeemer re-indexing utilities
  - `DetachedRedeemer`: Redeemers detached from transaction indices
  - `RedeemerPurpose`: Maps redeemers to transaction components by content, not index
  - `EditableTransaction`: Transaction with detached redeemers for safe editing
  - `TransactionEditor.editTransaction()`: Main function for index-safe transaction modification

#### Key Innovation: Automatic Redeemer Management
Traditional Cardano transaction building requires manual management of redeemer indices:
```scala
// Manual approach - fragile to changes
Redeemer(tag = Spend, index = 2, data = myRedeemer) // Points to 3rd input
```

The declarative approach uses content-based references:
```scala
// Declarative approach - automatically finds the correct index
DetachedRedeemer(data = myRedeemer, purpose = ForSpend(specificInput))
```

#### Usage Pattern
```scala
// Edit transaction safely - redeemers automatically re-indexed
val editedTx = TransactionEditor.editTransaction { tx =>
  // Add inputs, outputs, mints - indices handled automatically
  tx.addInput(newInput).addOutput(newOutput)
}(originalTx)
```

#### Current Status
- Core types and redeemer management: ✅ Complete
- Transaction conversion utilities: ✅ Implemented
- Integration with existing TxBuilder: 🔄 In progress
- Full PureScript feature parity: ⏳ Planned

#### Type Mappings from PureScript to Scalus
- `RewardAddress` → `RewardAccount`
- `VotingProposal` → `ProposalProcedure`
- `GovernanceActionId` → `GovActionId`
- `NativeScript` → `Script.Native`
- `RedeemerTag.Propose/Vote` → `RedeemerTag.Proposing/Voting`

## Important Notes

- The project is currently undergoing refactoring (expected completion: October 2025)
- Some README instructions may not work during the refactor period
- Integration test suites need updating (issue #111)
- Use implicit conversions carefully when working with opaque types
- Always run linting and formatting before committing changes
