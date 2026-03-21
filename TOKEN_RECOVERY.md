# Token Recovery

This document describes the token recovery feature for recovering user-deposited tokens from the head address.

## Overview

When users deposit funds into the head address, both ADA and tokens accumulate there. The token recovery feature provides a temporary solution to separate and recover these assets:

- **ADA** → Returns to the faucet address (used for operational costs)
- **Tokens** → Recovers to a designated token recovery address (returns user assets)

This feature works in two modes:

1. **Automatic recovery on shutdown** - The Janitor automatically splits assets when the node shuts down
2. **Manual recovery tool** - Recovers tokens that have already accumulated in the faucet address

## Configuration

Add the following optional environment variable to your `.env` file:

```bash
# Optional: Bech32 address to receive recovered tokens
TOKEN_RECOVERY_ADDRESS=addr_test1...
```

If `TOKEN_RECOVERY_ADDRESS` is not set:
- Janitor will send all assets (ADA + tokens) to the faucet address (existing behavior)
- Manual token recovery tool will refuse to run

## Automatic Recovery (Janitor)

The Janitor runs automatically when the Hydrozoa node shuts down.

**Behavior when `TOKEN_RECOVERY_ADDRESS` is set:**

When UTXOs at the head address contain non-head tokens:
1. Burns all head tokens (policy tokens)
2. Sends non-head tokens (with minimum required ADA) → Token recovery address
3. Sends remaining ADA → Faucet address

**Transaction structure:**
- **Inputs:** All UTXOs from head multisig address
- **Outputs:**
  - Token output: `{tokens + minADA}` → Recovery address
  - ADA output: `{totalADA - minADA - headTokens}` → Faucet address
- **Mints:** Burns all head tokens (negative amounts)
- **Witnesses:** Signed by head peer wallet

## Manual Token Recovery

Use this tool to recover tokens that have already accumulated in the faucet address.

### Prerequisites

1. Set `TOKEN_RECOVERY_ADDRESS` in `.env`
2. Ensure `CARDANO_SIGNING_KEY` is set (faucet wallet private key)
3. Ensure `BLOCKFROST_API_KEY` is configured

### Usage

```bash
# Using just
just token-recovery

# Or directly with sbt
sbt "runMain hydrozoa.app.TokenRecovery"

# Or with nix
nix develop --command just token-recovery
```

### What it does

1. Loads configuration from `.env`
2. Validates that `TOKEN_RECOVERY_ADDRESS` is set
3. Creates faucet address from `CARDANO_VERIFICATION_KEY`
4. Queries all UTXOs at faucet address
5. Filters for UTXOs containing tokens
6. If tokens found:
   - Builds transaction to send all tokens (with min ADA) to recovery address
   - Sends excess ADA back to faucet as change
   - Signs with faucet wallet
   - Submits to blockchain
7. If no tokens found: Exits gracefully

### Transaction structure

- **Inputs:** All token-bearing UTXOs from faucet address
- **Outputs:**
  - Token output: `{all tokens + minADA}` → Recovery address
  - Change output: `{excess ADA}` → Faucet address (added automatically by balancer)
- **Witnesses:** Signed by faucet wallet (PubKey witness)

## Example .env Configuration

```bash
# Required for all operations
BLOCKFROST_API_KEY=preprodXXXXXXXXXXXXXXXX
CARDANO_VERIFICATION_KEY=a3b2c1d4e5f6...
CARDANO_SIGNING_KEY=1a2b3c4d5e6f...
EQUITY=2000000

# Optional: Enable token recovery
TOKEN_RECOVERY_ADDRESS=addr_test1qz8xzy7pvqm3xr9k8nlh9wq2...

# Optional: Sugar Rush connection
SUGAR_RUSH_HOST=localhost
SUGAR_RUSH_PORT=3001
```

## Minimum ADA Calculation

The feature uses `CardanoNetwork.ensureMinAda()` to calculate the minimum ADA required for token-bearing outputs. This ensures compliance with Cardano's minimum UTXO requirements based on:

- Protocol parameters (`utxoCostPerByte`)
- Serialized size of the output (including tokens)
- Babbage era formula: `(160 + size) * utxoCostPerByte`

## Notes

- This is a **temporary solution** until full evacuation protocol is implemented
- Head tokens (policy tokens) are **never** sent to the recovery address - they are always burned
- Both tools handle empty token sets gracefully (no-op if no tokens to recover)
- Transactions are signed and submitted on-chain, incurring transaction fees
- The faucet wallet must have sufficient ADA to cover transaction fees and minimum UTXO requirements

## Future Work

This feature will be replaced by a proper evacuation mechanism that allows users to withdraw their own funds directly from the head protocol.
