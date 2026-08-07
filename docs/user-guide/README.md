# Hydrozoa user guide

Task-oriented how-tos for running and using a Hydrozoa head.

| Guide | What it covers |
| --- | --- |
| [DEPLOYMENT.md](DEPLOYMENT.md) | End-to-end: scaffold, configure, and run a multi-party head; deposit, transact, and close. |
| [DEPOSIT.md](DEPOSIT.md) | Build a deposit: the L2 payload (EUTXO ledger) and the L1 deposit tx whose metadata pins the payload's hash. |
| [L2TXS.md](L2TXS.md) | Build a basic L2 transaction (no mint/burn), including withdrawals and the mandatory tx metadata. |
| [L2MINTING.md](L2MINTING.md) | Mint and burn tokens on L2: the transient-token declarations and the metadata a minting tx must carry. |

Each of DEPOSIT / L2TXS / L2MINTING has a matching CLI subcommand of the packaged `hydrozoa`
launcher (`submit-deposit`, `submit-l2-tx`) that serves as a runnable worked example; the guides
walk both the wire format and the CLI.

The reference EUTXO L2 ledger drives an isomorphic ledger with **native Cardano transactions** — an
L2 tx is an ordinary Cardano `Transaction`, and a deposit is an ordinary Cardano tx plus an
out-of-band payload. The guides assume that model.

For the machine-readable request/response shapes these how-tos build against, see the
[API reference](../api/).
