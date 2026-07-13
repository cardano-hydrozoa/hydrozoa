# Hydrozoa examples

Runnable, self-contained demos of Hydrozoa in action, each paired with a short readable
transcript (a "tutorial") of what happens when you run it.

Every example runs **fully in-process against a mock L1** — no Yaci DevKit, no testnet, no keys, no
network. It reuses the integration test harness (`MultiPeerHeadHarness`, `TestPeers`), so a reviewer
can bring up a real multi-peer head with genuine fast + slow consensus and watch it drive L1
operations, all from one `sbtn` command.

## Examples

| Example | What it shows | Run | Tutorial |
| --- | --- | --- | --- |
| Transient tokens | Minting and burning tokens inside the L2 ledger, scoped to the head's lifetime | `sbtn "examples/testOnly *TransientTokenDemo*"` | [tutorials/transient-tokens.md](tutorials/transient-tokens.md) |

## How the examples are built

Each example is a ScalaCheck `Properties` demo suite:

- brings up a live head on the mock L1 via `MultiPeerHeadHarness.resource`,
- narrates each step to stdout (`[<demo>] ...` lines) so the run reads as a transcript,
- asserts the on-L1 end state, so the demo doubles as a regression test.

The matching tutorial in `tutorials/` walks through that narrated log prose-first: what the head is
doing at each step and why, so it can be read without running anything.
