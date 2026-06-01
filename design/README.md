# design/

In-repo design specifications for Hydrozoa/Gummiworm features.

These documents are working specs derived from the Gummiworm
whitepaper and refined for implementation. They are more
detailed and implementation-oriented than the whitepaper's prose, and they make
the whitepaper's open questions explicit as decision points.

Distinct from `specification/` (the LaTeX source of the formal PDF
specification) and `.scratch/` (ephemeral working notes).

| Spec | Status        | Summary |
|---|---------------|---|
| [coil-network.md](coil-network.md) | Decisions locked | Coil-ready peer node-type (M5, active): follower-only delta on the head-peer actor set — fast-side producers (BlockWeaver/JL/FCA/SC) in follower mode only, single-hub PeerLiaison, gappy own-HardAck. Decided 2026-05-30: tagged `PeerId` (Head/Coil, one-bit wire tag), threshold native script `AllOf(head) ∧ AtLeast(coilQuorum, coil)`, fixed-count coil witnesses (saturate at `coilQuorum`), reuse `HeadConfig` + parallel `CoilMultisigRegimeManager`. `CardanoLiaison` unchanged (R8/R9). Marketplace deferred to a future-work spec. |
| [persistence-and-crash-recovery.md](persistence-and-crash-recovery.md) | Implementing  | Durable consensus data + crash recovery for head peers: what each actor must persist, equivocation avoidance, snapshot + log replay. Whitepaper M5 (May 2026). Coil-peer persistence to land later as §11. |
