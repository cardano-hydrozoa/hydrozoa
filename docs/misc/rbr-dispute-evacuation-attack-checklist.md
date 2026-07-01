# Rule-Based Regime — Dispute / Evacuation Attack Checklist

Working checklist of attacks against the rule-based dispute-resolution + evacuation contracts
(`DisputeResolutionScript`, `RuleBasedTreasuryScript`). Tracks what the adversarial tests exercise
and where the gaps are.

## Flow

`Fallback` (mints `nHeadPeers+1` vote tokens + unique head **beacon** NFT under the head multisig
policy; creates a ring of ballot boxes + an Unresolved treasury) → **Vote** (per peer, pre-deadline)
→ **Tally** (post-deadline, contract the ring to one survivor via `maxVote`) → **Resolve**
(`ResolutionTx` → Resolved treasury committing to the winning `evacuationActive` map) → **Evacuate**
(`EvacuationTx` drains the map via KZG proofs → evacuee payouts + residual treasury) → **Deinit**.

## Test coverage

- **Test A** `DisputeResolutionScenarioTest` — exhaustive resolution-safety (vote subset × tally
  order), exact "resolve = max voted" oracle, capped at 3 boxes. Plus `DisputeVoteAttackTest` (E1
  vote-on-another's-box, E2 vote-past-deadline).
- **Test B** `EvacuationDrainTest` — happy-path evacuation drain (no adversarial).
- **Test C** `L2OutputEvacuabilityTest` — L2-conformant-but-unevacuable gap + infinite-halving DoS
  (#488).
- **Test D** `DisputeResolutionCommandsTest` — bounded-trace ScalaCheck `Commands` explorer at 4
  boxes, with the adversarial attacks below.

## Per-phase attack surface

Legend: ✅ tested (test D unless noted) · ◐ partial · 🔎 untested but **predictable PASS** by source
inspection (cited check directly rejects the attack) · ⚠️ gap / **uncertain outcome** (source does
not let us predict the result — worth a test)

### Vote phase
- ✅ forge version `Voted(X2,3)` / forge foreign commitment → `VoteOutputDatumCheck`
- ✅ vote on another peer's box (E1) · ✅ vote past deadline (E2)
- ✅ redirect box to attacker → output-address check · ✅ skim vote token (rebalanced) →
  value-preservation (`VoteVoteOutputExists`)
- ✅ staking-credential-only swap (E5) → `VoteVoteOutputExists` (full-address `===` covers the
  staking part; see technique taxonomy)
- ✅ inflate weight by minting an extra vote token → head minting policy ("missing mint script")
- 🔎 tamper key/link (ring corruption) → `VoteOutputDatumAdditionalChecks`
  (`DisputeResolutionScript:367-368`, key & link preserved input→output)
- 🔎 attach a reference script to the vote output → `VoteOutputNoScriptRef` (`:345-347`,
  denial-of-evacuation bloat)
- ✅ **double-vote in one tx (spend two boxes)** (E6) → `VoteOnlyOneVoteUtxoIsSpent`. The current
  guard filters co-spent inputs by **token identity** — `i.outRef !== ownRef &&
  i.resolved.value.containsCurrencySymbol(headMp)` must be empty — so a second ballot box (or the
  treasury) in the same Vote tx is rejected regardless of its source txid. *History:* the
  `pr/test-a-dispute` branch had the weaker `outRef.id`-only filter, which would have **accepted**
  two already-ratcheted boxes (divergent source txs + distinct ADA slip past the txid filter and the
  exact-value `VoteVoteOutputExists` backstop) — a real gap. The on-chain audit (`8b63c6fe`, merge of
  `gf/onchain-audit-fixes`) replaced it with the token-identity filter; the code comment cites the
  exact "txid filter misses co-spent boxes once ratcheting diverges a box's source tx" failure mode.
  E6 (`DisputeVoteAttackTest`) co-spends two distinct-txid boxes and asserts the builder's evaluator
  rejects with the guard message — a regression test locking in the fix. (Abstain's guard was
  likewise rewritten off `outRef`.)
  **Safe by analysis** (both the txid *and* the index dimension): `i.outRef !== ownRef` excludes
  *only* the validated box, because the on-chain `Eq[TxOutRef]` (scalus `v3.Contexts:62`) is
  `a.id === b.id && a.idx === b.idx` — a full `(id, idx)` compare — and two distinct spent utxos
  always differ in `(id, idx)` (the ledger forbids spending one utxo twice). So any *other*
  head-policy input — a second ballot box or the treasury — is caught by
  `containsCurrencySymbol(headMp)` whether it differs from `ownRef` in txid, index, or both. That
  single `!==` handles the divergent-txid (ratcheted) case and the same-txid/different-index
  (fresh-from-FallbackTx) case identically, so no separate index test is needed: E6 (different txid)
  suffices and an E6b (same txid, idx 0/1) would be redundant. The proof rests on two primitives —
  structural `Eq[TxOutRef]` and ledger input-uniqueness — so a refactor touching either is what E6
  guards against.
- 🔎 coil-signature / quorum manipulation; sec/sig mismatch — predictable PASS by inspection:
  `verifySignatures` (`:271-281`) is guarded by the headPeers-length check (`:263`);
  `verifyCoilSignatures` (`:293-325`) requires `coilQuorum` distinct, position-aligned valid sigs.
  Intricate but sound.

### Tally phase  ← biggest tractable gap (vote aggregation happens here)
Script defenses (read from `DisputeResolutionScript` Tally branch):
- `HighestVoteCheck` — `continuingOutput.voteStatus === maxVote(cont, rem)`
- `LinkCheck` — survivor link == removed.link; `KeyLinkFieldsDoNotMatch` — removed.key > cont.key &&
  == cont.link
- continuingOutput must have same address, combined non-ADA tokens, ADA ≥ cont + residual
- `NoOtherInputs` — **no other input may hold this head's policy token** (blocks co-spending extra
  ballot boxes OR the treasury in a Tally tx)
- treasury must be a **reference** input (`TreasuryReferenceInputExists`); `TallyOnlyAfterVotingDeadline`;
  `TallyOutputNoScriptRef`
- ✅ forge survivor merged version `Voted(X2,3)` → `HighestVoteCheck` ("continuingOutput must match
  the highest voteStatus") *(test D, `9995b629`)*
- ✅ redirect survivor / skim survivor token → address/value checks ("continuing output not found or
  wrong value") *(test D, `9995b629`)*
- 🔎 forge survivor link (→ `LinkCheck` `:534`) ; co-spend an extra box (→ `NoOtherInputs`
  `:440-448`) — predictable PASS
- 🔎 tally an illegal `(cont,rem)` pair (→ `KeyLinkFieldsDoNotMatch` `:432-435`, adjacency
  `removed.key > cont.key && == cont.link`) ; tally before deadline (→ `TallyOnlyAfterVotingDeadline`
  `:487-494`) — predictable PASS

### Resolve phase
- ✅ different evacuation map (forge `evacuationActive`) → `ResolveUtxoActiveCheck` ("activeUtxo must
  match voting results")
- ✅ cross-script evacuate-during-resolve (swap treasury Resolve→Evacuate redeemer) →
  `EvacuateNeedsResolvedDatum`
- 🔎 two treasury outputs → `ResolveTreasuryOutputFailure` (`RuleBasedTreasuryScript:181-186`,
  exactly one output at treasury address) — predictable PASS
- 🔎 steal/redirect resolved treasury → `ResolveValueShouldBePreserved` (`:181-192`, output found by
  address + exact value `===`) — predictable PASS
- 🔎 forge `setupG2` → `ResolveTreasuryInputOutputSetupG2` (`:234-237`) — predictable PASS
- 🔎 resolve a non-final box (survivor lacks all `nHeadPeers+1` tokens) — predictable PASS, but
  defended **treasury-side**, not by the dispute Resolve branch: `containsExactlyOneAsset(headMp,
  disputeId, headPeersN+1)` (`RuleBasedTreasuryScript:158-166`, `ValueExtensions:29`) → a partial
  survivor isn't found → `ResolveVoteInputNotFound`.
- ⚠️ two map-bearing inputs (double-satisfaction) — uncertain *from these scripts*: rests on beacon
  NFT **mint-time** uniqueness, which lives in the mint policy, not the dispute/treasury validators
  → not provable here; untested.

### Evacuate phase  ← highest value (funds leave here) — `EvacuationAttackTest` (real KZG map)
- ✅ steal treasury value → `EvacuateValueShouldBePreserved` ("Value invariant should hold")
- ✅ steal/not-preserve beacon → `EvacuateBeaconTokenShouldBePreserved`
- ✅ redirect an evacuee payout → `EvacuateMembershipValidationFailed` (payouts are **bound to the
  map by the KZG proof** — the membership hash covers the output, so a redirected payout no longer
  validates; this *answers* the "wrong-address payout" risk: it can't)
- ✅ corrupt output `setupG2` (degenerate-setup downgrade) → `EvacuateSetupG2ShouldBePreserved`
- ✅ **positional-parsing** insertion → shifts treasury slot onto the beacon-less change →
  `EvacuateBeaconTokenShouldBePreserved` (the beacon check *does* save the positional parse; the
  payout binding above covers the evacuee-output side)
- ✅ forge/stale output accumulator (leave it at the input commitment) →
  `EvacuateOutputAccumulatorUpdated`
- ✅ **double-drain / replay** the same entry (chained) — value-conservation makes the already-paid
  obligation un-payable from the residual treasury (builder refuses); accumulator-monotonicity also
  enforced at the script level (stale-accumulator + membership)
- ✅ **forged KZG membership proof** — forge a valid-but-wrong G1 into the redeemer `proof` + output
  accumulator (isolating membership) → `EvacuateMembershipValidationFailed` (KZG soundness)
- 🔎 forge headMp/version on the residual (`EvacuateHeadMp/VersionShouldBePreserved`,
  `RuleBasedTreasuryScript:354-365`) — predictable PASS; trivial datum-mutation add-on
- ✅ infinite-halving DoS (#488) — test C (`L2OutputEvacuabilityTest`, branch `pr/test-c-evacuability`):
  `pendingUntilFixed` "EvacuationTx.Build should terminate on a single un-evacuable output". Bug
  documented + regression-tracked; flips red the day #488 is fixed. (Halving a singleton map is
  `drop(0)` → the over-size build retries forever.)

## Creation-time invariants (off-chain + multisig, NOT on-chain)

Some guarantees the dispute scripts rely on are established when the ballot boxes are *created*
(FallbackTx), not checked by any Plutus validator. Worth recording because the on-chain tests can't
cover them — they rest on the off-chain builder plus the n-of-n signature over the FallbackTx body.

- **Box address / stake part.** `FallbackTx.mkBallotBox` hardcodes every box to
  `config.ruleBasedDisputeResolutionAddress` = `HydrozoaBlueprint.mkDisputeAddress(network)`, a
  script address with `delegation = Null` (enterprise, no stake part). FallbackTx spends the
  multisig treasury (`HeadMultisigScript`, all-peer native script), so the whole body — including
  those addresses — is signed by every peer; a single peer can't substitute a stake-bearing address
  without breaking the signatures. The **Vote validator only *preserves* the address**
  (`voteOutput.address === voteInput.address`, E5) — it never pins it to `mkDisputeAddress`. So a
  vote can't both count *and* carry a custom stake credential (E5 ✅), but the guarantee that boxes
  *start* stake-less is FallbackTx + multisig, not a dispute-script check.
- **Box funding.** Each box's min-ADA is that peer's own `individualContingency.forBallotBox`
  (`voteDeposit + voteTxFee`), pre-paid at head init; the "public" box adds peer-0's collective
  contingency. Returned per-peer at clean finalization
  (`distributeFallbackContingencyInFinalization`). No shared-fund theft is possible via the box.
- ⚠️ optional belt-and-suspenders: a FallbackTx-builder test pinning `mkBallotBox`'s output address
  to `mkDisputeAddress` (delegation `Null`), so a refactor that introduces a stake part trips a
  test — or, stronger, an on-chain check in the multisig-treasury fallback branch that the produced
  ballot-box outputs are at the canonical address.

## Technique-level taxonomy (eUTxO / Plutus)

- ✅ datum/value/address tampering · ✅ unauthorized mint · ✅ state-skip
- ✅ **positional output parsing** (Evacuate) — tested; defended by the beacon check on the treasury
  slot + the proof binding on payouts
- ✅ double satisfaction — cross-script + two-same-map + evacuee-payout (all bound/rejected); the
  evacuee-payout form is foiled by the KZG membership binding
- ✅ deadline/time — E2 (finite-too-late) + E3 (open/unbounded `to`, the `case _ => fail` path)
- ✅ locked-funds / size DoS — `#488` infinite-halving (test C, `pendingUntilFixed`) + the L2
  datum-size evacuability gap (test C)
- ✅ **validity-interval manipulation** (open/unbounded `to`) — E3 `DisputeVoteAttackTest`,
  `VoteTimeValidityCheck`
- 🔎 **extra-token / dust / loose value** — predictable PASS: `onlyNonAdaAsset` (`ValueExtensions:57`)
  fails on a second currency symbol/name, and Vote/Resolve/Evacuate value checks are exact `===`. The
  only non-exact spot is Tally's continuing-output ADA `>=` (`:510`), which only lets the attacker
  over-pay. No dedicated test; marginal.
- ✅ **staking-credential swap** (payment part matches, staking part is attacker's) — E5
  `DisputeVoteAttackTest`: swap only the delegation part of the continuing vote output (same
  payment script, attacker's stake key) → `VoteVoteOutputExists`. Provable by inspection too: the
  on-chain `Eq[Address]` (scalus `v1.Contexts:554`) is `a.credential === b.credential &&
  a.stakingCredential === b.stakingCredential` — the full address (payment **and** staking) is
  compared by every `address ===` check (Vote :341, Tally :508, Abstain :600, treasury
  `RuleBasedTreasuryScript:182`); evacuee payouts are bound tighter still by the KZG hash.
- 🔎 stale reference input (reference an old/expired treasury) — predictable PASS: reference inputs
  must be unspent (ledger phase-1) and the head beacon is unique, so only the current Unresolved
  treasury can be referenced.
- ✅ replay / non-monotonic accumulator (evacuate) — covered by the tested double-drain /
  stale-accumulator cases above.
- ✅ forged membership proof (KZG) — covered by the tested forged-G1 case above
  (`EvacuateMembershipValidationFailed`).
- 🔎 script-purpose confusion (Spend vs Mint vs Reward) — **unreachable by construction**: both
  script addresses use `delegation = Null` (never a stake/DRep/cert credential) and neither hash is
  ever a minting policy, so the ledger only ever invokes them under `SpendingScript`. The scalus
  `Validator` base leaves the other purpose handlers abstract, so the compiled behavior if *forced*
  is opaque — but it is not reachable.
- 🔎 signature cross-context reuse — predictable PASS: the signed message is the full SEC, domain-
  separated by `sec.headId` (`:239`) and `versionMajor` (`:329`); a signature from another
  head/version won't match.
- ✅ terminal-state re-entry — E4 re-vote a Voted box (builder `VoteAlreadyCast`); re-resolve a
  Resolved treasury is builder-refused too (`TreasuryAlreadyResolved`)

## Priorities

1. ✅ **Tally-phase attacks** (test D) — forge survivor version, redirect survivor. *(done,
   `9995b629`)*
2. ✅ **Adversarial evacuate** (`EvacuationAttackTest`, 9 cases) — value/beacon theft,
   payout-redirect, setupG2-corrupt, positional-parsing, stale-accumulator, double-drain,
   forged-proof. *(done, `6e97a799` + `99455241`)*. All known evacuate techniques covered.
3. ✅ **Cheap cross-cutting** — E3 open-interval validity + E4 re-vote + E5 staking-credential-only
   swap (`DisputeVoteAttackTest`). Only residual: extra-token/dust (largely covered by the exact
   `value ===` checks; no dedicated test, marginal).
4. ✅ **Open-phase double-spend (Vote)** (E6) — *already fixed on main.* The `outRef.id`-only filter
   that made this a gap on `pr/test-a-dispute` was replaced by the on-chain audit (`8b63c6fe`) with a
   token-identity filter (`containsCurrencySymbol(headMp)`), which also blocks co-spending the
   treasury. E6 (`DisputeVoteAttackTest`) is the regression test: two distinct-txid boxes in one Vote
   tx are rejected with `VoteOnlyOneVoteUtxoIsSpent`. Remaining ⚠️ is the lower-value
   beacon-uniqueness "two map-bearing inputs" double-satisfaction, which is a mint-policy property,
   not a dispute/treasury one.

## Notes

- Output mutations (datum/value/address) stay phase-1 valid and don't touch the script-data hash.
- Redeemer / mint / extra-output mutations grow the tx → bump the fee (charge the attacker's own
  output) so the *script* rejects, not a phase-1 fee check; redeemer swaps also need a
  `ScriptDataHashGenerator.computeScriptDataHash` recompute.
- Each accepted attack must trip a safety invariant (token conservation / version-legitimacy /
  resolution-legitimacy) — otherwise the test silently passes a real exploit.
