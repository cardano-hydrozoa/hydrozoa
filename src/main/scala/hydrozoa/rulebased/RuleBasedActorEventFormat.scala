package hydrozoa.rulebased

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.RuleBasedActorEvent.*

object RuleBasedActorEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: RuleBasedActorEvent): LogEvent =
        val ev = LogEvent.From.forPeer("RuleBasedActor", peerNum)
        import ev.*
        e match
            case Backend.ErrorDisputeUtxos(err) =>
                warn(s"Backend error querying dispute UTxOs. Will retry.\n\tError: $err")
            case Backend.ErrorTreasuryUtxos(err) =>
                warn(s"Backend error querying treasury UTxOs. Will retry.\n\tError: $err")
            case Backend.ErrorRegimeUtxos(err) =>
                warn(s"Backend error querying regime UTxOs. Will retry.\n\tError: $err")
            case Backend.ErrorPeerUtxos(err) =>
                warn(s"Backend error querying peer UTxOs. Will retry.\n\tError: $err")
            case Backend.ErrorFeeUtxos(err) =>
                warn(s"Backend error querying fee UTxOs. Will retry.\n\tError: $err")
            case Backend.ErrorContinuingTxs(err) =>
                warn(s"Backend error querying continuing txs. Will retry.\n\tError: $err")
            case Backend.ErrorSubmittingTx(err) =>
                // Submission failures are normal in the rule-based regime — peers race the same
                // dispute/evacuation txs, and inputs get contended or already-spent — so retry
                // quietly at debug rather than warn.
                debug(s"Backend error submitting tx (normal; will retry).\n\tError: $err")

            case Treasury.Querying         => debug("Querying treasury")
            case Treasury.Found(value)     => debug(s"Found treasury utxo with $value")
            case Treasury.NotFound         => debug("Treasury utxo not found, retrying")
            case Treasury.Parsing          => debug("Parsing treasury")
            case Treasury.ParsedUnresolved => info("Treasury is Unresolved")
            case Treasury.ParsedResolved   => info("Treasury is Resolved")

            case Regime.Querying => debug("Querying regime utxo")
            case Regime.Found    => debug("Found regime utxo")
            case Regime.NotFound => debug("Regime utxo not found, retrying")

            case Collateral.Querying(address) =>
                debug(s"Querying collateral utxos at address $address")
            case Collateral.Found => debug("Found collateral utxo")
            case Collateral.NotFound(address) =>
                error(
                  s"No ADA-only utxo found at $address. " +
                      "Please send an ADA-only utxo for collateral to this address."
                )

            case Fee.Querying(address) =>
                debug(s"Querying fee utxos at address $address")

            case Dispute.Querying          => debug("Querying dispute utxos")
            case Dispute.Parsing           => debug("Parsing dispute utxos")
            case Dispute.ParsingCastVote   => info("Dispute state: own ballot awaits a vote")
            case Dispute.ParsingTally      => info("Dispute state: ready to tally")
            case Dispute.ParsingResolve    => info("Dispute state: ready to resolve")
            case Dispute.ParsingEmptyVotes => warn("Dispute state: no vote utxos (unexpected)")
            case Dispute.WaitingForVotingDeadline =>
                info("Dispute state: deferring tally until voting deadline elapses")
            case Dispute.VotingDeadlineElapsed =>
                info(
                  "Dispute state: voting deadline elapsed; skipping our own vote/ratchet " +
                      "and dispatching to residual tally/resolve"
                )

            case Dispute.Coil.ParsingRatchet =>
                info("Coil dispute state: ratcheting a public ballot box")
            case Dispute.Coil.AlreadyAtTarget =>
                info("Coil dispute state: a public box already carries the target SEC; noop")
            case Dispute.Coil.NoRatchetTarget =>
                info("Coil dispute state: no ratchet target; falling through to tally/resolve")

            case Tick.RecoverableRetry(reason) =>
                // Info, not debug: this is the per-tick "what am I waiting on" heartbeat (treasury
                // not up yet, no evacuatees left, deadline not elapsed, …) — the main signal that
                // the actor is alive and why it is not progressing this tick.
                info(s"Tick did not complete (recoverable); will retry. Reason: $reason")

            case Tx.Building(family) => info(s"Building $family")
            case Tx.Submitting(tx) =>
                info(s"Submitting ${tx.transactionFamily} with Id ${tx.tx.id}")
            case Tx.SubmitSuccess(tx) =>
                info(s"SUCCESS submitting ${tx.transactionFamily} with Id ${tx.tx.id}")
            case Tx.Tallying => info("Tallying...")

            case Evacuation.NoMore =>
                info("No more evacuations to be done. Staying alive in case of rollbacks")
            case Evacuation.PayoutsLeft(n) =>
                info(s"$n payout obligations left")
            case Evacuation.ContinuingTreasuryTxs(0) =>
                info(
                  "Evacuation: no continuing treasury txs surfaced after the fallback anchor yet; " +
                      "retrying (NoTreasuryFound)"
                )
            case Evacuation.ContinuingTreasuryTxs(n) =>
                info(s"Evacuation: $n continuing treasury tx(s) surfaced after the fallback anchor")

            // Diagnostic-only events carry no production rendering — a test-side diagnostic tracer
            // (composed with `|+|`) formats them. Kept at trace so production stays silent.
            case d: Evacuation.CandidateMaps       => trace(d.toString)
            case d: Evacuation.CandidateMapSources => trace(d.toString)
            case d: Evacuation.ResolvedKzg         => trace(d.toString)
            case d: Evacuation.EvacuationAnchor    => trace(d.toString)
