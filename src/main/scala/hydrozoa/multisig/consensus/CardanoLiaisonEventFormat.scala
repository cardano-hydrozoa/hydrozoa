package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.CardanoLiaison.{DisjointWindowViolation as _, *}
import hydrozoa.multisig.consensus.CardanoLiaisonEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[CardanoLiaisonEvent]] to [[LogEvent]]. */
object CardanoLiaisonEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: CardanoLiaisonEvent): LogEvent = {
        val ev = LogEvent.From.forPeer("CardanoLiaison", peerNum)
        import ev.*
        e match {
            case TimeoutReceived =>
                info("received Timeout, run effects...")
            case StackHardConfirmedReceived(stackNum) =>
                info(s"received Stack.HardConfirmed for stack $stackNum")
            case InitialStackEffectsLearned =>
                info("initial stack effects learned; overriding unsigned init tx + fallback")
            case InitialStackEffectsState(state) =>
                trace(s"state after initial-stack effects: ${state.prettyDump}")
            case MinorOnlyStackReceived =>
                info("minor-only stack: no backbone L1 effects to submit; nothing to do")
            case StackEffectsLearned(settlements, fallbacks, rollouts, hasFinalization) =>
                info(
                  s"stack effects learned: ${settlements}s ${fallbacks}fb ${rollouts}ro finalization=$hasFinalization"
                )
            case StackEffectsState(state) =>
                trace(s"state after stack effects: ${state.prettyDump}")
            case RunEffectsStarted =>
                trace("entering `runEffects`")
            case L1StateQueryError(err) =>
                error(s"error when getting Cardano L1 state: $err")
            case CurrentL1State(time, utxoIds, state) =>
                trace(
                  s"current time=$time utxoIds=${utxoIds.mkString(",")} state=${state.prettyDump}"
                )
            case CriticalError(msg) =>
                error(s"Critical error: $msg")
            case NoDirectActions =>
                trace("no direct actions due; reconciling target state")
            case TargetUtxoStatus(targetId, true) =>
                trace(s"target $targetId found, do nothing")
            case TargetUtxoStatus(targetId, false) =>
                trace(s"target $targetId gone; probing rule-based treasury / init tx")
            case InitWindowElapsed(currentTime, endTime) =>
                warn(
                  s"init tx validity window elapsed (currentTime=$currentTime >=" +
                      s" initializationTxEndTime=$endTime); head can no longer be initialized on the" +
                      " happy path — regenerate head-config or widen the init window"
                )
            case FinalizationTxStatus(hash, isKnown) =>
                trace(
                  s"finalizationTx: hash=$hash known=${if isKnown then "known" else "not known"}"
                )
            case FinalizationTxQueryError(err) =>
                error(s"error when getting finalization tx info: $err")
            case DisjointWindowViolation(treasuryUtxo, happyPathTtl, fallbackValidityStart) =>
                error(
                  s"disjoint-window invariant violated for treasury $treasuryUtxo: happy-path TTL" +
                      s" $happyPathTtl > fallback validity start $fallbackValidityStart — bad tx" +
                      " timing; stopping the liaison"
                )
            case InitTxStatus(hash, isKnown) =>
                trace(s"initTx: hash=$hash known=${if isKnown then "known" else "not known"}")
            case InitTxQueryError(err) =>
                error(s"error when getting init tx info: $err")
            case RuleBasedTreasuryQueryError(err) =>
                error(s"error when probing the rule-based treasury: $err")
            case ActionsDispatched(actions, hasFallback) =>
                val text =
                    "Liaison's actions:" + actions.map(a => s"\n\t- ${actionMsg(a)}").mkString
                if hasFallback then warn(text) else info(text)
            case FallbackToRuleBasedDispatched(txId) =>
                warn(s"FallbackToRuleBased dispatched: $txId — head entering rule-based regime")
            case TxSubmitting(txId) =>
                trace(s"Submitting tx hash: $txId")
            case SubmissionErrors(count) =>
                trace(s"Submission errors (generally ignored): $count")
        }
    }

    /** Render one [[CardanoLiaison.Action]] for logging — relocated from the actor so the event can
      * carry raw actions and this runs only when the level is enabled.
      */
    private def actionMsg(action: Action): String =
        import Action.*
        action match
            case FallbackToRuleBased(tx)         => s"FallbackToRuleBased (${tx.tx.id})"
            case PushForwardMultisig(txs)        => s"PushForwardMultisig (${txs.map(_.tx.id)}"
            case Rollout(txs)                    => s"Rollout (${txs.map(_.tx.id)}"
            case sp @ SilencePeriodNoop(_, _, _) => s"$sp"
            case InitializeHead(txs)             => s"InitializeHead (${txs.map(_.tx.id)}"
