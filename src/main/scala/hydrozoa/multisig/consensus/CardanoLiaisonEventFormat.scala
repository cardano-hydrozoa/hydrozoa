package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
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
            case InitialStackEffectsState(dump) =>
                trace(s"state after initial-stack effects: $dump")
            case MinorOnlyStackReceived =>
                info("minor-only stack: no backbone L1 effects to submit; nothing to do")
            case StackEffectsLearned(settlements, fallbacks, rollouts, hasFinalization) =>
                info(
                  s"stack effects learned: ${settlements}s ${fallbacks}fb ${rollouts}ro finalization=$hasFinalization"
                )
            case StackEffectsState(dump) =>
                trace(s"state after stack effects: $dump")
            case RunEffectsStarted =>
                trace("entering `runEffects`")
            case L1StateQueryError(err) =>
                error(s"error when getting Cardano L1 state: $err")
            case CurrentL1State(time, utxoIds, dump) =>
                trace(s"current time=$time utxoIds=$utxoIds state=$dump")
            case CriticalError(msg) =>
                error(s"Critical error: $msg")
            case NoActionsScheduled =>
                trace("due actions is empty")
            case TargetUtxoStatus(targetId, true) =>
                trace(s"target $targetId found, do nothing")
            case TargetUtxoStatus(targetId, false) =>
                trace(s"no target $targetId found, submitting init action")
            case InitWindowElapsed(currentTime, endTime) =>
                warn(
                  s"init tx validity window elapsed (currentTime=$currentTime >=" +
                      s" initializationTxEndTime=$endTime); head can no longer be initialized on the" +
                      " happy path — regenerate head-config or widen the init window"
                )
            case FinalizationTxStatus(hash, isKnown) =>
                trace(s"finalizationTx: hash=$hash known=$isKnown")
            case FinalizationTxQueryError(err) =>
                error(s"error when getting finalization tx info: $err")
            case ActionsDispatched(msgs, hasFallback) =>
                val text = "Liaison's actions:" + msgs.map(m => s"\n\t- $m").mkString
                if hasFallback then warn(text) else info(text)
            case FallbackToRuleBasedDispatched(txId) =>
                warn(s"FallbackToRuleBased dispatched: $txId — head entering rule-based regime")
            case TxSubmitting(txId) =>
                trace(s"Submitting tx hash: $txId")
            case SubmissionErrors(count) =>
                trace(s"Submission errors (generally ignored): $count")
        }
    }
