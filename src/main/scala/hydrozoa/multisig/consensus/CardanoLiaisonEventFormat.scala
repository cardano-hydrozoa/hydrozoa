package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.consensus.CardanoLiaisonEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[CardanoLiaisonEvent]] to [[LogEvent]] for various back-end sinks. */
object CardanoLiaisonEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"CardanoLiaison.$peerNum"

    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    def humanFormat(peerNum: HeadPeerNumber)(e: CardanoLiaisonEvent): LogEvent = {
        val rk = Some(routingKey(peerNum))
        val ctx0 = baseCtx(peerNum)
        def trace(msg: String) = LogEvent(Level.Trace, msg, ctx0, routingKey = rk)
        def info(msg: String) = LogEvent(Level.Info, msg, ctx0, routingKey = rk)
        def warn(msg: String) = LogEvent(Level.Warn, msg, ctx0, routingKey = rk)
        def error(msg: String) = LogEvent(Level.Error, msg, ctx0, routingKey = rk)
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
            case FinalizationTxStatus(hash, isKnown) =>
                trace(s"finalizationTx: hash=$hash known=$isKnown")
            case FinalizationTxQueryError(err) =>
                error(s"error when getting finalization tx info: $err")
            case ActionsDispatched(msgs, hasFallback) =>
                val text = "Liaison's actions:" + msgs.map(m => s"\n\t- $m").mkString
                if hasFallback then warn(text) else info(text)
            case TxSubmitting(txId) =>
                trace(s"Submitting tx hash: $txId")
            case SubmissionErrors(count) =>
                trace(s"Submission errors (generally ignored): $count")
        }
    }

    def jsonlFormat(peerNum: HeadPeerNumber)(e: CardanoLiaisonEvent): Option[LogEvent] = {
        val ts = System.currentTimeMillis()
        val rk = Some("hydrozoa.trace")
        def htrace(json: String) = LogEvent(Level.Info, s"HTRACE|$json", routingKey = rk)
        e match {
            case InitialStackEffectsLearned =>
                Some(
                  htrace(
                    s"""{"ts":$ts,"node":"$peerNum","event":"initial_stack_effects_learned"}"""
                  )
                )
            case StackEffectsLearned(settlements, fallbacks, rollouts, hasFinalization) =>
                Some(
                  htrace(
                    s"""{"ts":$ts,"node":"$peerNum","event":"stack_effects_learned","settlements":$settlements,"fallbacks":$fallbacks,"rollouts":$rollouts,"finalization":$hasFinalization}"""
                  )
                )
            case StackHardConfirmedReceived(stackNum) =>
                Some(
                  htrace(
                    s"""{"ts":$ts,"node":"$peerNum","event":"stack_hard_confirmed_received","stack":"$stackNum"}"""
                  )
                )
            case ActionsDispatched(_, hasFallback) =>
                Some(
                  htrace(
                    s"""{"ts":$ts,"node":"$peerNum","event":"actions_dispatched","has_fallback":$hasFallback}"""
                  )
                )
            case _ => None
        }
    }
