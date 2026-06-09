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
        e match {
            case InitialStackEffectsLearned =>
                LogEvent(
                  Level.Info,
                  "initial stack effects learned; overriding unsigned init tx + fallback",
                  ctx0,
                  routingKey = rk
                )
            case StackEffectsLearned(settlements, fallbacks, rollouts, hasFinalization) =>
                LogEvent(
                  Level.Info,
                  s"stack effects learned: ${settlements}s ${fallbacks}fb ${rollouts}ro finalization=$hasFinalization",
                  ctx0,
                  routingKey = rk
                )
        }
    }

    def jsonlFormat(peerNum: HeadPeerNumber)(e: CardanoLiaisonEvent): Option[LogEvent] = {
        val ts = System.currentTimeMillis()
        val rk = Some("hydrozoa.trace")
        e match {
            case InitialStackEffectsLearned =>
                Some(
                  LogEvent(
                    Level.Info,
                    s"""HTRACE|{"ts":$ts,"node":"$peerNum","event":"initial_stack_effects_learned"}""",
                    routingKey = rk
                  )
                )
            case StackEffectsLearned(settlements, fallbacks, rollouts, hasFinalization) =>
                Some(
                  LogEvent(
                    Level.Info,
                    s"""HTRACE|{"ts":$ts,"node":"$peerNum","event":"stack_effects_learned","settlements":$settlements,"fallbacks":$fallbacks,"rollouts":$rollouts,"finalization":$hasFinalization}""",
                    routingKey = rk
                  )
                )
        }
    }
