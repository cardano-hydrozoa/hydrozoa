package hydrozoa.integration.harness

import cats.effect.IO
import hydrozoa.lib.logging.{ContraTracer, LogEvent, Slf4jTracer}
import hydrozoa.multisig.RuleBasedOnlyChildEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.rulebased.RuleBasedActorEvent

/** Test-side diagnostic tracers: rich renderings of diagnostic-only actor events that are
  * deliberately kept OUT of production formatting. Compose one with the harness tracer via `|+|`
  * during an investigation to surface extra detail, then drop it again — production formatters stay
  * clean and these accumulate as a reusable diagnostic library.
  */
object DiagnosticTracers:

    /** Render an RBA evacuation diagnostic event under the existing `RuleBasedActor` logger (so it
      * needs no new logback route and sits alongside the actor's normal output). Returns `None` for
      * any non-diagnostic event, which the harness tracer drops.
      */
    def rbrDiagnosticFormat(peerNum: HeadPeerNumber)(e: RuleBasedActorEvent): Option[LogEvent] =
        val ev = LogEvent.From.forPeer("RuleBasedActor", peerNum)
        import ev.*
        e match
            case RuleBasedActorEvent.Evacuation.CandidateMaps(latestHardConfirmed, candidateKzgs) =>
                Some(
                  info(
                    "DIAGNOSTIC candidate evac maps (latest hard-confirmed stack " +
                        s"$latestHardConfirmed): ${candidateKzgs.mkString("[", ", ", "]")}"
                  )
                )
            case RuleBasedActorEvent.Evacuation.ResolvedKzg(kzg) =>
                Some(info(s"DIAGNOSTIC resolution committed to kzg $kzg"))
            case RuleBasedActorEvent.Evacuation.CandidateMapSources(
                  defaultMapBlock,
                  defaultKzg,
                  secs
                ) =>
                Some(
                  info(
                    "DIAGNOSTIC candidate map sources: default map block " +
                        s"$defaultMapBlock -> $defaultKzg, SECs: ${secs.mkString("[", ", ", "]")}"
                  )
                )
            case RuleBasedActorEvent.Evacuation.EvacuationAnchor(anchorStack, fallbackTxId) =>
                Some(
                  info(
                    s"DIAGNOSTIC evacuation anchor: stack $anchorStack, fallback tx $fallbackTxId"
                  )
                )
            case _ => None

    /** A [[MultiPeerHeadHarness.Event]] tracer that routes head-peer RBA evacuation diagnostics to
      * slf4j via [[rbrDiagnosticFormat]]. Evacuation diagnostics are head-side, so coil events
      * (which carry none) are ignored.
      */
    val rbrDiagnostics: ContraTracer[IO, MultiPeerHeadHarness.Event] =
        def emit(peerNum: HeadPeerNumber, child: Any): IO[Unit] =
            child match
                case RuleBasedOnlyChildEvent.RuleBasedActor(e) =>
                    rbrDiagnosticFormat(peerNum)(e).fold(IO.unit)(Slf4jTracer.sink.traceWith)
                case _ => IO.unit
        ContraTracer[IO, MultiPeerHeadHarness.Event] {
            case MultiPeerHeadHarness.Event.Head(peerNum, evt) => emit(peerNum, evt)
            case MultiPeerHeadHarness.Event.Coil(_, _)         => IO.unit
        }
