package hydrozoa.multisig

import hydrozoa.lib.logging.{Level, LogEvent}
import hydrozoa.multisig.MultisigRegimeManagerEvent.{BW, BWL, CL, ES, FCA, JL, PL, SC, SCA, SCL, StartingActors, TerminatedActor, TerminatedDependency, WatchingActors}
import hydrozoa.multisig.consensus.limiter.LimiterEventFormat
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{BlockWeaverEventFormat, CardanoLiaisonEventFormat, EventSequencerEventFormat, FastConsensusActorEventFormat, PeerLiaisonEventFormat, SlowConsensusActorEventFormat, StackComposerEventFormat}
import hydrozoa.multisig.ledger.joint.JointLedgerEventFormat

/** Top-level formatters delegating to each producer's per-event formatter. Build the prod or test
  * tracer at the wiring layer with
  * `Slf4jTracer.sink.contramap(humanFormat(peer)) |+| Slf4jTracer.sink.traceMaybe(jsonlFormat(nodeId))`.
  */
object MultisigRegimeManagerEventFormat:

    private def routingKey(peerNum: HeadPeerNumber): String = s"MultisigRegimeManager.$peerNum"
    private def baseCtx(peerNum: HeadPeerNumber): Map[String, String] =
        Map("peer" -> peerNum.toString)

    def humanFormat(peerNum: HeadPeerNumber)(e: MultisigRegimeManagerEvent): LogEvent = e match
        case BW(bw)   => BlockWeaverEventFormat.humanFormat(peerNum)(bw)
        case JL(jl)   => JointLedgerEventFormat.humanFormat(peerNum)(jl)
        case FCA(fca) => FastConsensusActorEventFormat.humanFormat(peerNum)(fca)
        case CL(cl)   => CardanoLiaisonEventFormat.humanFormat(peerNum)(cl)
        case SC(sc)   => StackComposerEventFormat.humanFormat(peerNum)(sc)
        case SCA(sca) => SlowConsensusActorEventFormat.humanFormat(peerNum)(sca)
        case ES(es)   => EventSequencerEventFormat.humanFormat(peerNum)(es)
        case PL(remotePeerId, pl) =>
            PeerLiaisonEventFormat.humanFormat(peerNum, remotePeerId.peerNum)(pl)
        case BWL(bwl) => LimiterEventFormat.humanFormat("BlockWeaver")(bwl)
        case SCL(scl) => LimiterEventFormat.humanFormat("StackComposer")(scl)
        case StartingActors =>
            LogEvent(
              Level.Info,
              "Starting multisig actors...",
              baseCtx(peerNum),
              routingKey = Some(routingKey(peerNum))
            )
        case WatchingActors =>
            LogEvent(
              Level.Info,
              "Watching multisig actors...",
              baseCtx(peerNum),
              routingKey = Some(routingKey(peerNum))
            )
        case TerminatedActor(actor) =>
            LogEvent(
              Level.Warn,
              s"Terminated $actor actor",
              baseCtx(peerNum),
              routingKey = Some(routingKey(peerNum))
            )
        case TerminatedDependency(dep) =>
            LogEvent(
              Level.Warn,
              s"Terminated dependency $dep",
              baseCtx(peerNum),
              routingKey = Some(routingKey(peerNum))
            )

    def jsonlFormat(peerNumber: HeadPeerNumber)(e: MultisigRegimeManagerEvent): Option[LogEvent] =
        e match
            case BW(bw)   => BlockWeaverEventFormat.jsonlFormat(peerNumber)(bw)
            case JL(jl)   => JointLedgerEventFormat.jsonlFormat(peerNumber)(jl)
            case FCA(fca) => FastConsensusActorEventFormat.jsonlFormat(peerNumber)(fca)
            case CL(cl)   => CardanoLiaisonEventFormat.jsonlFormat(peerNumber)(cl)
            case SC(sc)   => StackComposerEventFormat.jsonlFormat(peerNumber)(sc)
            case SCA(sca) => SlowConsensusActorEventFormat.jsonlFormat(peerNumber)(sca)
            case ES(_)    => None
            case PL(remotePeerId, pl) =>
                PeerLiaisonEventFormat.mermaidFormat(peerNumber, remotePeerId.peerNum)(pl)
            case BWL(_) | SCL(_) => None
            case StartingActors | WatchingActors | TerminatedActor(_) | TerminatedDependency(_) =>
                None
