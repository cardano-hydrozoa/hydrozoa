package hydrozoa.multisig

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.HeadMultisigRegimeManagerEvent.{BlockWeaver, BlockWeaverLimiter, CardanoLiaison, CoilAckSequencer, EventSequencer, FastConsensusActor, HubWsTransport, JointLedger, NodeWsServer, PeerLiaison, PeerTransport, SlowConsensusActor, StackComposer, StackComposerLimiter, StartingActors, TerminatedActor, TerminatedDependency, WatchingActors}
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEventFormat
import hydrozoa.multisig.consensus.limiter.LimiterEventFormat
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.transport.{HubWsTransportEventFormat, NodeWsServerEventFormat, PeerTransportEventFormat}
import hydrozoa.multisig.consensus.{BlockWeaverEventFormat, CardanoLiaisonEventFormat, CoilAckSequencerEventFormat, EventSequencerEventFormat, FastConsensusActorEventFormat, SlowConsensusActorEventFormat, StackComposerEventFormat}
import hydrozoa.multisig.ledger.joint.JointLedgerEventFormat

/** Top-level formatter delegating to each producer's per-event formatter. */
object HeadMultisigRegimeManagerEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: HeadMultisigRegimeManagerEvent): LogEvent = {
        val ev = LogEvent.From.forPeer("HeadMultisigRegimeManager", peerNum)
        import ev.*
        e match
            case BlockWeaver(bw)         => BlockWeaverEventFormat.humanFormat(peerNum)(bw)
            case JointLedger(jl)         => JointLedgerEventFormat.humanFormat(peerNum)(jl)
            case FastConsensusActor(fca) => FastConsensusActorEventFormat.humanFormat(peerNum)(fca)
            case CardanoLiaison(cl)      => CardanoLiaisonEventFormat.humanFormat(peerNum)(cl)
            case StackComposer(sc)       => StackComposerEventFormat.humanFormat(peerNum)(sc)
            case SlowConsensusActor(sca) => SlowConsensusActorEventFormat.humanFormat(peerNum)(sca)
            case EventSequencer(es)      => EventSequencerEventFormat.humanFormat(peerNum)(es)
            case PeerLiaison(remotePeerId, pl) =>
                PeerLiaisonEventFormat.humanFormat(PeerId.Head(peerNum), remotePeerId)(pl)
            case BlockWeaverLimiter(bwl)   => LimiterEventFormat.humanFormat("BlockWeaver")(bwl)
            case StackComposerLimiter(scl) => LimiterEventFormat.humanFormat("StackComposer")(scl)
            case PeerTransport(pt)         => PeerTransportEventFormat.humanFormat(peerNum)(pt)
            case HubWsTransport(hwt)       => HubWsTransportEventFormat.humanFormat(peerNum)(hwt)
            case NodeWsServer(nws)         => NodeWsServerEventFormat.humanFormat(peerNum)(nws)
            case CoilAckSequencer(cas)     => CoilAckSequencerEventFormat.humanFormat(peerNum)(cas)
            case StartingActors            => info("Starting multisig actors...")
            case WatchingActors            => info("Watching multisig actors...")
            case TerminatedActor(actor)    => warn(s"Terminated $actor actor")
            case TerminatedDependency(dep) => warn(s"Terminated dependency $dep")
    }
