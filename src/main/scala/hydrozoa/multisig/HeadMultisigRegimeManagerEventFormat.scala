package hydrozoa.multisig

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.HeadMultisigRegimeManagerEvent.{BW, BWL, CAS, CL, ES, FCA, HWT, JL, NWS, PL, PWT, SC, SCA, SCL, StartingActors, TerminatedActor, TerminatedDependency, WatchingActors}
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEventFormat
import hydrozoa.multisig.consensus.limiter.LimiterEventFormat
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.transport.{HubWsTransportEventFormat, NodeWsServerEventFormat, PeerWsTransportEventFormat}
import hydrozoa.multisig.consensus.{BlockWeaverEventFormat, CardanoLiaisonEventFormat, CoilAckSequencerEventFormat, EventSequencerEventFormat, FastConsensusActorEventFormat, SlowConsensusActorEventFormat, StackComposerEventFormat}
import hydrozoa.multisig.ledger.joint.JointLedgerEventFormat

/** Top-level formatter delegating to each producer's per-event formatter. */
object HeadMultisigRegimeManagerEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: HeadMultisigRegimeManagerEvent): LogEvent = {
        val ev = LogEvent.From.forPeer("HeadMultisigRegimeManager", peerNum)
        import ev.*
        e match
            case BW(bw)   => BlockWeaverEventFormat.humanFormat(peerNum)(bw)
            case JL(jl)   => JointLedgerEventFormat.humanFormat(peerNum)(jl)
            case FCA(fca) => FastConsensusActorEventFormat.humanFormat(peerNum)(fca)
            case CL(cl)   => CardanoLiaisonEventFormat.humanFormat(peerNum)(cl)
            case SC(sc)   => StackComposerEventFormat.humanFormat(peerNum)(sc)
            case SCA(sca) => SlowConsensusActorEventFormat.humanFormat(peerNum)(sca)
            case ES(es)   => EventSequencerEventFormat.humanFormat(peerNum)(es)
            case PL(remotePeerId, pl) =>
                PeerLiaisonEventFormat.humanFormat(PeerId.Head(peerNum), remotePeerId)(pl)
            case BWL(bwl)                  => LimiterEventFormat.humanFormat("BlockWeaver")(bwl)
            case SCL(scl)                  => LimiterEventFormat.humanFormat("StackComposer")(scl)
            case PWT(pwt)                  => PeerWsTransportEventFormat.humanFormat(peerNum)(pwt)
            case HWT(hwt)                  => HubWsTransportEventFormat.humanFormat(peerNum)(hwt)
            case NWS(nws)                  => NodeWsServerEventFormat.humanFormat(peerNum)(nws)
            case CAS(cas)                  => CoilAckSequencerEventFormat.humanFormat(peerNum)(cas)
            case StartingActors            => info("Starting multisig actors...")
            case WatchingActors            => info("Watching multisig actors...")
            case TerminatedActor(actor)    => warn(s"Terminated $actor actor")
            case TerminatedDependency(dep) => warn(s"Terminated dependency $dep")
    }
