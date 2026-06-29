package hydrozoa.multisig

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEventFormat
import hydrozoa.multisig.consensus.limiter.LimiterEventFormat
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.transport.{HubWsTransportEventFormat, NodeWsServerEventFormat, PeerTransportEventFormat}
import hydrozoa.multisig.consensus.{BlockWeaverEventFormat, CardanoLiaisonEventFormat, CoilAckSequencerEventFormat, EventSequencerEventFormat, FastConsensusActorEventFormat, SlowConsensusActorEventFormat, StackComposerEventFormat}
import hydrozoa.multisig.ledger.joint.JointLedgerEventFormat

/** Top-level formatter delegating by category trait, then by per-actor format. */
object HeadMultisigRegimeManagerEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: HeadMultisigRegimeManagerEvent): LogEvent = {
        val ev = LogEvent.From.forPeer("HeadMultisigRegimeManager", peerNum)
        import ev.*
        e match
            case LifecycleEvent.StartingActors            => info("Starting multisig actors...")
            case LifecycleEvent.WatchingActors            => info("Watching multisig actors...")
            case LifecycleEvent.TerminatedActor(actor)    => warn(s"Terminated $actor actor")
            case LifecycleEvent.TerminatedDependency(dep) => warn(s"Terminated dependency $dep")
            case CommonChildEvent.BlockWeaver(bw) =>
                BlockWeaverEventFormat.humanFormat(peerNum)(bw)
            case CommonChildEvent.JointLedger(jl) =>
                JointLedgerEventFormat.humanFormat(peerNum)(jl)
            case CommonChildEvent.FastConsensusActor(fca) =>
                FastConsensusActorEventFormat.humanFormat(peerNum)(fca)
            case CommonChildEvent.CardanoLiaison(cl) =>
                CardanoLiaisonEventFormat.humanFormat(peerNum)(cl)
            case CommonChildEvent.StackComposer(sc) =>
                StackComposerEventFormat.humanFormat(peerNum)(sc)
            case CommonChildEvent.SlowConsensusActor(sca) =>
                SlowConsensusActorEventFormat.humanFormat(peerNum)(sca)
            case HeadOnlyChildEvent.EventSequencer(es) =>
                EventSequencerEventFormat.humanFormat(peerNum)(es)
            case CommonChildEvent.PeerLiaison(remotePeerId, pl) =>
                PeerLiaisonEventFormat.humanFormat(PeerId.Head(peerNum), remotePeerId)(pl)
            case HeadOnlyChildEvent.PeerTransport(pt) =>
                PeerTransportEventFormat.humanFormat(peerNum)(pt)
            case HeadOnlyChildEvent.NodeWsServer(nws) =>
                NodeWsServerEventFormat.humanFormat(peerNum)(nws)
            case HeadOnlyChildEvent.HubWsTransport(hwt) =>
                HubWsTransportEventFormat.humanFormat(peerNum)(hwt)
            case HeadOnlyChildEvent.CoilAckSequencer(cas) =>
                CoilAckSequencerEventFormat.humanFormat(peerNum)(cas)
            case MultisigOnlyChildEvent.BlockWeaverLimiter(bwl) =>
                LimiterEventFormat.humanFormat("BlockWeaver")(bwl)
            case MultisigOnlyChildEvent.StackComposerLimiter(scl) =>
                LimiterEventFormat.humanFormat("StackComposer")(scl)
    }
