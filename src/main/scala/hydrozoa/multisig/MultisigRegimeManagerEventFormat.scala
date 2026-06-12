package hydrozoa.multisig

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.MultisigRegimeManagerEvent.{BW, BWL, CL, ES, FCA, JL, PL, SC, SCA, SCL, StartingActors, TerminatedActor, TerminatedDependency, WatchingActors}
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEventFormat
import hydrozoa.multisig.consensus.limiter.LimiterEventFormat
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{BlockWeaverEventFormat, CardanoLiaisonEventFormat, EventSequencerEventFormat, FastConsensusActorEventFormat, SlowConsensusActorEventFormat, StackComposerEventFormat}
import hydrozoa.multisig.ledger.joint.JointLedgerEventFormat

/** Top-level formatter delegating to each producer's per-event formatter. */
object MultisigRegimeManagerEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: MultisigRegimeManagerEvent): LogEvent = {
        val ev = LogEvent.From.forPeer("MultisigRegimeManager", peerNum)
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
                PeerLiaisonEventFormat.humanFormat(peerNum, remotePeerId)(pl)
            case BWL(bwl)                  => LimiterEventFormat.humanFormat("BlockWeaver")(bwl)
            case SCL(scl)                  => LimiterEventFormat.humanFormat("StackComposer")(scl)
            case StartingActors            => info("Starting multisig actors...")
            case WatchingActors            => info("Watching multisig actors...")
            case TerminatedActor(actor)    => warn(s"Terminated $actor actor")
            case TerminatedDependency(dep) => warn(s"Terminated dependency $dep")
    }
