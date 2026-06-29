package hydrozoa.multisig

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.liaison.PeerLiaisonEventFormat
import hydrozoa.multisig.consensus.limiter.LimiterEventFormat
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.transport.CoilPeerWsTransportEventFormat
import hydrozoa.multisig.consensus.{BlockWeaverEventFormat, CardanoLiaisonEventFormat, FastConsensusActorEventFormat, SlowConsensusActorEventFormat, StackComposerEventFormat}
import hydrozoa.multisig.ledger.joint.JointLedgerEventFormat

/** Top-level formatter for the (coil, multisig) cell, delegating by category trait then by
  * per-actor format.
  *
  * TODO: the per-actor sub-formatters (`BlockWeaverEventFormat`, `JointLedgerEventFormat`, …) all
  * take a `HeadPeerNumber`. We pass `syntheticLabel` here for `nHeadPeers + coilNum.convert` so
  * coil log lines stay distinguishable from head ones in the same run. The proper fix is to make
  * those sub-formatters polymorphic in the peer identity — out of scope here.
  */
object CoilMultisigRegimeManagerEventFormat:

    def humanFormat(
        syntheticLabel: HeadPeerNumber,
        coilNum: CoilPeerNumber,
    )(e: CoilMultisigRegimeManagerEvent): LogEvent = {
        val ev = LogEvent.From.forPeer("CoilMultisigRegimeManager", syntheticLabel)
        import ev.*
        e match
            case LifecycleEvent.StartingActors            => info("Starting multisig actors...")
            case LifecycleEvent.WatchingActors            => info("Watching multisig actors...")
            case LifecycleEvent.TerminatedActor(actor)    => warn(s"Terminated $actor actor")
            case LifecycleEvent.TerminatedDependency(dep) => warn(s"Terminated dependency $dep")
            case CommonChildEvent.BlockWeaver(bw) =>
                BlockWeaverEventFormat.humanFormat(syntheticLabel)(bw)
            case CommonChildEvent.JointLedger(jl) =>
                JointLedgerEventFormat.humanFormat(syntheticLabel)(jl)
            case CommonChildEvent.FastConsensusActor(fca) =>
                FastConsensusActorEventFormat.humanFormat(syntheticLabel)(fca)
            case CommonChildEvent.CardanoLiaison(cl) =>
                CardanoLiaisonEventFormat.humanFormat(syntheticLabel)(cl)
            case CommonChildEvent.StackComposer(sc) =>
                StackComposerEventFormat.humanFormat(syntheticLabel)(sc)
            case CommonChildEvent.SlowConsensusActor(sca) =>
                SlowConsensusActorEventFormat.humanFormat(syntheticLabel)(sca)
            case CommonChildEvent.PeerLiaison(remotePeerId, pl) =>
                PeerLiaisonEventFormat.humanFormat(PeerId.Coil(coilNum), remotePeerId)(pl)
            case CoilOnlyChildEvent.CoilPeerWsTransport(cpwt) =>
                CoilPeerWsTransportEventFormat.humanFormat(coilNum)(cpwt)
            case MultisigOnlyChildEvent.BlockWeaverLimiter(bwl) =>
                LimiterEventFormat.humanFormat("BlockWeaver")(bwl)
            case MultisigOnlyChildEvent.StackComposerLimiter(scl) =>
                LimiterEventFormat.humanFormat("StackComposer")(scl)
    }
