package hydrozoa.multisig

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.MultisigRegimeManagerEvent.{FCA, JL}
import hydrozoa.multisig.consensus.FastConsensusActorEventFormat
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.joint.JointLedgerEventFormat

/** Top-level formatters delegating to each producer's per-event formatter. Build the prod or test
  * tracer at the wiring layer with
  * `Tracer.sink.contramap(humanFormat(peer)) |+| Tracer.sink.traceMaybe(jsonlFormat(nodeId))`.
  */
object MultisigRegimeManagerEventFormat:

    def humanFormat(peerNum: HeadPeerNumber)(e: MultisigRegimeManagerEvent): LogEvent = e match
        case JL(jl)   => JointLedgerEventFormat.humanFormat(peerNum)(jl)
        case FCA(fca) => FastConsensusActorEventFormat.humanFormat(peerNum)(fca)

    def jsonlFormat(peerNumber: HeadPeerNumber)(e: MultisigRegimeManagerEvent): Option[LogEvent] =
        e match
            case JL(jl)   => JointLedgerEventFormat.jsonlFormat(peerNumber)(jl)
            case FCA(fca) => FastConsensusActorEventFormat.jsonlFormat(peerNumber)(fca)
