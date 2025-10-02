package hydrozoa.multisig.protocol

import hydrozoa.multisig.protocol.ConsensusProtocol.Actors

import com.suprnation.actor.ActorRef.NoSendActorRef

import cats.effect.IO

object ManagerProtocol {
    object Manager {

        /** Requests received by the multisig regime manager. */
        type Request = TerminatedChild | TerminatedDependency

        type Children = Actors

        enum Dependencies:
            case CardanoBackend, Persistence

        /** ==Multisig regime manager's messages== */
        final case class TerminatedChild(childType: Actors, ref: NoSendActorRef[IO])

        final case class TerminatedDependency(dependencyType: Dependencies, ref: NoSendActorRef[IO])
    }
}
