package hydrozoa.rulebased

import cats.effect.IO
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import scalus.cardano.ledger.Utxos

case class LiquidationActor()

object LiquidationActor {
    type Handle = ActorRef[IO, Requests.Request]

    object Requests {
        type Request = Unit
    }

    case class State(utxosToWithdraw: Utxos, currentKZG: KzgCommitment)

}
