package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.Wallet
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.tx.initialization.InitTxBuilder
import hydrozoa.l1.multisig.tx.refund.RefundTxBuilder
import hydrozoa.l2.consensus.network.*
import hydrozoa.node.state.NodeState
import ox.channels.{ActorRef, Source}

trait ConsensusActor:

    /** Type for requests that run an actor. Through this type we learn the result type of the actor
      * as well.
      */
    type ReqType <: Req

    protected var req: ReqType = _

    /** Type for acknowledges that an actor produces.
      */
    type AckType <: Ack // FIXME: this guy is already in the ReqType

    /** Non-reentrant method that handles a Req*, producing own Ack*. For proactive spawning should
      * be called immediately after instantiating. The method should call deliver to deliver the own
      * Ack* locally.
      * @param req
      *   own or some else's Req*
      * @return
      *   own Ack*
      */
    def init(req: ReqType): AckType

    /** Handles an incoming ack, including own ack. Mostly is supposed to store the ack and to
      * attempt to produce the final result. Can be called multiple times. The semantics of
      * duplicate invocations is not clear yet. Should be idempotent, but probably should throw if
      * previous peer's ack was different.
      * @param ack
      *   own or someone else's ack
      */
    def deliver(ack: AckType): Unit

    /** Rendezvous channel to get the final result.
      * @return
      *   the channel to receive the result from
      */
    def result(using req: Req): Source[req.resultType]

end ConsensusActor

class ConsensusActorFactory(
    val stateActor: ActorRef[NodeState],
    val walletActor: ActorRef[Wallet],
    val cardanoActor: ActorRef[CardanoL1],
    val initTxBuilder: InitTxBuilder,
    val refundTxBuilder: RefundTxBuilder
):

    private val log = Logger(getClass)

    def spawnByReq(req: Req): (ConsensusActor, Ack) =
        log.info("spawnByReq")
        req match
            case req: ReqVerKey =>
                val actor = mkVerificationKeyActor
                val ownAck = actor.init(req)
                actor -> ownAck
            case req: ReqInit =>
                val actor = mkInitHeadActor
                val ownAck = actor.init(req)
                actor -> ownAck
            case req: ReqRefundLater =>
                val actor = mkRefundLaterActor
                val ownAck = actor.init(req)
                actor -> ownAck
            case req: ReqEventL2 =>
                val actor = mkEventL2Actor
                val ownAck = actor.init(req)
                actor -> ownAck

    def spawnByAck(ack: Ack): Option[ConsensusActor] =
        log.info("spawnByAck")
        ack match
            case ack: AckVerKey =>
                val actor = mkVerificationKeyActor
                actor.deliver(ack)
                Some(actor)
            case ack: AckInit =>
                val actor = mkInitHeadActor
                actor.deliver(ack)
                Some(actor)
            case ack: AckRefundLater =>
                val actor = mkRefundLaterActor
                actor.deliver(ack)
                Some(actor)
            case ack: AckUnit =>
                None

    private def mkVerificationKeyActor =
        new VerificationKeyActor(stateActor, walletActor)

    private def mkRefundLaterActor =
        new RefundLaterActor(
          stateActor,
          walletActor,
          refundTxBuilder
        )

    private def mkInitHeadActor =
        new InitHeadActor(
          stateActor,
          walletActor,
          cardanoActor,
          initTxBuilder
        )

    private def mkEventL2Actor = new EventL2Actor(stateActor)

end ConsensusActorFactory
