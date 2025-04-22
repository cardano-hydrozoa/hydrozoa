package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.Wallet
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.tx.finalization.FinalizationTxBuilder
import hydrozoa.l1.multisig.tx.initialization.InitTxBuilder
import hydrozoa.l1.multisig.tx.refund.RefundTxBuilder
import hydrozoa.l1.multisig.tx.settlement.SettlementTxBuilder
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
      *   own Ack* -
      */
    def init(req: ReqType): Seq[AckType]

    /** Handles an incoming ack, including own ack. Mostly is supposed to store the ack and to
      * attempt to produce the final result. Can be called multiple times. The semantics of
      * duplicate invocations is not clear yet. Should be idempotent, but probably should throw if
      * previous peer's ack was different.
      * @param ack
      *   own or someone else's ack
      * @return
      *   for actors that uses two-phase acks (major/final block producers) deliver may reurn a
      *   phase-two ack.
      */
    def deliver(ack: AckType): Option[AckType]

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
    val refundTxBuilder: RefundTxBuilder,
    val settlementTxBuilder: SettlementTxBuilder,
    val finalizationTxBuilder: FinalizationTxBuilder
):

    private val log = Logger(getClass)

    def spawnByReq(req: Req): (ConsensusActor, Seq[Ack]) =
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
            case req: ReqMinor =>
                val actor = mkMinorBlockActor
                val ownAck = actor.init(req)
                actor -> ownAck
            case req: ReqMajor =>
                val actor = mkMajorBlockActor
                val ownAck = actor.init(req)
                actor -> ownAck
            case req: ReqFinal =>
                val actor = mkFinalBlockActor
                val ownAck = actor.init(req)
                actor -> ownAck

    def spawnByAck(ack: Ack): (Option[ConsensusActor], Option[Ack]) =
        log.info("spawnByAck")
        ack match
            case ack: AckVerKey =>
                val actor = mkVerificationKeyActor
                val mbAck = actor.deliver(ack)
                Some(actor) -> mbAck
            case ack: AckInit =>
                val actor = mkInitHeadActor
                val mbAck = actor.deliver(ack)
                Some(actor) -> mbAck
            case ack: AckRefundLater =>
                val actor = mkRefundLaterActor
                val mbAck = actor.deliver(ack)
                Some(actor) -> None
            case ack: AckUnit =>
                (None, None)
            case ack: AckMinor =>
                val actor = mkMinorBlockActor
                val mbAck = actor.deliver(ack)
                Some(actor) -> mbAck
            case ack: AckMajor =>
                val actor = mkMajorBlockActor
                val mbAck = actor.deliver(ack)
                Some(actor) -> mbAck
            case ack: AckMajor2 =>
                val actor = mkMajorBlockActor
                val mbAck = actor.deliver(ack)
                Some(actor) -> mbAck
            case ack: AckFinal =>
                val actor = mkFinalBlockActor
                val mbAck = actor.deliver(ack)
                Some(actor) -> mbAck
            case ack: AckFinal2 =>
                val actor = mkFinalBlockActor
                val mbAck = actor.deliver(ack)
                Some(actor) -> mbAck

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

    private def mkMinorBlockActor = new MinorBlockConfirmationActor(stateActor, walletActor)

    private def mkMajorBlockActor =
        new MajorBlockConfirmationActor(stateActor, walletActor, settlementTxBuilder)

    private def mkFinalBlockActor =
        new FinalBlockConfirmationActor(stateActor, walletActor, finalizationTxBuilder)

end ConsensusActorFactory
