package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.Wallet
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.tx.initialization.InitTxBuilder
import hydrozoa.l2.consensus.network.*
import hydrozoa.node.state.NodeState
import ox.channels.{ActorRef, Source}

trait ConsensusActor:

    /** Type for requests that run an actor. Through this type we learn the result type of the actor
      * as well.
      */
    type ReqType <: Req

    /** Type for acknowledges that an actor produces.
      */
    type AckType <: Ack

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
    val initTxBuilder: InitTxBuilder
):

    private val log = Logger(getClass)

    def spawnByReq(req: Req): (ConsensusActor, Ack) =
        log.info("spawnByReq")
        req match
            case reqVerKey: ReqVerKey =>
                val actor = new VerificationKeyActor(stateActor, walletActor)
                val ack = actor.init(reqVerKey)
                actor -> ack
            case reqInit: ReqInit =>
                val actor = new InitHeadActor(
                  stateActor,
                  walletActor,
                  cardanoActor,
                  initTxBuilder
                )
                val ack = actor.init(reqInit)
                actor -> ack

    def spawnByAck(ack: Ack): ConsensusActor =
        log.info("spawnByAck")
        ack match
            case ackVerKey: AckVerKey =>
                val actor = new VerificationKeyActor(stateActor, walletActor)
                actor.deliver(ackVerKey)
                actor
            case ackInit: AckInit =>
                val actor = new InitHeadActor(
                    stateActor,
                    walletActor,
                    cardanoActor,
                    initTxBuilder
                )
                actor.deliver(ackInit)
                actor