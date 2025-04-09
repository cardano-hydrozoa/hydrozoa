package hydrozoa.l2.consensus.network.actor

import ox.channels.Source

trait ConsensusActor:

    /** Type for requests that run an actor.
      */
    type ReqType

    /** Type for acknowledges that an actor produces.
      */
    type AckType

    /** Type for final result an actor can produce once it has acks from all peers.
      */
    type Result

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
    def result: Source[Result]
