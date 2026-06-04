package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.PeerLiaison.*
import hydrozoa.multisig.consensus.PeerLiaison.Request.NewMsgBatch
import hydrozoa.multisig.consensus.peer.RemotePeer

/** A hub head's liaison toward one coil it serves (§8 of `design/coil-network.md`).
  *
  * Asymmetric, the mirror image of [[CoilPeerToHeadLiaison]]: its outbox carries everything the hub
  * holds — briefs, soft-acks, head hard-acks, and the relayed `HubCoilAckLane` (fed by the hub's
  * actors) — while its inbound direction receives only that coil's own hard-acks. Each one is
  * routed BOTH to the hub's local [[SlowConsensusActor]] (so the hub counts it toward quorum) and
  * to the [[CoilAckSequencer]] (which stamps it and relays it onto the `HubCoilAckLane`).
  *
  * Pc3 scope is one head / one coil, where the hub is the sole head author so the outbox is sourced
  * from the hub's own production (sparse briefs == full at one leader). The multi-head relay — full
  * briefs from all leaders and multiplexed soft/hard-acks sourced from the hub's received-from-mesh
  * state (§8 "cost of being a hub") — is deferred to Pc4.
  */
abstract class HeadPeerToCoilLiaison(
    config: Config,
    coilPeer: RemotePeer,
    pendingConnections: MultisigRegimeManager.PendingConnections,
) extends BatchProtocolLiaison(config, coilPeer) {
    private val connections = Ref.unsafe[IO, Option[HeadPeerToCoilLiaison.Connections]](None)

    private def getConnections: IO[HeadPeerToCoilLiaison.Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error("Head→coil liaison is missing its connections to other actors.")
          )
        )(IO.pure)
    } yield conn

    override protected def initializeConnections: IO[Unit] =
        for {
            c <- pendingConnections.get
            sequencer <- c.coilAckSequencer.fold(
              IO.raiseError(
                java.lang.Error("Head→coil liaison requires a CoilAckSequencer on its hub.")
              )
            )(IO.pure)
            _ <- connections.set(
              Some(
                HeadPeerToCoilLiaison.Connections(
                  slowConsensusActor = c.slowConsensusActor,
                  coilAckSequencer = sequencer,
                  remotePeerLiaison = c.remotePeerLiaisons(coilPeer.peerId)
                )
              )
            )
        } yield ()

    override protected def sendToRemoteLiaison(msg: Request): IO[Unit] =
        getConnections.flatMap(_.remotePeerLiaison ! msg)

    /** The coil sends only its own hard-acks. Route each both to the hub's [[SlowConsensusActor]]
      * (for quorum) and to the [[CoilAckSequencer]] (for re-publication onto the `HubCoilAckLane`).
      */
    override protected def dispatchVerifiedBatch(batch: NewMsgBatch): IO[Unit] =
        for {
            conn <- getConnections
            _ <- batch.hardAck.traverse_ { ack =>
                (conn.slowConsensusActor ! ack) >> (conn.coilAckSequencer ! ack)
            }
        } yield ()
}

object HeadPeerToCoilLiaison {
    def apply(
        config: Config,
        coilPeer: RemotePeer,
        pendingConnections: MultisigRegimeManager.PendingConnections,
    ): IO[HeadPeerToCoilLiaison] =
        IO(new HeadPeerToCoilLiaison(config, coilPeer, pendingConnections) {})

    type Handle = ActorRef[IO, Request]

    final case class Connections(
        slowConsensusActor: SlowConsensusActor.Handle,
        coilAckSequencer: CoilAckSequencer.Handle,
        remotePeerLiaison: PeerLiaison.Handle
    )
}
