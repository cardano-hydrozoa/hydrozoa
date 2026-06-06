package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.PeerLiaisonHeadToHead.*
import hydrozoa.multisig.consensus.PeerLiaisonHeadToHead.Request.NewMsgBatch
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber

/** A hub head peer's liaison toward one coil peer it serves (§8 of `design/coil-network.md`).
  *
  * Asymmetric, the mirror image of [[PeerLiaisonCoilToHub]].
  *
  * '''Lanes exercised.''' All three liaison shapes share the same fat `GetMsgBatch` / `NewMsgBatch`
  * (8 lanes), but each uses only a subset; the rest stay `None`. Outbound (hub → coil) carries
  * exactly three lanes: `blockBrief` and `stackBrief` (fanned contiguously from `JointLedger` /
  * `StackComposer`) and `relayedMsg` (the whole population's soft-acks / hard-acks / requests,
  * already multiplexed by [[CoilLinkRelay]], the hub's own included). It carries NO separate
  * `softAck` / `hardAck` / `hubHardAck` (`HubHardAckLane`) / `request` lanes — those either ride
  * inside `relayedMsg` or go only to the head mesh. Inbound (coil → hub) carries exactly one lane:
  * the coil peer's own `hardAck`, routed BOTH to the hub's [[SlowConsensusActor]] (for quorum) and
  * to the [[CoilAckSequencer]] (which stamps it onto the mesh's `HubHardAckLane`). A clean
  * per-shape split of these types is deferred (see `design/coil-network.md` §8).
  */
abstract class PeerLiaisonHubToCoil(
    config: Config,
    coil: CoilPeerNumber,
    pendingConnections: MultisigRegimeManager.PendingConnections,
) extends PeerLiaisonBase(config) {
    private val connections = Ref.unsafe[IO, Option[PeerLiaisonHubToCoil.Connections]](None)

    override protected def remotePeerId: PeerId = PeerId.Coil(coil)
    override protected def remoteLabel: String = s"c${coil.convert}"

    private def getConnections: IO[PeerLiaisonHubToCoil.Connections] = for {
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
                PeerLiaisonHubToCoil.Connections(
                  slowConsensusActor = c.slowConsensusActor,
                  coilAckSequencer = sequencer,
                  remotePeerLiaison = c.remotePeerLiaisons(remotePeerId)
                )
              )
            )
        } yield ()

    // The hub relays every block/stack in number order, so the outbound brief lanes are contiguous
    // (§8), not the head mesh's sparse own-led subset. Block 0 / stack 0 are out-of-band bootstrap,
    // so the first relayed item — and the initial cursor — is 1.
    override protected def nextOwnBriefBlock(after: BlockNumber): Option[BlockNumber] =
        Some(after.increment)
    override protected def nextOwnBriefStack(after: StackNumber): Option[StackNumber] =
        Some(after.increment)
    override protected def nextRemoteBriefBlock(after: BlockNumber): Option[BlockNumber] =
        Some(after.increment)
    override protected def nextRemoteBriefStack(after: StackNumber): Option[StackNumber] =
        Some(after.increment)

    override protected def sendToRemoteLiaison(msg: Request): IO[Unit] =
        getConnections.flatMap(_.remotePeerLiaison ! msg)

    /** The coil peer sends only its own hard-acks. Route each both to the hub's
      * [[SlowConsensusActor]] (for quorum) and to the [[CoilAckSequencer]] (for re-publication onto
      * the `HubHardAckLane`).
      */
    override protected def dispatchVerifiedBatch(batch: NewMsgBatch): IO[Unit] =
        for {
            conn <- getConnections
            _ <- batch.hardAck.traverse_ { ack =>
                (conn.slowConsensusActor ! ack) >> (conn.coilAckSequencer ! ack)
            }
        } yield ()
}

object PeerLiaisonHubToCoil {
    def apply(
        config: Config,
        coil: CoilPeerNumber,
        pendingConnections: MultisigRegimeManager.PendingConnections,
    ): IO[PeerLiaisonHubToCoil] =
        IO(new PeerLiaisonHubToCoil(config, coil, pendingConnections) {})

    type Handle = ActorRef[IO, Request]

    final case class Connections(
        slowConsensusActor: SlowConsensusActor.Handle,
        coilAckSequencer: CoilAckSequencer.Handle,
        remotePeerLiaison: PeerLiaisonHeadToHead.Handle
    )
}
