package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.PeerLiaisonHeadToHead.*
import hydrozoa.multisig.consensus.PeerLiaisonHeadToHead.Request.NewMsgBatch
import hydrozoa.multisig.consensus.ack.RelayedMsg
import hydrozoa.multisig.consensus.peer.{HeadPeerId, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber

/** A coil peer's single liaison, toward its hub head peer (§8 of `design/coil-network.md`).
  *
  * Asymmetric: its outbox carries only this coil peer's own hard-acks (a coil peer never leads a
  * block or stack, never soft-acks, and authors no user requests — those lanes stay empty), while
  * its inbound direction receives the hub's full relayed population stream and routes it to every
  * local actor, exactly as a head-peer-mesh [[PeerLiaisonHeadToHead]] does.
  *
  * The hub's own soft-/hard-acks ride the direct lanes (the inherited `author == remote` verify
  * checks hold); the rest of the population rides the relayed `relayedMsg` / `HubHardAckLane`
  * lanes, de-muxed by embedded author and verified end-to-end by signature.
  */
abstract class PeerLiaisonCoilToHead(
    config: Config,
    hubHead: HeadPeerId,
    pendingConnections: MultisigRegimeManager.PendingConnections,
) extends PeerLiaisonBase(config) {
    private val connections = Ref.unsafe[IO, Option[PeerLiaisonCoilToHead.Connections]](None)

    override protected def remotePeerId: PeerId = PeerId.Head(hubHead.peerNum)
    override protected def remoteLabel: String = hubHead.peerNum.convert.toString

    private def getConnections: IO[PeerLiaisonCoilToHead.Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error("Coil→head liaison is missing its connections to other actors.")
          )
        )(IO.pure)
    } yield conn

    override protected def initializeConnections: IO[Unit] =
        for {
            c <- pendingConnections.get
            _ <- connections.set(
              Some(
                PeerLiaisonCoilToHead.Connections(
                  blockWeaver = c.blockWeaver,
                  consensusActor = c.consensusActor,
                  stackComposer = c.stackComposer,
                  slowConsensusActor = c.slowConsensusActor,
                  remotePeerLiaison = c.remotePeerLiaisons(remotePeerId)
                )
              )
            )
        } yield ()

    // The hub relays every block/stack in number order (not a leader subset), so the inbound brief
    // lanes are contiguous (§8). Block 0 / stack 0 are the out-of-band bootstrap items, so the
    // first relayed item — and the initial cursor — is 1.
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

    override protected def dispatchVerifiedBatch(batch: NewMsgBatch): IO[Unit] =
        for {
            conn <- getConnections
            _ <- batch.softAck.traverse_(conn.consensusActor ! _)
            _ <- batch.blockBrief.traverse_(conn.blockWeaver ! _)
            _ <- batch.stackBrief.traverse_(conn.stackComposer ! _)
            _ <- batch.hardAck.traverse_(conn.slowConsensusActor ! _)
            // Relayed coil peer hard-acks (other coil peers, plus this coil peer's own echo,
            // deduped downstream): unwrap to the raw signed ack for the local SlowConsensusActor,
            // verified end-to-end.
            _ <- batch.hubHardAck.traverse_(hc => conn.slowConsensusActor ! hc.ack)
            // The hub→coil relay lane: de-mux each wrapped ack by type+author. Soft-acks to the
            // FastConsensusActor, hard-acks to the SlowConsensusActor; each verifies the embedded
            // signature and aggregates by the embedded author (the de-mux into per-author lanes).
            _ <- batch.relayedMsg.traverse_ {
                case RelayedMsg.Soft(_, ack) => conn.consensusActor ! ack
                case RelayedMsg.Hard(_, ack) => conn.slowConsensusActor ! ack
                case RelayedMsg.Req(_, req)  => conn.blockWeaver ! req
            }
            _ <- batch.requests.traverse_(conn.blockWeaver ! _)
        } yield ()
}

object PeerLiaisonCoilToHead {
    def apply(
        config: Config,
        hubHead: HeadPeerId,
        pendingConnections: MultisigRegimeManager.PendingConnections,
    ): IO[PeerLiaisonCoilToHead] =
        IO(new PeerLiaisonCoilToHead(config, hubHead, pendingConnections) {})

    type Handle = ActorRef[IO, Request]

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        consensusActor: FastConsensusActor.Handle,
        stackComposer: StackComposer.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        remotePeerLiaison: PeerLiaisonHeadToHead.Handle
    )
}
