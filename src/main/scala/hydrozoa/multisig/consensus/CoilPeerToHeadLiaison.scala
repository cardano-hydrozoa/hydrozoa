package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.PeerLiaison.*
import hydrozoa.multisig.consensus.PeerLiaison.Request.NewMsgBatch
import hydrozoa.multisig.consensus.ack.RelayedMsg
import hydrozoa.multisig.consensus.peer.{HeadPeerId, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber

/** A coil's single liaison, toward its hub head (§8 of `design/coil-network.md`).
  *
  * Asymmetric: its outbox carries only this coil's own hard-acks (a coil never leads a block or
  * stack, never soft-acks, and authors no user requests — those lanes stay empty), while its
  * inbound direction receives the hub's full relayed population stream and routes it to every local
  * actor, exactly as a head-mesh [[PeerLiaison]] does.
  *
  * At one head the hub is the sole author, so the inherited `author == remote` verify checks hold.
  * The multi-head relax (`author ∈ population, verified by signature`, §8.3) is a Pc4 concern,
  * noted on `BatchProtocolLiaison.State.verifyAgainst`.
  */
abstract class CoilPeerToHeadLiaison(
    config: Config,
    hubHead: HeadPeerId,
    pendingConnections: MultisigRegimeManager.PendingConnections,
) extends BatchProtocolLiaison(config) {
    private val connections = Ref.unsafe[IO, Option[CoilPeerToHeadLiaison.Connections]](None)

    override protected def remotePeerId: PeerId = PeerId.Head(hubHead.peerNum)
    override protected def remoteLabel: String = hubHead.peerNum.convert.toString

    private def getConnections: IO[CoilPeerToHeadLiaison.Connections] = for {
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
                CoilPeerToHeadLiaison.Connections(
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
            // Relayed coil hard-acks (other coils, plus this coil's own echo, deduped downstream):
            // unwrap to the raw signed ack for the local SlowConsensusActor, verified end-to-end.
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

object CoilPeerToHeadLiaison {
    def apply(
        config: Config,
        hubHead: HeadPeerId,
        pendingConnections: MultisigRegimeManager.PendingConnections,
    ): IO[CoilPeerToHeadLiaison] =
        IO(new CoilPeerToHeadLiaison(config, hubHead, pendingConnections) {})

    type Handle = ActorRef[IO, Request]

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        consensusActor: FastConsensusActor.Handle,
        stackComposer: StackComposer.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        remotePeerLiaison: PeerLiaison.Handle
    )
}
