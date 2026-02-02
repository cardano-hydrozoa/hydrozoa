package hydrozoa.integration.stage1

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.catsSyntaxFlatMapOps
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.ActorSystem
import hydrozoa.integration.stage1
import hydrozoa.integration.stage1.AgentActor.CompleteBlock
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.consensus.ConsensusActor
import hydrozoa.multisig.consensus.ack.AckBlock
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.block.{Block, BlockNumber}
import hydrozoa.multisig.ledger.event.LedgerEvent

/** Stage 1 SUT. */
case class Stage1Sut(
    system: ActorSystem[IO],
    agent: AgentActor.Handle
)

object AgentActor:

    case class CompleteBlock(
        block: CompleteBlockRegular | CompleteBlockFinal,
        blockNumber: BlockNumber
    ) extends SyncRequest[IO, CompleteBlock, Block.Unsigned.Next] {
        export CompleteBlock.Sync
        def ?: : this.Send = SyncRequest.send(_, this)
    }

    object CompleteBlock:
        type Sync = SyncRequest.Envelope[IO, CompleteBlock, Block.Unsigned.Next]

    type Request =
        LedgerEvent | StartBlock | CompleteBlock.Sync | ConsensusActor.Request | Unit

    type Handle = ActorRef[IO, Request]

case class AgentActor(
    jointLedgerD: Deferred[IO, JointLedger.Handle],
    consensusActor: ConsensusActor.Handle
) extends Actor[IO, AgentActor.Request]:

    private val jointLedgerRef = Ref.unsafe[IO, Option[JointLedger.Handle]](None)

    private def jointLedger: IO[JointLedger.Handle] = jointLedgerRef.get.map(_.get)

    override def preStart: IO[Unit] = for {
        // Message to itself to get the jointLedger actor
        _ <- context.self ! ()
    } yield ()

    override def receive: Receive[IO, AgentActor.Request] = {
        case _: Unit =>
            for {
                jointLedger <- jointLedgerD.get
                _ <- jointLedgerRef.set(Some(jointLedger))
            } yield ()

        // Sync SUT commands
        case req: CompleteBlock.Sync =>
            for {
                _ <- ref.update(_ + (req.request.blockNumber -> req))
                _ <- jointLedger >>= (_ ! req.request.block)
            } yield ()

        // Joint ledger - proxying
        case x: LedgerEvent => jointLedger >>= (_ ! x)
        case x: StartBlock  => jointLedger >>= (_ ! x)

        // Consensus actor
        // Intercepting unsigned blocks
        case x: Block.Unsigned.Next => proxyBlockUnsigned(x)
        // Direct proxying
        case x: AckBlock => consensusActor ! x
    }

    private val ref = Ref.unsafe[IO, Map[BlockNumber, CompleteBlock.Sync]](Map.empty)

    def proxyBlockUnsigned(block: Block.Unsigned.Next): IO[Unit] = for {
        _ <- consensusActor ! block
        envelope <- ref.modify { map =>
            val blockNum = block.blockNum
            val v = map(blockNum)
            val newMap = map - blockNum
            (newMap, v)
        }
        _ <- envelope.dResponse.complete(block)
    } yield ()

end AgentActor
