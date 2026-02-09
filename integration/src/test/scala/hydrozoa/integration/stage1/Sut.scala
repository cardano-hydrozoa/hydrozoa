package hydrozoa.integration.stage1

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.catsSyntaxFlatMapOps
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.ActorSystem
import hydrozoa.integration.stage1
import hydrozoa.integration.stage1.AgentActor.CompleteBlock
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.ConsensusActor
import hydrozoa.multisig.consensus.ack.AckBlock
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockNumber}
import hydrozoa.multisig.ledger.event.LedgerEvent
import org.scalacheck.commands.SutCommand
import scala.concurrent.duration.DurationInt

// ===================================
// Stage 1 SUT
// ===================================

case class Stage1Sut(
    system: ActorSystem[IO],
    cardanoBackend: CardanoBackend[IO],
    agent: AgentActor.Handle,
    effectsAcc: Ref[IO, List[BlockEffects.Unsigned]] = Ref.unsafe(List.empty)
)

// ===================================
// Agent Actor
// ===================================

object AgentActor:

    /** Synchronous complete block msg that returns unsigned block. This is needed for at least one
      * command should return a meaningful result - the block brief. Additionally, the Stage 1 test
      * suite saves all block L1 effects in [[Stage1Sut.effectsAcc]] to verify that all needed were
      * submitted to L1.
      *
      * @param block
      * @param blockNumber
      */
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

// ===================================
// SutCommand instances
// ===================================

val logger = Logging.loggerIO("Stage1.Commands")

implicit object DelayCommandSut extends SutCommand[DelayCommand, Unit, Stage1Sut] {
    override def run(cmd: DelayCommand, sut: Stage1Sut): IO[Unit] = for {
        _ <- logger.debug(s">> DelayCommand(delay=${cmd.delaySpec.duration})")
        now <- IO.realTimeInstant
        _ <- logger.debug(s"Current time: $now")
    } yield ()
}

implicit object StartBlockCommandSut extends SutCommand[StartBlockCommand, Unit, Stage1Sut] {
    override def run(cmd: StartBlockCommand, sut: Stage1Sut): IO[Unit] =
        logger.debug(s">> StartBlockCommand(blockNumber=${cmd.blockNumber})") >>
            (sut.agent ! StartBlock(
              blockNum = cmd.blockNumber,
              blockCreationTime = cmd.creationTime
            ))
}

implicit object LedgerEventCommandSut extends SutCommand[LedgerEventCommand, Unit, Stage1Sut] {
    override def run(cmd: LedgerEventCommand, sut: Stage1Sut): IO[Unit] =
        logger.debug(">> LedgerEventCommand") >>
            (sut.agent ! cmd.event)
}

implicit object CompleteBlockCommandSut
    extends SutCommand[CompleteBlockCommand, BlockBrief, Stage1Sut] {
    override def run(cmd: CompleteBlockCommand, sut: Stage1Sut): IO[BlockBrief] = for {
        _ <- logger.debug(
          s">> CompleteBlockCommand(blockNumber=${cmd.blockNumber}, isFinal=${cmd.isFinal})"
        )
        block <- IO.pure(
          if cmd.isFinal
          then CompleteBlockFinal(None)
          else CompleteBlockRegular(None, Set.empty, false)
        )
        // All sync commands should be timed out since the system may terminate
        d <- (sut.agent ?: AgentActor.CompleteBlock(block, cmd.blockNumber)).timeout(5.seconds)
        // Save unsigned block effects
        _ <- sut.effectsAcc.update(_ :+ d.effects.asInstanceOf[BlockEffects.Unsigned])
    } yield d.blockBrief
}
