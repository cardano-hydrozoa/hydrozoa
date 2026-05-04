package hydrozoa.integration.stage1

import cats.effect.{Deferred, IO, IOLocal, Ref}
import cats.syntax.all.catsSyntaxFlatMapOps
import com.bloxbean.cardano.client.util.HexUtil
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.ActorSystem
import hydrozoa.integration.stage1
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.integration.stage1.AgentActor.CompleteBlock
import hydrozoa.integration.stage1.Commands.*
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.BlockWeaver.LocalFinalizationTrigger
import hydrozoa.multisig.consensus.CardanoLiaison.Timeout
import hydrozoa.multisig.consensus.ack.AckBlock
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.consensus.{CardanoLiaison, ConsensusActor, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockNumber}
import hydrozoa.multisig.ledger.joint
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import org.scalacheck.commands.SutCommand

import scala.concurrent.duration.DurationInt
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.Transaction
import scalus.utils.Pretty

// ===================================
// Stage 1 SUT
// ===================================

case class Stage1Sut(
    headAddress: ShelleyAddress,
    system: ActorSystem[IO],
    cardanoBackend: CardanoBackend[IO],
    agent: AgentActor.Handle,
    tracerLocal: IOLocal[Tracer],
    effectsAcc: Ref[IO, List[BlockEffects.Unsigned]] = Ref.unsafe(List.empty),
    runId: String = "",
    traceRef: Ref[IO, List[String]] = Ref.unsafe(List.empty)
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
        UserRequestWithId | StartBlock | CompleteBlock.Sync | ConsensusActor.Request |
            CardanoLiaison.Timeout.type | Unit | joint.JointLedger.Requests.GetState.Sync

    type Handle = ActorRef[IO, Request]

case class AgentActor(
    jointLedgerD: Deferred[IO, JointLedger.Handle],
    consensusActorD: Deferred[IO, ConsensusActor.Handle],
    cardanoLiaison: CardanoLiaison.Handle
) extends Actor[IO, AgentActor.Request]:

    private val jointLedgerRef = Ref.unsafe[IO, Option[JointLedger.Handle]](None)

    private def jointLedger: IO[JointLedger.Handle] = jointLedgerRef.get.map(_.get)

    private val consensusActorRef = Ref.unsafe[IO, Option[ConsensusActor.Handle]](None)

    private val consensusActor: IO[ConsensusActor.Handle] = consensusActorRef.get.map(_.get)

    override def preStart: IO[Unit] = for {
        // Message to itself to get the jointLedger actor
        _ <- context.self ! ()
    } yield ()

    override def receive: Receive[IO, AgentActor.Request] = {
        case _: Unit =>
            for {
                jointLedger <- jointLedgerD.get
                _ <- jointLedgerRef.set(Some(jointLedger))
                consensusActor <- consensusActorD.get
                _ <- consensusActorRef.set(Some(consensusActor))
            } yield ()

        case t: CardanoLiaison.Timeout.type => cardanoLiaison ! t

        // Sync SUT commands
        case req: CompleteBlock.Sync =>
            for {
                _ <- ref.update(_ + (req.request.blockNumber -> req))
                _ <- jointLedger >>= (_ ! req.request.block)
            } yield ()

        // Joint ledger - proxying
        case x: UserRequestWithId => jointLedger >>= (_ ! x)
        case x: StartBlock        => jointLedger >>= (_ ! x)

        // Consensus actor
        // Intercepting unsigned blocks
        case x: Block.Unsigned.Next => proxyBlockUnsigned(x)
        // Direct proxying
        case x: AckBlock => consensusActor >>= (_ ! x)

    }

    private val ref = Ref.unsafe[IO, Map[BlockNumber, CompleteBlock.Sync]](Map.empty)

    def proxyBlockUnsigned(block: Block.Unsigned.Next): IO[Unit] = for {
        _ <- consensusActor >>= (_ ! block)
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

object SutCommands:

    implicit given SutCommand[DelayCommand, Unit, Stage1Sut] with {
        override def run(cmd: DelayCommand, sut: Stage1Sut): IO[Unit] =
            given IOLocal[Tracer] = sut.tracerLocal
            for {
                _ <- Tracer.debug(s">> DelayCommand(delay=${cmd.delaySpec})")
                now <- IO.realTimeInstant
                _ <- Tracer.debug(s"\tCurrent time: ${now.toEpochMilli}")
                _ <- sut.agent ! Timeout
            } yield ()
    }

    implicit given SutCommand[StartBlockCommand, Unit, Stage1Sut] with {
        override def run(cmd: StartBlockCommand, sut: Stage1Sut): IO[Unit] =
            given IOLocal[Tracer] = sut.tracerLocal
            Tracer.debug(s">> StartBlockCommand(blockNumber=${cmd.blockNumber})") >>
                (sut.agent ! StartBlock(
                  blockNum = cmd.blockNumber,
                  blockCreationStartTime = cmd.creationTime
                ))
    }

    implicit given SutCommand[L2TxCommand, Unit, Stage1Sut] with {
        override def run(cmd: L2TxCommand, sut: Stage1Sut): IO[Unit] =
            given IOLocal[Tracer] = sut.tracerLocal
            Tracer.debug(">> LedgerEventCommand") >>
                (sut.agent ! cmd.request)
    }

    implicit given SutCommand[CompleteBlockCommand, BlockBrief, Stage1Sut] with {
        override def run(cmd: CompleteBlockCommand, sut: Stage1Sut): IO[BlockBrief] =
            given IOLocal[Tracer] = sut.tracerLocal
            for {
                _ <- Tracer.debug(
                  s">> CompleteBlockCommand(blockNumber=${cmd.blockNumber}, " +
                      s"blockDuration=${cmd.blockDuration}, " +
                      s"blockCreationEndTime=${cmd.blockCreationEndTime}, " +
                      s"isFinal=${cmd.isFinal})"
                )
                now <- IO.realTimeInstant
                _ <- Tracer.trace(
                  s"SUTCommand[CompleteBlockCommand, (...)]: current time: ${now.toEpochMilli}"
                )
                headUtxos <- sut.cardanoBackend
                    .utxosAt(sut.headAddress)
                    .map(_.fold(err => throw RuntimeException(err.toString), _.keySet))
                block <- IO.pure(
                  if cmd.isFinal
                  then
                      CompleteBlockFinal(
                        referenceBlockBrief = None,
                        blockCreationEndTime = cmd.blockCreationEndTime
                      )
                  else
                      CompleteBlockRegular(
                        referenceBlockBrief = None,
                        pollResults = PollResults(headUtxos),
                        finalizationLocallyTriggered = LocalFinalizationTrigger.NotTriggered,
                        blockCreationEndTime = cmd.blockCreationEndTime
                      )
                )
                // All sync commands should be timed out since the system may terminate
                d <- (sut.agent ?: AgentActor.CompleteBlock(block, cmd.blockNumber))
                    .timeout(10.seconds)
                // Save unsigned block effects
                _ <- sut.effectsAcc.update(_ :+ d.effects.asInstanceOf[BlockEffects.Unsigned])
            } yield d.blockBrief
    }

    given SutCommand[RegisterDepositCommand, Unit, Stage1Sut] with {
        override def run(cmd: RegisterDepositCommand, sut: Stage1Sut): IO[Unit] =
            given IOLocal[Tracer] = sut.tracerLocal
            Tracer.debug(">> RegisterDepositCommand") >>
                (sut.agent ! cmd.request)
    }

    given SutCommand[SubmitDepositsCommand, Unit, Stage1Sut] with {

        // This uses only depositsForSubmission and ignores rejected deposits
        override def run(cmd: SubmitDepositsCommand, sut: Stage1Sut): IO[Unit] =
            given IOLocal[Tracer] = sut.tracerLocal
            for {
                _ <- Tracer.debug(
                  s">> SubmitDepositCommand (${cmd.depositsForSubmission.map(_._1)})"
                )
                ret <- IO.traverse(cmd.depositsForSubmission)(cmd => {
                    val id = cmd.request.requestId
                    val tx = cmd.depositTxBytesSigned

                    sut.cardanoBackend.submitTx(tx) >>= (ret => IO.pure((id, tx) -> ret))
                })

                submissionErrors = ret.filter(_._2.isLeft)
                _ <- IO.whenA(submissionErrors.nonEmpty)(
                  Tracer.error(
                    "Submit deposit errors:" + submissionErrors
                        .map(a =>
                            s"\n\t- ${a._1._1},\n\terror:\n\t${a._2.left}" +
                                "\n\tPretty: ${summon[Pretty[Transaction]].pretty(a._1._2)}" +
                                "\n\tcbor: ${HexUtil.encodeHexString(a._1._2.toCbor)}"
                        )
                        .mkString
                  )
                )
            } yield ()
    }
