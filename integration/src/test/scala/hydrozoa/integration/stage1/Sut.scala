package hydrozoa.integration.stage1

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.bloxbean.cardano.client.util.HexUtil
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.ActorSystem
import hydrozoa.integration.stage1
import hydrozoa.integration.stage1.AgentActor.CompleteBlock
import hydrozoa.integration.stage1.Commands.*
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, debug, error, trace}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.BlockWeaver.LocalFinalizationTrigger
import hydrozoa.multisig.consensus.CardanoLiaison.Timeout
import hydrozoa.multisig.consensus.ack.SoftAck
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.consensus.{CardanoLiaison, FastConsensusActor, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.l1.tx.RawTx
import hydrozoa.multisig.persistence.{Persistence, StoreKey}
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
    // The shared L1 backend handle. `CompleteBlockCommand` reads the head's UTxOs through it as
    // the block's poll results, which is how the JointLedger sees deposits on L1, and
    // `SubmitDepositsCommand` submits signed deposit txs through it. The real `CardanoLiaison`
    // polls it too, but its `PollResults` go to `BlockWeaverMock`, which drops them.
    system: ActorSystem[IO],
    cardanoBackend: CardanoBackend[IO],
    agent: AgentActor.Handle,
    // The JointLedger's store, read after each block for the deposit rows it persisted.
    persistence: Persistence[IO],
    // Every deposit request sent to the JointLedger, whose decision rows are read after each block.
    depositRequestIds: Ref[IO, List[RequestId]],
    log: ContraTracer[IO, Slf4jMsg],
    runId: String = "",
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
    ) extends SyncRequest[IO, CompleteBlock, BlockBrief.Next] {
        export CompleteBlock.Sync
        def ?: : this.Send = SyncRequest.send(_, this)
    }

    object CompleteBlock:
        type Sync = SyncRequest.Envelope[IO, CompleteBlock, BlockBrief.Next]

    type Request =
        UserRequestWithId | StartBlock | CompleteBlock.Sync | FastConsensusActor.Request |
            CardanoLiaison.Timeout.type | Unit | joint.JointLedger.Requests.GetState.Sync

    type Handle = ActorRef[IO, Request]

case class AgentActor(
    jointLedgerD: Deferred[IO, JointLedger.Handle],
    consensusActorD: Deferred[IO, FastConsensusActor.Handle],
    cardanoLiaison: CardanoLiaison.Handle
) extends Actor[IO, AgentActor.Request]:

    private val jointLedgerRef = Ref.unsafe[IO, Option[JointLedger.Handle]](None)

    private def jointLedger: IO[JointLedger.Handle] = jointLedgerRef.get.map(_.get)

    private val consensusActorRef = Ref.unsafe[IO, Option[FastConsensusActor.Handle]](None)

    private val consensusActor: IO[FastConsensusActor.Handle] = consensusActorRef.get.map(_.get)

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
        // Intercepting briefs (used to be `Block.Unsigned.Next` before the fast/slow split)
        case x: BlockBrief.Next => proxyBlockBrief(x)
        // Direct proxying
        case x: SoftAck => consensusActor >>= (_ ! x)

    }

    private val ref = Ref.unsafe[IO, Map[BlockNumber, CompleteBlock.Sync]](Map.empty)

    def proxyBlockBrief(brief: BlockBrief.Next): IO[Unit] = for {
        _ <- consensusActor >>= (_ ! brief)
        envelope <- ref.modify { map =>
            val blockNum = brief.blockNum
            val v = map(blockNum)
            val newMap = map - blockNum
            (newMap, v)
        }
        _ <- envelope.dResponse.complete(brief)
    } yield ()

end AgentActor

// ===================================
// SutCommand instances
// ===================================

object SutCommands:

    implicit given SutCommand[DelayCommand, Unit, Stage1Sut] with {
        override def run(cmd: DelayCommand, sut: Stage1Sut): IO[Unit] =
            for {
                _ <- sut.log.debug(s">> DelayCommand(delay=${cmd.delaySpec})")
                now <- IO.realTimeInstant
                _ <- sut.log.debug(s"\tCurrent time: ${now.toEpochMilli}")
                _ <- sut.agent ! Timeout
            } yield ()
    }

    implicit given SutCommand[StartBlockCommand, Unit, Stage1Sut] with {
        override def run(cmd: StartBlockCommand, sut: Stage1Sut): IO[Unit] =
            sut.log.debug(s">> StartBlockCommand(blockNumber=${cmd.blockNumber})") >>
                (sut.agent ! StartBlock(
                  blockNum = cmd.blockNumber,
                  blockCreationStartTime = cmd.creationTime
                ))
    }

    implicit given SutCommand[L2TxCommand, Unit, Stage1Sut] with {
        override def run(cmd: L2TxCommand, sut: Stage1Sut): IO[Unit] =
            sut.log.debug(">> LedgerEventCommand") >>
                (sut.agent ! cmd.request)
    }

    implicit given SutCommand[CompleteBlockCommand, CompletedBlock, Stage1Sut] with {
        override def run(cmd: CompleteBlockCommand, sut: Stage1Sut): IO[CompletedBlock] =
            for {
                _ <- sut.log.debug(
                  s">> CompleteBlockCommand(blockNumber=${cmd.blockNumber}, " +
                      s"blockDuration=${cmd.blockDuration}, " +
                      s"blockCreationEndTime=${cmd.blockCreationEndTime}, " +
                      s"isFinal=${cmd.isFinal})"
                )
                now <- IO.realTimeInstant
                _ <- sut.log.trace(
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
                // The JointLedger persists a block's rows before it hands the brief on, so they are
                // in the store by the time the brief arrives here.
                depositMap <- sut.persistence.getOrFail(StoreKey.DepositMap(cmd.blockNumber))
                depositRequestIds <- sut.depositRequestIds.get
                depositDecisions <- depositRequestIds.traverseFilter(id =>
                    sut.persistence
                        .get(StoreKey.DepositDecisionIndex(id))
                        .map(_.map(id -> _))
                )
            } yield CompletedBlock(d.blockBrief, depositMap, depositDecisions.toMap)
    }

    given SutCommand[RegisterDepositCommand, Unit, Stage1Sut] with {
        override def run(cmd: RegisterDepositCommand, sut: Stage1Sut): IO[Unit] =
            sut.log.debug(">> RegisterDepositCommand") >>
                sut.depositRequestIds.update(_ :+ cmd.request.requestId) >>
                (sut.agent ! cmd.request)
    }

    given SutCommand[SubmitDepositsCommand, Unit, Stage1Sut] with {

        // This uses only depositsForSubmission and ignores rejected deposits
        override def run(cmd: SubmitDepositsCommand, sut: Stage1Sut): IO[Unit] =
            for {
                _ <- sut.log.debug(
                  s">> SubmitDepositCommand (${cmd.depositsForSubmission.map(_._1)})"
                )
                ret <- IO.traverse(cmd.depositsForSubmission)(cmd => {
                    val id = cmd.request.requestId
                    val tx = cmd.depositTxBytesSigned

                    sut.cardanoBackend.submitTx(RawTx(tx)) >>= (ret => IO.pure((id, tx) -> ret))
                })

                submissionErrors = ret.filter(_._2.isLeft)
                // The model counts every submitted deposit as on L1 and predicts its absorption,
                // so a rejected submission fails the command rather than only being logged.
                _ <- IO.whenA(submissionErrors.nonEmpty)(
                  sut.log.error(
                    "Submit deposit errors:" + submissionErrors
                        .map(a =>
                            s"\n\t- ${a._1._1},\n\terror:\n\t${a._2.fold(_.toString, _ => "")}" +
                                s"\n\tPretty: ${summon[Pretty[Transaction]].pretty(a._1._2).render(100)}" +
                                s"\n\tcbor: ${HexUtil.encodeHexString(a._1._2.toCbor)}"
                        )
                        .mkString
                  ) >> IO.raiseError(
                    RuntimeException(
                      "L1 rejected the deposit txs of requests " +
                          submissionErrors.map(_._1._1).mkString(", ")
                    )
                  )
                )
            } yield ()
    }
