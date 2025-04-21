package hydrozoa.demo

import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.demo.PeersNetworkPhase.{Freed, NewlyCreated, RunningHead, Shutdown}
import hydrozoa.infra.{Piper, toEither}
import hydrozoa.l2.ledger.{SimpleOutput, SimpleTransaction, SimpleWithdrawal}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.{Alice, Bob, Carol, account}
import hydrozoa.node.server.{DepositError, DepositRequest, DepositResponse, InitializationError}
import hydrozoa.node.state.HeadPhase
import hydrozoa.node.state.HeadPhase.Open
import org.scalacheck.Gen
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed
import ox.*
import ox.channels.{Actor, ActorRef, Channel}
import ox.flow.Flow
import ox.logback.InheritableMDC
import ox.scheduling.{RepeatConfig, repeat}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*

object Workload extends OxApp:

    private val log = Logger("main")

    val backendService = BFBackendService("http://localhost:8080/api/v1/", "")

    var l2State: ActorRef[mutable.Map[UtxoIdL2, OutputL2]] = _
    var sut: ActorRef[HydrozoaSUT] = _

    override def run(args: Vector[String])(using Ox): ExitCode =
        InheritableMDC.init

        val commands: Channel[WorkloadCommand] = Channel.rendezvous

        l2State = Actor.create(mutable.Map[UtxoIdL2, OutputL2]())

        sut = Actor.create(RealHydrozoaSUT())

        // Command generator
        forkUser {
            var s = genInitialState().runGen().get
            log.info(s"Initial state: $s")
            repeat(RepeatConfig.fixedRateForever(500.millis, Some(1.second))) {
                genCommand(s).runGen() match
                    case Some(cmd) =>
                        log.info(s"Next command: $cmd")
                        s = cmd.runState(s)
                        commands.send(cmd)
                        if cmd.isInstanceOf[DepositCommand] then sleep(3.seconds)
                    case None =>
                        log.warn("Unable to generate next command")
            }
        }

        // Command sender
        forkUser {
            log.info("Started command executor thread...")
            Flow.fromSource(commands).runForeach(cmd => runCommand(cmd))
        }

        // L2 state update loop
        forkUser {
            repeat(RepeatConfig.fixedRateForever(3.seconds, Some(10.seconds))) {
                val stateL2 = sut.ask(_.stateL2()).toMap
                l2State.ask(m =>
                    m.clear()
                    m.addAll(stateL2)
                )
                log.info(s"L2 state was updates, utxo count is: ${stateL2.size}")
            }

        }

        log.info(s"Started Hydrozoa workload with args: ${args.mkString(", ")}")
        ExitCode.Success

    def genInitialState(): Gen[HydrozoaState] =
        Gen.const(HydrozoaState(Set(Alice, Bob, Carol)))

    def genCommand(s: HydrozoaState): Gen[WorkloadCommand] =
        s.peersNetworkPhase match
            case NewlyCreated =>
                Gen.frequency(
                  10 -> genInitializeCommand(s)
                  // 1 -> Gen.const(ShutdownCommand)
                )
            case RunningHead =>
                lazy val l2InputCommandGen = Gen.frequency(
                  20 -> genTransactionL2(s),
                  1 -> genL2Withdrawal(s)
                )

                val l2Size = l2State.ask(_.size)
                log.info(s"L2 state size is: $l2Size")
                if l2Size < 3 then
                    // Gen.oneOf(genDepositCommand(s), genWaitCommand(s))
                    genDepositCommand(s)
                else
                    Gen.frequency(
                      1 -> genDepositCommand(s),
                      25 -> l2InputCommandGen
                    )
            case Freed =>
                Gen.frequency(
                  2 -> genInitializeCommand(s)
                  // 1 -> Gen.const(ShutdownCommand)
                )
            case Shutdown => Gen.fail

    def genInitializeCommand(s: HydrozoaState): Gen[InitializeCommand] =
        for
            initiator <- Gen.oneOf(s.knownPeers)
            account = TestPeer.account(initiator)
            headPeers = s.knownPeers.filterNot(p => p == initiator)
            utxoIds: Set[UtxoIdL1] = utxoIdsAtAddress(AddressBechL1(account.toString))

            seedUtxoId <- Gen.oneOf(utxoIds)
        yield InitializeCommand(initiator, headPeers, seedUtxoId)

    def genDepositCommand(s: HydrozoaState): Gen[DepositCommand] =
        for
            depositor <- Gen.oneOf(s.headPeers + s.initiator.get)
            depositorAccount = TestPeer.account(depositor)
            depositorAddressL1 = AddressBechL1(depositorAccount.toString)
            utxoIds = utxosAtAddress(depositorAddressL1)
            (seedUtxoId, coins) <- Gen.oneOf(utxoIds)

            recipient <- Gen.oneOf(s.knownPeers + s.initiator.get)
            recipientAccount = TestPeer.account(recipient)
            recipientAddressL2 = AddressBechL2(depositorAccount.toString)
            depositAmount <- Gen.choose(
              5_000_000.min(coins.intValue),
              100_000_000.min(coins.intValue)
            )
        yield DepositCommand(
          depositor,
          seedUtxoId,
          depositAmount,
          recipientAddressL2,
          depositorAddressL1
        )

    def genTransactionL2(s: HydrozoaState): Gen[TransactionL2Command] =
        val l2state = l2State.ask(_.toMap)

        for
            numberOfInputs <- Gen.choose(1, 5.min(l2state.size))
            inputs <- Gen.pick(numberOfInputs, l2state.keySet)
            totalCoins = inputs.map(l2state(_).coins).sum.intValue

            outputCoins <- Gen.tailRecM[List[Int], List[Int]](List.empty) { tails =>
                val residual = totalCoins - tails.sum
                if residual < 15_000_000
                then Gen.const(Right(residual :: tails))
                else
                    for next <- Gen.choose(5_000_000, residual)
                    yield Left(next :: tails)
            }

            recipients <- Gen.containerOfN[List, TestPeer](
              outputCoins.length,
              Gen.oneOf(s.headPeers)
            )

            outputs = outputCoins
                .zip(recipients.map(account(_).toString |> AddressBechL2.apply))
                .map((coins, address) => SimpleOutput(address, coins))
        yield TransactionL2Command(SimpleTransaction(inputs.toList, outputs))

    def genL2Withdrawal(s: HydrozoaState): Gen[WithdrawalL2Command] =
        val l2state = l2State.ask(_.toMap)

        for
            numberOfInputs <- Gen.choose(1, 3.min(l2state.size))
            inputs <- Gen.pick(numberOfInputs, l2state.keySet)
        yield WithdrawalL2Command(SimpleWithdrawal(inputs.toList))

    def runCommand(cmd: WorkloadCommand): Unit =
        log.info(s"Running command: $cmd")
        cmd.runSut(sut)

    // L1 Utils
    def utxoIdsAtAddress(headAddress: AddressBechL1): Set[UtxoIdL1] =
        // NB: can't be more than 100
        backendService.getUtxoService.getUtxos(headAddress.bech32, 100, 1).toEither match
            case Left(err) =>
                throw RuntimeException(err)
            case Right(utxos) =>
                utxos.asScala
                    .map(u => UtxoIdL1(TxId(u.getTxHash), TxIx(u.getOutputIndex)))
                    .toSet

    def utxosAtAddress(headAddress: AddressBechL1): Map[UtxoIdL1, Long] =
        // NB: can't be more than 100
        backendService.getUtxoService.getUtxos(headAddress.bech32, 100, 1).toEither match
            case Left(err) =>
                throw RuntimeException(err)
            case Right(utxos) =>
                utxos.asScala
                    .map(u =>
                        (
                          UtxoIdL1(TxId(u.getTxHash), TxIx(u.getOutputIndex)),
                          u.getAmount.asScala
                              .find(a => a.getUnit.equals("lovelace"))
                              .get
                              .getQuantity
                              .longValue()
                        )
                    )
                    .toMap

    // Run Gen with random seed
    extension [T](g: Gen[T])
        def runGen(): Option[T] =
            g.apply(
              Parameters.default,
              Seed.random()
            )

sealed trait WorkloadCommand:
    type Result

    def preCondition(state: HydrozoaState): Boolean
    def runState(state: HydrozoaState): (HydrozoaState)
    def runSut(sut: ActorRef[HydrozoaSUT]): Result

class InitializeCommand(
    initiator: TestPeer,
    otherHeadPeers: Set[TestPeer],
    seedUtxo: UtxoIdL1
) extends WorkloadCommand:

    private val log = Logger(getClass)

    override type Result = Either[InitializationError, TxId]

    override def toString: String =
        s"Initialize command {initiator=$initiator, other peers = $otherHeadPeers, seed utxo = $seedUtxo}"

    override def preCondition(state: HydrozoaState): Boolean =
        state.peersNetworkPhase match
            case NewlyCreated => true
            case Freed        => true
            case _            => false

    override def runState(state: HydrozoaState): HydrozoaState =
        if otherHeadPeers.isEmpty then throw RuntimeException("Solo mode is not supported yet")

        state.copy(
          peersNetworkPhase = RunningHead,
          headPhase = Some(Open),
          initiator = Some(initiator),
          headPeers = otherHeadPeers
        )

    override def runSut(sut: ActorRef[HydrozoaSUT]): Result =
        sut.ask(
          _.initializeHead(
            initiator,
            1000,
            seedUtxo.txId,
            seedUtxo.outputIx
          )
        )

class DepositCommand(
    depositor: TestPeer,
    fundUtxo: UtxoIdL1,
    depositAmount: Int,
    address: AddressBechL2,
    refundAddress: AddressBechL1
) extends WorkloadCommand:

    private val log = Logger(getClass)

    override type Result = Either[DepositError, DepositResponse]

    override def toString: String =
        s"Deposit command { depositor = $depositor, amount = $depositAmount, fund utxo = $fundUtxo, L2 address = $address, refund address = $refundAddress}"

    override def preCondition(state: HydrozoaState): Boolean =
        state.peersNetworkPhase == RunningHead
            && state.headPhase.contains(Open)

    override def runState(state: HydrozoaState): HydrozoaState = state

    override def runSut(sut: ActorRef[HydrozoaSUT]): Result =
        val request = DepositRequest(
          fundUtxo.txId,
          fundUtxo.outputIx,
          depositAmount,
          None, // FIXME
          address,
          None, // FIXME
          refundAddress,
          None
        )
        val ret = sut.ask(_.deposit(depositor, request))
        // FIXME: might help temporarily prevent "not known deposits" validation error
        // sleep(3.seconds)
        ret

class TransactionL2Command(simpleTransaction: SimpleTransaction) extends WorkloadCommand:

    private val log = Logger(getClass)

    override type Result = Unit

    override def toString: String = s"Transaction L2 command { $simpleTransaction }"

    override def preCondition(state: HydrozoaState): Boolean =
        state.peersNetworkPhase == RunningHead
            && state.headPhase.contains(Open)

    override def runState(state: HydrozoaState): HydrozoaState = state

    override def runSut(sut: ActorRef[HydrozoaSUT]): Result =
        sut.ask(_.submitL2(simpleTransaction))

class WithdrawalL2Command(simpleWithdrawal: SimpleWithdrawal) extends WorkloadCommand:

    private val log = Logger(getClass)

    override type Result = Unit

    override def toString: String = s"Withdrawal L2 command { $simpleWithdrawal }"

    override def preCondition(state: HydrozoaState): Boolean =
        log.info(".preCondition")
        state.peersNetworkPhase == RunningHead
        && state.headPhase.contains(Open)

    override def runState(state: HydrozoaState): HydrozoaState = state

    override def runSut(sut: ActorRef[HydrozoaSUT]): Unit =
        sut.ask(_.submitL2(simpleWithdrawal))

enum PeersNetworkPhase derives CanEqual:
    case NewlyCreated
    case RunningHead
    case Freed
    case Shutdown

case class HydrozoaState(
    peersNetworkPhase: PeersNetworkPhase,
    knownPeers: Set[TestPeer],

    // Head
    headPhase: Option[HeadPhase] = None,
    initiator: Option[TestPeer] = None,
    headPeers: Set[TestPeer] = Set.empty
):
    override def toString: String =
        "Hydrozoa state:" +
            s"\tNetwork phase: ${peersNetworkPhase.toString}\n" +
            s"\tKnown peers: ${knownPeers.toString()}\n" +
            s"\tHead phase: ${headPhase.toString}\n" +
            s"\tInitiator: ${initiator.toString}\n" +
            s"\tHead peers: ${headPeers.toString()}\n"

object HydrozoaState:
    def apply(
        knownPeers: Set[TestPeer]
    ): HydrozoaState =
        new HydrozoaState(
          peersNetworkPhase = NewlyCreated,
          knownPeers = knownPeers
        )
