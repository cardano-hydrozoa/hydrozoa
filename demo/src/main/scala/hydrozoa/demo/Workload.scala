package hydrozoa.demo

import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.demo.PeersNetworkPhase.{Freed, NewlyCreated, RunningHead, Shutdown}
import hydrozoa.l1.CardanoL1YaciDevKit
import hydrozoa.l2.ledger.{L2EventTransaction, L2EventWithdrawal}
import hydrozoa.node.TestPeer.{Alice, Bob, Carol}
import hydrozoa.node.server.{DepositError, DepositRequest, DepositResponse, InitializationError}
import hydrozoa.node.state.HeadPhase
import hydrozoa.node.state.HeadPhase.Open
import hydrozoa.node.{TestPeer, signTx}
import hydrozoa.sut.{HydrozoaFacade, RealFacade}
import org.scalacheck.Gen
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed
import ox.*
import ox.channels.{Actor, ActorRef, Channel}
import ox.flow.Flow
import ox.logback.InheritableMDC
import ox.scheduling.{RepeatConfig, repeat}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import sttp.client4.UriContext

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

val demoPeers = Map.from(
  List(
    Alice -> uri"http://localhost:8093",
    Bob -> uri"http://localhost:8094",
    Carol -> uri"http://localhost:8095"
  )
)

object Workload extends OxApp:

    val backendService = BFBackendService("http://localhost:8080/api/v1/", "")
    val cardanoL1YaciDevKit = CardanoL1YaciDevKit(backendService)
    private val log = Logger("main")
    var l2State: ActorRef[mutable.Map[UtxoIdL2, OutputL2]] = _
    var sut: ActorRef[HydrozoaFacade] = _

    override def run(args: Vector[String])(using Ox): ExitCode =
        InheritableMDC.init

        val commands: Channel[WorkloadCommand] = Channel.rendezvous

        l2State = Actor.create(mutable.Map[UtxoIdL2, OutputL2]())

        sut = Actor.create(RealFacade.apply(demoPeers))

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
            repeat(RepeatConfig.fixedRateForever(1.seconds, Some(10.seconds))) {
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
                if l2Size < 10 then
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
            headPeers = s.knownPeers.filterNot(p => p == initiator)
            utxoIds: Set[UtxoIdL1] = cardanoL1YaciDevKit
                .utxoIdsAdaAtAddress(Address[L1](TestPeer.address(initiator)))
                .keySet

            seedUtxoId <- Gen.oneOf(utxoIds)
        yield InitializeCommand(initiator, headPeers, seedUtxoId)

    def genDepositCommand(s: HydrozoaState): Gen[DepositCommand] =
        for
            depositor <- Gen.oneOf(s.headPeers + s.initiator.get)
            depositorAddressL1 = Address[L1](TestPeer.address(depositor))
            utxos = cardanoL1YaciDevKit.utxoIdsAdaAtAddress(depositorAddressL1)
            (seedUtxoId, coins) <- Gen.oneOf(utxos)

            // more addresses the better
            recipient <- Gen.oneOf(ArraySeq.unsafeWrapArray(TestPeer.values))
            recipientAccount = TestPeer.account(recipient)
            recipientAddressL2 = Address[L2](TestPeer.address(depositor))
            depositAmount <- Gen.choose(
              (5_000_000L).min(coins.value),
              (100_000_000L).min(coins.value)
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
            totalCoins = Coin(inputs.map(l2state(_).value.coin.value).sum)

            outputCoins <- Gen.tailRecM[List[Coin], List[Coin]](List.empty) { tails =>
                val residual = totalCoins - Coin(tails.map(_.value).sum)
                if residual < Coin(5_000_000L)
                then Gen.const(Right(residual :: tails))
                else
                    for next <- Gen.choose(5_000_000L, residual.value)
                    yield Left(Coin(next) :: tails)
            }

            recipients <- Gen.containerOfN[List, TestPeer](
              outputCoins.length,
              Gen.oneOf(ArraySeq.unsafeWrapArray(TestPeer.values))
            )
            recipients <- Gen.containerOfN[List, TestPeer](
              outputCoins.length,
              Gen.oneOf(s.headPeers)
            )

            outputs: IndexedSeq[Sized[TransactionOutput]] =
                outputCoins
                    .zip(recipients.map(TestPeer.address))
                    .map((coins, addr) =>
                        Babbage(
                          address = addr,
                          value = Value(coins),
                          datumOption = None,
                          scriptRef = None
                        )
                    )
                    .toIndexedSeq
                    .map(Sized(_))

            txBody: TransactionBody = TransactionBody(
              inputs = inputs.toSet.map(_.untagged),
              outputs = outputs,
              fee = Coin(0L)
            )

            neededSigners: Set[TestPeer] = {
                // Generate a lookup table mapping addresses to peers.
                // (We can probably move this outside of the generator for a speedup)
                val addrMap: Map[AddressL2, TestPeer] = {
                    s.knownPeers.foldLeft(Map.empty)((m, peer) =>
                        m.updated(Address[L2](TestPeer.address(peer)), peer)
                    )
                }

                // Note: `Set` isn't a functor, so if multiple inputs resolve to the same address, we'll only keep
                // a single element for the required signer. This is the desired behavior
                inputs
                    .map(ti =>
                        addrMap(Address[L2](l2state(ti).address.asInstanceOf[ShelleyAddress]))
                    )
                    .toSet
            }

            txUnsigned = Tx[L2](
              Transaction(
                body = KeepRaw(txBody),
                witnessSet = TransactionWitnessSet.empty,
                isValid = true,
                auxiliaryData = None
              )
            )

            tx = neededSigners.foldLeft(txUnsigned)((tx, peer) => signTx(peer, tx))
        yield TransactionL2Command(L2EventTransaction(tx))

    def genL2Withdrawal(s: HydrozoaState): Gen[WithdrawalL2Command] =
        val l2state = l2State.ask(_.toMap)

        for
            numberOfInputs <- Gen.choose(1, 3.min(l2state.size))
            inputs <- Gen.pick(numberOfInputs, l2state.keySet)
            txBody: TransactionBody = TransactionBody(
              inputs = inputs.toSet.map(UtxoId[L2](_)),
              outputs = IndexedSeq.empty,
              fee = Coin(0L)
            )

            neededSigners: Set[TestPeer] = {
                // Generate a lookup table mapping addresses to peers.
                // (We can probably move this outside of the generator for a speedup)
                val addrMap: Map[AddressL2, TestPeer] = {
                    s.knownPeers.foldLeft(Map.empty)((m, peer) =>
                        m.updated(Address[L2](TestPeer.address(peer)), peer)
                    )
                }

                // Note: `Set` isn't a functor, so if multiple inputs resolve to the same address, we'll only keep
                // a single element for the required signer. This is the desired behavior
                inputs
                    .map(ti =>
                        addrMap(Address[L2](l2state(ti).address.asInstanceOf[ShelleyAddress]))
                    )
                    .toSet
            }

            txUnsigned: Tx[L2] = Tx[L2](
              Transaction(
                body = KeepRaw(txBody),
                witnessSet = TransactionWitnessSet.empty,
                isValid = true,
                auxiliaryData = None
              )
            )

            tx = neededSigners.foldLeft(txUnsigned)((tx, peer) => signTx(peer, tx))
        yield WithdrawalL2Command(L2EventWithdrawal(tx))

    def runCommand(cmd: WorkloadCommand): Unit =
        log.info(s"Running command: $cmd")
        cmd.runSut(sut) : Unit

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
    def runSut(sut: ActorRef[HydrozoaFacade]): Result

class InitializeCommand(
    initiator: TestPeer,
    otherHeadPeers: Set[TestPeer],
    seedUtxo: UtxoIdL1
) extends WorkloadCommand:

    override type Result = Either[InitializationError, TransactionHash]
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

    override def runSut(sut: ActorRef[HydrozoaFacade]): Result =
        sut.ask(
          _.initializeHead(
            initiator,
            otherHeadPeers.map(TestPeer.mkWalletId),
            1000,
            seedUtxo.transactionId,
            TxIx(seedUtxo.index)
          )
        )

class DepositCommand(
    depositor: TestPeer,
    fundUtxo: UtxoIdL1,
    depositAmount: BigInt,
    address: AddressL2,
    refundAddress: AddressL1
) extends WorkloadCommand:

    override type Result = Either[DepositError, DepositResponse]
    @annotation.unused
    private val log = Logger(getClass)

    @annotation.unused
    override def toString: String =
        s"Deposit command { depositor = $depositor, amount = $depositAmount, fund utxo = $fundUtxo, L2 address = $address, refund address = $refundAddress}"

    override def preCondition(state: HydrozoaState): Boolean =
        state.peersNetworkPhase == RunningHead
            && state.headPhase.contains(Open)

    override def runState(state: HydrozoaState): HydrozoaState = state

    override def runSut(sut: ActorRef[HydrozoaFacade]): Result =
        val request = DepositRequest(
          fundUtxo.transactionId,
          TxIx(fundUtxo.index),
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

class TransactionL2Command(simpleTransaction: L2EventTransaction) extends WorkloadCommand:

    override type Result = Unit
    @annotation.unused
    private val log = Logger(getClass)

    override def toString: String = s"Transaction L2 command { $simpleTransaction }"

    override def preCondition(state: HydrozoaState): Boolean =
        state.peersNetworkPhase == RunningHead
            && state.headPhase.contains(Open)

    override def runState(state: HydrozoaState): HydrozoaState = state

    override def runSut(sut: ActorRef[HydrozoaFacade]): Result =
        sut.ask(_.submitL2(simpleTransaction)) : Unit

class WithdrawalL2Command(simpleWithdrawal: L2EventWithdrawal) extends WorkloadCommand:

    override type Result = Unit
    private val log = Logger(getClass)

    override def toString: String = s"Withdrawal L2 command { $simpleWithdrawal }"

    override def preCondition(state: HydrozoaState): Boolean =
        log.info(".preCondition")
        state.peersNetworkPhase == RunningHead
        && state.headPhase.contains(Open)

    override def runState(state: HydrozoaState): HydrozoaState = state

    override def runSut(sut: ActorRef[HydrozoaFacade]): Unit =
        sut.ask(_.submitL2(simpleWithdrawal)) : Unit

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
