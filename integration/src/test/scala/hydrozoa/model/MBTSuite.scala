package hydrozoa.model

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.l1.CardanoL1Mock
import hydrozoa.model.PeersNetworkPhase.{Freed, NewlyCreated, RunningHead, Shutdown}
import hydrozoa.node.TestPeer
import hydrozoa.node.server.InitializeError
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}
import sttp.client4.Response
import sttp.client4.quick.*

import scala.jdk.CollectionConverters.*

object MBTSuite extends Commands:
    private val log = Logger(getClass)
    override type State = HydrozoaState // Wrapper around a simplified head
    override type Sut = HydrozoaSUT // Facade to a network of Hydrozoa's peeers

    override def canCreateNewSut(
        newState: State,
        initSuts: Traversable[State],
        runningSuts: Traversable[Sut]
    ): Boolean = initSuts.isEmpty && runningSuts.isEmpty

    override def newSut(state: State): Sut =
        // Reset Yaci DevKit
        val response: Response[String] = quickRequest
            .post(uri"http://localhost:10000/local-cluster/api/admin/devnet/reset")
            .send()
        // Create a new SUT
        OneNodeHydrozoaSUT(
          state.networkPeers.head,
          state.networkPeers.tail,
          Utils.protocolParams
        )

    override def destroySut(_sut: Sut): Unit = () // TODO: shall we do something here?

    override def initialPreCondition(state: State): Boolean =
        state.peersNetworkPhase == NewlyCreated

    override def genInitialState: Gen[State] = for
        numberOfNetworkPeers <- Gen.chooseNum(3, 8)
        knownPeers <- Gen.pick(numberOfNetworkPeers, TestPeer.values)
    yield HydrozoaState(Utils.protocolParams, knownPeers.toSeq)

    override def genCommand(state: State): Gen[Command] =
        state.peersNetworkPhase match
            case NewlyCreated => genInitializeCommand(state)
            case RunningHead  => NoOp // genHeadCommand
            case Freed        => ??? // genInitializeOrShutdownCommand
            case Shutdown     => ???

    def genInitializeCommand(state: State): Gen[Command] = for
        numberOfHeadPeers <- Gen.chooseNum(1, state.networkPeers.tail.length)
        headPeers <- Gen.pick(numberOfHeadPeers, state.networkPeers.tail)
        // initializer <- Gen.oneOf(state.networkPeers)
        initializer = state.networkPeers.head // Already randomly chosen
        account = TestPeer.account(initializer)
        l1 = CardanoL1Mock(state.knownTxs, state.utxosActive)
        utxoIds = l1.utxoIdsByAddress(AddressBechL1(account.toString))
        _ = log.info(s"Initializer: $initializer, account: $account, utxos: $utxoIds")
        seedUtxoId <- Gen.oneOf(utxoIds)
    yield InitializeCommand(headPeers.toList, initializer, seedUtxoId)

//        case Initializing => ???
//        case Open         => ???
//        case Finalizing   => ???
//        case Finalized    => ???

    case class InitializeCommand(
        headPeers: List[TestPeer],
        initializer: TestPeer,
        seedUtxo: UtxoIdL1
    ) extends SuccessCommand:
        override type Result = Either[InitializeError, TxId]
        override def run(sut: HydrozoaSUT): Result =
            sut.initializeHead(initializer, 100, seedUtxo.txId, seedUtxo.outputIx)
        override def nextState(state: State): State =
            // state.getNode.initializeHead(100, seedUtxo.txId, seedUtxo.outputIx)
            state.copy(peersNetworkPhase = RunningHead)

        override def preCondition(state: State): Boolean =
            state.peersNetworkPhase match
                case NewlyCreated => true
                case Freed        => true
                case _            => false
        override def postCondition(stateBefore: State, result: Result): Prop =
            result.isRight &&
                // check other conditions
                true

object SomeProperty extends Properties("Hydrozoa properties") {
    property("Some property") = MBTSuite.property()
}
