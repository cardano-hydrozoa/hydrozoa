package hydrozoa.model

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.txHash
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.tx.initialization.{
    BloxBeanInitializationTxBuilder,
    InitTxBuilder,
    InitTxRecipe
}
import hydrozoa.l1.multisig.tx.toL1Tx
import hydrozoa.l1.{BackendServiceMock, CardanoL1Mock}
import hydrozoa.model.PeersNetworkPhase.{Freed, NewlyCreated, RunningHead, Shutdown}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.{mkPeer, mkPeerInfo}
import hydrozoa.node.server.InitializeError
import hydrozoa.node.state.HeadPhase.Initializing
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}

import scala.jdk.CollectionConverters.*
import scala.util.Try

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
//        val response: Response[String] = quickRequest
//            .post(uri"http://localhost:10000/local-cluster/api/admin/devnet/reset")
//            .send()
        // Create a new SUT
        println("--------------------> new SUT")
        OneNodeHydrozoaSUT(
          state.networkPeers.head,
          state.networkPeers.tail,
          Utils.protocolParams
        )

    // TODO: shall we do something here?
    override def destroySut(_sut: Sut): Unit =
        println("<-------------------- destroy SUT")

    override def initialPreCondition(state: State): Boolean =
        state.peersNetworkPhase == NewlyCreated

    override def genInitialState: Gen[State] = for
        numberOfNetworkPeers <- Gen.chooseNum(3, 8)
        networkPeers <- Gen.pick(numberOfNetworkPeers, TestPeer.values)
    yield HydrozoaState(Utils.protocolParams, networkPeers.toSeq)

    override def genCommand(state: State): Gen[Command] =
        state.peersNetworkPhase match
            case NewlyCreated => genInitializeCommand(state)
            case RunningHead  => NoOp // genHeadCommand
            case Freed        => ??? // genInitializeOrShutdownCommand
            case Shutdown     => ???

    def genInitializeCommand(state: State): Gen[Command] = for
        numberOfHeadPeers <- Gen.chooseNum(0, state.networkPeers.tail.length)
        headPeers <- Gen.pick(numberOfHeadPeers, state.networkPeers.tail)
        initiator = state.networkPeers.head // FIXME: should be any peer
        account = TestPeer.account(initiator)
        l1 = CardanoL1Mock(state.knownTxs, state.utxosActive)
        utxoIds = l1.utxoIdsByAddress(AddressBechL1(account.toString))
        seedUtxoId <- Gen.oneOf(utxoIds)
        _ = log.info(s"Initiator: $initiator, account: $account, headPeers: $headPeers, seedUtxo: $seedUtxoId")
    yield InitializeCommand(initiator, headPeers.toList, seedUtxoId)

//        case Initializing => ???
//        case Open         => ???
//        case Finalizing   => ???
//        case Finalized    => ???

    case class InitializeCommand(
        initiator: TestPeer,
        otherHeadPeers: List[TestPeer],
        seedUtxo: UtxoIdL1
    ) extends Command:
        override type Result = (Either[InitializeError, TxId], NodeStateInspector)

        override def run(sut: HydrozoaSUT): Result =
            sut.initializeHead(
              otherHeadPeers.map(mkPeerInfo).toSet,
              100,
              seedUtxo.txId,
              seedUtxo.outputIx
            )

        override def nextState(state: State): State =

            // Native script, head address, and token
            val pubKeys = (otherHeadPeers.toSet + initiator).map(tp => mkPeer(tp).getPublicKey)
            val (headNativeScript, headAddress) =
                mkHeadNativeScriptAndAddress(pubKeys, networkL1static)
            val beaconTokenName = mkBeaconTokenName(seedUtxo)

            // Recipe to build init tx
            val initTxRecipe = InitTxRecipe(
              headAddress,
              seedUtxo,
              100_000_000,
              headNativeScript,
              beaconTokenName
            )

            val l1Mock = CardanoL1Mock(state.knownTxs, state.utxosActive)
            val backendService = BackendServiceMock(l1Mock, state.pp)
            val initTxBuilder: InitTxBuilder = BloxBeanInitializationTxBuilder(backendService)
            val Right(tx, _) = initTxBuilder.mkInitializationTxDraft(initTxRecipe)
            log.info(s"Initialization tx hash: ${txHash(tx)}")

            l1Mock.submit(tx.toL1Tx)

            state.copy(
              peersNetworkPhase = RunningHead,
              initiator = Some(initiator),
              headPeers = otherHeadPeers,
              knownTxs = l1Mock.getKnownTxs,
              utxosActive = l1Mock.getUtxosActive
            )

        override def preCondition(state: State): Boolean =
            state.peersNetworkPhase match
                case NewlyCreated => true
                case Freed        => true
                case _            => false

        override def postCondition(stateBefore: State, result: Try[Result]): Prop =
            otherHeadPeers match
                case Nil => result.isFailure
                case _ =>
                    val (ret, inspector) = result.get
                    val reader = inspector.reader
                    val headPhase = reader.currentPhase

                    val sutHeadPeers = reader.openPhaseReader(_.headPeers)
                    val headPeers = (otherHeadPeers.toSet + initiator).map(mkPeerInfo)

                    ("headPeers should match command in Initializing phase"
                        |: headPhase == Initializing ==> (reader.initializingPhaseReader(
                          _.headPeers
                        ) == headPeers))
                    && ("headPeers should match command in Open phase"
                        |: headPeers == sutHeadPeers)
                    && ("initialize response is Right" |: ret.isRight)

object SomeProperty extends Properties("Hydrozoa_one_node ") {
    property("Just_works") = MBTSuite.property()
}
