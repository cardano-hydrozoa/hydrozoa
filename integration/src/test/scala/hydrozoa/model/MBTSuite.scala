package hydrozoa.model

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{serializeTxHex, txHash}
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
import hydrozoa.node.TestPeer.{mkPeerInfo, mkWallet}
import hydrozoa.node.server.InitializeError
import hydrozoa.node.state.HeadPhase.{Initializing, Open}
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}
import sttp.client4.Response
import sttp.client4.quick.*

import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

object MBTSuite extends Commands:

    var useYaci = false
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
        if useYaci then
            val response: Response[String] = quickRequest
                .post(uri"http://localhost:10000/local-cluster/api/admin/devnet/reset")
                .send()
        // Create a new SUT
        println("--------------------> new SUT")
        OneNodeHydrozoaSUT(
          state.knownPeers.head,
          state.knownPeers.tail,
          Utils.protocolParams,
          useYaci
        )

    // TODO: shall we do something here?
    override def destroySut(_sut: Sut): Unit =
        println("<-------------------- destroy SUT")

    override def initialPreCondition(state: State): Boolean =
        state.peersNetworkPhase == NewlyCreated

    override def genInitialState: Gen[State] = for
        numberOfNetworkPeers <- Gen.chooseNum(3, 8)
        networkPeers <- Gen.pick(numberOfNetworkPeers, TestPeer.values)
    yield HydrozoaState(Utils.protocolParams, networkPeers.toSet)

    override def genCommand(state: State): Gen[Command] =
        state.peersNetworkPhase match
            case NewlyCreated => genInitializeCommand(state)
            case RunningHead  => NoOp // genHeadCommand
            case Freed        => ??? // genInitializeOrShutdownCommand
            case Shutdown     => ???

    def genInitializeCommand(state: State): Gen[InitializeCommand] = for
        numberOfHeadPeers <- Gen.chooseNum(0, state.knownPeers.tail.size)
        headPeers <- Gen.pick(numberOfHeadPeers, state.knownPeers.tail)
        initiator = state.knownPeers.head // FIXME: should be any peer
        account = TestPeer.account(initiator)
        l1 = CardanoL1Mock(state.knownTxs, state.utxosActive)
        utxoIds = l1.utxoIdsByAddress(AddressBechL1(account.toString))
        seedUtxoId <- Gen.oneOf(utxoIds)
        _ = log.info(
          s"Initiator: $initiator, account: $account, headPeers: $headPeers, seedUtxo: $seedUtxoId"
        )
    yield InitializeCommand(initiator, headPeers.toSet, seedUtxoId)

//        case Initializing => ???
//        case Open         => ???
//        case Finalizing   => ???
//        case Finalized    => ???

    /** State-like Command that uses `runState` instead of `nextState`. Additionally branches on
      * `result`, providing `SutInspector` for successful path.
      */
    trait StateLikeInspectabeCommand extends Command:

        type SutInspector

        type RealResult

        type Result = (RealResult, SutInspector)

        final override def nextState(state: State): State = runState(state)._2

        def runState(state: State): (RealResult, State)

        final override def postCondition(stateBefore: State, result: Try[Result]): Prop =
            val (expectedResult, stateAfter) = runState(stateBefore)
            result match
                case Success(realResult: RealResult, sutInspector: SutInspector) =>
                    postConditionSuccess(
                      expectedResult,
                      stateBefore,
                      stateAfter,
                      realResult,
                      sutInspector
                    )
                case Failure(e) =>
                    postConditionFailure(expectedResult, stateBefore, stateAfter, e)

        def postConditionSuccess(
            expectedResult: RealResult,
            stateBefore: State,
            stateAfter: State,
            result: RealResult,
            sutInspector: SutInspector
        ): Prop

        def postConditionFailure(
            expectedResult: RealResult,
            stateBefore: State,
            stateAfter: State,
            err: Throwable
        ): Prop

    case class InitializeCommand(
        initiator: TestPeer,
        otherHeadPeers: Set[TestPeer],
        seedUtxo: UtxoIdL1
    ) extends StateLikeInspectabeCommand:

        override type SutInspector = NodeStateInspector

        override type RealResult = Either[InitializeError, TxId]

        override def run(sut: HydrozoaSUT): Result =
            sut.initializeHead(
              otherHeadPeers.map(mkPeerInfo).toSet,
              100,
              seedUtxo.txId,
              seedUtxo.outputIx
            )

        override def runState(state: State): (RealResult, State) =

            // Native script, head address, and token
            val pubKeys =
                (otherHeadPeers.toSet + initiator).map(tp =>
                    mkWallet(tp).exportVerificationKeyBytes
                )
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
            log.info(s"Initialization tx hash: ${serializeTxHex(tx)}")
            val txId = txHash(tx)
            log.info(s"Initialization tx hash: $txId")

            l1Mock.submit(tx.toL1Tx)

            val newState = state.copy(
              peersNetworkPhase = RunningHead,
              initiator = Some(initiator),
              headPeers = otherHeadPeers,
              knownTxs = l1Mock.getKnownTxs,
              utxosActive = l1Mock.getUtxosActive
            )

            (Right(txId), newState)

        override def preCondition(state: State): Boolean =
            state.peersNetworkPhase match
                case NewlyCreated => true
                case Freed        => true
                case _            => false

        override def postConditionSuccess(
            expectedResult: RealResult,
            stateBefore: State,
            stateAfter: State,
            sutResult: RealResult,
            sutInspector: SutInspector
        ): Prop =
            sutResult match
                case Left(err) =>
                    s"Unexpected negative resposne: $err" |: Prop.falsified
                case Right(sutTxHash) =>
                    val reader = sutInspector.reader
                    val headPhase = reader.currentPhase
                    val headPeers = stateAfter.headPeers + stateAfter.initiator.get

                    (s"headPeers in Initializing phase should be: $headPeers"
                        |: headPhase == Initializing ==> (reader
                            .initializingPhaseReader(_.headPeers)
                            .map(w => TestPeer.valueOf(w.name)) == headPeers))
                    && (s"headPeers in Open phase should be: $headPeers"
                        |: headPhase == Open ==> (reader
                            .openPhaseReader(_.headPeers)
                            .map(w => TestPeer.valueOf(w.name)) == headPeers))
                    && (s"result should be the same" |:
                        sutResult == expectedResult)

        // TODO: this is more for demonstration purposes
        override def postConditionFailure(
            _expectedResult: RealResult,
            _stateBefore: State,
            _stateAfter: State,
            _err: Throwable
        ) = "Should not crash unless number of peers is too small" |: otherHeadPeers.isEmpty

object HydrozoaOneNodeWithL1Mock extends Properties("Hydrozoa One node mode with L1 mock") {
    property("Just_works") = MBTSuite.property()
}

object HydrozoaOneNodeWithYaci extends Properties("Hydrozoa One node mode with Yaci") {
    MBTSuite.useYaci = true
    property("Just_works") = MBTSuite.property()
}
