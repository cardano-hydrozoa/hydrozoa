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

    /** State-like Command that returns both result and state like `runState` in Haskell does. TODO:
      * Inspectable part docs
      */
    trait StateLikeInspectabeCommand extends Command:

        type SutInspector

        type RealResult

        type Result = (RealResult, SutInspector)

        final override def nextState(state: State): State = runState(state)._2

        def runState(state: State): (Result, State)

        final override def postCondition(stateBefore: State, result: Try[Result]): Prop =
            result match
                case Success(realResult: RealResult, inspector: SutInspector) =>
                    postConditionInspectable(stateBefore, Success(realResult), Some(inspector))
                case Failure(e) =>
                    postConditionInspectable(stateBefore, Failure(e), None)

        def postConditionInspectable(
            stateBefore: State,
            result: Try[RealResult],
            inspector: Option[SutInspector]
        ): Prop

    case class InitializeCommand(
        initiator: TestPeer,
        otherHeadPeers: Set[TestPeer],
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

        // FIXME: return tuple of State and Return
        override def nextState(state: State): State =

            // Native script, head address, and token
            val pubKeys =
                (otherHeadPeers.toSet + initiator).map(tp => mkWallet(tp).exportVerificationKeyBytes)
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

            state.copy(
              peersNetworkPhase = RunningHead,
              initiator = Some(initiator),
              headPeers = otherHeadPeers,
              retInitializationTxHash = Some(txId),
              knownTxs = l1Mock.getKnownTxs,
              utxosActive = l1Mock.getUtxosActive
            )

        override def preCondition(state: State): Boolean =
            state.peersNetworkPhase match
                case NewlyCreated => true
                case Freed        => true
                case _            => false

        override def postCondition(stateBefore: State, result: Try[Result]): Prop =
            if otherHeadPeers.isEmpty then result.isFailure
            else
                val (ret, inspector) = result.get
                val reader = inspector.reader
                val headPhase = reader.currentPhase

                val stateAfter = nextState(stateBefore)
                val headPeers = stateAfter.headPeers.toSet + stateAfter.initiator.get
                val txHash = stateAfter.retInitializationTxHash.get

                ("headPeers should match command in Initializing phase"
                    |: headPhase == Initializing ==> (reader
                        .initializingPhaseReader(_.headPeers)
                        .map(w => TestPeer.valueOf(w.name)) == headPeers))
                && (s"headPeers in Open phase: ${reader.openPhaseReader(_.headPeers).map(w => TestPeer.valueOf(w.name))}," +
                    s" should be: $headPeers"
                    |: headPhase == Open ==> (reader
                        .openPhaseReader(_.headPeers)
                        .map(w => TestPeer.valueOf(w.name)) == headPeers))
                && ("initialize response should be Right" |: ret.isRight)
                && (s"initialization tx hash is: ${ret.right.get} should be $txHash" |:
                    ret.right.get == txHash)

object HydrozoaOneNodeWithL1Mock extends Properties("Hydrozoa One node mode with L1 mock") {
    property("Just_works") = MBTSuite.property()
}

object HydrozoaOneNodeWithYaci extends Properties("Hydrozoa One node mode with Yaci") {
    MBTSuite.useYaci = true
    property("Just_works") = MBTSuite.property()
}
