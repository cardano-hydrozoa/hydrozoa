package hydrozoa.model

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{PSStyleAssoc, Piper, decodeBech32AddressL1, decodeBech32AddressL2, serializeTxHex, txHash}
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.tx.deposit.{BloxBeanDepositTxBuilder, DepositTxBuilder, DepositTxRecipe}
import hydrozoa.l1.multisig.tx.initialization.{BloxBeanInitializationTxBuilder, InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.refund.{BloxBeanRefundTxBuilder, PostDatedRefundRecipe, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.toL1Tx
import hydrozoa.l1.{BackendServiceMock, CardanoL1Mock}
import hydrozoa.l2.block.createBlock
import hydrozoa.l2.ledger.*
import hydrozoa.model.PeersNetworkPhase.{Freed, NewlyCreated, RunningHead, Shutdown}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.{account, mkPeerInfo, mkWallet}
import hydrozoa.node.server.{DepositError, DepositRequest, DepositResponse, InitializeError, mkBlockEffects}
import hydrozoa.node.state.BlockRecord
import hydrozoa.node.state.HeadPhase.{Finalizing, Initializing, Open}
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop, Properties}
import scalus.prelude.Maybe
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
        print("<-------------------- destroy SUT...")
        // Thread.sleep(2_000)
        // println(" Done!")

    override def initialPreCondition(state: State): Boolean =
        state.peersNetworkPhase == NewlyCreated

    override def genInitialState: Gen[State] = for
        numberOfNetworkPeers <- Gen.chooseNum(3, 8)
        networkPeers <- Gen.pick(numberOfNetworkPeers, TestPeer.values)
    yield HydrozoaState(Utils.protocolParams, networkPeers.toSet)

    override def genCommand(s: State): Gen[Command] =
        s.peersNetworkPhase match
            case NewlyCreated =>
                Gen.frequency(
                  10 -> genInitializeCommand(s),
                  1 -> Gen.const(ShutdownCommand)
                )
            case RunningHead =>
                lazy val l2InputCommandGen = Gen.frequency(
                  5 -> genTransactionL2(s),
                  1 -> genL2Withdrawal(s)
                )
                if s.utxosActiveL2.isEmpty then
                    Gen.oneOf(genDepositCommand(s), genCreateBlock(s))
                else
                    Gen.frequency(
                      1 -> genDepositCommand(s),
                      3 -> genCreateBlock(s),
                      10 -> l2InputCommandGen
                    )

            case Freed =>
                Gen.frequency(
                  2 -> genInitializeCommand(s),
                  1 -> Gen.const(ShutdownCommand)
                )
            case Shutdown => NoOp

    def genInitializeCommand(s: State): Gen[InitializeCommand] =
        val initiator = s.knownPeers.head
        val account = TestPeer.account(initiator)
        val l1 = CardanoL1Mock(s.knownTxs, s.utxosActive)
        val utxoIds = l1.utxoIdsByAddress(AddressBechL1(account.toString))

        for
            numberOfHeadPeers <- Gen.chooseNum(0, s.knownPeers.tail.size)
            headPeers <- Gen.pick(numberOfHeadPeers, s.knownPeers.tail)
            seedUtxoId <- Gen.oneOf(utxoIds)
        yield InitializeCommand(initiator, headPeers.toSet, seedUtxoId)

    def genDepositCommand(s: State): Gen[DepositCommand] =
        // in one-node hydrozoa we can only deposit on behalf of initiator
        val depositor = s.initiator.get
        val depositorAccount = TestPeer.account(depositor)
        val depositorAddressL1 = AddressBechL1(depositorAccount.toString) // FIXME: extension
        val l1 = CardanoL1Mock(s.knownTxs, s.utxosActive)
        val utxoIds = l1.utxoIdsByAddress(depositorAddressL1)

        for
            seedUtxoId <- Gen.oneOf(utxoIds)
            recipient <- Gen.oneOf(s.knownPeers + s.initiator.get)
            recipientAccount = TestPeer.account(recipient)
            recipientAddressL2 = AddressBechL2(depositorAccount.toString) // FIXME: extension
        yield DepositCommand(depositor, seedUtxoId, recipientAddressL2, depositorAddressL1)

    def genTransactionL2(s: State): Gen[TransactionL2Command] =
        val l2 = AdaSimpleLedger.apply(s.utxosActiveL2)

        for
            numberOfInputs <- Gen.choose(1, 5.min(s.utxosActiveL2.size))
            inputs <- Gen.pick(numberOfInputs, s.utxosActiveL2.keySet)
            totalAda = inputs.map(l2.getOutput(_).coins).sum.intValue

            outputAda: List[Int] <- Gen.recursive[List[Int]] { fix =>
                Gen.choose(1, totalAda).flatMap { step =>
                    fix.map(tail =>
                        val tailSum = tail.sum
                        if (tailSum + step < totalAda)
                            step :: tail
                        else totalAda - tailSum :: Nil
                    )
                }
            }

            recipients <- Gen
                .pick(outputAda.length, s.knownPeers)
                .map(_.map(account(_).toString |> AddressBechL2.apply))

            outputs = outputAda
                .zip(recipients)
                .map((coins, address) => SimpleOutput(address, coins))
        yield TransactionL2Command(SimpleTransaction(inputs.toList, outputs))

    def genL2Withdrawal(s: State): Gen[WithdrawalL2Command] =
        for
            numberOfInputs <- Gen.choose(1, 3.min(s.utxosActiveL2.size))
            inputs <- Gen.pick(numberOfInputs, s.utxosActiveL2.keySet)
        yield WithdrawalL2Command(SimpleWithdrawal(inputs.toList))

    def genCreateBlock(s: State): Gen[ProduceBlockCommand] =
        for finalize <- Gen.prob(0.01)
    yield ProduceBlockCommand(finalize)

    /** State-like Command that uses `runState` instead of `nextState`. Additionally branches on
      * `result`, providing `SutInspector` for successful path.
      */
    trait StateLikeInspectabeCommand extends Command:

        final type SutInspector = NodeStateInspector

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

    class InitializeCommand(
        initiator: TestPeer,
        otherHeadPeers: Set[TestPeer],
        seedUtxo: UtxoIdL1
    ) extends StateLikeInspectabeCommand:

        val log = Logger(getClass)

        override type RealResult = Either[InitializeError, TxId]

        override def run(sut: HydrozoaSUT): Result =
            log.info(".run")

            sut.initializeHead(
              otherHeadPeers.map(mkPeerInfo).toSet,
              100,
              seedUtxo.txId,
              seedUtxo.outputIx
            )

        override def runState(state: State): (RealResult, State) =
            log.info(".runState")

            // Native script, head address, and token
            val pubKeys =
                (otherHeadPeers.toSet + initiator).map(tp =>
                    mkWallet(tp).exportVerificationKeyBytes
                )
            val (headMultisigScript, headAddress) =
                mkHeadNativeScriptAndAddress(pubKeys, networkL1static)
            val beaconTokenName = mkBeaconTokenName(seedUtxo)

            // Recipe to build init tx
            val initTxRecipe = InitTxRecipe(
              headAddress,
              seedUtxo,
              100_000_000,
              headMultisigScript,
              beaconTokenName
            )

            val l1Mock = CardanoL1Mock(state.knownTxs, state.utxosActive)
            val backendService = BackendServiceMock(l1Mock, state.pp)
            val initTxBuilder: InitTxBuilder = BloxBeanInitializationTxBuilder(backendService)
            val Right(tx, _) = initTxBuilder.mkInitializationTxDraft(initTxRecipe)
            log.info(s"Init tx: ${serializeTxHex(tx)}")
            val txId = txHash(tx)
            log.info(s"Init tx hash: $txId")

            l1Mock.submit(tx.toL1Tx)

            val newState = state.copy(
              peersNetworkPhase = RunningHead,
              headPhase = Some(Open), // FIXME: Initializing in some impls
              initiator = Some(initiator),
              headPeers = otherHeadPeers,
              headAddressBech32 = headAddress |> AddressBechL1.apply |> Some.apply,
              headMultisigScript = Some(headMultisigScript),
              knownTxs = l1Mock.getKnownTxs,
              utxosActive = l1Mock.getUtxosActive
            )

            /*
            TODO: Not really nice.
            Sequential Commands:
              1. hydrozoa.model.MBTSuite$InitializeCommand@125c082e => Failure(java.lan
              g.AssertionError: assertion failed: Solo node mode is not supported yet.)
              2. hydrozoa.model.MBTSuite$DepositCommand@584f5497 => Failure(scala.Match
              Error: Left(utxo not found: 41bf95a7b9a9ed0794cd8a4987126b8c3062b7cc18f05
              2d23c51250da7467b21#1) (of class scala.util.Left))
            */

            if otherHeadPeers.isEmpty then
                (Left("Solo mode is not supported yet"), state)
            else
                (Right(txId), newState)

        override def preCondition(state: State): Boolean =
            log.info(".preCondition")

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
            log.info(".postConditionSuccess")

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
            err: Throwable
        ): Prop =
            log.info(".postConditionFailure")

            s"Should not crash unless number of peers is too small: $err"
            |: otherHeadPeers.isEmpty

    class DepositCommand(
        depositor: TestPeer,
        fundUtxo: UtxoIdL1,
        address: AddressBechL2,
        refundAddress: AddressBechL1
    ) extends StateLikeInspectabeCommand:

        val log = Logger(getClass)

        override type RealResult = Either[DepositError, DepositResponse]

        override def runState(
            state: HydrozoaState
        ): (Either[DepositError, DepositResponse], HydrozoaState) =
            log.info(".runState")

            // Make the datum and the recipe
            val depositDatum = DepositDatum(
                decodeBech32AddressL2(address),
                Maybe.Nothing,
                BigInt.apply(0),
                decodeBech32AddressL1(refundAddress),
                Maybe.Nothing
            )

            val depositTxRecipe = DepositTxRecipe(fundUtxo, depositDatum)

            val l1Mock = CardanoL1Mock(state.knownTxs, state.utxosActive)
            val backendService = BackendServiceMock(l1Mock, state.pp)
            val nodeStateReader = NodeStateReaderMock(state)
            val depositTxBuilder: DepositTxBuilder = BloxBeanDepositTxBuilder(backendService, nodeStateReader)

            // Build a deposit transaction draft as a courtesy of Hydrozoa (no signature)
            val Right(depositTxDraft, index) = depositTxBuilder.buildDepositTxDraft(depositTxRecipe)
            val depositTxHash = txHash(depositTxDraft)

            val serializedTx = serializeTxHex(depositTxDraft)
            log.info(s"Expected deposit tx: $serializedTx")
            log.info(s"Expected deposit tx hash: $depositTxHash, deposit output index: $index")

            l1Mock.submit(depositTxDraft |> toL1Tx)

            val refundTxBuilder: RefundTxBuilder = BloxBeanRefundTxBuilder(l1Mock, backendService, nodeStateReader)

            val Right(refundTxDraft) =
                refundTxBuilder.mkPostDatedRefundTxDraft(
                    PostDatedRefundRecipe(depositTxDraft, index)
                )

            val depositUtxoId = UtxoIdL1(depositTxHash, index)
            val ret = Right(DepositResponse(refundTxDraft, depositUtxoId))

            val depositUtxo = l1Mock.utxoById(depositUtxoId).get

            val newState = state.copy(
                depositUtxos = UtxoSet(state.depositUtxos.map ++ Map.apply((depositUtxoId, depositUtxo))),
                knownTxs = l1Mock.getKnownTxs,
                utxosActive = l1Mock.getUtxosActive
            )

            (ret, newState)

        override def postConditionSuccess(
            expectedResult: Either[DepositError, DepositResponse],
            stateBefore: HydrozoaState,
            stateAfter: HydrozoaState,
            result: Either[DepositError, DepositResponse],
            sutInspector: SutInspector
        ): Prop =
            log.info(".postConditionSuccess")

            val expectedResponse = expectedResult.right.get
            val response = result.right.get

            ("Deposit txs hashed should be identical" |:
                expectedResponse.depositId.txId == response.depositId.txId)
                &&  ("Deposit txs outputs should be identical" |:
                        expectedResponse.depositId.outputIx == response.depositId.outputIx)
                && ("Post-dated refund txs should have the same hash" |:
                    txHash(expectedResponse.postDatedRefundTx) == txHash(response.postDatedRefundTx))


        override def postConditionFailure(
            expectedResult: Either[DepositError, DepositResponse],
            stateBefore: HydrozoaState,
            stateAfter: HydrozoaState,
            err: Throwable
        ): Prop =
            log.info(".postConditionFailure")

            s"Should not crash: $err"
                |: false

        override def run(sut: HydrozoaSUT): (Either[DepositError, DepositResponse], SutInspector) =
            val request = DepositRequest(
                fundUtxo.txId,
                fundUtxo.outputIx,
                None,
                address,
                None,
                refundAddress,
                None
            )
            log.info(".run")
            sut.deposit(request)

        override def preCondition(state: HydrozoaState): Boolean =
            val preCondition = state.peersNetworkPhase == RunningHead
                && state.headPhase == Some(Open)
                && state.initiator.get == depositor
                && state.utxosActive.contains(fundUtxo)

            log.info(s".preCondition: $preCondition")
            preCondition

    class TransactionL2Command(simpleTransaction: SimpleTransaction) extends Command:
        override type Result = Unit

        override def run(sut: HydrozoaSUT): Unit = ???

        override def nextState(state: HydrozoaState): HydrozoaState = ???

        override def preCondition(state: HydrozoaState): Boolean = ???

        override def postCondition(state: HydrozoaState, result: Try[Unit]): Prop = ???

    class WithdrawalL2Command(simpleWithdrawal: SimpleWithdrawal) extends Command:
        override type Result = Unit

        override def run(sut: HydrozoaSUT): Unit = ???

        override def nextState(state: HydrozoaState): HydrozoaState = ???

        override def preCondition(state: HydrozoaState): Boolean = ???

        override def postCondition(state: HydrozoaState, result: Try[Unit]): Prop = ???

    class ProduceBlockCommand(finalization: Boolean) extends StateLikeInspectabeCommand:

        val log = Logger(getClass)

        override type RealResult = Either[String, (BlockRecord, UtxosSet, UtxosSet)]

        override def runState(
            state: HydrozoaState
        ): (Either[String, (BlockRecord, UtxosSet, UtxosSet)], HydrozoaState) =
            log.info(".runState")

            // Produce block
            val l2 = AdaSimpleLedger.apply[TBlockProduction](state.utxosActiveL2)

            val maybeNewBlock = createBlock(
                l2,
                state.poolEvents,
                state.depositUtxos,
                state.l2Tip.get.blockHeader,
                timeCurrent,
                (state.headPhase.get == Finalizing)
            )

            maybeNewBlock match
                case None => Left("Block can't be produced at the moment.") /\ state
                case Some(block, utxosActive, utxosAdded, utxosWithdrawn, mbGenesis) =>
                    val (l1effect, l2Effect) = mkBlockEffects(block, utxosActive, utxosWithdrawn, mbGenesis))
                    val record = BlockRecord(block, ???, (), ())
                    val newState = state.copy(
                        headPhase = if finalization then Some(Finalizing) else state.headPhase
                    )

                    Right(record, utxosAdded, utxosWithdrawn) /\ newState

        override def postConditionSuccess(
            expectedResult: Either[String, (BlockRecord, UtxosSet, UtxosSet)],
            stateBefore: HydrozoaState,
            stateAfter: HydrozoaState,
            result: Either[String, (BlockRecord, UtxosSet, UtxosSet)],
            sutInspector: SutInspector
        ): Prop =
            log.info(".postConditionSuccess")

            true // FIXME:

        override def postConditionFailure(
            expectedResult: Either[String, (BlockRecord, UtxosSet, UtxosSet)],
            stateBefore: HydrozoaState,
            stateAfter: HydrozoaState,
            err: Throwable
        ): Prop =
            log.info(".postConditionFailure")

            true // FIXME:

        override def run(sut: HydrozoaSUT): (Either[String, (BlockRecord, UtxosSet, UtxosSet)], SutInspector) =
            log.info(".run")

            sut.produceBlock(finalization)

        override def preCondition(state: HydrozoaState): Boolean =
            log.info(".preCondition")

            state.peersNetworkPhase == RunningHead
                && state.headPhase == Some(Open)

    object ShutdownCommand extends UnitCommand:

        val log = Logger(getClass)

        override def postCondition(state: HydrozoaState, success: Boolean): Prop =
            log.info(".postCondition")

            val phase = state.peersNetworkPhase
            "Shutdown always possible in NewlyCreated and Freed phases" |:
                phase == NewlyCreated || phase == Freed ==> (success == true)

        override def run(sut: HydrozoaSUT): Unit =
            log.info(".run")

            sut.shutdownSut()

        override def nextState(state: HydrozoaState): HydrozoaState =
            log.info(".nextState")

            state.copy(
              peersNetworkPhase = Shutdown,
              knownPeers = Set.empty,
              knownTxs = Map.empty,
              utxosActive = Map.empty
            )

        override def preCondition(state: HydrozoaState): Boolean =
            log.info(".preCondition")

            state.peersNetworkPhase match
            case NewlyCreated => true
            case Freed        => true
            case _            => false

object HydrozoaOneNodeWithL1Mock extends Properties("Hydrozoa One node mode with L1 mock") {
    property("Just_works") = MBTSuite.property()
}

object HydrozoaOneNodeWithYaci extends Properties("Hydrozoa One node mode with Yaci") {
    MBTSuite.useYaci = true
    property("Just_works") = MBTSuite.property()
}
