//noinspection OptionEqualsSome
package hydrozoa.demo

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.demo.PeersNetworkPhase.{Freed, NewlyCreated, RunningHead, Shutdown}
import hydrozoa.infra.{
    NoMatch,
    PSStyleAssoc,
    Piper,
    TooManyMatches,
    decodeBech32AddressL1,
    decodeBech32AddressL2,
    onlyOutputToAddress,
    serializeTxHex,
    txHash
}
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.state.{DepositDatum, DepositTag}
import hydrozoa.l1.multisig.tx.deposit.{BloxBeanDepositTxBuilder, DepositTxBuilder, DepositTxRecipe}
import hydrozoa.l1.multisig.tx.finalization.BloxBeanFinalizationTxBuilder
import hydrozoa.l1.multisig.tx.initialization.{
    BloxBeanInitializationTxBuilder,
    InitTxBuilder,
    InitTxRecipe
}
import hydrozoa.l1.multisig.tx.refund.{
    BloxBeanRefundTxBuilder,
    PostDatedRefundRecipe,
    RefundTxBuilder
}
import hydrozoa.l1.multisig.tx.settlement.BloxBeanSettlementTxBuilder
import hydrozoa.l1.multisig.tx.toL1Tx
import hydrozoa.l1.{BackendServiceMock, CardanoL1Mock}
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.block.{BlockEffect, createBlock}
import hydrozoa.l2.ledger.*
import hydrozoa.l2.ledger.state.unliftUtxoSet
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import hydrozoa.node.server.*
import hydrozoa.node.state.HeadPhase.{Finalizing, Open}
import hydrozoa.node.state.{*, given}
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}
import scalus.prelude.Maybe

import scala.jdk.CollectionConverters.*
import scala.language.strictEquality
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
        // Create a new SUT
        println("--------------------> new SUT")
        RealHydrozoaSUT.apply()

    override def destroySut(_sut: Sut): Unit =
        println("<-------------------- destroy SUT")

    override def initialPreCondition(state: State): Boolean =
        state.peersNetworkPhase == NewlyCreated

    override def genInitialState: Gen[State] =
        Gen.const(HydrozoaState(Utils.protocolParams, Set(Alice, Bob, Carol)))

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
                      2 -> genCreateBlock(s),
                      7 -> l2InputCommandGen
                    )

            case Freed =>
                Gen.frequency(
                  2 -> genInitializeCommand(s),
                  1 -> Gen.const(ShutdownCommand)
                )
            case Shutdown => NoOp

    def genInitializeCommand(s: State): Gen[InitializeCommand] =
        for
            initiator <- Gen.oneOf(s.knownPeers)
            account = TestPeer.account(initiator)
            //numberOfHeadPeers <- Gen.chooseNum(0, s.knownPeers.tail.size)
            //headPeers <- Gen.pick(numberOfHeadPeers, s.knownPeers.tail)
            headPeers = s.knownPeers.filterNot(p => p == initiator)
            l1 = CardanoL1Mock(s.knownTxs, s.utxosActive)
            utxoIds = l1.utxoIdsByAddress(AddressBechL1(account.toString))
            seedUtxoId <- Gen.oneOf(utxoIds)
        yield InitializeCommand(initiator, headPeers, seedUtxoId)

    def genDepositCommand(s: State): Gen[DepositCommand] =
        for
            depositor <- Gen.oneOf(s.headPeers + s.initiator.get)
            depositorAccount = TestPeer.account(depositor)
            depositorAddressL1 = AddressBechL1(depositorAccount.toString) // FIXME: extension
            l1 = CardanoL1Mock(s.knownTxs, s.utxosActive)
            utxoIds = l1.utxoIdsByAddress(depositorAddressL1)
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
            totalCoins = inputs.map(l2.getOutput(_).coins).sum.intValue

            outputCoins <- Gen.tailRecM[List[Int], List[Int]](List.empty){
                tails =>
                    val residual = totalCoins - tails.sum
                    if residual < 15_000_000
                        then Gen.const(Right(residual :: tails))
                        else
                            for
                                next <- Gen.choose(5_000_000, residual)
                            yield Left(next :: tails)
            }

            recipients <- Gen.containerOfN[List, TestPeer](outputCoins.length, Gen.oneOf(s.headPeers))

            outputs = outputCoins
                .zip(recipients.map(account(_).toString |> AddressBechL2.apply))
                .map((coins, address) => SimpleOutput(address, coins))
        yield TransactionL2Command(SimpleTransaction(inputs.toList, outputs))

    def genL2Withdrawal(s: State): Gen[WithdrawalL2Command] =
        for
            numberOfInputs <- Gen.choose(1, 3.min(s.utxosActiveL2.size))
            inputs <- Gen.pick(numberOfInputs, s.utxosActiveL2.keySet)
        yield WithdrawalL2Command(SimpleWithdrawal(inputs.toList))

    def genCreateBlock(s: State): Gen[ProduceBlockCommand] =
        for finalize <- Gen.prob(0.01)
    yield ProduceBlockCommand(s.knownPeers + s.initiator.get, finalize)

    /** State-like Command that uses `runState` instead of `nextState`. Additionally branches on
      * `result`, providing `SutInspector` for successful path.
      */
    trait StateLikeInspectabeCommand extends Command:

        final type SutInspector = NodeStateInspector

        type RealResult

        type Result = RealResult

        final override def nextState(state: State): State = runState(state)._2

        def runState(state: State): (RealResult, State)

        final override def postCondition(stateBefore: State, result: Try[Result]): Prop =
            val (expectedResult, stateAfter) = runState(stateBefore)
            result match
                case Success(realResult: RealResult) =>
                    postConditionSuccess(
                      expectedResult,
                      stateBefore,
                      stateAfter,
                      realResult,
                    )
                case Failure(e) =>
                    postConditionFailure(expectedResult, stateBefore, stateAfter, e)

        def postConditionSuccess(
            expectedResult: RealResult,
            stateBefore: State,
            stateAfter: State,
            result: RealResult
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

        private val log = Logger(getClass)

        override type RealResult = Either[InitializationError, TxId]

        override def toString: String =
            s"Initialize command {initiator=$initiator, other peers = $otherHeadPeers, seed utxo = $seedUtxo}"

        override def run(sut: HydrozoaSUT): Result =
            log.info(".run")

            sut.initializeHead(
              initiator,
              1000,
              seedUtxo.txId,
              seedUtxo.outputIx
            )

        override def runState(state: State): (RealResult, State) =
            log.info(".runState")

            // Native script, head address, and token
            val pubKeys =
                (otherHeadPeers + initiator).map(tp =>
                    mkWallet(tp).exportVerificationKeyBytes
                )
            val (headMultisigScript, headAddress) =
                mkHeadNativeScriptAndAddress(pubKeys, networkL1static)
            val beaconTokenName = mkBeaconTokenName(seedUtxo)

            // Recipe to build init initTx
            val initTxRecipe = InitTxRecipe(
              headAddress,
              seedUtxo,
              1000_000_000,
              headMultisigScript,
              beaconTokenName
            )

            val l1Mock = CardanoL1Mock(state.knownTxs, state.utxosActive)
            val backendService = BackendServiceMock(l1Mock, state.pp)
            val initTxBuilder: InitTxBuilder = BloxBeanInitializationTxBuilder(backendService)
            val Right(initTx, _) = initTxBuilder.mkInitializationTxDraft(initTxRecipe)
            log.info(s"Init initTx: ${serializeTxHex(initTx)}")
            val txId = txHash(initTx)
            log.info(s"Init initTx hash: $txId")

            l1Mock.submit(initTx.toL1Tx)

            val treasuryUtxoId = onlyOutputToAddress(initTx |> toL1Tx, headAddress) match
                case Right(ix, _, _) => UtxoIdL1(txId, ix)
                case Left(err) => err match
                        case _: NoMatch => throw RuntimeException("Can't find treasury in the initialization tx!")
                        case _: TooManyMatches => throw RuntimeException("Initialization tx contains more than one multisig outputs!")

            val newState = state.copy(
              peersNetworkPhase = RunningHead,
              headPhase = Some(Open), // FIXME: Initializing in some impls
              initiator = Some(initiator),
              headPeers = otherHeadPeers,
              headAddressBech32 = Some(headAddress),
              headMultisigScript = Some(headMultisigScript),
              treasuryUtxoId = Some(treasuryUtxoId),
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
            sutResult: RealResult
        ): Prop =
            log.info(".postConditionSuccess")

            sutResult match
                case Left(err) =>
                    s"Unexpected negative resposne: $err" |: Prop.falsified
                case Right(sutTxHash) =>
                    s"result should be the same" |:
                        sutResult == expectedResult

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

        private val log = Logger(getClass)

        override type RealResult = Either[DepositError, DepositResponse]

        override def toString: String =
            s"Deposit command { depositor = $depositor, fund utxo = $fundUtxo, address = $address, refund address = $refundAddress}"

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

            val Right(_) = l1Mock.submit(depositTxDraft |> toL1Tx)

            val refundTxBuilder: RefundTxBuilder = BloxBeanRefundTxBuilder(l1Mock, backendService, nodeStateReader)

            val Right(refundTxDraft) =
                refundTxBuilder.mkPostDatedRefundTxDraft(
                    PostDatedRefundRecipe(depositTxDraft, index)
                )

            val depositUtxoId = UtxoIdL1(depositTxHash, index)
            val ret = Right(DepositResponse(refundTxDraft, depositUtxoId))

            val depositUtxo: OutputL1 = l1Mock.utxoById(depositUtxoId).get
                // FIXME: temporarily, use the whole deposit datum
                .copy(address = this.address.asL1)

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
            result: Either[DepositError, DepositResponse]
        ): Prop =
            log.info(".postConditionSuccess")

            val expectedResponse = expectedResult.right.get
            val response = result.right.get

            ("Deposit txs hashed should be identical" |:
                expectedResponse.depositId.txId == response.depositId.txId)
                &&  ("Deposit txs outputs should be identical" |:
                        expectedResponse.depositId.outputIx == response.depositId.outputIx)
// FIXME: this fails for some reason in the demo
//                && ("Post-dated refund txs should have the same hash" |:
//                    txHash(expectedResponse.postDatedRefundTx) == txHash(response.postDatedRefundTx))


        override def postConditionFailure(
            expectedResult: Either[DepositError, DepositResponse],
            stateBefore: HydrozoaState,
            stateAfter: HydrozoaState,
            err: Throwable
        ): Prop =
            log.info(".postConditionFailure")

            s"Should not crash: $err"
                |: false

        override def run(sut: HydrozoaSUT): (Either[DepositError, DepositResponse]) =
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
            sut.deposit(depositor, request)

        override def preCondition(state: HydrozoaState): Boolean =
            val preCondition = state.peersNetworkPhase == RunningHead
                && state.headPhase == Some(Open)
                && state.utxosActive.contains(fundUtxo)

            log.info(s".preCondition: $preCondition")
            preCondition

    class TransactionL2Command(simpleTransaction: SimpleTransaction) extends Command:

        private val log = Logger(getClass)

        override type Result = Unit

        override def toString: String = s"Transaction L2 command { $simpleTransaction }"

        override def run(sut: HydrozoaSUT): Unit =
            log.info(".run")
            sut.submitL2(simpleTransaction)

        override def nextState(state: HydrozoaState): HydrozoaState =
            log.info(".nextState")
            state.copy(
                poolEvents = state.poolEvents ++ Seq(AdaSimpleLedger.mkTransactionEvent(simpleTransaction))
            )

        override def preCondition(state: HydrozoaState): Boolean =
            log.info(".preCondition")
            state.peersNetworkPhase == RunningHead
                && state.headPhase == Some(Open)

        override def postCondition(state: HydrozoaState, result: Try[Unit]): Prop =
            log.info(".postCondition")
            true

    class WithdrawalL2Command(simpleWithdrawal: SimpleWithdrawal) extends Command:

        private val log = Logger(getClass)

        override type Result = Unit

        override def toString: String = s"Withdrawal L2 command { $simpleWithdrawal }"

        override def run(sut: HydrozoaSUT): Unit =
            log.info(".run")
            sut.submitL2(simpleWithdrawal)

        override def nextState(state: HydrozoaState): HydrozoaState =
            log.info(".nextState")
            state.copy(
                poolEvents = state.poolEvents ++ Seq(AdaSimpleLedger.mkWithdrawalEvent(simpleWithdrawal))
            )

        override def preCondition(state: HydrozoaState): Boolean =
            log.info(".preCondition")
            state.peersNetworkPhase == RunningHead
                && state.headPhase == Some(Open)

        override def postCondition(state: HydrozoaState, result: Try[Unit]): Prop =
            log.info(".postCondition")
            true

    class ProduceBlockCommand(
         headPeers: Set[TestPeer],
         finalization: Boolean
        ) extends StateLikeInspectabeCommand:

        private val log = Logger(getClass)

        override type RealResult = Either[String, Unit]

        override def toString: String = s"Produce block command {finalization = $finalization}"

        override def runState(
            state: HydrozoaState
        ): (Either[String, Unit], HydrozoaState) =
            log.info(".runState")


            // Produce block
            val l2 = AdaSimpleLedger.apply[TBlockProduction](state.utxosActiveL2)

            val maybeNewBlock = createBlock(
                l2,
                state.poolEvents,
                state.depositUtxos,
                state.l2Tip.blockHeader,
                timeCurrent,
                (state.headPhase.get == Finalizing)
            )

            maybeNewBlock match
                case None => Left("Block can't be produced at the moment.") /\ state
                case Some(block, utxosActive, utxosAdded, utxosWithdrawn, mbGenesis) =>

                    val l1Mock = CardanoL1Mock(state.knownTxs, state.utxosActive)
                    val backendService = BackendServiceMock(l1Mock, state.pp)
                    val nodeStateReader = NodeStateReaderMock(state)

                    val settlementTxBuilder = BloxBeanSettlementTxBuilder(backendService, nodeStateReader)
                    val finalizationTxBuilder = BloxBeanFinalizationTxBuilder(backendService, nodeStateReader)

                    val l1Effect = BlockEffect.mkL1BlockEffectModel(settlementTxBuilder, finalizationTxBuilder, block, utxosWithdrawn)
                    val l2Effect: L2BlockEffect = block.blockHeader.blockType match
                        case Minor => utxosActive
                        case Major => utxosActive /\ mbGenesis
                        case Final => ()

                    val record = BlockRecord(block, l1Effect, (), l2Effect)

                    // Calculate new state
                    // Submit L1
                    (l1Effect |> maybeMultisigL1Tx).map(l1Mock.submit)

                    l2Effect match
                        case utxosActive: MinorBlockL2Effect =>
                            l2.replaceUtxosActive(utxosActive)
                        case (utxosActive -> mbGenesis) =>
                            l2.replaceUtxosActive(utxosActive)
                        // TODO: delete block events (both valid and invalid)
                        case _: FinalBlockL2Effect =>
                            l2.flush

                    val blockEvents = block.blockBody.eventsValid.map(_._1) ++ block.blockBody.eventsInvalid.map(_._1)

                    // Possibly new treasury utxo id
                    val treasuryUtxoId =
                        (l1Effect |> maybeMultisigL1Tx).map(tx =>
                            val txId = txHash(tx)
                            val Right(ix,_,_ ) = onlyOutputToAddress(tx, state.headAddressBech32.get)
                            Some(UtxoIdL1(txId, ix))
                         ).getOrElse(state.treasuryUtxoId)

                    // Why does it typecheck?
                    //val newDepositUtxos = state.depositUtxos.map.filterNot(block.blockBody.depositsAbsorbed.contains) |> UtxoSet.apply[L1, DepositTag]
                    val newDepositUtxos = state.depositUtxos.map
                        .filterNot((k,_) => block.blockBody.depositsAbsorbed.contains(k)) |> UtxoSet.apply[L1, DepositTag]

                    val newState = state.copy(
                        depositUtxos = newDepositUtxos,
                        poolEvents = state.poolEvents.filterNot(e => blockEvents.contains(e.getEventId)),
                        l2Tip = block,
                        headPhase = if finalization then Some(Finalizing) else state.headPhase,
                        knownTxs = l1Mock.getKnownTxs,
                        treasuryUtxoId = treasuryUtxoId,
                        utxosActive = l1Mock.getUtxosActive,
                        utxosActiveL2 = l2.getUtxosActive |> unliftUtxoSet
                    )

                    Right(()) /\ newState

        override def postConditionSuccess(
            expectedResult: Either[String, Unit],
            stateBefore: HydrozoaState,
            stateAfter: HydrozoaState,
            result: Either[String, Unit]
        ): Prop =
            log.info(".postConditionSuccess")
            true

        override def postConditionFailure(
            expectedResult: Either[String, Unit],
            stateBefore: HydrozoaState,
            stateAfter: HydrozoaState,
            err: Throwable
        ): Prop =
            log.error(".postConditionFailure should never happen")
            false

        override def run(sut: HydrozoaSUT): (Either[String, Unit]) =
            log.info(".run")
            sut.produceBlock(headPeers)

        override def preCondition(state: HydrozoaState): Boolean =
            log.info(".preCondition")
            state.peersNetworkPhase == RunningHead
                && state.headPhase == Some(Open)

    object ShutdownCommand extends UnitCommand:

        private val log = Logger(getClass)

        override def toString: String = "Shutdown command"

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

object HydrozoaOneNodeWithYaci extends Properties("Hydrozoa demo"):
    MBTSuite.useYaci = true
    property("Just works, nothing bad happens") = MBTSuite.property()
//        .useSeed(Seed.fromBase64("QquIyEzeWlhTG6U2J1BLXhOCZxx4eLm9nUDYdlw9LjO=").get)
