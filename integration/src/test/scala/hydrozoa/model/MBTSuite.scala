//noinspection OptionEqualsSome
package hydrozoa.model

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{NoMatch, PSStyleAssoc, Piper, TooManyMatches, decodeBech32AddressL1, decodeBech32AddressL2, onlyOutputToAddress, serializeTxHex, txHash}
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.state.{DepositDatum, DepositTag}
import hydrozoa.l1.multisig.tx.deposit.{BloxBeanDepositTxBuilder, DepositTxBuilder, DepositTxRecipe}
import hydrozoa.l1.multisig.tx.finalization.BloxBeanFinalizationTxBuilder
import hydrozoa.l1.multisig.tx.initialization.{BloxBeanInitializationTxBuilder, InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.refund.{BloxBeanRefundTxBuilder, PostDatedRefundRecipe, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.BloxBeanSettlementTxBuilder
import hydrozoa.l1.multisig.tx.toL1Tx
import hydrozoa.l1.{BackendServiceMock, CardanoL1Mock}
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.block.{BlockEffect, BlockProducer}
import hydrozoa.l2.ledger.*
import hydrozoa.l2.ledger.HydrozoaL2Ledger
import hydrozoa.model.PeersNetworkPhase.{Freed, NewlyCreated, RunningHead, Shutdown}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.{account, mkWallet}
import hydrozoa.node.server.*
import hydrozoa.node.state.HeadPhase.{Finalizing, Open}
import hydrozoa.node.state.{*, given}
import hydrozoa.sut.{HydrozoaFacade, LocalFacade, Utils}
import org.scalacheck.Gen.resize
import org.scalacheck.Prop.{Result, propBoolean}
import org.scalacheck.Test.Parameters
import org.scalacheck.commands.Commands
import org.scalacheck.rng.Seed
import org.scalacheck.util.Pretty
import org.scalacheck.util.ConsoleReporter
import org.scalacheck.{Gen, Prop, Properties, Test}
import scalus.prelude.Option as ScalusOption
import sttp.client4.Response
import sttp.client4.quick.*

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.Set as Opt
import scala.jdk.CollectionConverters.*
import scala.language.strictEquality
import scala.util.{Failure, Success, Try}


object MBTSuite extends Commands:

    var useYaci = false
    private val log = Logger(getClass)

    override type State = HydrozoaState // Immutable simplified head's state
    override type Sut = HydrozoaFacade // Facade to a network of Hydrozoa's peers

    override def canCreateNewSut(
        newState: State,
        initSuts: Traversable[State],
        runningSuts: Traversable[Sut]
    ): Boolean = initSuts.isEmpty && runningSuts.isEmpty

    override def newSut(state: State): Sut =
        log.warn("--------------------> create SUT")
        // Reset Yaci DevKit
        if useYaci then
            println("/// resetting Yaci dev kit")
            val response: Response[String] = quickRequest
                .post(uri"http://localhost:10000/local-cluster/api/admin/devnet/reset")
                .send()
        LocalFacade.apply(state.knownPeers, false, useYaci)

    override def destroySut(sut: Sut): Unit =
        log.warn("<-------------------- destroy SUT")
        sut.shutdownSut()

    override def initialPreCondition(state: State): Boolean =
        state.peersNetworkPhase == NewlyCreated

    override def genInitialState: Gen[State] = for
        numberOfNetworkPeers <- Gen.chooseNum(3, 8)
        networkPeers <- Gen.pick(numberOfNetworkPeers, TestPeer.values)
    yield HydrozoaState(Utils.protocolParams, networkPeers.toSet)

    // This is used to numerate commands we generate
    val cnt = AtomicInteger(0)

    override def genCommand(s: State): Gen[Command0] =
        s.peersNetworkPhase match
            case NewlyCreated =>
                Gen.frequency(
                  10 -> genInitializeCommand(s),
                  1 -> Gen.const(ShutdownCommand(cnt.incrementAndGet()))
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
                  1 -> Gen.const(ShutdownCommand(cnt.incrementAndGet()))
                )
            case Shutdown => NoOp0(cnt.incrementAndGet())

    def genInitializeCommand(s: State): Gen[InitializeCommand] =
        val initiator = s.knownPeers.head
        val account = TestPeer.account(initiator)
        val l1 = CardanoL1Mock(s.knownTxs, s.utxosActive)
        val utxoIds = l1.utxoIdsByAddress(AddressBech[L1](account.toString))

        for
            numberOfHeadPeers <- Gen.chooseNum(0, s.knownPeers.tail.size)
            headPeers <- Gen.pick(numberOfHeadPeers, s.knownPeers.tail)
            seedUtxoId <- Gen.oneOf(utxoIds)
        yield InitializeCommand(cnt.incrementAndGet(), initiator, headPeers.toSet, seedUtxoId)

    def genDepositCommand(s: State): Gen[DepositCommand] =

        for
            depositor <- Gen.oneOf(s.headPeers + s.initiator.get)
            depositorAccount = TestPeer.account(depositor)
            depositorAddressL1 = AddressBech[L1](depositorAccount.toString) // FIXME: extension
            l1 = CardanoL1Mock(s.knownTxs, s.utxosActive)
            (seedUtxoId, coins) <- Gen.oneOf(l1.utxoIdsAdaAtAddress(depositorAddressL1))

            recipient <- Gen.oneOf(s.knownPeers + s.initiator.get)
            recipientAccount = TestPeer.account(recipient)
            recipientAddressL2 = AddressBech[L2](depositorAccount.toString) // FIXME: extension
            depositAmount: BigInt <- Gen.choose(
                BigInt.apply(5_000_000).min(coins),
                BigInt.apply(100_000_000).min(coins)
            )
        yield DepositCommand(
            cnt.incrementAndGet(),
            depositor,
            seedUtxoId,
            depositAmount,
            recipientAddressL2,
            depositorAddressL1
        )

    def genTransactionL2(s: State): Gen[TransactionL2Command] =
        val l2 = HydrozoaL2Ledger.mkLedgerForBlockProducer(s.utxosActiveL2)

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
                .zip(recipients.map(account(_).toString |> AddressBech[L2].apply))
                .map((coins, address) => OutputNoTokens.apply(address, coins))
        yield TransactionL2Command(cnt.incrementAndGet(), L2Transaction(inputs.toList, outputs))

    def genL2Withdrawal(s: State): Gen[WithdrawalL2Command] =
        for
            numberOfInputs <- Gen.choose(1, 3.min(s.utxosActiveL2.size))
            inputs <- Gen.pick(numberOfInputs, s.utxosActiveL2.keySet)
        yield WithdrawalL2Command(cnt.incrementAndGet(), L2Withdrawal(inputs.toList))

    def genCreateBlock(s: State): Gen[ProduceBlockCommand] =
        // TODO: take it up
        for finalize <- Gen.prob(0.01)
    yield ProduceBlockCommand(cnt.incrementAndGet(), finalize)

    /** ------------------------------------------------------------------------------------------
     * StateLikeCommand
     * ------------------------------------------------------------------------------------------
     */

    trait Command0(val id: Int) extends Command:

        private[MBTSuite] def runPC0(sut: Sut): (Try[String], State => Prop) = {
            import Prop.propBoolean
            val r = Try(run(sut))
            (r.map(_.toString), s => preCondition(s) ==> postCondition(s, r))
        }

    case class NoOp0(override val id: Int) extends Command0(id) {
        type Result = Null

        def run(sut: Sut) = null

        def nextState(state: State) = state

        def preCondition(state: State) = true

        def postCondition(state: State, result: Try[Null]) = true
    }

    /** State-like Command that uses Haskell-style `runState` instead of `nextState`.
      */
    trait StateLikeCommand extends Command0:

        final override def nextState(state: State): State = runState(state)._2

        def runState(state: State): (Result, State)

        final override def postCondition(stateBefore: State, result: Try[Result]): Prop =
            val (expectedResult, stateAfter) = runState(stateBefore)
            result match
                case Success(realResult: Result) =>
                    postConditionSuccess(
                      expectedResult,
                      stateBefore,
                      stateAfter,
                      realResult
                    )
                case Failure(e) =>
                    postConditionFailure(expectedResult, stateBefore, stateAfter, e)

        def postConditionSuccess(
            expectedResult: Result,
            stateBefore: State,
            stateAfter: State,
            result: Result
        ): Prop

        def postConditionFailure(
            expectedResult: Result,
            stateBefore: State,
            stateAfter: State,
            err: Throwable
        ): Prop


    /** ------------------------------------------------------------------------------------------
     * InitializeCommand
     * ------------------------------------------------------------------------------------------
     */

    class InitializeCommand(
        override val id: Int,
        initiator: TestPeer,
        otherHeadPeers: Set[TestPeer],
        seedUtxo: UtxoIdL1
    ) extends StateLikeCommand with Command0(id):

        private val log = Logger(getClass)

        override type Result = Either[InitializationError, TxId]

        override def toString: String =
            s"($id) Initialize command {initiator=$initiator, other peers = $otherHeadPeers, seed utxo = $seedUtxo}"

        override def run(sut: Sut): Result =
            sut.initializeHead(
              initiator,
              otherHeadPeers.map(TestPeer.mkWalletId),
              1000,
              seedUtxo.txId,
              seedUtxo.outputIx
            )

        override def runState(state: State): (Result, State) =
            if otherHeadPeers.isEmpty then
                (Left("Solo node mode is not supported yet"), state)
            else

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
                    case Right(ix, _, _, _) => UtxoIdL1(txId, ix)
                    case Left(err) => err match
                            case _: NoMatch => throw RuntimeException("Can't find treasury in the initialization tx!")
                            case _: TooManyMatches => throw RuntimeException("Initialization tx contains more than one multisig outputs!")

                val newState = state.copy(
                  peersNetworkPhase = RunningHead,
                  // Since the local facade initialize routine waits for the `Initialization` phase is over
                  // we can directly go to `Open` phase.
                  headPhase = Some(Open),
                  initiator = Some(initiator),
                  headPeers = otherHeadPeers,
                  headAddressBech32 = Some(headAddress),
                  headMultisigScript = Some(headMultisigScript),
                  treasuryUtxoId = Some(treasuryUtxoId),
                  knownTxs = l1Mock.getKnownTxs,
                  utxosActive = l1Mock.getUtxosActive
                )

                (Right(txId), newState)

        override def preCondition(state: State): Boolean =
            state.peersNetworkPhase match
                case NewlyCreated => true
                case Freed        => true
                case _            =>
                    log.error(s"Negative!")
                    false

        override def postConditionSuccess(
            expectedResult: Result,
            stateBefore: State,
            stateAfter: State,
            sutResult: Result
        ): Prop = s"results should be the same" |: cmpLabel(this.id, sutResult, expectedResult)

        override def postConditionFailure(
            _expectedResult: Result,
            _stateBefore: State,
            _stateAfter: State,
            err: Throwable
        ): Prop = s"Should never crash: $err" |: Prop.exception

    /** ------------------------------------------------------------------------------------------
     * DepositCommand
     * ------------------------------------------------------------------------------------------
     */

    class DepositCommand(
        override val id: Int,
        depositor: TestPeer,
        fundUtxo: UtxoIdL1,
        amount: BigInt,
        address: AddressBech[L2],
        refundAddress: AddressBech[L1]
    ) extends StateLikeCommand with Command0(id):

        private val log = Logger(getClass)

        override type Result = Either[DepositError, DepositResponse]

        override def toString: String =
            s"($id) Deposit command { depositor = $depositor, " +
                s"amount = $amount, fund utxo = $fundUtxo, " +
                s"address = $address, refund address = $refundAddress}"

        override def runState(
            state: HydrozoaState
        ): (Either[DepositError, DepositResponse], HydrozoaState) =
            // Make the datum and the recipe
            val depositDatum = DepositDatum(
                decodeBech32AddressL2(address),
                ScalusOption.None,
                BigInt.apply(0),
                decodeBech32AddressL1(refundAddress),
                ScalusOption.None
            )

            val depositTxRecipe = DepositTxRecipe(fundUtxo, amount, depositDatum)

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

            val refundTxBuilder: RefundTxBuilder = BloxBeanRefundTxBuilder(backendService, nodeStateReader)

            val Right(refundTxDraft) =
                refundTxBuilder.mkPostDatedRefundTxDraft(
                    PostDatedRefundRecipe(depositTxDraft, index)
                )

            val depositUtxoId = UtxoIdL1(depositTxHash, index)
            val depositUtxo: OutputL1 = l1Mock.utxoById(depositUtxoId).get

            val newState = state.copy(
                depositUtxos = TaggedUtxoSet(state.depositUtxos.unTag.utxoMap ++ Map.apply((depositUtxoId, depositUtxo))),
                knownTxs = l1Mock.getKnownTxs,
                utxosActive = l1Mock.getUtxosActive
            )

            val ret = Right(DepositResponse(refundTxDraft, depositUtxoId))
            (ret, newState)

        override def postConditionSuccess(
            expectedResult: Either[DepositError, DepositResponse],
            stateBefore: HydrozoaState,
            stateAfter: HydrozoaState,
            result: Either[DepositError, DepositResponse]
        ): Prop =
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
        ): Prop = s"Should never crash: $err" |: Prop.exception

        override def run(sut: Sut): (Either[DepositError, DepositResponse]) =
            val request = DepositRequest(
                fundUtxo.txId,
                fundUtxo.outputIx,
                amount,
                None,
                address,
                None,
                refundAddress,
                None
            )
            sut.deposit(depositor, request)

        override def preCondition(state: HydrozoaState): Boolean =
            val ret = state.peersNetworkPhase == RunningHead
                && state.headPhase == Some(Open)
                && state.utxosActive.contains(fundUtxo)

            if (!ret)
                log.error(s"Negative!")
            ret

    /** ------------------------------------------------------------------------------------------
     * L2 Transaction Command
     * ------------------------------------------------------------------------------------------
     */

    class TransactionL2Command(override val id: Int, simpleTransaction: L2Transaction) extends Command0(id):

        private val log = Logger(getClass)

        override type Result = Unit

        override def toString: String = s"($id) Transaction L2 command { $simpleTransaction }"

        override def run(sut: Sut): Unit = sut.submitL2(simpleTransaction)

        override def nextState(state: HydrozoaState): HydrozoaState =
            state.copy(
                poolEvents = state.poolEvents ++ Seq(mkTransactionEvent(simpleTransaction))
            )

        override def preCondition(state: HydrozoaState): Boolean =
            val ret = state.peersNetworkPhase == RunningHead
                && state.headPhase == Some(Open)
            if (!ret)
                log.error(s"Negative!")
            ret

        override def postCondition(state: HydrozoaState, result: Try[Unit]): Prop = Prop.passed

    /** ------------------------------------------------------------------------------------------
     * L2 Withdrawal Command
     * ------------------------------------------------------------------------------------------
     */

    class WithdrawalL2Command(override val id: Int, simpleWithdrawal: L2Withdrawal) extends Command0(id):

        private val log = Logger(getClass)

        override type Result = Unit

        override def toString: String = s"($id) Withdrawal L2 command { $simpleWithdrawal }"

        override def run(sut: Sut): Unit = sut.submitL2(simpleWithdrawal)

        override def nextState(state: HydrozoaState): HydrozoaState =
            state.copy(
                poolEvents = state.poolEvents ++ Seq(mkWithdrawalEvent(simpleWithdrawal))
            )

        override def preCondition(state: HydrozoaState): Boolean =
            val ret = state.peersNetworkPhase == RunningHead
                && state.headPhase == Some(Open)
            if (!ret)
                log.error(s"Negative!")
            ret

        override def postCondition(state: HydrozoaState, result: Try[Unit]): Prop = Prop.passed

    /** ------------------------------------------------------------------------------------------
     * Produce Block Command
     * ------------------------------------------------------------------------------------------
     */
    class ProduceBlockCommand(override val id: Int, finalization: Boolean) extends StateLikeCommand with Command0(id):

        private val log = Logger(getClass)

        override type Result = Either[String, (BlockRecord, Option[(TxId, L2Genesis)])]

        override def toString: String = s"($id) Produce block command {finalization = $finalization}"

        override def runState(
            state: HydrozoaState
        ): (Result, HydrozoaState) =

            // Produce block
            val l2 = HydrozoaL2Ledger.mkLedgerForBlockProducer(state.utxosActiveL2)

            // TODO: move to the block producer?
            val sortedPoolEvents = state.poolEvents.sortBy(_.getEventId.hash)

            log.info(s"Model pool events for block production: ${state.poolEvents.map(_.getEventId.hash)}")
            log.info(s"Model pool events for block production (sorted): ${sortedPoolEvents.map(_.getEventId.hash)}")

            val finalizing = state.headPhase.get == Finalizing

            val maybeNewBlock = BlockProducer.createBlock(
                l2,
                if finalizing then Seq.empty else sortedPoolEvents,
                if finalizing then TaggedUtxoSet.apply() else state.depositUtxos,
                state.l2Tip.blockHeader,
                timeCurrent,
                finalizing
            )

            maybeNewBlock match
                case None => Left("Block can't be produced at the moment") /\ state
                case Some(block, utxosActive, utxosAdded, utxosWithdrawn, mbGenesis) =>

                    log.info(s"A new block was produced by model: $block")

                    val l1Mock = CardanoL1Mock(state.knownTxs, state.utxosActive)
                    val backendService = BackendServiceMock(l1Mock, state.pp)
                    val nodeStateReader = NodeStateReaderMock(state)

                    val settlementTxBuilder = BloxBeanSettlementTxBuilder(backendService, nodeStateReader)
                    val finalizationTxBuilder = BloxBeanFinalizationTxBuilder(backendService, nodeStateReader)

                    val l1Effect = BlockEffect.mkL1BlockEffectModel(settlementTxBuilder, finalizationTxBuilder, block, utxosWithdrawn)
                    val l2Effect: L2BlockEffect = block.blockHeader.blockType match
                        case Minor => Some(utxosActive)
                        case Major => Some(utxosActive)
                        case Final => None

                    val record = BlockRecord(block, l1Effect, (), l2Effect)

                    // Calculate new state
                    // Submit L1
                    (l1Effect |> maybeMultisigL1Tx).map(l1Mock.submit)

                    if (block.blockHeader.blockType == Final)
                        val _ = l2.flushAndGetState
                    else
                        l2Effect.foreach(l2.replaceUtxosActive)

                    // TODO: delete block events (both valid and invalid)
                    val blockEvents = block.blockBody.eventsValid.map(_._1) ++ block.blockBody.eventsInvalid.map(_._1)

                    // Possibly new treasury utxo id
                    val treasuryUtxoId =
                        (l1Effect |> maybeMultisigL1Tx).map(tx =>
                            val txId = txHash(tx)
                            val Right(ix, _, _, _ ) = onlyOutputToAddress(tx, state.headAddressBech32.get)
                            Some(UtxoIdL1(txId, ix))
                         ).getOrElse(state.treasuryUtxoId)

                    // Why does it typecheck?
                    //val newDepositUtxos = state.depositUtxos.map.filterNot(block.blockBody.depositsAbsorbed.contains) |> UtxoSet.apply[L1, DepositTag]
                    val newDepositUtxos = state.depositUtxos.unTag.utxoMap
                        .filterNot((k,_) => block.blockBody.depositsAbsorbed.contains(k)) |> TaggedUtxoSet.apply[L1, DepositTag]

                    val newState = state.copy(
                        depositUtxos = newDepositUtxos,
                        poolEvents = state.poolEvents.filterNot(e => blockEvents.contains(e.getEventId)),
                        l2Tip = block,
                        headPhase = if finalization then Some(Finalizing) else state.headPhase,
                        knownTxs = l1Mock.getKnownTxs,
                        treasuryUtxoId = treasuryUtxoId,
                        utxosActive = l1Mock.getUtxosActive,
                        utxosActiveL2 = l2.getUtxosActive |> HydrozoaL2Ledger.unliftUtxoSet
                    )

                    Right(record, mbGenesis) /\ newState

        override def postConditionSuccess(
            expectedResult: Result,
            stateBefore: HydrozoaState,
            stateAfter: HydrozoaState,
            result: Result
        ): Prop =
            (result, expectedResult) match
                case (Right(blockRecord, mbGenesis),
                        Right(expectedBlockRecord, expectedMbGenesis)) =>
                    val header = blockRecord.block.blockHeader
                    val eHeader = expectedBlockRecord.block.blockHeader

                    val body = blockRecord.block.blockBody
                    val eBody = expectedBlockRecord.block.blockBody

                    val l1EffectTxHash = mbTxHash(blockRecord.l1Effect).map(_.hash)
                    val l1EffectHashExpected = mbTxHash(expectedBlockRecord.l1Effect).map(_.hash)

                    val ret =
                        ("Block number should be the same"
                            |: cmpLabel(this.id, header.blockNum, eHeader.blockNum))
                            && ("Block type should be the same"
                            |: cmpLabel(this.id, header.blockType, eHeader.blockType))
                            && ("Major version should be the same"
                            |: cmpLabel(this.id, header.versionMajor, eHeader.versionMajor))
                            && ("Minor version should be the same"
                            |: cmpLabel(this.id, header.versionMinor, eHeader.versionMinor))
                            && ("Valid events should be the same"
                            |: cmpLabel(this.id, body.eventsValid, eBody.eventsValid))
                            && ("Invalid events should be the same"
                            |: cmpLabel(this.id, body.eventsInvalid, eBody.eventsInvalid))
                            && ("Deposit absorbed should be the same"
                            |: cmpLabel(this.id, body.depositsAbsorbed.toSet, eBody.depositsAbsorbed.toSet))
                            && ("Blocks should be the same"
                            |: cmpLabel(this.id, body, eBody))
                            && ("Genesis should be the same"
                            |: cmpLabel(this.id, mbGenesis, expectedMbGenesis))
                            && ("L1 effect tx hashes should be the same"
                            |:
                                // NB: Implication arrow: if the lhs exepression is false, the
                                // whole test will be considered undecidable. So we should NOT
                                // use it. The following didn't work - most tests went to
                                // the junk bin.

                                // mbTxHash(blockRecord.l1Effect).isDefined ==>
                                    cmpLabel(this.id, l1EffectTxHash, l1EffectHashExpected))
                            && (cmpLabel(this.id, blockRecord.l2Effect, expectedBlockRecord.l2Effect)
                            :| "L2 effects should be the same")

                    ret
                case (Left(error), Left(expectedError)) =>
                    s"errors should be the same" |: cmpLabel(this.id, error, expectedError)
                case _ => s"Block create responses are not comparable, got: $result, expected: $expectedResult" |: Prop.falsified

        override def postConditionFailure(
            expectedResult: Result,
            stateBefore: HydrozoaState,
            stateAfter: HydrozoaState,
            err: Throwable
        ): Prop = s"Block production failures should not happen" |: Prop.exception

        override def run(sut: Sut): Result = sut.produceBlock(finalization)

        override def preCondition(state: HydrozoaState): Boolean =
            val ret = state.peersNetworkPhase == RunningHead
                && state.headPhase == Some(Open)
            if (!ret)
                log.error(s"Negative!")
            ret

    /** ------------------------------------------------------------------------------------------
     * Shutdown Command
     * ------------------------------------------------------------------------------------------
     */

    trait UnitCommand0 extends Command0 {
        final type Result = Unit
        def postCondition(state: State, success: Boolean): Prop
        final override def postCondition(state: State, result: Try[Unit]) =
            postCondition(state, result.isSuccess)
    }

    class ShutdownCommand(override val id: Int) extends UnitCommand0 with Command0(id):

        private val log = Logger(getClass)

        override def toString: String = s"($id) Shutdown command"

        override def postCondition(state: HydrozoaState, success: Boolean): Prop =
            val phase = state.peersNetworkPhase
            "Shutdown always possible in NewlyCreated and Freed phases" |:
                phase == NewlyCreated || phase == Freed ==> (success == true)

        override def run(sut: Sut): Unit = sut.shutdownSut()

        override def nextState(state: HydrozoaState): HydrozoaState =
            state.copy(
              peersNetworkPhase = Shutdown,
              knownPeers = Set.empty,
              knownTxs = Map.empty,
              utxosActive = Map.empty
            )

        override def preCondition(state: HydrozoaState): Boolean =
            state.peersNetworkPhase match
            case NewlyCreated => true
            case Freed        => true
            case _            =>
                log.error(s"Negative!")
                false

    /** ------------------------------------------------------------------------------------------
     * Copy-paste from Commands
     * ------------------------------------------------------------------------------------------
     */

    final def property0(): Prop = {
        val suts = collection.mutable.Map.empty[AnyRef, (State, Option[Sut])]

        Prop.forAll(actions()) { as =>
            println("----------------------------------------")
            as.seqCmds.foreach(println)
            println("----------------------------------------")

            try {
                val sutId = suts.synchronized {
                    val initSuts = suts.values.collect { case (state, None) => state }
                    val runningSuts = suts.values.collect { case (_, Some(sut)) => sut }
                    if (canCreateNewSut(as.s, initSuts, runningSuts)) {
                        val sutId = new AnyRef
                        suts += (sutId -> (as.s -> None))
                        Some(sutId)
                    } else None
                }
                sutId match {
                    case Some(id) =>
                        val sut = newSut(as.s)

                        def removeSut(): Unit = {
                            suts.synchronized {
                                suts -= id
                                destroySut(sut)
                            }
                        }

                        val doRun = suts.synchronized {
                            if (suts.contains(id)) {
                                suts += (id -> (as.s -> Some(sut)))
                                true
                            } else false
                        }
                        if (doRun) {
                            runActions(sut, as, removeSut())
                        }
                        else {
                            print("you should not see that")
                            removeSut()
                            Prop.undecided
                        }

                    case None => // NOT IMPLEMENTED Block until canCreateNewSut is true
                        println("NOT IMPL")
                        Prop.undecided
                }
            } catch {
                case e: Throwable =>
                    suts.synchronized {
                        suts.clear()
                    }
                    throw e
            }
        }
    }

    private type Commands0 = List[Command0]

    private case class Actions(
                                  s: State,
                                  seqCmds: Commands0
                              )

    private def actions(): Gen[Actions] = {
        import Gen.{const, sized}

        def sizedCmds(s: State)(sz: Int): Gen[(State, Commands0)] = {
            val l: List[Unit] = List.fill(sz)(())
            l.foldLeft(const((s, Nil: Commands0))) { case (g, ()) =>
                for {
                    (s0, cs) <- g
                    c <- genCommand(s0).suchThat(_.preCondition(s0))
                } yield (c.nextState(s0), cs :+ c)
            }
        }

        def cmdsPrecond(s: State, cmds: Commands0): (State, Boolean) = cmds match {
            case Nil => (s, true)
            case c :: cs if c.preCondition(s) => cmdsPrecond(c.nextState(s), cs)
            case _ => (s, false)
        }

        def actionsPrecond(as: Actions): Boolean =
            initialPreCondition(as.s) && (cmdsPrecond(as.s, as.seqCmds) match {
                case (s, true) => true
                case _ => false
            })

        val g = for {
            s0 <- genInitialState
            (s1, seqCmds) <- resize(100,sized(sizedCmds(s0)))
        } yield Actions(s0, seqCmds)

        g.suchThat(actionsPrecond)
    }

    private def runActions(sut: Sut, as: Actions, finalize: => Unit): Prop = {
        val maxLength = as.seqCmds.length
        val (p1, s, rs1, lastCmd) = runSeqCmds(sut, as.s, as.seqCmds)
        val l1 = s"Initial State:\n  ${as.s}\n" +
            s"Sequential Commands:\n${prettyCmdsRes(as.seqCmds zip rs1, maxLength)}\n" +
            s"Last executed command: $lastCmd"
        p1 :| l1
    }

    // Added the last succeded command (last `Int`)
    private def runSeqCmds(sut: Sut, s0: State, cs: Commands0): (Prop, State, List[Try[String]], Int) =
        cs.foldLeft((Prop.proved, s0, List[Try[String]](), 0)) { case ((p, s, rs, lastCmd), c) =>
            val (r, pf) = c.runPC0(sut)
            (p && pf(s), c.nextState(s), rs :+ r, lastCmd + 1)
        }

    private def prettyCmdsRes(rs: List[(Command0, Try[String])], maxLength: Int) = {
        val maxNumberWidth = "%d".format(maxLength).length
        val lineLayout = "  %%%dd. %%s".format(maxNumberWidth)
        val cs = rs.zipWithIndex.map {
            case (r, i) => lineLayout.format(
                i + 1,
                r match {
                    case (c, Success("()")) => c.toString
                    case (c, Success(r)) => s"$c => $r"
                    case (c, r) => s"$c => $r"
                })
        }
        if (cs.isEmpty)
            "  <no commands>"
        else
            cs.mkString("\n")
    }


/** ------------------------------------------------------------------------------------------
 * Runners
 * ------------------------------------------------------------------------------------------
 */

class Properties0(override val name: String) extends Properties(name):
    override def main(args: Array[String]): Unit =
        CmdLineParser.parseParams(args) match {
            case (applyCmdParams, Nil) =>
                val params = applyCmdParams(overrideParameters(Test.Parameters.default))
                val res = Test.checkProperties(params, this)
                val numFailed = res.count(!_._2.passed)
                if (numFailed > 0) {
                    Console.out.println(s"Found $numFailed failing properties.")
                    System.exit(1)
                } else {
                    System.exit(0)
                }
            case (_, os) =>
                Console.out.println("Incorrect options:\n  " + os.mkString(", "))
                CmdLineParser.printHelp()
                System.exit(-1)
        }

trait CmdLineParser {

    trait Opt[+T] {
        val default: T
        val names: Set[String]
        val help: String
    }

    trait Flag extends Opt[Unit]

    trait IntOpt extends Opt[Int]

    trait FloatOpt extends Opt[Float]

    trait StrOpt extends Opt[String]

    trait OpStrOpt extends Opt[Option[String]]

    abstract class OpStrOptCompat extends OpStrOpt {
        val default: Option[String] = None
    }

    class OptMap(private val opts: Map[Opt[?], Any] = Map.empty) {
        def apply(flag: Flag): Boolean = opts.contains(flag)

        def apply[T](opt: Opt[T]): T = opts.get(opt) match {
            case None => opt.default
            case Some(v) => v.asInstanceOf[T]
        }

        def set[T](o: (Opt[T], T)) = new OptMap(opts + o)
    }

    val opts: Set[Opt[?]]

    private def getOpt(s: String) = {
        if (s == null || s.length == 0 || s.charAt(0) != '-') None
        else opts.find(_.names.contains(s.drop(1)))
    }

    private def getStr(s: String) = Some(s)

    private def getInt(s: String) =
        if (s != null && s.length > 0 && s.forall(_.isDigit)) Some(s.toInt)
        else None

    private def getFloat(s: String) =
        if (s != null && s.matches("[0987654321]+\\.?[0987654321]*")) Some(s.toFloat)
        else None

    def printHelp(): Unit = {
        Console.out.println("Available options:")
        opts.foreach { opt =>
            Console.out.println("  " + opt.names.map("-" + _).mkString(", ") + ": " + opt.help)
        }
    }

    /** Parses a command line and returns a tuple of the parsed options, and any unrecognized strings
     */
    def parseArgs[T](args: Array[String]): (OptMap, List[String]) = {

        def parse(
                     as: List[String],
                     om: OptMap,
                     us: List[String]
                 ): (OptMap, List[String]) =
            as match {
                case Nil => (om, us)
                case a :: Nil =>
                    getOpt(a) match {
                        case Some(o: Flag) =>
                            parse(Nil, om.set((o, ())), us)
                        case _ =>
                            (om, us :+ a)
                    }
                case a1 :: a2 :: as => getOpt(a1) match {
                    case Some(o: Flag) =>
                        parse(a2 :: as, om.set((o, ())), us)
                    case otherwise =>
                        (otherwise match {
                            case Some(o: IntOpt) => getInt(a2).map(v => parse(as, om.set(o -> v), us))
                            case Some(o: FloatOpt) => getFloat(a2).map(v => parse(as, om.set(o -> v), us))
                            case Some(o: StrOpt) => getStr(a2).map(v => parse(as, om.set(o -> v), us))
                            case Some(o: OpStrOpt) => getStr(a2).map(v => parse(as, om.set(o -> Option(v)), us))
                            case _ => None
                        }).getOrElse(parse(a2 :: as, om, us :+ a1))
                }
            }

        parse(args.toList, new OptMap(), Nil)
    }
}


object CmdLineParser extends CmdLineParser{
    object OptMinSuccess extends IntOpt {
        val default = Parameters.default.minSuccessfulTests
        val names: Set[String] = Set("minSuccessfulTests", "s")
        val help = "Number of tests that must succeed in order to pass a property"
    }

    object OptMaxDiscardRatio extends FloatOpt {
        val default = Parameters.default.maxDiscardRatio
        val names: Set[String] = Set("maxDiscardRatio", "r")
        val help =
            "The maximum ratio between discarded and succeeded tests " +
                "allowed before ScalaCheck stops testing a property. At " +
                "least minSuccessfulTests will always be tested, though."
    }

    object OptMinSize extends IntOpt {
        val default = Parameters.default.minSize
        val names: Set[String] = Set("minSize", "n")
        val help = "Minimum data generation size"
    }

    object OptMaxSize extends IntOpt {
        val default = Parameters.default.maxSize
        val names: Set[String] = Set("maxSize", "x")
        val help = "Maximum data generation size"
    }

    object OptWorkers extends IntOpt {
        val default = Parameters.default.workers
        val names: Set[String] = Set("workers", "w")
        val help = "Number of threads to execute in parallel for testing"
    }

    object OptVerbosity extends IntOpt {
        val default = 1
        val names: Set[String] = Set("verbosity", "v")
        val help = "Verbosity level"
    }

    object OptPropFilter extends OpStrOptCompat {
        override val default = Parameters.default.propFilter
        val names: Set[String] = Set("propFilter", "f")
        val help = "Regular expression to filter properties on"
    }

    object OptInitialSeed extends OpStrOptCompat {
        override val default: None.type = None
        val names: Set[String] = Set("initialSeed")
        val help = "Use Base-64 seed for all properties"
    }

    object OptDisableLegacyShrinking extends Flag {
        val default = ()
        val names: Set[String] = Set("disableLegacyShrinking")
        val help = "Disable legacy shrinking using Shrink instances"
    }

    object OptMaxRNGSpins extends IntOpt {
        val default = 1
        val names: Set[String] = Set("maxRNGSpins")
        val help = "Maximum number of RNG spins to perform between checks"
    }

    val opts: Set[Opt[?]] = Set[Opt[?]](
        OptMinSuccess,
        OptMaxDiscardRatio,
        OptMinSize,
        OptMaxSize,
        OptWorkers,
        OptVerbosity,
        OptPropFilter,
        OptInitialSeed,
        OptDisableLegacyShrinking,
        OptMaxRNGSpins
    )

    def parseParams(args: Array[String]): (Parameters => Parameters, List[String]) = {
        val (optMap, us) = parseArgs(args)
        val minSuccess0: Int = optMap(OptMinSuccess)
        val minSize0: Int = optMap(OptMinSize)
        val maxSize0: Int = optMap(OptMaxSize)
        val workers0: Int = optMap(OptWorkers)
        val verbosity0 = optMap(OptVerbosity)
        val discardRatio0: Float = optMap(OptMaxDiscardRatio)
        val propFilter0: Option[String] = optMap(OptPropFilter)
        val initialSeed0: Option[Seed] =
            optMap(OptInitialSeed).flatMap { str =>
                Seed.fromBase64(str) match {
                    case Success(seed) =>
                        Some(seed)
                    case Failure(_) =>
                        println(s"WARNING: ignoring invalid Base-64 seed ($str)")
                        None
                }
            }

        val useLegacyShrinking0: Boolean = !optMap(OptDisableLegacyShrinking)
        val maxRNGSpins: Int = optMap(OptMaxRNGSpins)
        val params = { (p: Parameters) =>
            p.withMinSuccessfulTests(minSuccess0)
                .withMinSize(minSize0)
                .withMaxSize(maxSize0)
                .withWorkers(workers0)
                .withTestCallback(ConsoleReporter(verbosity0, 100000))
                .withMaxDiscardRatio(discardRatio0)
                .withPropFilter(propFilter0)
                .withInitialSeed(initialSeed0)
                .withLegacyShrinking(useLegacyShrinking0)
                .withMaxRNGSpins(maxRNGSpins)
        }
        (params, us)
    }
}

def cmpLabel[A](id: Int, actual: A, expected: A)(using CanEqual[A, A]): Prop =
    val r =
        if actual == expected
        then Result.apply(Prop.True)
        else Result.apply(Prop.False)
            .label(s"Expected: $expected")
            .label(s"Got: $actual")
            .label(s"Failed command's ID: $id")
    Prop.apply(r)

object HydrozoaOneNodeWithL1Mock extends Properties0("Hydrozoa One node mode with L1 mock"):
    property("Just works, nothing bad happens") = MBTSuite.property0()
        .useSeed(Seed.fromBase64("bGr0AUfvwXSFZWTPVyXA9S5fHWn-NsCe3silG6HWbPD=").get)


object HydrozoaOneNodeWithYaci extends Properties0("Hydrozoa One node mode with Yaci"):
    MBTSuite.useYaci = true
    // It doesn't work in fact, since Yaci hangs up once in a while
    property("Just works, nothing bad happens") = MBTSuite.property0()
//        .useSeed(Seed.fromBase64("QquIyEzeWlhTG6U2J1BLXhOCZxx4eLm9nUDYdlw9LjO=").get)
