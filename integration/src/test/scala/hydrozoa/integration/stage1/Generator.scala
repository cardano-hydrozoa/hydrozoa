package hydrozoa.integration.stage1

import hydrozoa.integration.stage1.model.Deposits.DepositStatus
import cats.data.NonEmptyList
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.config.head.initialization.CappedValueGen.{ensureMinAdaLenient, generateCappedValue}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.integration.stage1.CommandGenerators.L2txGen
import hydrozoa.integration.stage1.CommandGenerators.TxMutator.Identity
import hydrozoa.integration.stage1.CommandGenerators.TxStrategy.{Dust, RandomWithdrawals, Regular}
import hydrozoa.integration.stage1.Commands.*
import hydrozoa.integration.stage1.Model.BlockCycle.HeadFinalized
import hydrozoa.integration.stage1.Model.given
import hydrozoa.integration.stage1.SutCommands.given
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.lib.cardano.scalus.given_Choose_QuantizedInstant
import hydrozoa.lib.cardano.scalus.ledger.{asUtxoList, withZeroFees}
import hydrozoa.lib.cardano.scalus.txbuilder.DiffHandler.prebalancedLovelaceDiffHandler
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{UserRequest, UserRequestHeader, UserRequestWithId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.tx.GenesisObligation
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import io.bullet.borer.Cbor
import org.scalacheck.Gen
import org.scalacheck.commands.{AnyCommand, ScenarioGen, noOp}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AuxiliaryData, Coin, Metadatum, SlotConfig, TransactionInput, TransactionOutput, Utxo, Utxos, Value, Word64}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, ModifyAuxiliaryData, Send, Spend}
import scalus.cardano.txbuilder.{PubKeyWitness, TransactionBuilder}
import scalus.uplc.builtin.ByteString

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.math.Ordering.Implicits.infixOrderingOps

// ===================================
// Per-command generators
// ===================================

/** Per-command generators. These produce concrete command values; the caller wraps each into
  * [[AnyCommand]] where the [[org.scalacheck.commands.SutCommand]] implicit is in scope.
  */
object CommandGenerators:

    private val logger: org.slf4j.Logger = Logging.logger("Stage1.CommandGenerators")

    // ===================================
    // Delay
    // ===================================

    def genStayOnHappyPathDelay(
        currentTime: QuantizedInstant,
        settlementExpirationTime: QuantizedInstant
    ): Gen[DelayCommand] = {
        Gen
            // TODO: parameter?
            //    .choose(currentTime, settlementExpirationTime - 20.seconds)
            .choose(
              currentTime,
              (currentTime + 10.seconds).min(settlementExpirationTime - 20.seconds)
            )
            .flatMap(d => DelayCommand(Delay.EndsBeforeHappyPathExpires(d - currentTime)))
    }

    def genRandomDelay(
        currentTime: QuantizedInstant,
        settlementExpirationTime: QuantizedInstant,
        competingFallbackStartTime: QuantizedInstant,
        slotConfig: SlotConfig,
        blockNumber: BlockNumber
    ): Gen[DelayCommand] =

        val genStayOnHappyPathDelay1 =
            genStayOnHappyPathDelay(currentTime, settlementExpirationTime)

        for {
            delay <-
                // TODO: make a parameter?
                if blockNumber.convert < 20
                then genStayOnHappyPathDelay1
                else
                    Gen
                        .frequency(
                          50 -> genStayOnHappyPathDelay1,
                          1 -> Gen
                              .choose(settlementExpirationTime, competingFallbackStartTime)
                              .flatMap(d =>
                                  DelayCommand(Delay.EndsInTheSilencePeriod(d - currentTime))
                              ),
                          1 ->
                              Gen
                                  .choose(
                                    competingFallbackStartTime,
                                    competingFallbackStartTime + QuantizedFiniteDuration(
                                      slotConfig,
                                      (competingFallbackStartTime - currentTime).finiteDuration / 10
                                    )
                                  )
                                  .flatMap(d =>
                                      DelayCommand(Delay.EndsAfterHappyPathExpires(d - currentTime))
                                  ),
                        )
        } yield delay

    // ===================================
    // Start block
    // ===================================

    def genStartBlock(
        prevBlockNumber: BlockNumber,
        currentTime: QuantizedInstant
    ): Gen[StartBlockCommand] =
        Gen.const(
          StartBlockCommand(
            prevBlockNumber.increment,
            BlockCreationStartTime(currentTime)
          )
        )

    // ===================================
    // Complete block
    // ===================================

    def genBlockDuration(currentTime: QuantizedInstant): Gen[BlockCreationEndTime] =
        Gen
            .choose(10, 60_000) // 10ms - 1m, should be shorter than deposit's validity end
            .map(blockDurationMs =>
                BlockCreationEndTime(
                  currentTime + FiniteDuration(blockDurationMs, TimeUnit.MILLISECONDS)
                )
            )

    def genCompleteBlockRegular(
        blockNumber: BlockNumber,
        currentTime: QuantizedInstant
    ): Gen[CompleteBlockCommand] = for {
        blockCreationEndTime <- genBlockDuration(currentTime)
    } yield CompleteBlockCommand(blockNumber, blockCreationEndTime, false)
    
    def genCompleteBlockFinal(
        blockNumber: BlockNumber,
        currentTime: QuantizedInstant
    ): Gen[CompleteBlockCommand] = for {
        blockCreationEndTime <- genBlockDuration(currentTime)
    } yield CompleteBlockCommand(blockNumber, blockCreationEndTime, true)

    def genCompleteBlock(
        blockNumber: BlockNumber,
        currentTime: QuantizedInstant
    ): Gen[CompleteBlockCommand] =

        for {
            ret <-
                if blockNumber.convert < 20
                then genCompleteBlockRegular(blockNumber, currentTime)
                else
                    Gen.frequency(
                      1 -> genCompleteBlockFinal(blockNumber, currentTime),
                      20 -> genCompleteBlockRegular(blockNumber, currentTime)
                    )
        } yield ret

    // ===================================
    // L2 tx command
    // ===================================

    enum TxStrategy:
        /** Completely arbitrary transaction, always invalid (unless you are very lucky). */
        case Arbitrary

        /** Just valid L2 txs, no withdrawals. */
        case Regular

        /** Valid L2 txs that withdraw some arbitrary outputs. */
        case RandomWithdrawals

        /** Selects the biggest utxo available and split it into small chunks, but no more than
          * [[maxOutputs]]
          * @param maxOutputs
          *   Number of small outputs (there likely will be the additional bigger one with the rest)
          */
        case Dust(maxOutputs: Int = 50)

    // TODO: implement, now always Identity which is noop
    enum TxMutator:
        case Identity
        case DropWitnesses

    def genInputs(
        utxos: Utxos,
        txStrategy: TxStrategy,
    ): Gen[Seq[TransactionInput]] = txStrategy match {

        case TxStrategy.Dust(_) =>
            Gen.const(List(utxos.maxBy((_, o) => o.value.coin.value)._1))

        case _ =>
            for {
                numberOfInputs <- Gen.choose(1, 10.min(utxos.size))
                inputs <- Gen.pick(numberOfInputs, utxos.keySet)
            } yield inputs.toSeq

    }

    def genOutputValues(
        capValue: Value,
        txStrategy: TxStrategy,
        step: (Value, Option[Long], Option[Long], Option[Long]) => Gen[Value]
    ): Gen[List[Value]] = for {
        values <- txStrategy match {

            case TxStrategy.Dust(maxOutputs) =>
                Gen.tailRecM((List.empty[Value], capValue, maxOutputs))((acc, rest, stepsLeft) =>
                    for {
                        next <- step(rest, None, Some(3_000_000L), Some(1L))
                        acc_ = acc :+ next
                    } yield {
                        if stepsLeft == 1 || next == rest
                        then
                            if next == rest
                            then Right(acc_)
                            else Right(acc_ :+ (rest - next))
                        else Left(acc_, rest - next, stepsLeft - 1)
                    }
                )

            case _ =>
                Gen.tailRecM(List.empty[Value] -> capValue)((acc, rest) =>
                    for {
                        // TODO: rest here can happen to be too small, fix that
                        next <- step(rest, None, None, None)
                        acc_ = acc :+ next
                    } yield
                        if next == rest
                        then Right(acc_)
                        else Left(acc_ -> (rest - next))
                )
        }
    } yield values

    type L2txGen = (state: Model.State) => Gen[L2TxCommand]

    def genAuxiliaryData(
        outputs: List[TransactionOutput],
        txStrategy: TxStrategy
    ): Gen[AuxiliaryData] = for {
        flags <- txStrategy match {
            case TxStrategy.RandomWithdrawals => Gen.listOfN(outputs.size, Gen.choose(1, 2))
            case _                            => Gen.const(outputs.map(_ => 2))
        }
    } yield Metadata(
      Map(
        Word64(CIP67.Tags.head)
            -> Metadatum.List(flags.map(Metadatum.Int(_)).toIndexedSeq)
      )
    )

    def genValidNonPlutusL2Tx(
        txStrategy: TxStrategy,
        txMutator: TxMutator
    )(state: Model.State): Gen[L2TxCommand] =

        val config = state.multiNodeConfig
        val cardanoNetwork: CardanoNetwork = config.headConfig.cardanoNetwork
        val generateCappedValueC = generateCappedValue(cardanoNetwork)
        val l2AddressesInUse = state.utxosL2Active.map(_._2.address).toSet

        val ownedUtxos = state.utxosL2Active
            .filter((_, o) =>
                o.address.asInstanceOf[ShelleyAddress] == config.addressOf(HeadPeerNumber.zero)
            )

        for {
            // Inputs
            inputs <- genInputs(ownedUtxos, txStrategy)
            totalValue = Value.combine(inputs.map(ownedUtxos(_).value))
            _ = logger.trace(s"totalValue: $totalValue")

            // Outputs
            outputValues <- genOutputValues(totalValue, txStrategy, generateCappedValueC)
            _ = logger.trace(s"output values: $outputValues")
            outputs <- Gen.sequence[List[TransactionOutput], TransactionOutput](
              outputValues
                  .map(v => Gen.oneOf(l2AddressesInUse).map(a => Babbage(a, v)))
            )

            auxiliaryData <- genAuxiliaryData(outputs, txStrategy).map(Some.apply)

            txUnsigned = TransactionBuilder
                .build(
                  cardanoNetwork.cardanoInfo.network,
                  (inputs.map(utxoId =>
                      Spend(utxo = Utxo(utxoId, ownedUtxos(utxoId)), witness = PubKeyWitness)
                  )
                      ++ outputs.map(Send.apply)
                      :+ Fee(Coin.zero)).toList
                      :+ ModifyAuxiliaryData(_ => auxiliaryData)
                )
                .flatMap(
                  _.finalizeContext(
                    protocolParams = config.headConfig.cardanoProtocolParams.withZeroFees,
                    diffHandler = prebalancedLovelaceDiffHandler,
                    evaluator = config.headConfig.plutusScriptEvaluatorForTxBuild,
                    validators = Seq.empty
                  )
                )
                .fold(
                  err => throw RuntimeException(s"Can't build l2 tx: $err"),
                  ctx => ctx.transaction
                )

            txSigned = config.multisignTx(txUnsigned)

            _ = logger.trace(s"signed l2Tx: ${HexUtil.encodeHexString(txSigned.toCbor)}")

            body = TransactionRequestBody(
              l2Payload = ByteString.fromArray(txSigned.toCbor)
            )

            header = UserRequestHeader(
              headId = config.headConfig.headId,
              validityStart = RequestValidityStartTime(
                (state.currentTime.instant - 5.seconds)
              ),
              validityEnd = RequestValidityEndTime(
                (state.currentTime.instant + 2.minutes)
              ),
              bodyHash = body.hash
            )

            // Get verification key for peer 0, though it's not needed for EUTXO ledger
            userVk = config.nodeConfigs(HeadPeerNumber.zero).ownHeadWallet.exportVerificationKey

        } yield L2TxCommand(
          request = UserRequestWithId.TransactionRequest(
            requestId = state.nextRequestId,
            request = UserRequest.TransactionRequest(
              header = header,
              body = body.asInstanceOf[TransactionRequestBody],
              userVk = userVk
            )
          ),
          txStrategy = txStrategy,
          txMutator = txMutator
        )

    // ===================================
    // Register deposit command
    // ===================================

    /** May fail if there is no enough funds in peer's utxos.
      */
    def genRegisterDepositCommand(state: Model.State): Gen[Option[RegisterDepositCommand]] = {
        import state.multiNodeConfig

        val peerAddress = multiNodeConfig.addressOf(HeadPeerNumber.zero)

        // TODO: remove?
        // TODO: This gives the enterprise address without the stake, which is not compatible with the model
        // val peerAddress = state.ownTestPeer.address(state.headConfig.network)
        // peerAddress
        // <- Gen.oneOf(state.peerUtxosL1.map(_._2.address)).map(_.asInstanceOf[ShelleyAddress])

        val cardanoNetwork = multiNodeConfig.headConfig.cardanoNetwork
        val generateCappedValueC = generateCappedValue(cardanoNetwork)
        val ensureMinAdaLenientC = ensureMinAdaLenient(cardanoNetwork)

        val l1UtxoAvailable = state.peerUtxosL1 -- state.utxoLocked
        if l1UtxoAvailable.isEmpty
        then Gen.const(None)
        else
            for {
                fundingUtxos <- Gen.atLeastOne(l1UtxoAvailable).map(_.toMap)
                totalValue = Value.combine(fundingUtxos.map(_._2.value))

                _ = logger.trace(s"fundingUtxos: $fundingUtxos")
                _ = logger.trace(s"totalValue: $totalValue")

                // Change should be big enough to make balancing of the DEPOSIT tx always possible
                minimalChangeCoins = 1_500_000L
                // Deposit value should be big enough to make balancing of the REFUND tx always possible
                minimalDepositValueCoins = 1_500_000L
                // Total
                minimalTotalValueCoins = minimalChangeCoins + minimalDepositValueCoins

                ret <-
                    if ensureMinAdaLenientC(
                          totalValue
                        ) != totalValue || minimalTotalValueCoins > totalValue.coin.value
                    then Gen.const(None)
                    else {
                        // Reserved to cover refund tx fee
                        val reserved = Value.lovelace(minimalDepositValueCoins)
                        val totalValueAvailable = totalValue - reserved
                        for {
                            change <- generateCappedValueC(
                              totalValueAvailable,
                              Some(minimalChangeCoins),
                              None,
                              None
                            )
                            _ = logger.trace(s"change: $change")

                            depositValue = totalValue - change
                            _ = logger.trace(s"depositValue: $depositValue")

                            ret <-
                                if ensureMinAdaLenientC(depositValue) != depositValue
                                then Gen.const(None)
                                else
                                    for {

                                        outputValues <- genOutputValues(
                                          depositValue,
                                          TxStrategy.Regular,
                                          generateCappedValueC
                                        )

                                        _ = logger.trace(s"outputValues: $outputValues")

                                        outputs <- Gen
                                            .sequence[List[TransactionOutput], TransactionOutput](
                                              outputValues
                                                  .map(v =>
                                                      Gen.const(peerAddress).map(a => Babbage(a, v))
                                                  )
                                            )

                                        l2Outputs = NonEmptyList.fromListUnsafe(
                                          outputs.map(
                                            GenesisObligation
                                                .fromTransactionOutput(_)
                                                .fold(err => throw RuntimeException(err), identity)
                                          )
                                        )

                                        l2Value = Value.combine(
                                          l2Outputs.toList.map(_.l2OutputValue)
                                        )
                                        _ = logger.trace(s"l2Value: $l2Value")

                                        requestId = state.nextRequestId

                                        // This should be bigger than the longest possible block duration, see [[genCompleteBlock]].
                                        requestValidityEndTime = RequestValidityEndTime(
                                          RequestValidityEndTime(
                                            (state.currentTime.instant + 2.minutes)
                                          )
                                        )

                                        depositRefundSeq = DepositRefundTxSeq
                                            .Build(
                                              l2Payload = GenesisObligation.serialize(l2Outputs),
                                              depositFee = Coin.zero,
                                              utxosFunding = NonEmptyList
                                                  .fromListUnsafe(fundingUtxos.asUtxoList),
                                              changeAddress = peerAddress,
                                              l2Value = l2Value,
                                              refundAddress = peerAddress,
                                              refundDatum = None,
                                              requestValidityEndTime = requestValidityEndTime,
                                              requestId = requestId
                                            )(using multiNodeConfig.headConfig)
                                            .result
                                            .fold(
                                              err => throw RuntimeException(err.toString),
                                              identity
                                            )

                                        depositTxSigned = multiNodeConfig
                                            .signTxAs(HeadPeerNumber.zero)(
                                              depositRefundSeq.depositTx.tx
                                            )

                                        _ = logger.trace(
                                          s"deposit tx signed: ${HexUtil.encodeHexString(depositTxSigned.toCbor)}"
                                        )

                                        body = DepositRequestBody(
                                          l1Payload = ByteString
                                              .fromArray(depositRefundSeq.depositTx.tx.toCbor),
                                          l2Payload = GenesisObligation.serialize(l2Outputs)
                                        )

                                        header = UserRequestHeader(
                                          headId = multiNodeConfig.headConfig.headId,
                                          validityStart = RequestValidityStartTime(
                                            (state.currentTime.instant - 5.seconds)
                                          ),
                                          validityEnd =
                                            requestValidityEndTime,
                                          bodyHash = body.hash
                                        )

                                        // Get verification key for peer 0
                                        userVk = multiNodeConfig
                                            .nodeConfigs(HeadPeerNumber.zero)
                                            .ownHeadWallet
                                            .exportVerificationKey

                                    } yield Some(
                                      RegisterDepositCommand(
                                        request = UserRequestWithId.DepositRequest(
                                          requestId = requestId,
                                          request = UserRequest.DepositRequest(
                                            header = header,
                                            body = body.asInstanceOf[DepositRequestBody],
                                            userVk = userVk
                                          )
                                        ),
                                        depositRefundTxSeq = depositRefundSeq,
                                        depositTxBytesSigned = depositTxSigned
                                      )
                                    )
                        } yield ret
                    }
            } yield ret
    }

    // ===================================
    // Submit Deposits Command
    // ===================================

    // TODO: we have to decide here whether we want to try to submit a deposit
    //   because when executing we don't have access to the time

    def genSubmitDepositsCommand(
        state: Model.State
    ): Gen[SubmitDepositsCommand] = {
        val registeredDeposits = state.deposits.depositsRegistered
        
        // Prefix is easier to think about, though we can pick up arbitrary elements
        Gen.choose(1, registeredDeposits.size).map { n =>
            logger.trace(
              s"genSubmitDepositsCommand registered deposits: $registeredDeposits, n=$n"
            )
            val selected = registeredDeposits.take(n)

            val partition = selected.partition { registered =>

                val submissionDeadline = registered.cmd.request.request.header.validityEnd
                val submissionRunway = state.currentTime.instant + 20.seconds

                logger.trace(s"genSubmitDepositsCommand: submissionDeadline=$submissionDeadline")
                logger.trace(s"genSubmitDepositsCommand: submissionRunway=$submissionRunway")
                submissionDeadline.convert > submissionRunway
            }
            SubmitDepositsCommand(
              depositsForSubmission = partition._1,
              depositsToDecline = partition._2
            )
        }
    }

end CommandGenerators

// ===================================
// Suite scenario generators
// ===================================

object ScenarioGenerators:

    private val logger: org.slf4j.Logger = Logging.logger("Stage1.ScenarioGenerators")

    /** Produces L2 transactions (valid and non-valid) with no withdrawals.
      *
      * There is a customizable delay before starting every new block. If the delay happens to be
      * long enough so the latest fallback becomes active, all next commands are NoOp and the
      * fallback is expected to be submitted. Otherwise, only happy path effects are expected to be
      * submitted.
      */
    object NoWithdrawalsScenarioGen
        extends SimpleScenarioGen(
          CommandGenerators.genValidNonPlutusL2Tx(
            txStrategy = Regular,
            txMutator = Identity
          )
        )

    object OngoingWithdrawalsScenarioGen
        extends SimpleScenarioGen(
          CommandGenerators.genValidNonPlutusL2Tx(
            txStrategy = RandomWithdrawals,
            txMutator = Identity
          )
        )

    case class SimpleScenarioGen(generateL2Tx: L2txGen) extends ScenarioGen[Model.State, Stage1Sut]:

        override def genNextCommand(
            state: Model.State
        ): Gen[AnyCommand[Model.State, Stage1Sut]] = {
            import hydrozoa.integration.stage1.Model.BlockCycle.*
    import hydrozoa.integration.stage1.Model.CurrentTime.BeforeHappyPathExpiration

            state.currentTime match {
                case BeforeHappyPathExpiration(_) =>
                    state.blockCycle match {
                        case Done(blockNumber, _) =>
                            val settlementExpirationTime =
                                state.multiNodeConfig.headConfig.txTiming.newSettlementEndTime(
                                  state.competingFallbackStartTime
                                )
                            CommandGenerators
                                .genRandomDelay(
                                  currentTime = state.currentTime.instant,
                                  settlementExpirationTime = settlementExpirationTime,
                                  competingFallbackStartTime = state.competingFallbackStartTime,
                                  slotConfig = state.multiNodeConfig.headConfig.slotConfig,
                                  blockNumber = blockNumber
                                )
                                .map(AnyCommand.apply)

                        case Ready(blockNumber, _) =>
                            CommandGenerators
                                .genStartBlock(blockNumber, state.currentTime.instant)
                                .map(AnyCommand.apply)

                        case InProgress(blockNumber, _, _, _) =>
                            Gen.frequency(
                              1 -> CommandGenerators
                                  .genCompleteBlock(blockNumber, state.currentTime.instant)
                                  .map(AnyCommand.apply),
                              10 -> (if state.utxosL2Active.isEmpty
                                     then Gen.const(noOp)
                                     else generateL2Tx(state).map(AnyCommand.apply))
                            )

                        case HeadFinalized => Gen.const(noOp)
                    }
                case _ => Gen.const(noOp)
            }
        }

    case class MakeDustScenarioGen(minL2Utxos: Int) extends ScenarioGen[Model.State, Stage1Sut]:

        override def genNextCommand(
            state: Model.State
        ): Gen[AnyCommand[Model.State, Stage1Sut]] = {
            import hydrozoa.integration.stage1.Model.BlockCycle.*
    import hydrozoa.integration.stage1.Model.CurrentTime.BeforeHappyPathExpiration

            state.currentTime match {
                case BeforeHappyPathExpiration(_) =>
                    state.blockCycle match {
                        case Done(blockNumber, _) =>
                            val settlementExpirationTime =
                                state.multiNodeConfig.headConfig.txTiming.newSettlementEndTime(
                                  state.competingFallbackStartTime
                                )
                            // We need to avoid fallbacks to finalize the head
                            CommandGenerators
                                .genStayOnHappyPathDelay(
                                  currentTime = state.currentTime.instant,
                                  settlementExpirationTime = settlementExpirationTime
                                )
                                .map(AnyCommand.apply)

                        case Ready(blockNumber, _) =>
                            CommandGenerators
                                .genStartBlock(blockNumber, state.currentTime.instant)
                                .map(AnyCommand.apply)

                        case InProgress(blockNumber, _, _, _) =>
                            Gen.frequency(
                              1 -> (if state.utxosL2Active.size >= minL2Utxos
                                    then
                                        CommandGenerators
                                            .genCompleteBlockFinal(
                                              blockNumber,
                                              state.currentTime.instant
                                            )
                                            .map(AnyCommand.apply)
                                    else
                                        CommandGenerators
                                            .genCompleteBlockRegular(
                                              blockNumber,
                                              state.currentTime.instant
                                            )
                                            .map(AnyCommand.apply)),
                              10 -> CommandGenerators
                                  .genValidNonPlutusL2Tx(
                                    txStrategy = Dust(),
                                    txMutator = Identity
                                  )(
                                    state = state
                                  )
                                  .map(AnyCommand.apply)
                            )

                        case HeadFinalized => Gen.const(noOp)
                    }
                case _ => Gen.const(noOp)
            }
        }

        override def targetStatePrecondition(
            targetState: Model.State
        ): Boolean =
            targetState.blockCycle == HeadFinalized

    case object DepositsScenarioGen extends ScenarioGen[Model.State, Stage1Sut]:

        override def genNextCommand(
            state: Model.State
        ): Gen[AnyCommand[Model.State, Stage1Sut]] = {
            import hydrozoa.integration.stage1.Model.BlockCycle.*
    import hydrozoa.integration.stage1.Model.CurrentTime.BeforeHappyPathExpiration

            state.currentTime match {
                case BeforeHappyPathExpiration(_) =>
                    state.blockCycle match {
                        case Done(blockNumber, _) =>
                            val settlementExpirationTime =
                                state.multiNodeConfig.headConfig.txTiming.newSettlementEndTime(
                                  state.competingFallbackStartTime
                                )
                            CommandGenerators
                                .genRandomDelay(
                                  currentTime = state.currentTime.instant,
                                  settlementExpirationTime = settlementExpirationTime,
                                  competingFallbackStartTime = state.competingFallbackStartTime,
                                  slotConfig = state.multiNodeConfig.headConfig.slotConfig,
                                  blockNumber = blockNumber
                                )
                                .map(AnyCommand.apply)

                        case Ready(blockNumber, _) =>
                            CommandGenerators
                                .genStartBlock(blockNumber, state.currentTime.instant)
                                .map(AnyCommand.apply)

                        case InProgress(blockNumber, _, _, _) =>

                            Gen.frequency(
                              3 -> CommandGenerators
                                  .genRegisterDepositCommand(state)
                                  .map(_.map(AnyCommand.apply(_))),
                              5 -> (if state.deposits.depositsRegistered.nonEmpty
                                    then
                                        CommandGenerators
                                            .genSubmitDepositsCommand(state)
                                            .map(cmd => Some(AnyCommand.apply(cmd)))
                                    else Gen.const(None)),
                              1 -> CommandGenerators
                                  .genCompleteBlock(
                                    blockNumber,
                                    state.currentTime.instant
                                  )
                                  .map(cmd => Some(AnyCommand.apply(cmd))),
                              3 -> (if state.utxosL2Active.nonEmpty
                                    then
                                        CommandGenerators
                                            .genValidNonPlutusL2Tx(
                                              txStrategy = RandomWithdrawals,
                                              txMutator = Identity
                                            )(state)
                                            .map(cmd => Some(AnyCommand.apply(cmd)))
                                    else Gen.const(None))
                            ).retryUntil(_.isDefined)
                                .map(_.get)

                        case HeadFinalized => Gen.const(noOp)
                    }
                case _ => Gen.const(noOp)
            }
        }

end ScenarioGenerators
