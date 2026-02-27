package hydrozoa.integration.stage1

import cats.data.NonEmptyList
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.config.head.initialization.CappedValueGen.{ensureMinAdaLenient, generateCappedValue}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.integration.stage1.CommandGen.L2txGen
import hydrozoa.integration.stage1.CommandGen.TxMutator.Identity
import hydrozoa.integration.stage1.CommandGen.TxStrategy.{Dust, RandomWithdrawals, Regular}
import hydrozoa.integration.stage1.Commands.*
import hydrozoa.integration.stage1.Model.BlockCycle.HeadFinalized
import hydrozoa.integration.stage1.Model.given
import hydrozoa.integration.stage1.SutCommands.given
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.lib.cardano.scalus.given_Choose_QuantizedInstant
import hydrozoa.lib.cardano.scalus.ledger.{asUtxoList, withZeroFees}
import hydrozoa.lib.cardano.scalus.txbuilder.DiffHandler.prebalancedLovelaceDiffHandler
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.event.LedgerEvent.L2TxEvent
import hydrozoa.multisig.ledger.event.{LedgerEvent, LedgerEventId}
import hydrozoa.multisig.ledger.virtual.tx.GenesisObligation
import org.scalacheck.Gen
import org.scalacheck.commands.{AnyCommand, ScenarioGen, noOp}
import scala.concurrent.duration.DurationInt
import scala.math.Ordering.Implicits.infixOrderingOps
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AuxiliaryData, Coin, Metadatum, SlotConfig, TransactionInput, TransactionOutput, Utxo, Utxos, Value, Word64}
import scalus.cardano.txbuilder.TransactionBuilder
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, ModifyAuxiliaryData, Send, Spend}
import scalus.uplc.builtin.Builtins.blake2b_224

// ===================================
// Per-command generators
// ===================================

/** Per-command generators. These produce concrete command values; the caller wraps each into
  * [[AnyCommand]] where the [[org.scalacheck.commands.SutCommand]] implicit is in scope.
  */
object CommandGen:

    private val logger: org.slf4j.Logger = Logging.logger("Stage1.CommandGennerators")

    // ===================================
    // Delay
    // ===================================

    def genStayOnHappyPathDelay(
        currentTime: QuantizedInstant,
        settlementExpirationTime: QuantizedInstant
    ): Gen[DelayCommand] = {
        Gen
            // TODO: parameter
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
            currentTime
          )
        )

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

    // TODO: implement
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

        val cardanoNetwork: CardanoNetwork = state.headConfig.cardanoNetwork
        val generateCappedValueC = generateCappedValue(cardanoNetwork)
        val l2AddressesInUse = state.activeUtxos.values.map(_.address).toSet
        val ownedUtxos = state.activeUtxos.filter((_, o) =>
            o.address.asInstanceOf[ShelleyAddress].payment.asHash == blake2b_224(
              state.ownTestPeer.wallet.exportVerificationKey
            )
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
                  (inputs.map(utxoId => Spend(Utxo(utxoId, ownedUtxos(utxoId))))
                      ++ outputs.map(Send.apply)
                      :+ Fee(Coin.zero)).toList
                      :+ ModifyAuxiliaryData(_ => auxiliaryData)
                )
                .flatMap(
                  _.finalizeContext(
                    protocolParams = state.headConfig.cardanoProtocolParams.withZeroFees,
                    diffHandler = prebalancedLovelaceDiffHandler,
                    evaluator = state.headConfig.plutusScriptEvaluatorForTxBuild,
                    validators = Seq.empty
                  )
                )
                .fold(
                  err => throw RuntimeException(s"Can't build l2 tx: $err"),
                  ctx => ctx.transaction
                )

            witness = state.ownTestPeer.wallet.mkVKeyWitness(txUnsigned)
            txSigned = txUnsigned.attachVKeyWitnesses(List(witness))

            _ = logger.trace(s"l2Tx: ${HexUtil.encodeHexString(txSigned.toCbor)}")

        } yield L2TxCommand(
          event = L2TxEvent(
            eventId = state.nextLedgerEventId,
            tx = txSigned.toCbor
          ),
          txStrategy = txStrategy,
          txMutator = txMutator
        )

    // ===================================
    // Complete block
    // ===================================

    def genCompleteBlockRegular(
        blockNumber: BlockNumber
    ): Gen[CompleteBlockCommand] =
        Gen.const(CompleteBlockCommand(blockNumber, false))

    def genCompleteBlockFinal(blockNumber: BlockNumber): Gen[CompleteBlockCommand] =
        Gen.const(CompleteBlockCommand(blockNumber, true))

    def genCompleteBlock(
        blockNumber: BlockNumber
    ): Gen[CompleteBlockCommand] = {
        if blockNumber.convert < 20
        then genCompleteBlockRegular(blockNumber)
        else
            Gen.frequency(
              1 -> genCompleteBlockFinal(blockNumber),
              20 -> genCompleteBlockRegular(blockNumber)
            )
    }

    // ===================================
    // Register deposit command
    // ===================================

    /** May fail if there is no enough funds in peer's utxos.
      */
    def genRegisterDepositCommand(state: Model.State): Gen[Option[RegisterDepositCommand]] = {
        import state.headConfig

        // TODO: This gives the enterprise address without the stake, which is not compatible with the model
        val peerAddress = state.ownTestPeer.address(state.headConfig.network)
        // peerAddress
        // <- Gen.oneOf(state.peerUtxosL1.map(_._2.address)).map(_.asInstanceOf[ShelleyAddress])
        val generateCappedValueC = generateCappedValue(headConfig.cardanoNetwork)
        val ensureMinAdaLenientC = ensureMinAdaLenient(headConfig.cardanoNetwork)

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

                                        virtualOutputs = NonEmptyList.fromListUnsafe(
                                          outputs.map(
                                            GenesisObligation
                                                .fromTransactionOutput(_)
                                                .fold(err => throw RuntimeException(err), identity)
                                          )
                                        )

                                        depositAmount = Value.combine(
                                          virtualOutputs.toList.map(vo => Value(vo.l2OutputValue))
                                        )
                                        _ = logger.trace(s"depositAmount: $depositAmount")

                                        depositRefundSeq = DepositRefundTxSeq
                                            .Build(headConfig)(
                                              virtualOutputs = virtualOutputs,
                                              depositFee = Coin.zero,
                                              utxosFunding = NonEmptyList
                                                  .fromListUnsafe(fundingUtxos.asUtxoList),
                                              changeAddress = peerAddress,
                                              // TODO: will it work with Mock?
                                              submissionDeadline =
                                                  state.currentTime.instant + 1.minutes, // 1.hour,
                                              refundAddress = peerAddress,
                                              refundDatum = None
                                            )
                                            .result
                                            .fold(
                                              err => throw RuntimeException(err.toString),
                                              identity
                                            )

                                        depositTxSigned = state.ownTestPeer.signTx(
                                          depositRefundSeq.depositTx.tx
                                        )

                                        _ = logger.trace(
                                          s"deposit tx signed: ${HexUtil.encodeHexString(depositTxSigned.toCbor)}"
                                        )

                                    } yield Some(
                                      RegisterDepositCommand(
                                        registerDeposit = LedgerEvent.DepositEvent(
                                          eventId = state.nextLedgerEventId,
                                          depositTxBytes = depositRefundSeq.depositTx.tx.toCbor,
                                          refundTxBytes = depositRefundSeq.refundTx.tx.toCbor,
                                          virtualOutputsBytes =
                                              GenesisObligation.serialize(virtualOutputs),
                                          depositFee = Coin.zero
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
    // When executing we don't have access to the time

    def genSubmitDepositsCommand(
        depositForSubmission: List[(LedgerEventId, QuantizedInstant)],
        state: Model.State
    ): Gen[SubmitDepositsCommand] = {
        // Prefix is easier to think about, though we can pick up arbitrary elements
        Gen.choose(1, depositForSubmission.size).map { n =>
            logger.trace(
              s"genSubmitDepositsCommand depositForSubmission: ${depositForSubmission.map(_._1)}, n=$n"
            )
            val selected = depositForSubmission.take(n)

            val partition = selected.map { (eventId, submissionDeadline) =>

                // Find the deposit command to get the signed transaction
                val depositCmd = state.depositEnqueued
                    .find(_.registerDeposit.eventId == eventId)
                    .getOrElse {
                        throw RuntimeException(
                          s"Deposit with event ID $eventId not found in enqueued"
                        )
                    }

                val tuple = eventId -> depositCmd.depositTxBytesSigned
                // TODO: parameter
                val submissionRunway = state.currentTime.instant + 20.seconds

                logger.trace(s"genSubmitDepositsCommand: submissionDeadline=$submissionDeadline")
                logger.trace(s"genSubmitDepositsCommand: submissionRunway=$submissionRunway")
                if submissionDeadline > submissionRunway
                then (true, tuple)
                else (false, tuple)
            }
            SubmitDepositsCommand(
              depositsForSubmission = partition.filter(_._1).map(_._2),
              depositsForRejection = partition.filterNot(_._1).map(_._2._1)
            )
        }
    }

end CommandGen

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
          CommandGen.genValidNonPlutusL2Tx(
            txStrategy = Regular,
            txMutator = Identity
          )
        )

    object OngoingWithdrawalsScenarioGen
        extends SimpleScenarioGen(
          CommandGen.genValidNonPlutusL2Tx(
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
                                state.headConfig.txTiming.newSettlementEndTime(
                                  state.competingFallbackStartTime
                                )
                            CommandGen
                                .genRandomDelay(
                                  currentTime = state.currentTime.instant,
                                  settlementExpirationTime = settlementExpirationTime,
                                  competingFallbackStartTime = state.competingFallbackStartTime,
                                  slotConfig = state.headConfig.slotConfig,
                                  blockNumber = blockNumber
                                )
                                .map(AnyCommand.apply)

                        case Ready(blockNumber, _) =>
                            CommandGen
                                .genStartBlock(blockNumber, state.currentTime.instant)
                                .map(AnyCommand.apply)

                        case InProgress(blockNumber, _, _, _) =>
                            Gen.frequency(
                              1 -> CommandGen
                                  .genCompleteBlock(blockNumber)
                                  .map(AnyCommand.apply),
                              10 -> (if state.activeUtxos.isEmpty
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
                                state.headConfig.txTiming.newSettlementEndTime(
                                  state.competingFallbackStartTime
                                )
                            // We need to avoid fallbacks to finalize the head
                            CommandGen
                                .genStayOnHappyPathDelay(
                                  currentTime = state.currentTime.instant,
                                  settlementExpirationTime = settlementExpirationTime
                                )
                                .map(AnyCommand.apply)

                        case Ready(blockNumber, _) =>
                            CommandGen
                                .genStartBlock(blockNumber, state.currentTime.instant)
                                .map(AnyCommand.apply)

                        case InProgress(blockNumber, _, _, _) =>
                            Gen.frequency(
                              1 -> (if state.activeUtxos.size >= minL2Utxos
                                    then
                                        CommandGen
                                            .genCompleteBlockFinal(blockNumber)
                                            .map(AnyCommand.apply)
                                    else
                                        CommandGen
                                            .genCompleteBlockRegular(
                                              blockNumber
                                            )
                                            .map(AnyCommand.apply)),
                              10 -> CommandGen
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
                                state.headConfig.txTiming.newSettlementEndTime(
                                  state.competingFallbackStartTime
                                )
                            CommandGen
                                .genRandomDelay(
                                  currentTime = state.currentTime.instant,
                                  settlementExpirationTime = settlementExpirationTime,
                                  competingFallbackStartTime = state.competingFallbackStartTime,
                                  slotConfig = state.headConfig.slotConfig,
                                  blockNumber = blockNumber
                                )
                                .map(AnyCommand.apply)

                        case Ready(blockNumber, _) =>
                            CommandGen
                                .genStartBlock(blockNumber, state.currentTime.instant)
                                .map(AnyCommand.apply)

                        case InProgress(blockNumber, _, _, _) =>

                            val depositsForSubmission = state.depositForSubmission

                            val genFreq = Gen.frequency(
                              3 -> CommandGen
                                  .genRegisterDepositCommand(state)
                                  .map(_.map(AnyCommand.apply(_))),
                              5 -> (if depositsForSubmission.nonEmpty
                                    then
                                        CommandGen
                                            .genSubmitDepositsCommand(depositsForSubmission, state)
                                            .map(cmd => Some(AnyCommand.apply(cmd)))
                                    else Gen.const(None)),
                              1 -> CommandGen
                                  .genCompleteBlock(blockNumber)
                                  .map(cmd => Some(AnyCommand.apply(cmd))),
                              3 -> (if state.activeUtxos.nonEmpty
                                    then
                                        CommandGen
                                            .genValidNonPlutusL2Tx(
                                              txStrategy = RandomWithdrawals,
                                              txMutator = Identity
                                            )(state)
                                            .map(cmd => Some(AnyCommand.apply(cmd)))
                                    else Gen.const(None))
                            )
                            genFreq.retryUntil(_.isDefined).map(_.get)

                        case HeadFinalized => Gen.const(noOp)
                    }
                case _ => Gen.const(noOp)
            }
        }

end ScenarioGenerators
