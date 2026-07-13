package hydrozoa.integration.stage1

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.contravariant.*
import cats.syntax.flatMap.*
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.config.head.initialization.CappedValueGen.{ensureMinAdaLenient, generateCappedValue}
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime, FallbackTxStartTime}
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
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, Slf4jMsgFormat, Slf4jTracer, trace}
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{UserRequest, UserRequestWithId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.eutxol2.tx.GenesisObligation
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import java.util.concurrent.TimeUnit
import org.scalacheck.commands.{AnyCommand, ScenarioGen, noOp}
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, PropertyM}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.math.Ordering.Implicits.infixOrderingOps
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AuxiliaryData, Coin, Metadatum, SlotConfig, TransactionInput, TransactionOutput, Utxo, Utxos, Value, Word64}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, ModifyAuxiliaryData, Send, Spend}
import scalus.cardano.txbuilder.{PubKeyWitness, TransactionBuilder}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString

// ===================================
// Per-command generators
// ===================================

/** Per-command generators. These produce concrete command values; the caller wraps each into
  * [[AnyCommand]] where the [[org.scalacheck.commands.SutCommand]] implicit is in scope.
  */
object CommandGenerators:

    private given log: ContraTracer[IO, Slf4jMsg] =
        Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("Stage1.CommandGenerators"))

    private def pick[A](gen: Gen[A])(using pp: A => Pretty): PropertyM[IO, A] =
        PropertyM.pick[IO, A](gen)

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

    // The pre-delay that the model will add prior to CompleteBlock* command
    def genBlockDuration(
        currentTime: QuantizedInstant,
        competingFallbackStartTime: FallbackTxStartTime,
        txTiming: TxTiming.Section,
        reservedSubmissionDuration: FiniteDuration
    ): Gen[QuantizedFiniteDuration] = {
        // The duration of the block must not put us in the silence period
        val silencePeriodStart =
            txTiming.txTiming.newSettlementEndTime(competingFallbackStartTime)
        val availableDuration = silencePeriodStart - currentTime

        require(
          availableDuration.finiteDuration > reservedSubmissionDuration,
          "too late to complete the block"
        )

        Gen
            .frequency(
               5 -> Gen.const(1_000L),
               5 -> Gen.choose(0L, (availableDuration.finiteDuration - reservedSubmissionDuration).toMillis)
            )
            .map(blockDurationMs =>
                QuantizedFiniteDuration(
                  currentTime.slotConfig,
                  FiniteDuration(blockDurationMs, TimeUnit.MILLISECONDS)
                )
            )
    }

    def genCompleteBlockRegular(
        blockNumber: BlockNumber,
        currentTime: QuantizedInstant,
        competingFallbackStartTime: FallbackTxStartTime,
        txTiming: TxTiming.Section,
        reservedSubmissionDuration: FiniteDuration
    ): Gen[CompleteBlockCommand] = for {
        blockDuration <- genBlockDuration(
          currentTime,
          competingFallbackStartTime,
          txTiming,
          reservedSubmissionDuration
        )
    } yield CompleteBlockCommand(
      blockNumber,
      blockDuration,
      BlockCreationEndTime(currentTime + blockDuration),
      false
    )

    def genCompleteBlockFinal(
        blockNumber: BlockNumber,
        currentTime: QuantizedInstant,
        competingFallbackStartTime: FallbackTxStartTime,
        txTiming: TxTiming.Section,
        reservedSubmissionDuration: FiniteDuration
    ): Gen[CompleteBlockCommand] = for {
        blockDuration <- genBlockDuration(
          currentTime,
          competingFallbackStartTime,
          txTiming,
          reservedSubmissionDuration
        )
    } yield CompleteBlockCommand(
      blockNumber,
      blockDuration,
      BlockCreationEndTime(currentTime + blockDuration),
      true
    )

    def genCompleteBlock(
        blockNumber: BlockNumber,
        currentTime: QuantizedInstant,
        competingFallbackStartTime: FallbackTxStartTime,
        txTiming: TxTiming.Section,
        reservedSubmissionDuration: FiniteDuration
    ): Gen[CompleteBlockCommand] =

        for {
            ret <-
                if blockNumber.convert < 20
                then
                    genCompleteBlockRegular(
                      blockNumber,
                      currentTime,
                      competingFallbackStartTime,
                      txTiming,
                      reservedSubmissionDuration
                    )
                else
                    Gen.frequency(
                      1 -> genCompleteBlockFinal(
                        blockNumber,
                        currentTime,
                        competingFallbackStartTime,
                        txTiming,
                        reservedSubmissionDuration
                      ),
                      20 -> genCompleteBlockRegular(
                        blockNumber,
                        currentTime,
                        competingFallbackStartTime,
                        txTiming,
                        reservedSubmissionDuration
                      )
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

    type L2txGen = (state: Model.State) => PropertyM[IO, L2TxCommand]

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
    )(state: Model.State): PropertyM[IO, L2TxCommand] = {
        import PropertyM.run

        val config = state.multiNodeConfig
        val cardanoNetwork: CardanoNetwork = config.headConfig.cardanoNetwork
        val generateCappedValueC = generateCappedValue(cardanoNetwork)
        val l2AddressesInUse = state.utxosL2Active.map(_._2.address).toSet

        val ownedUtxos = state.utxosL2Active
            .filter((_, o) =>
                o.address.asInstanceOf[ShelleyAddress] == config.addressOf(HeadPeerNumber.zero)
            )

        for {
            inputs       <- pick(genInputs(ownedUtxos, txStrategy))
            totalValue   = Value.combine(inputs.map(ownedUtxos(_).value))
            _            <- run(log.trace(s"totalValue: $totalValue"))
            outputValues <- pick(genOutputValues(totalValue, txStrategy, generateCappedValueC))
            _            <- run(log.trace(s"outputValues: $outputValues"))
            outputs      <- pick(Gen.sequence[List[TransactionOutput], TransactionOutput](
                                outputValues
                                    .map(v => Gen.oneOf(l2AddressesInUse).map(a => Babbage(a, v)))
                            ))
            auxiliaryData <- pick(genAuxiliaryData(outputs, txStrategy).map(Some.apply))

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
            _        <- run(log.trace(s"signed l2Tx: ${HexUtil.encodeHexString(txSigned.toCbor)}"))

            body = TransactionRequestBody(
              l2Payload = ByteString.fromArray(txSigned.toCbor)
            )

        } yield L2TxCommand(
          request = UserRequestWithId.TransactionRequest(
            requestId = state.nextRequestId,
            request = UserRequest.TransactionRequest(
              body = body.asInstanceOf[TransactionRequestBody]
            )
          ),
          txStrategy = txStrategy,
          txMutator = txMutator
        )
    }

    // ===================================
    // Register deposit command
    // ===================================

    /** May fail if there is no enough funds in peer's utxos.
      */
    def genRegisterDepositCommand(state: Model.State): PropertyM[IO, Option[RegisterDepositCommand]] = {
        import PropertyM.run
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
        then pick(Gen.const(None))
        else
            for {
                fundingUtxos <- pick(Gen.atLeastOne(l1UtxoAvailable).map(_.toMap))
                totalValue   = Value.combine(fundingUtxos.map(_._2.value))
                _            <- run(log.trace(s"fundingUtxos: $fundingUtxos"))
                _            <- run(log.trace(s"totalValue: $totalValue"))

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
                    then pick(Gen.const(None))
                    else {
                        // Reserved to cover refund tx fee
                        val reserved = Value.lovelace(minimalDepositValueCoins)
                        val totalValueAvailable = totalValue - reserved
                        for {
                            change       <- pick(generateCappedValueC(
                                                totalValueAvailable,
                                                Some(minimalChangeCoins),
                                                None,
                                                None
                                            ))
                            depositValue = totalValue - change

                            ret <-
                                if ensureMinAdaLenientC(depositValue) != depositValue
                                then pick(Gen.const(None))
                                else
                                    for {
                                        outputValues <- pick(genOutputValues(
                                                          depositValue,
                                                          TxStrategy.Regular,
                                                          generateCappedValueC
                                                        ))

                                        outputs <- pick(Gen.sequence[List[TransactionOutput], TransactionOutput](
                                                       outputValues
                                                           .map(v =>
                                                               Gen.const(peerAddress).map(a => Babbage(a, v))
                                                           )
                                                   ))

                                        l2Outputs = NonEmptyList.fromListUnsafe(
                                          outputs.map(
                                            GenesisObligation
                                                .fromTransactionOutput(_)
                                                .fold(err => throw RuntimeException(err), identity)
                                          )
                                        )

                                        l2Value   = Value.combine(l2Outputs.toList.map(_.l2OutputValue))
                                        requestId = state.nextRequestId

                                        // This should be bigger than the longest possible block duration, see [[genCompleteBlock]].
                                        requestValidityEndTime = RequestValidityEndTime(
                                          RequestValidityEndTime(
                                            state.getCurrentTime.instant + 2.minutes
                                          )
                                        )

                                        // The depositor's COSE endorsement of the L2 payload
                                        // (design note §5.5); head peer 0 plays the depositor.
                                        l2PayloadSerialized = GenesisObligation.serialize(l2Outputs)
                                        l2PayloadCose = multiNodeConfig
                                            .nodeConfigs(HeadPeerNumber.zero)
                                            .ownWallet
                                            .signCoseCip30(blake2b_256(l2PayloadSerialized).bytes)

                                        depositRefundSeq = DepositRefundTxSeq
                                            .Build(
                                              l2Payload = l2PayloadSerialized,
                                              l2PayloadCose = l2PayloadCose,
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

                                        _ <- run(log.trace(
                                               s"deposit tx signed: ${HexUtil.encodeHexString(depositTxSigned.toCbor)}"
                                             ))

                                        body = DepositRequestBody(
                                          l1Payload = ByteString
                                              .fromArray(depositRefundSeq.depositTx.tx.toCbor),
                                          l2Payload = GenesisObligation.serialize(l2Outputs)
                                        )

                                    } yield Some(
                                      RegisterDepositCommand(
                                        request = UserRequestWithId.DepositRequest(
                                          requestId = requestId,
                                          request = UserRequest.DepositRequest(
                                            body = body.asInstanceOf[DepositRequestBody]
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
    ): PropertyM[IO, SubmitDepositsCommand] = {
        import PropertyM.run
        val registeredDeposits = state.deposits.depositsRegistered

        // Prefix is easier to think about, though we can pick up arbitrary elements
        for {
            n         <- pick(Gen.choose(1, registeredDeposits.size))
            _         <- run(log.trace(
                              s"genSubmitDepositsCommand registered deposits: $registeredDeposits, n=$n"
                          ))
            selected  = registeredDeposits.take(n)
            partition = selected.partition { registered =>
                            val submissionDeadline =
                                registered.cmd.depositRefundTxSeq.depositTx.depositProduced.requestValidityEndTime
                            val submissionRunway = state.getCurrentTime.instant + 20.seconds
                            submissionDeadline.convert > submissionRunway
                        }
            _         <- run(log.trace(
                              s"genSubmitDepositsCommand: forSubmission=${partition._1.size} toDecline=${partition._2.size}"
                          ))
        } yield SubmitDepositsCommand(
            depositsForSubmission = partition._1,
            depositsToDecline = partition._2
        )
    }

end CommandGenerators

// ===================================
// Suite scenario generators
// ===================================

object ScenarioGenerators:

    private def pick[A](gen: Gen[A])(using pp: A => Pretty): PropertyM[IO, A] =
        PropertyM.pick[IO, A](gen)

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
        ): PropertyM[IO, AnyCommand[Model.State, Stage1Sut]] = {
            import hydrozoa.integration.stage1.Model.BlockCycle.*
            import hydrozoa.integration.stage1.Model.CurrentTime.BeforeHappyPathExpiration


            state.getCurrentTime match {
                case BeforeHappyPathExpiration(_) =>
                    state.blockCycle match {
                        case Done(blockNumber, _) =>
                            val settlementExpirationTime =
                                state.multiNodeConfig.headConfig.txTiming.newSettlementEndTime(
                                  state.competingFallbackStartTime
                                )
                            pick(CommandGenerators
                                .genRandomDelay(
                                  currentTime = state.getCurrentTime.instant,
                                  settlementExpirationTime = settlementExpirationTime,
                                  competingFallbackStartTime = state.competingFallbackStartTime,
                                  slotConfig = state.multiNodeConfig.headConfig.slotConfig,
                                  blockNumber = blockNumber
                                )
                                .map(AnyCommand.apply))

                        case Ready(blockNumber, _) =>
                            pick(CommandGenerators
                                .genStartBlock(blockNumber, state.getCurrentTime.instant)
                                .map(AnyCommand.apply))

                        case InProgress(blockNumber, _, _, _) if blockNumber == BlockNumber.zero =>
                            pick(CommandGenerators
                                .genCompleteBlock(
                                  blockNumber,
                                  state.multiNodeConfig.initialBlock.blockBrief.endTime,
                                  state.competingFallbackStartTime,
                                  state.multiNodeConfig,
                                  state.reservedSubmissionDuration
                                )
                                .map(AnyCommand(_)))

                        case InProgress(blockNumber, _, _, _) =>
                            for {
                                branch <- pick(Gen.frequency(1 -> 0, 10 -> 1))
                                cmd    <- branch match {
                                              case 0 =>
                                                  pick(CommandGenerators
                                                      .genCompleteBlock(
                                                        blockNumber,
                                                        state.getCurrentTime.instant,
                                                        state.competingFallbackStartTime,
                                                        state.multiNodeConfig,
                                                        state.reservedSubmissionDuration
                                                      )
                                                      .map(AnyCommand.apply))
                                              case _ =>
                                                  if state.utxosL2Active.isEmpty
                                                  then pick(Gen.const(noOp))
                                                  else generateL2Tx(state).map(AnyCommand.apply)
                                          }
                            } yield cmd

                        case HeadFinalized => pick(Gen.const(noOp))
                    }
                case _ => pick(Gen.const(noOp))
            }
        }

    case class MakeDustScenarioGen(minL2Utxos: Int) extends ScenarioGen[Model.State, Stage1Sut]:

        override def genNextCommand(
            state: Model.State
        ): PropertyM[IO, AnyCommand[Model.State, Stage1Sut]] = {
            import hydrozoa.integration.stage1.Model.BlockCycle.*
            import hydrozoa.integration.stage1.Model.CurrentTime.BeforeHappyPathExpiration


            state.getCurrentTime match {
                case BeforeHappyPathExpiration(_) =>
                    state.blockCycle match {
                        case Done(blockNumber, _) =>
                            val settlementExpirationTime =
                                state.multiNodeConfig.headConfig.txTiming.newSettlementEndTime(
                                  state.competingFallbackStartTime
                                )
                            // We need to avoid fallbacks to finalize the head
                            pick(CommandGenerators
                                .genStayOnHappyPathDelay(
                                  currentTime = state.getCurrentTime.instant,
                                  settlementExpirationTime = settlementExpirationTime
                                )
                                .map(AnyCommand.apply))

                        case Ready(blockNumber, _) =>
                            pick(CommandGenerators
                                .genStartBlock(blockNumber, state.getCurrentTime.instant)
                                .map(AnyCommand.apply))

                        case InProgress(blockNumber, _, _, _) if blockNumber == BlockNumber.zero =>
                            pick(CommandGenerators
                                .genCompleteBlock(
                                  blockNumber,
                                  state.multiNodeConfig.initialBlock.blockBrief.endTime,
                                  state.competingFallbackStartTime,
                                  state.multiNodeConfig,
                                  state.reservedSubmissionDuration
                                )
                                .map(AnyCommand(_)))
                        case InProgress(blockNumber, _, _, _) =>
                            for {
                                branch <- pick(Gen.frequency(1 -> 0, 10 -> 1))
                                cmd    <- branch match {
                                              case 0 =>
                                                  pick((if state.utxosL2Active.size >= minL2Utxos
                                                        then CommandGenerators.genCompleteBlockFinal(
                                                            blockNumber,
                                                            state.getCurrentTime.instant,
                                                            state.competingFallbackStartTime,
                                                            state.multiNodeConfig,
                                                            state.reservedSubmissionDuration
                                                        )
                                                        else CommandGenerators.genCompleteBlockRegular(
                                                            blockNumber,
                                                            state.getCurrentTime.instant,
                                                            state.competingFallbackStartTime,
                                                            state.multiNodeConfig,
                                                            state.reservedSubmissionDuration
                                                        )).map(AnyCommand.apply))
                                              case _ =>
                                                  CommandGenerators
                                                      .genValidNonPlutusL2Tx(
                                                        txStrategy = Dust(),
                                                        txMutator = Identity
                                                      )(state)
                                                      .map(AnyCommand.apply)
                                          }
                            } yield cmd

                        case HeadFinalized => pick(Gen.const(noOp))
                    }
                case _ => pick(Gen.const(noOp))
            }
        }

        override def targetStatePrecondition(
            targetState: Model.State
        ): Boolean =
            targetState.blockCycle == HeadFinalized

    case object DepositsScenarioGen extends ScenarioGen[Model.State, Stage1Sut]:

        override def genNextCommand(
            state: Model.State
        ): PropertyM[IO, AnyCommand[Model.State, Stage1Sut]] = {
            import hydrozoa.integration.stage1.Model.BlockCycle.*
            import hydrozoa.integration.stage1.Model.CurrentTime.BeforeHappyPathExpiration


            state.getCurrentTime match {
                case BeforeHappyPathExpiration(_) =>
                    state.blockCycle match {
                        case Done(blockNumber, _) =>
                            val settlementExpirationTime =
                                state.multiNodeConfig.headConfig.txTiming.newSettlementEndTime(
                                  state.competingFallbackStartTime
                                )
                            pick(CommandGenerators
                                .genRandomDelay(
                                  currentTime = state.getCurrentTime.instant,
                                  settlementExpirationTime = settlementExpirationTime,
                                  competingFallbackStartTime = state.competingFallbackStartTime,
                                  slotConfig = state.multiNodeConfig.headConfig.slotConfig,
                                  blockNumber = blockNumber
                                )
                                .map(AnyCommand.apply))

                        case Ready(blockNumber, _) =>
                            pick(CommandGenerators
                                .genStartBlock(blockNumber, state.getCurrentTime.instant)
                                .map(AnyCommand.apply))

                        case InProgress(blockNumber, _, _, _) if blockNumber == BlockNumber.zero =>
                            pick(CommandGenerators
                                .genCompleteBlock(
                                  blockNumber,
                                  state.multiNodeConfig.initialBlock.blockBrief.endTime,
                                  state.competingFallbackStartTime,
                                  state.multiNodeConfig,
                                  state.reservedSubmissionDuration
                                )
                                .map(AnyCommand(_)))
                        case InProgress(blockNumber, _, _, _) =>
                            for {
                                branch <- pick(Gen.frequency(3 -> 0, 5 -> 1, 1 -> 2, 3 -> 3))
                                cmd    <- branch match {
                                              case 0 =>
                                                  CommandGenerators
                                                      .genRegisterDepositCommand(state)
                                                      .map(_.fold(noOp)(AnyCommand.apply))
                                              case 1 =>
                                                  if state.deposits.depositsRegistered.nonEmpty
                                                  then CommandGenerators
                                                      .genSubmitDepositsCommand(state)
                                                      .map(AnyCommand.apply)
                                                  else pick(Gen.const(noOp))
                                              case 2 =>
                                                  pick(CommandGenerators
                                                      .genCompleteBlock(
                                                        blockNumber,
                                                        state.getCurrentTime.instant,
                                                        state.competingFallbackStartTime,
                                                        state.multiNodeConfig,
                                                        state.reservedSubmissionDuration
                                                      )
                                                      .map(AnyCommand.apply))
                                              case _ =>
                                                  if state.utxosL2Active.nonEmpty
                                                  then CommandGenerators
                                                      .genValidNonPlutusL2Tx(
                                                        txStrategy = RandomWithdrawals,
                                                        txMutator = Identity
                                                      )(state)
                                                      .map(AnyCommand.apply)
                                                  else pick(Gen.const(noOp))
                                          }
                            } yield cmd

                        case HeadFinalized => pick(Gen.const(noOp))
                    }
                case _ => pick(Gen.const(noOp))
            }
        }

end ScenarioGenerators
