package hydrozoa.integration.stage1

import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.config.head.initialization.generateCappedValue
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.integration.stage1.BlockCycle.HeadFinalized
import hydrozoa.integration.stage1.Generators.L2txGen
import hydrozoa.integration.stage1.Generators.TxMutator.Identity
import hydrozoa.integration.stage1.Generators.TxStrategy.{Dust, RandomWithdrawals, Regular}
import hydrozoa.integration.stage1.SutCommands.given
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.lib.cardano.scalus.given_Choose_QuantizedInstant
import hydrozoa.lib.cardano.scalus.ledger.withZeroFees
import hydrozoa.lib.cardano.scalus.txbuilder.DiffHandler.prebalancedLovelaceDiffHandler
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.event.LedgerEvent.TxL2Event
import org.scalacheck.Gen
import org.scalacheck.commands.{AnyCommand, CommandGen, noOp}
import scalus.builtin.Builtins.blake2b_224
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AuxiliaryData, Coin, Metadatum, SlotConfig, TransactionInput, TransactionOutput, Utxo, Utxos, Value, Word64}
import scalus.cardano.txbuilder.TransactionBuilder
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, ModifyAuxiliaryData, Send, Spend}
import test.Generators.Hydrozoa.genEventId

val logger1: org.slf4j.Logger = Logging.logger("Stage1.Generators")

// ===================================
// Per-command generators
// ===================================

/** Per-command generators. These produce concrete command values; the caller wraps each into
  * [[AnyCommand]] where the [[org.scalacheck.commands.SutCommand]] implicit is in scope.
  */
object Generators:

    def genStayOnHappyPathDelay(
        currentTime: QuantizedInstant,
        settlementExpirationTime: QuantizedInstant
    ): Gen[DelayCommand] =
        Gen
            .choose(currentTime, settlementExpirationTime)
            .flatMap(d => DelayCommand(Delay.EndsBeforeHappyPathExpires(d - currentTime)))

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
                if blockNumber < 1
                then genStayOnHappyPathDelay1
                else
                    Gen
                        .frequency(
                          1 -> genStayOnHappyPathDelay1,
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
        step: (Value, Option[Long], Option[Long]) => Gen[Value]
    ): Gen[List[Value]] = for {
        values <- txStrategy match {

            case TxStrategy.Dust(maxOutputs) =>
                Gen.tailRecM((List.empty[Value], capValue, maxOutputs))((acc, rest, stepsLeft) =>
                    for {
                        next <- step(rest, Some(3_000_000L), Some(1L))
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
                        next <- step(rest, None, None)
                        acc_ = acc :+ next
                    } yield
                        if next == rest
                        then Right(acc_)
                        else Left(acc_ -> (rest - next))
                )
        }
    } yield values

    type L2txGen = (state: ModelState) => Gen[L2TxCommand]

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
    )(state: ModelState): Gen[L2TxCommand] =

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
            totalValue = inputs.map(ownedUtxos(_).value).fold(Value.zero)(_ + _)
            _ = logger1.trace(s"totalValue: $totalValue")

            // Outputs
            outputValues <- genOutputValues(totalValue, txStrategy, generateCappedValueC)
            _ = logger1.trace(s"output values: $outputValues")
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
            eventId <- genEventId

            _ = logger1.trace(s"l2Tx: ${HexUtil.encodeHexString(txSigned.toCbor)}")

        } yield L2TxCommand(
          event = TxL2Event(
            eventId = eventId,
            tx = txSigned.toCbor
          ),
          txStrategy = txStrategy,
          txMutator = txMutator
        )

    def genCompleteBlockRegular(blockNumber: BlockNumber): Gen[CompleteBlockCommand] =
        Gen.const(CompleteBlockCommand(blockNumber, false))

    def genCompleteBlockFinal(blockNumber: BlockNumber): Gen[CompleteBlockCommand] =
        Gen.const(CompleteBlockCommand(blockNumber, true))

    def genCompleteBlock(blockNumber: BlockNumber): Gen[CompleteBlockCommand] =
        Gen.frequency(
          1 -> genCompleteBlockFinal(blockNumber),
          20 -> genCompleteBlockRegular(blockNumber)
        )

end Generators

// ===================================
// Suite command generators
// ===================================

/** Produces L2 transactions (valid and non-valid) with no withdrawals.
  *
  * There is a customizable delay before starting every new block. If the delay happens to be long
  * enough so the latest fallback becomes active, all next commands are NoOp and the fallback is
  * expected to be submitted. Otherwise, only happy path effects are expected to be submitted.
  */
object NoWithdrawalsCommandGen
    extends SimpleCommandGen(
      Generators.genValidNonPlutusL2Tx(
        txStrategy = Regular,
        txMutator = Identity
      )
    )

object OngoingWithdrawalsCommandGen
    extends SimpleCommandGen(
      Generators.genValidNonPlutusL2Tx(
        txStrategy = RandomWithdrawals,
        txMutator = Identity
      )
    )

case class SimpleCommandGen(generateL2Tx: L2txGen) extends CommandGen[ModelState, Stage1Sut]:

    override def genNextCommand(
        state: ModelState
    ): Gen[AnyCommand[ModelState, Stage1Sut]] = {
        import hydrozoa.integration.stage1.BlockCycle.*
        import hydrozoa.integration.stage1.CurrentTime.BeforeHappyPathExpiration

        state.currentTime match {
            case BeforeHappyPathExpiration(_) =>
                state.blockCycle match {
                    case Done(blockNumber, _) =>
                        val settlementExpirationTime =
                            state.headConfig.txTiming.newSettlementEndTime(
                              state.competingFallbackStartTime
                            )
                        Generators
                            .genRandomDelay(
                              currentTime = state.currentTime.instant,
                              settlementExpirationTime = settlementExpirationTime,
                              competingFallbackStartTime = state.competingFallbackStartTime,
                              slotConfig = state.headConfig.slotConfig,
                              blockNumber = blockNumber
                            )
                            .map(AnyCommand.apply)

                    case Ready(blockNumber, _) =>
                        Generators
                            .genStartBlock(blockNumber, state.currentTime.instant)
                            .map(AnyCommand.apply)

                    case InProgress(blockNumber, _, _) =>
                        Gen.frequency(
                          1 -> Generators
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

case class MakeDustCommandGen(minL2Utxos: Int) extends CommandGen[ModelState, Stage1Sut]:

    override def genNextCommand(
        state: ModelState
    ): Gen[AnyCommand[ModelState, Stage1Sut]] = {
        import hydrozoa.integration.stage1.BlockCycle.*
        import hydrozoa.integration.stage1.CurrentTime.BeforeHappyPathExpiration

        state.currentTime match {
            case BeforeHappyPathExpiration(_) =>
                state.blockCycle match {
                    case Done(blockNumber, _) =>
                        val settlementExpirationTime =
                            state.headConfig.txTiming.newSettlementEndTime(
                              state.competingFallbackStartTime
                            )
                        // We need to avoid fallbacks to finalize the head
                        Generators
                            .genStayOnHappyPathDelay(
                              currentTime = state.currentTime.instant,
                              settlementExpirationTime = settlementExpirationTime
                            )
                            .map(AnyCommand.apply)

                    case Ready(blockNumber, _) =>
                        Generators
                            .genStartBlock(blockNumber, state.currentTime.instant)
                            .map(AnyCommand.apply)

                    case InProgress(blockNumber, _, _) =>
                        Gen.frequency(
                          1 -> (if state.activeUtxos.size >= minL2Utxos
                                then
                                    Generators
                                        .genCompleteBlockFinal(blockNumber)
                                        .map(AnyCommand.apply)
                                else
                                    Generators
                                        .genCompleteBlockRegular(blockNumber)
                                        .map(AnyCommand.apply)),
                          10 -> Generators
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
        targetState: ModelState
    ): Boolean =
        targetState.blockCycle == HeadFinalized
