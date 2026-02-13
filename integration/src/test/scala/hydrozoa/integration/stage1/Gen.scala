package hydrozoa.integration.stage1

import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.config.head.initialization.generateCappedValue
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.lib.cardano.scalus.given_Choose_QuantizedInstant
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.event.LedgerEvent.TxL2Event
import org.scalacheck.commands.{AnyCommand, CommandGen, noOp}
import org.scalacheck.{Arbitrary, Gen}
import scalus.builtin.Builtins.blake2b_224
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Coin, Metadatum, SlotConfig, TransactionOutput, Utxo, Value, Word64}
import scalus.cardano.txbuilder.TransactionBuilder
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, ModifyAuxiliaryData, Send, Spend}
import test.Generators.Hydrozoa.ArbitraryInstances.given_Arbitrary_LedgerEvent
import test.Generators.Hydrozoa.genEventId

// ===================================
// Per-command generators
// ===================================

/** Per-command generators. These produce concrete command values; the caller wraps each into
  * [[AnyCommand]] where the [[org.scalacheck.commands.SutCommand]] implicit is in scope.
  */
object Generators:

    val logger: org.slf4j.Logger = Logging.logger("Stage1.Generators")

    def genDelay(
        currentTime: QuantizedInstant,
        settlementExpirationTime: QuantizedInstant,
        competingFallbackStartTime: QuantizedInstant,
        slotConfig: SlotConfig,
        blockNumber: BlockNumber
    ): Gen[DelayCommand] =
        val stayOnHappyPathDelay = Gen
            .choose(currentTime, settlementExpirationTime)
            .flatMap(d => DelayCommand(Delay.EndsBeforeHappyPathExpires(d - currentTime)))
        for {
            delay <-
                if blockNumber < 1
                then stayOnHappyPathDelay
                else
                    Gen
                        .frequency(
                          1 -> stayOnHappyPathDelay,
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

    def genArbitraryLedgerEvent: Gen[LedgerEventCommand] = for {
        event <- Arbitrary.arbitrary[hydrozoa.multisig.ledger.event.LedgerEvent]
    } yield LedgerEventCommand(event)

    def genValidNonPlutusL2Tx(state: ModelState): Gen[LedgerEventCommand] =

        val cardanoNetwork: CardanoNetwork = state.headConfig.cardanoNetwork
        val generateCappedValueC = generateCappedValue(cardanoNetwork)
        val l2AddressesInUse = state.activeUtxos.values.map(_.address).toSet
        val ownedUtxos = state.activeUtxos.filter((_, o) =>
            o.address.asInstanceOf[ShelleyAddress].payment.asHash == blake2b_224(
              state.ownTestPeer.wallet.exportVerificationKey
            )
        )

        for {
            numberOfInputs <- Gen.choose(1, 10.min(ownedUtxos.size))
            inputs <- Gen.pick(numberOfInputs, ownedUtxos.keySet)
            totalValue = inputs.map(ownedUtxos(_).value).fold(Value.zero)(_ + _)
            outputs <- Gen.tailRecM(List.empty[TransactionOutput] -> totalValue)((acc, rest) =>
                for {
                    next <- generateCappedValueC(rest)
                    address <- Gen.oneOf(l2AddressesInUse)
                    acc_ = acc :+ Babbage(address, next)
                } yield
                    if next == rest
                    then Right(acc_)
                    else Left(acc_ -> (rest - next))
            )

            auxiliaryData = Some(
              Metadata(
                Map(
                  Word64(CIP67.Tags.head)
                      -> Metadatum.List(outputs.map(_ => Metadatum.Int(2)).toIndexedSeq)
                )
              )
            )

            txUnsigned = TransactionBuilder
                .build(
                  cardanoNetwork.cardanoInfo.network,
                  (inputs.map(utxoId => Spend(Utxo(utxoId, ownedUtxos(utxoId))))
                      ++ outputs.map(Send.apply)
                      :+ Fee(Coin.zero)).toList
                      :+ ModifyAuxiliaryData(_ => auxiliaryData)
                )
                // .flatMap(
                //  _.finalizeContext(
                //    protocolParams = state.headConfig.cardanoProtocolParams,
                //    diffHandler = prebalancedLovelaceDiffHandler,
                //    evaluator = state.headConfig.plutusScriptEvaluatorForTxBuild,
                //    // TODO: Is it Ok?
                //    validators = Seq.empty
                //  )
                // )
                .fold(
                  err => throw RuntimeException(s"Can't build l2 tx: $err"),
                  ctx => ctx.transaction
                )

            witness = state.ownTestPeer.wallet.mkVKeyWitness(txUnsigned)
            txSigned = txUnsigned.attachVKeyWitnesses(List(witness))
            eventId <- genEventId

            _ = logger.trace(s"l2Tx: ${HexUtil.encodeHexString(txSigned.toCbor)}")

        } yield LedgerEventCommand(
          event = TxL2Event(
            eventId = eventId,
            tx = txSigned.toCbor
          )
        )

    def genCompleteBlock(blockNumber: BlockNumber): Gen[CompleteBlockCommand] = for {
        isFinal <- Gen.frequency(
          1 -> true,
          40 -> false
        )
    } yield CompleteBlockCommand(blockNumber, isFinal)

end Generators

// ===================================
// Default command generator
// ===================================

/** There is a customizable delay before starting every new block. If the delay happens to be long
  * enough so the latest fallback becomes active, all next commands are NoOp and the fallback is
  * expected to be submitted. Otherwise, only happy path effects are expected to be submitted.
  */
// TODO: that's not correct. The absense of txs with withdrawals is what I meant here
object ArbitraryEventsOnly extends CommandGen[ModelState, Stage1Sut]:

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
                            .genDelay(
                              currentTime = state.currentTime.instant,
                              settlementExpirationTime = settlementExpirationTime,
                              competingFallbackStartTime = state.competingFallbackStartTime,
                              slotConfig = state.headConfig.slotConfig,
                              blockNumber = blockNumber
                            )
                            .map(AnyCommand(_))

                    case Ready(blockNumber, _) =>
                        Generators
                            .genStartBlock(blockNumber, state.currentTime.instant)
                            .map(AnyCommand(_))

                    case InProgress(blockNumber, _, _) =>
                        Gen.frequency(
                          1 -> Generators
                              .genCompleteBlock(blockNumber)
                              .map(AnyCommand(_)),
                          10 -> Generators
                              .genValidNonPlutusL2Tx(state)
                              .map(AnyCommand(_))
                        )

                    case HeadFinalized => Gen.const(noOp)
                }
            case _ => Gen.const(noOp)
        }
    }
