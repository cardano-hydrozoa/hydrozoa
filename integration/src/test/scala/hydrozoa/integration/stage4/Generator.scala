package hydrozoa.integration.stage4

import cats.data.NonEmptyList
import hydrozoa.config.head.initialization.CappedValueGen.{ensureMinAdaLenient, generateCappedValue}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.RequestValidityEndTime
import hydrozoa.integration.stage4.Commands.*
import hydrozoa.integration.stage4.Commands.given
import hydrozoa.integration.stage4.Model.ModelState
import hydrozoa.integration.stage4.Model.given
import hydrozoa.integration.stage4.Stage4SutCommands.given
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import hydrozoa.lib.cardano.scalus.given_Choose_QuantizedInstant
import hydrozoa.lib.cardano.scalus.ledger.{asUtxoList, withZeroFees}
import hydrozoa.lib.cardano.scalus.txbuilder.DiffHandler.prebalancedLovelaceDiffHandler
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{
    RequestValidityEndTimeRaw,
    RequestValidityStartTimeRaw,
    UserRequest,
    UserRequestHeader,
    UserRequestWithId,
}
import hydrozoa.multisig.ledger.eutxol2.tx.GenesisObligation
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import org.scalacheck.Gen
import org.scalacheck.commands.{AnyCommand, ScenarioGen, noOp}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{
    AuxiliaryData,
    Coin,
    Metadatum,
    TransactionInput,
    TransactionOutput,
    Utxo,
    Value,
    Word64,
}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, ModifyAuxiliaryData, Send, Spend}
import scalus.cardano.txbuilder.{PubKeyWitness, TransactionBuilder}
import scalus.uplc.builtin.ByteString

import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}

// ===================================
// Per-command generators
// ===================================

object CommandGenerators:

    /** Sample inter-arrival time from Exp(1/mean) using the inverse-CDF transform T = -ln(U)/λ.
      * Each peer models an independent Poisson process: events arrive at rate λ = 1/mean, and the
      * time between successive events is exponentially distributed with that mean. This delay is
      * stored on the command and returned by ModelCommand.delay so the SUT fiber actually sleeps
      * for this duration before issuing the command — giving realistic pacing without a global clock.
      */
    private def genInterArrivalDelay(mean: FiniteDuration): Gen[FiniteDuration] =
        // Double.MinPositiveValue avoids log(0) = -Infinity; in practice the tail is negligible.
        Gen.choose(Double.MinPositiveValue, 1.0).map { u =>
            (-math.log(u) * mean.toMillis).toLong.milliseconds
        }

    private def genInputs(
        utxos: Map[TransactionInput, scalus.cardano.ledger.TransactionOutput],
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

    private def genOutputValues(
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
                        next <- step(rest, None, None, None)
                        acc_ = acc :+ next
                    } yield
                        if next == rest
                        then Right(acc_)
                        else Left(acc_ -> (rest - next))
                )
        }
    } yield values

    private def genAuxiliaryData(
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

    def genL2TxCommand(
        peerNum: HeadPeerNumber,
        txStrategy: TxStrategy,
        txMutator: TxMutator
    )(state: ModelState): Gen[Option[L2TxCommand]] = {
        val config = state.params.multiNodeConfig
        val cardanoNetwork = config.headConfig.cardanoNetwork
        val generateCappedValueC = generateCappedValue(cardanoNetwork)
        val peerAddress = config.addressOf(peerNum)
        val l2AddressesInUse = state.utxosL2Active.map(_._2.address).toSet

        val ownedUtxos = state.utxosL2Active.filter((_, o) =>
            o.address.asInstanceOf[ShelleyAddress] == peerAddress
        )

        if ownedUtxos.isEmpty then Gen.const(None)
        else
            for {
                inputs <- genInputs(ownedUtxos, txStrategy)
                totalValue = Value.combine(inputs.map(ownedUtxos(_).value))

                outputValues <- genOutputValues(totalValue, txStrategy, generateCappedValueC)
                outputs <- Gen.sequence[List[TransactionOutput], TransactionOutput](
                  outputValues.map(v => Gen.oneOf(l2AddressesInUse.toSeq).map(a => Babbage(a, v)))
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

                txSigned = config.signTxAs(peerNum)(txUnsigned)

                body = TransactionRequestBody(l2Payload = ByteString.fromArray(txSigned.toCbor))

                currentTime = state.modelTimeFor(peerNum)

                header = UserRequestHeader(
                  headId = config.headConfig.headId,
                  validityStart = RequestValidityStartTimeRaw(
                    (currentTime - 5.seconds).getEpochSecond
                  ),
                  validityEnd = RequestValidityEndTimeRaw(
                    (currentTime + 2.minutes).getEpochSecond
                  ),
                  bodyHash = body.hash
                )

                userVk = config.nodeConfigs(peerNum).ownHeadWallet.exportVerificationKey

                interArrivalDelay <- genInterArrivalDelay(state.params.meanInterArrivalTimes(peerNum))

            } yield Some(L2TxCommand(
              peerNum = peerNum,
              request = UserRequestWithId.TransactionRequest(
                requestId = state.nextRequestId(peerNum),
                request = UserRequest.TransactionRequest(
                  header = header,
                  body = body.asInstanceOf[TransactionRequestBody],
                  userVk = userVk
                )
              ),
              txStrategy = txStrategy,
              txMutator = txMutator,
              interArrivalDelay = interArrivalDelay,
            ))
    }

    def genRegisterDepositCommand(
        peerNum: HeadPeerNumber
    )(state: ModelState): Gen[Option[RegisterAndSubmitDepositCommand]] = {
        val config = state.params.multiNodeConfig
        val peerAddress = config.addressOf(peerNum)
        val cardanoNetwork = config.headConfig.cardanoNetwork
        val generateCappedValueC = generateCappedValue(cardanoNetwork)
        val ensureMinAdaLenientC = ensureMinAdaLenient(cardanoNetwork)
        val availableL1 = state.peerUtxosL1(peerNum)

        if availableL1.isEmpty then Gen.const(None)
        else
            for {
                fundingUtxos <- Gen.atLeastOne(availableL1).map(_.toMap)
                totalValue = Value.combine(fundingUtxos.map(_._2.value))

                minimalChangeCoins = 1_500_000L
                minimalDepositValueCoins = 1_500_000L
                minimalTotalValueCoins = minimalChangeCoins + minimalDepositValueCoins

                ret <-
                    if ensureMinAdaLenientC(totalValue) != totalValue ||
                        minimalTotalValueCoins > totalValue.coin.value
                    then Gen.const(None)
                    else {
                        val reserved = Value.lovelace(minimalDepositValueCoins)
                        val totalValueAvailable = totalValue - reserved
                        for {
                            change <- generateCappedValueC(
                              totalValueAvailable,
                              Some(minimalChangeCoins),
                              None,
                              None
                            )
                            depositValue = totalValue - change

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

                                        outputs <- Gen
                                            .sequence[List[TransactionOutput], TransactionOutput](
                                              outputValues.map(v =>
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

                                        currentTime = state.modelTimeFor(peerNum)
                                        requestId = state.nextRequestId(peerNum)

                                        requestValidityEndTime = RequestValidityEndTime(
                                          config.headConfig.slotConfig,
                                          RequestValidityEndTimeRaw(
                                            (currentTime + 2.minutes).getEpochSecond
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
                                            )(using config.headConfig)
                                            .result
                                            .fold(
                                              err => throw RuntimeException(err.toString),
                                              identity
                                            )

                                        depositTxSigned = config.signTxAs(peerNum)(
                                          depositRefundSeq.depositTx.tx
                                        )

                                        body = DepositRequestBody(
                                          l1Payload = ByteString
                                              .fromArray(depositRefundSeq.depositTx.tx.toCbor),
                                          l2Payload = GenesisObligation.serialize(l2Outputs)
                                        )

                                        header = UserRequestHeader(
                                          headId = config.headConfig.headId,
                                          validityStart = RequestValidityStartTimeRaw(
                                            (currentTime - 5.seconds).getEpochSecond
                                          ),
                                          validityEnd = RequestValidityEndTimeRaw(
                                            requestValidityEndTime.getEpochSecond
                                          ),
                                          bodyHash = body.hash
                                        )

                                        userVk = config
                                            .nodeConfigs(peerNum)
                                            .ownHeadWallet
                                            .exportVerificationKey

                                        absorptionStart =
                                            config.headConfig.txTiming
                                                .depositAbsorptionStartTime(requestValidityEndTime)
                                                .convert

                                        expectedAbsorptionTime =
                                            absorptionStart + state.params.absorptionSlack

                                        interArrivalDelay <- genInterArrivalDelay(
                                          state.params.meanInterArrivalTimes(peerNum)
                                        )

                                    } yield Some(
                                      RegisterAndSubmitDepositCommand(
                                        peerNum = peerNum,
                                        request = UserRequestWithId.DepositRequest(
                                          requestId = requestId,
                                          request = UserRequest.DepositRequest(
                                            header = header,
                                            body = body.asInstanceOf[DepositRequestBody],
                                            userVk = userVk
                                          )
                                        ),
                                        depositRefundTxSeq = depositRefundSeq,
                                        depositTxBytesSigned = depositTxSigned,
                                        interArrivalDelay = interArrivalDelay,
                                        expectedAbsorptionTime = expectedAbsorptionTime,
                                      )
                                    )
                        } yield ret
                    }
            } yield ret
    }

    /** Force-advance time until all pending deposits for this peer are absorbed.
      * Jumps to max(absorptionStart + slack) + [0, 30s] jitter.
      * Used when the peer has no L2 UTxOs and must wait for absorption.
      */
    def genDelayForAbsorption(
        peerNum: HeadPeerNumber,
        state: ModelState
    ): Gen[DelayCommand] = {
        val currentTime = state.modelTimeFor(peerNum)
        val pendingForPeer = state.pendingDeposits(peerNum)

        val absorptionTargets = pendingForPeer
            .map(_.expectedAbsorptionTime)
            .filter(_ > currentTime)

        if absorptionTargets.isEmpty then
            Gen.const(
              DelayCommand(
                peerNum,
                QuantizedFiniteDuration(currentTime.slotConfig, 1.second)
              )
            )
        else
            val latestTarget = absorptionTargets.maxBy(_.getEpochSecond)
            Gen.choose(latestTarget, latestTarget + 30.seconds).map { targetInstant =>
                DelayCommand(peerNum, targetInstant - currentTime)
            }
    }

end CommandGenerators

// ===================================
// Suite scenario generators
// ===================================

object Stage4ScenarioGen extends ScenarioGen[ModelState, Stage4Sut]:

    override def genNextCommand(
        state: ModelState
    ): Gen[AnyCommand[ModelState, Stage4Sut]] = {
        val peers = state.params.multiNodeConfig.nodeConfigs.keys.toSeq
        // In a superposition of independent Poisson processes the next event comes from peer p
        // with probability λ_p / Σλ_i (proportional to rate = 1/μ). Using Gen.oneOf would give
        // each peer equal probability regardless of rate, causing slow peers to dominate the
        // tail of the time-sorted table and fast peers to be underrepresented.
        val minMeanMs = peers.map(p => state.params.meanInterArrivalTimes(p).toMillis).min
        val weightedPeers = peers.map { p =>
            val w = (minMeanMs.toDouble / state.params.meanInterArrivalTimes(p).toMillis * 100).toInt.max(1)
            w -> Gen.const(p)
        }
        for {
            peerNum <- Gen.frequency(weightedPeers*)
            cmd     <- genCommandForPeer(peerNum, state)
        } yield cmd
    }

    private def genCommandForPeer(
        peerNum: HeadPeerNumber,
        state: ModelState,
    ): Gen[AnyCommand[ModelState, Stage4Sut]] = {
        val config = state.params.multiNodeConfig
        val peerAddress = config.addressOf(peerNum)
        val ownedL2Utxos = state.utxosL2Active.filter((_, o) =>
            o.address.asInstanceOf[ShelleyAddress] == peerAddress
        )
        val availableL1 = state.peerUtxosL1(peerNum)
        val hasPendingAbsorption = state.pendingDeposits(peerNum).exists { pd =>
            state.modelTimeFor(peerNum) < pd.expectedAbsorptionTime
        }

        // Two distinct delay cases with different semantics:
        // 1. Forced absorption delay (below): peer is stuck with nothing to spend — must wait.
        //    Jumps directly to expectedAbsorptionTime + jitter so the next state has UTxOs.
        // 2. Background ticking: every L2Tx/Deposit command carries an exponential inter-arrival
        //    delay (ModelCommand.delay) that advances the peer's clock naturally. No explicit
        //    delay command needed while the peer has UTxOs to spend.
        if ownedL2Utxos.isEmpty && hasPendingAbsorption then
            CommandGenerators.genDelayForAbsorption(peerNum, state).map(AnyCommand.apply)
        else if ownedL2Utxos.isEmpty && availableL1.isEmpty then Gen.const(noOp)
        else {
            val genL2TxOpt: Gen[Option[AnyCommand[ModelState, Stage4Sut]]] =
                if ownedL2Utxos.isEmpty then Gen.const(None)
                else
                    Gen.frequency(
                      5 -> Gen.const(TxStrategy.Regular),
                      2 -> Gen.const(TxStrategy.RandomWithdrawals),
                      2 -> Gen.const(TxStrategy.Dust(50)),
                      1 -> Gen.const(TxStrategy.Arbitrary)
                    ).flatMap(strategy =>
                        CommandGenerators
                            .genL2TxCommand(peerNum, strategy, TxMutator.Identity)(state)
                            .map(_.map(AnyCommand.apply(_)))
                    )

            val genDepositOpt: Gen[Option[AnyCommand[ModelState, Stage4Sut]]] =
                if availableL1.isEmpty then Gen.const(None)
                else
                    CommandGenerators
                        .genRegisterDepositCommand(peerNum)(state)
                        .map(_.map(AnyCommand.apply(_)))

            Gen.frequency(
              10 -> genL2TxOpt,
              3 -> genDepositOpt,
            ).map(_.getOrElse(noOp))
        }
    }
