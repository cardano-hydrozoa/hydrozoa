package hydrozoa.integration.stage4

import cats.data.NonEmptyList
import cats.effect.IO
import hydrozoa.config.head.initialization.CappedValueGen.{ensureMinAdaLenient, generateCappedValue}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.integration.stage4.Commands.{*, given}
import hydrozoa.integration.stage4.Model.{ModelState, given}
import hydrozoa.integration.stage4.Stage4SutCommands.given
import hydrozoa.lib.cardano.scalus.QuantizedTime.given_Ordering_QuantizedInstant.mkOrderingOps
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.lib.cardano.scalus.given_Choose_QuantizedInstant
import hydrozoa.lib.cardano.scalus.ledger.{asUtxoList, withZeroFees}
import hydrozoa.lib.cardano.scalus.txbuilder.DiffHandler.prebalancedLovelaceDiffHandler
import hydrozoa.multisig.consensus.UserRequestBody.{DepositRequestBody, TransactionRequestBody}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{UserRequest, UserRequestHeader, UserRequestWithId}
import hydrozoa.multisig.ledger.eutxol2.tx.GenesisObligation
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import org.scalacheck.commands.{AnyCommand, ScenarioGen, noOp}
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, PropertyM}
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AuxiliaryData, Coin, Metadatum, TransactionInput, TransactionOutput, Utxo, Value, Word64}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, ModifyAuxiliaryData, Send, Spend}
import scalus.cardano.txbuilder.{PubKeyWitness, TransactionBuilder}
import scalus.uplc.builtin.ByteString

// ===================================
// Per-command generators
// ===================================

object CommandGenerators:

    /** Sample the next event of the *superposition* of N independent Poisson processes (one per
      * peer, rate lambda_p = 1/mean_p). Returns `(peer, interArrivalDelay)` where:
      *   - `interArrivalDelay ~ Exp(sum of lambda_p)` — gap from the last event in the global
      *     merged stream
      *   - `peer` is sampled with probability proportional to `lambda_p`
      *
      * The marginal stream of any peer p (project the merged stream onto peer-p events) is exactly
      * `Poisson(lambda_p)` — so configuring peer-specific rates produces the intended less-active
      * vs more-active behavior.
      */
    def genSuperposedNextEvent(
        meanInterArrivalTimes: Map[HeadPeerNumber, FiniteDuration]
    ): Gen[(HeadPeerNumber, FiniteDuration)] = {
        val rates: Seq[(HeadPeerNumber, Double)] =
            meanInterArrivalTimes.toSeq.map { case (p, mu) => p -> 1.0 / mu.toMillis }
        val totalRate = rates.map(_._2).sum
        val weights: Seq[(Int, Gen[HeadPeerNumber])] =
            rates.map { case (p, lambda) =>
                (math.round(lambda * 1e9).toInt.max(1), Gen.const(p))
            }
        for {
            u <- Gen.choose(Double.MinPositiveValue, 1.0)
            interArrivalMs = (-math.log(u) / totalRate).toLong.max(1L)
            peer <- Gen.frequency(weights*)
        } yield (peer, interArrivalMs.millis)
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
        interArrivalDelay: FiniteDuration,
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

                // Validity is computed against the SUT's virtual time at submission, which equals
                // `state.currentModelTime + interArrivalDelay` (the model clock right after this
                // command is run — same value the SUT IO.sleep advances to).
                submissionTime = state.currentModelTime + interArrivalDelay

                header = UserRequestHeader(
                  headId = config.headConfig.headId,
                  validityStart = RequestValidityStartTime(
                    QuantizedInstant.ofEpochSeconds(
                      config.slotConfig,
                      (submissionTime - 5.seconds).getEpochSecond
                    )
                  ),
                  validityEnd = RequestValidityEndTime(
                    QuantizedInstant.ofEpochSeconds(
                      config.slotConfig,
                      (submissionTime + 2.minutes).getEpochSecond
                    )
                  ),
                  bodyHash = body.hash
                )

                userVk = config.nodeConfigs(peerNum).ownWallet.exportVerificationKey

            } yield Some(
              L2TxCommand(
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
              )
            )
    }

    def genRegisterDepositCommand(
        peerNum: HeadPeerNumber,
        interArrivalDelay: FiniteDuration,
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

                                        submissionTime =
                                            state.currentModelTime + interArrivalDelay
                                        requestId = state.nextRequestId(peerNum)

                                        requestValidityEndTime = RequestValidityEndTime(
                                          QuantizedInstant.ofEpochSeconds(
                                            config.slotConfig,
                                            (submissionTime + 2.minutes).getEpochSecond
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
                                          validityStart = RequestValidityStartTime(
                                            QuantizedInstant.ofEpochSeconds(
                                              config.slotConfig,
                                              (submissionTime - 5.seconds).getEpochSecond
                                            )
                                          ),
                                          validityEnd = RequestValidityEndTime(
                                            QuantizedInstant.ofEpochSeconds(
                                              config.slotConfig,
                                              requestValidityEndTime.getEpochSecond
                                            )
                                          ),
                                          bodyHash = body.hash
                                        )

                                        userVk = config
                                            .nodeConfigs(peerNum)
                                            .ownWallet
                                            .exportVerificationKey

                                        absorptionStartTime =
                                            config.headConfig.txTiming
                                                .depositAbsorptionStartTime(requestValidityEndTime)
                                                .convert

                                        expectedAbsorptionTime =
                                            absorptionStartTime + state.params.absorptionSlack

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
                                        l2Payload =
                                            depositRefundSeq.depositTx.depositProduced.l2Payload,
                                        depositProduced =
                                            depositRefundSeq.depositTx.depositProduced.utxoId,
                                        depositTxBytesSigned = depositTxSigned,
                                        interArrivalDelay = interArrivalDelay,
                                        absorptionStartTime = absorptionStartTime,
                                        expectedAbsorptionTime = expectedAbsorptionTime,
                                      )
                                    )
                        } yield ret
                    }
            } yield ret
    }

    /** Force-advance the global clock until all pending deposits for this peer are absorbed. Jumps
      * to max(expectedAbsorptionTime) + [0, 30s] jitter. Used when the peer has no L2 UTxOs and
      * must wait for absorption.
      *
      * TODO: within the new approach that uses cumulative time this looks odd, shall we remove it?
      */
    def genDelayForAbsorption(
        peerNum: HeadPeerNumber,
        state: ModelState
    ): Gen[DelayCommand] = {
        val currentTime = state.currentModelTime
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

    private def pick[A](gen: Gen[A])(using pp: A => Pretty): PropertyM[IO, A] =
        PropertyM.pick[IO, A](gen)

    override def genNextCommand(
        state: ModelState
    ): PropertyM[IO, AnyCommand[ModelState, Stage4Sut]] =
        // Single global Poisson superposition: sample (peer, gap-since-last-event). Each peer's
        // marginal stream is exactly Poisson at its configured rate, so peers with smaller mean
        // inter-arrival are picked proportionally more often and the global rate is Σλ_p.
        pick(
            for {
                (peerNum, interArrivalDelay) <- CommandGenerators.genSuperposedNextEvent(
                  state.params.meanInterArrivalTimes
                )
                cmd <- genCommandForPeer(peerNum, interArrivalDelay, state)
            } yield cmd
        )

    private def genCommandForPeer(
        peerNum: HeadPeerNumber,
        interArrivalDelay: FiniteDuration,
        state: ModelState,
    ): Gen[AnyCommand[ModelState, Stage4Sut]] = {
        val config = state.params.multiNodeConfig
        val peerAddress = config.addressOf(peerNum)
        val ownedL2Utxos = state.utxosL2Active.filter((_, o) =>
            o.address.asInstanceOf[ShelleyAddress] == peerAddress
        )
        val availableL1 = state.peerUtxosL1(peerNum)
        val hasPendingAbsorption = state.pendingDeposits(peerNum).exists { pd =>
            state.currentModelTime < pd.expectedAbsorptionTime
        }

        // Two distinct delay cases with different semantics:
        // 1. Forced absorption delay (below): peer is stuck with nothing to spend — must wait.
        //    Jumps directly to expectedAbsorptionTime + jitter so the next state has UTxOs.
        // 2. Background ticking: every L2Tx/Deposit command carries the superposition's
        //    inter-arrival delay that advances the global clock naturally. No explicit delay
        //    command needed while the peer has UTxOs to spend.
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
                      // TODO: Too slow on my machine dure to KZG-commitments
                      // 2 -> Gen.const(TxStrategy.Dust(50)),
                      1 -> Gen.const(TxStrategy.Arbitrary)
                    ).flatMap(strategy =>
                        CommandGenerators
                            .genL2TxCommand(
                              peerNum,
                              interArrivalDelay,
                              strategy,
                              TxMutator.Identity
                            )(state)
                            .map(_.map(AnyCommand.apply(_)))
                    )

            val genDepositOpt: Gen[Option[AnyCommand[ModelState, Stage4Sut]]] =
                if availableL1.isEmpty then Gen.const(None)
                else
                    CommandGenerators
                        .genRegisterDepositCommand(peerNum, interArrivalDelay)(state)
                        .map(_.map(AnyCommand.apply(_)))

            Gen.frequency(
              10 -> genL2TxOpt,
              3 -> genDepositOpt,
            ).map(_.getOrElse(noOp))
        }
    }
