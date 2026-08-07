package hydrozoa.integration.stage4

import cats.data.NonEmptyList
import cats.effect.IO
import hydrozoa.config.head.initialization.CappedValueGen.{ensureMinAdaLenient, generateCappedValue}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.RequestValidityEndTime
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
import hydrozoa.multisig.consensus.{UserRequest, UserRequestWithId}
import hydrozoa.multisig.ledger.eutxol2.tx.GenesisObligation
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import org.scalacheck.commands.{AnyCommand, ScenarioGen, SutCommand, noOp}
import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, PropertyM}
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AuxiliaryData, Coin, DatumOption, Metadatum, TransactionInput, TransactionOutput, Utxo, Value, Word64}
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

    /** Per-output l1/l2 designation flags — one per output, in output order: `1` = l1-bound (a
      * withdrawal, exits L2 to L1), `2` = l2-bound (stays on L2). `RandomWithdrawals` mixes them;
      * every other strategy keeps all outputs on L2. See `L2Tx.utxoPartition` for the consuming
      * end.
      */
    private def genOutputFlags(numOutputs: Int, txStrategy: TxStrategy): Gen[List[Int]] =
        txStrategy match {
            case TxStrategy.RandomWithdrawals => Gen.listOfN(numOutputs, Gen.choose(1, 2))
            case _                            => Gen.const(List.fill(numOutputs)(2))
        }

    /** The CIP67 head-tag metadata carrying the per-output l1/l2 [[genOutputFlags]]. */
    private def headFlagsMetadata(flags: List[Int]): AuxiliaryData =
        Metadata(
          Map(
            Word64(CIP67.Tags.head)
                -> Metadatum.List(flags.map(Metadatum.Int(_)).toIndexedSeq)
          )
        )

    def genL2TxCommand(
        peerNum: HeadPeerNumber,
        interArrivalDelay: FiniteDuration,
        txStrategy: TxStrategy,
        txMutator: TxMutator,
        // Optional inline datum stamped on l2-bound (flag-2) outputs — e.g. the RBR MBT passes the
        // "evacuation" sentinel so its RBRClassifier buckets L2-tx-derived evacuation outputs on L1,
        // mirroring `genRegisterDepositCommand`'s `l2OutputDatum`.
        l2OutputDatum: Option[scalus.cardano.ledger.DatumOption] = None,
        // Optional inline datum stamped on withdrawal (flag-1, l1-bound) outputs specifically — the
        // RBR MBT passes a distinct "withdrawal" sentinel so its RBRClassifier buckets withdrawals
        // separately from evacuation outputs on L1. Defaults to None (a plain stage4 run stamps
        // nothing, matching its l2-bound outputs).
        withdrawalDatum: Option[scalus.cardano.ledger.DatumOption] = None,
        // Optional address for withdrawal outputs; defaults to the normal in-use L2 address
        // selection. The RBR MBT pins this to a script address so a withdrawn output on L1 can't be
        // re-selected as a peer's fee/collateral (which would drop it from the L1 withdrawal count).
        withdrawalAddress: Option[ShelleyAddress] = None,
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
                // Per-output l1/l2 flags first, so each output's datum + address reflect its fate: a
                // flag-1 (withdrawal) output carries `withdrawalDatum` at `withdrawalAddress`, a
                // flag-2 (stay-on-L2) output carries `l2OutputDatum` at an in-use L2 address. The
                // same flags drive the CIP67 metadata, keeping designation and datum consistent.
                flags <- genOutputFlags(outputValues.size, txStrategy)
                outputs <- Gen.sequence[List[TransactionOutput], TransactionOutput](
                  outputValues.zip(flags).map { (v, flag) =>
                      if flag == 1 then
                          withdrawalAddress match {
                              case Some(a) =>
                                  Gen.const(Babbage(a, v, datumOption = withdrawalDatum))
                              case None =>
                                  Gen.oneOf(l2AddressesInUse.toSeq)
                                      .map(a => Babbage(a, v, datumOption = withdrawalDatum))
                          }
                      else
                          Gen.oneOf(l2AddressesInUse.toSeq)
                              .map(a => Babbage(a, v, datumOption = l2OutputDatum))
                  }
                )

                auxiliaryData = Some(headFlagsMetadata(flags))

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

            } yield Some(
              L2TxCommand(
                peerNum = peerNum,
                request = UserRequestWithId.TransactionRequest(
                  requestId = state.nextRequestId(peerNum),
                  request = UserRequest.TransactionRequest(
                    body = body.asInstanceOf[TransactionRequestBody]
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
        // Optional inline datum stamped on each L2 output — e.g. the RBR MBT passes the "evacuation"
        // sentinel so its RBRClassifier can bucket deposit-derived evacuation outputs on L1.
        l2OutputDatum: Option[scalus.cardano.ledger.DatumOption] = None,
        // Optional override for the L2 output address; defaults to the depositing peer's address.
        // The RBR MBT pins this to a dedicated script address so evacuation outputs can never be
        // reselected as a peer's fee/collateral (which would consume them and drop the L1 count).
        l2OutputAddress: Option[ShelleyAddress] = None,
        // Deposit validity window: the deposit becomes absorbable only at
        // `validityEnd + depositSubmission + depositMaturity`, i.e. ~this duration after submission.
        // The RBR MBT shortens it so deposits absorb within its commit window instead of taking the
        // refund path.
        depositValidityDuration: FiniteDuration = 2.minutes,
    )(state: ModelState): Gen[Option[RegisterAndSubmitDepositCommand]] = {
        val config = state.params.multiNodeConfig
        val peerAddress = config.addressOf(peerNum)
        val outputAddress = l2OutputAddress.getOrElse(peerAddress)
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
                                                  Gen.const(outputAddress)
                                                      .map(a =>
                                                          Babbage(a, v, datumOption = l2OutputDatum)
                                                      )
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
                                            (submissionTime + depositValidityDuration).getEpochSecond
                                          )
                                        )

                                        l2PayloadSerialized = GenesisObligation.serialize(l2Outputs)

                                        depositRefundSeq = DepositRefundTxSeq
                                            .Build(
                                              l2Payload = l2PayloadSerialized,
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
                                            body = body.asInstanceOf[DepositRequestBody]
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

/** The reusable pre-fallback "setup phase" generator. This *is* stage4's scenario generator,
  * factored to be generic over the SUT type `S` so downstream suites (the RBR MBT) drive the
  * identical multisig regime — same Poisson superposition, same command mix, same absorption
  * handling — and inherit any new path stage4 grows here for free. Only the [[Config]] knobs
  * differ.
  *
  * Generic over `S`: the picker assembles `AnyCommand`s from the shared model-side givens
  * (`ModelCommand`/`CommandProp`/`CommandLabel`, all keyed on `ModelState`) plus the per-SUT
  * `SutCommand` instances the caller supplies for each command type.
  */
object SetupScenarioGen:

    /** Downstream-specific output stamping / deposit timing. The defaults reproduce stage4 exactly.
      */
    final case class Config(
        // Inline datum stamped on l2-bound (flag-2) L2 outputs and on deposit outputs. The RBR MBT
        // passes its "evacuation" sentinel so `beta` can bucket the eventual L1 evacuation outputs;
        // stage4 leaves it unset.
        l2OutputDatum: Option[DatumOption] = None,
        // Optional pin for deposit L2 output addresses; defaults to the depositing peer's address.
        l2OutputAddress: Option[ShelleyAddress] = None,
        // Inline datum stamped on withdrawal (flag-1) L2 outputs. The RBR MBT passes a distinct
        // "withdrawal" sentinel so `beta` buckets withdrawals separately from evacuation outputs
        // (the committed `EvacuationMap`, and hence `N`, is flag-2 only — withdrawals are separate
        // payout obligations). stage4 leaves it unset.
        withdrawalDatum: Option[DatumOption] = None,
        // Optional pin for withdrawal (flag-1) output addresses; defaults to the in-use L2 address
        // selection. The RBR MBT pins this to a script address so a withdrawn output on L1 can't be
        // re-selected as a peer's fee/collateral (which would drop it from the L1 withdrawal count).
        withdrawalAddress: Option[ShelleyAddress] = None,
        // Deposit validity window. The RBR MBT shortens it so deposits absorb within its commit
        // window instead of taking the refund path; stage4 keeps the 2-minute default.
        depositValidityDuration: FiniteDuration = 2.minutes,
        // L2-tx strategy mix; the default exercises stage4's full spread, including
        // `RandomWithdrawals`. A downstream suite inherits it as-is provided it can account for
        // withdrawals (the RBR MBT does, via `withdrawalDatum` + its `WithdrawalOutput` place).
        l2TxStrategies: Gen[TxStrategy] = Gen.frequency(
          5 -> Gen.const(TxStrategy.Regular),
          2 -> Gen.const(TxStrategy.RandomWithdrawals),
          // TODO: Too slow on my machine dure to KZG-commitments
          // 2 -> Gen.const(TxStrategy.Dust(50)),
          1 -> Gen.const(TxStrategy.Arbitrary),
        ),
    )

    /** Single global Poisson superposition: sample `(peer, gap-since-last-event)` — each peer's
      * marginal stream is exactly Poisson at its configured rate, so peers with smaller mean
      * inter-arrival are picked proportionally more often and the global rate is Σλ_p — then a
      * command for that peer.
      */
    def genNextCommand[S](state: ModelState, config: Config)(using
        SutCommand[DelayCommand, Unit, S],
        SutCommand[L2TxCommand, ValidityFlag, S],
        SutCommand[RegisterAndSubmitDepositCommand, ValidityFlag, S],
    ): Gen[AnyCommand[ModelState, S]] =
        for {
            (peerNum, interArrivalDelay) <- CommandGenerators.genSuperposedNextEvent(
              state.params.meanInterArrivalTimes
            )
            cmd <- genCommandForPeer(peerNum, interArrivalDelay, state, config)
        } yield cmd

    private def genCommandForPeer[S](
        peerNum: HeadPeerNumber,
        interArrivalDelay: FiniteDuration,
        state: ModelState,
        config: Config,
    )(using
        SutCommand[DelayCommand, Unit, S],
        SutCommand[L2TxCommand, ValidityFlag, S],
        SutCommand[RegisterAndSubmitDepositCommand, ValidityFlag, S],
    ): Gen[AnyCommand[ModelState, S]] = {
        val mnc = state.params.multiNodeConfig
        val peerAddress = mnc.addressOf(peerNum)
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
            CommandGenerators
                .genDelayForAbsorption(peerNum, state)
                .map(AnyCommand.apply[DelayCommand, Unit, ModelState, S](_))
        else if ownedL2Utxos.isEmpty && availableL1.isEmpty then Gen.const(noOp[ModelState, S])
        else {
            val genL2TxOpt: Gen[Option[AnyCommand[ModelState, S]]] =
                if ownedL2Utxos.isEmpty then Gen.const(None)
                else
                    config.l2TxStrategies.flatMap(strategy =>
                        CommandGenerators
                            .genL2TxCommand(
                              peerNum,
                              interArrivalDelay,
                              strategy,
                              TxMutator.Identity,
                              l2OutputDatum = config.l2OutputDatum,
                              withdrawalDatum = config.withdrawalDatum,
                              withdrawalAddress = config.withdrawalAddress,
                            )(state)
                            .map(
                              _.map(AnyCommand.apply[L2TxCommand, ValidityFlag, ModelState, S](_))
                            )
                    )

            val genDepositOpt: Gen[Option[AnyCommand[ModelState, S]]] =
                if availableL1.isEmpty then Gen.const(None)
                else
                    CommandGenerators
                        .genRegisterDepositCommand(
                          peerNum,
                          interArrivalDelay,
                          l2OutputDatum = config.l2OutputDatum,
                          l2OutputAddress = config.l2OutputAddress,
                          depositValidityDuration = config.depositValidityDuration,
                        )(state)
                        .map(
                          _.map(
                            AnyCommand
                                .apply[
                                  RegisterAndSubmitDepositCommand,
                                  ValidityFlag,
                                  ModelState,
                                  S
                                ](
                                  _
                                )
                          )
                        )

            Gen.frequency(
              10 -> genL2TxOpt,
              3 -> genDepositOpt,
            ).map(_.getOrElse(noOp[ModelState, S]))
        }
    }

end SetupScenarioGen

object Stage4ScenarioGen extends ScenarioGen[ModelState, Stage4Sut]:

    private def pick[A](gen: Gen[A])(using pp: A => Pretty): PropertyM[IO, A] =
        PropertyM.pick[IO, A](gen)

    override def genNextCommand(
        state: ModelState
    ): PropertyM[IO, AnyCommand[ModelState, Stage4Sut]] =
        pick(SetupScenarioGen.genNextCommand(state, SetupScenarioGen.Config()))
