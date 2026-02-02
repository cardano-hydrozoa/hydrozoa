package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.{Metadata as _, *}
import hydrozoa.multisig.ledger.dapp.utxo.MultisigTreasuryUtxo
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.dapp.state.VoteDatum as VD
import hydrozoa.{VerificationKeyBytes, ensureMinAda, maxNonPlutusTxFee, given}
import scala.collection.immutable.SortedMap
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.address.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.ledger.api.v1.PubKeyHash

final case class InitializationTxSeq(initializationTx: InitializationTx, fallbackTx: FallbackTx)

object InitializationTxSeq {
    case class Config(
        tallyFeeAllowance: Coin,
        votingDuration: QuantizedFiniteDuration,
        cardanoInfo: CardanoInfo,
        peerKeys: NonEmptyList[VerificationKeyBytes],
        startTime: QuantizedInstant,
        txTiming: TxTiming
    ) {
        def evaluator: PlutusScriptEvaluator =
            PlutusScriptEvaluator(cardanoInfo, EvaluateAndComputeCost)
        def headMultisigScript = HeadMultisigScript(peerKeys)
    }

    sealed trait ParseError
    case class InitializationTxParseError(wrapped: InitializationTx.ParseError) extends ParseError
    case class FallbackTxBuildError(wrapped: SomeBuildError) extends ParseError
    case class FallbackTxMismatch(expected: FallbackTx, actual: Transaction) extends ParseError
    case object FallbackTxValidityStartIsMissing extends ParseError
    case class FallbackTxValidityStartError(lowerPossible: Slot, upperPossible: Slot, actual: Slot)
        extends ParseError
    case class TTLValidityStartGapError(difference: Slot, actual: Slot) extends ParseError

    /** Given two transaction that should form a valid Initialization-Fallback Transaction Sequence,
      * we:
      *   - Parse the first transaction as an initialization transaction
      *   - Use the result to build a fallback transaction
      *   - Compare the second transaction given to the constructed fallback transaction. If they
      *     don't match exactly, we error.
      *
      * Note that the parsing of the initialization transaction isn't currently guaranteed to be
      * secure. We are parsing primarily to ensure that the given transaction won't result in a head
      * that will immediately crash.
      *
      * @param transactionSequence
      * @param expectedNetwork
      * @param peerKeys
      * @param expectedTallyFeeAllowance
      * @param expectedVotingDuration
      * @param env
      * @param evaluator
      * @param validators
      * @param resolvedUtxos
      * @return
      */
    def parse(
        transactionSequence: (Transaction, Transaction),
        tallyFeeAllowance: Coin,
        votingDuration: QuantizedFiniteDuration,
        cardanoInfo: CardanoInfo,
        peerKeys: NonEmptyList[VerificationKeyBytes],
        startTime: QuantizedInstant,
        txTiming: TxTiming,
        resolvedUtxos: ResolvedUtxos,
    ): Either[ParseError, InitializationTxSeq] = {

        val initializationTx = transactionSequence._1
        val fallbackTx = transactionSequence._2

        for {
            iTx <- InitializationTx
                .parse(
                  peerKeys = peerKeys,
                  cardanoInfo = cardanoInfo,
                  txTiming = txTiming,
                  startTime = startTime,
                  tx = initializationTx,
                  resolvedUtxos = resolvedUtxos
                )
                .left
                .map(InitializationTxParseError(_))

            fallbackValidityStartSlot <- fallbackTx.body.value.validityStartSlot
                .toRight(FallbackTxValidityStartIsMissing)
                .map(Slot.apply)

            ftxConfig = FallbackTx.Config(
              headMultisigScript = HeadMultisigScript(peerKeys),
              multisigRegimeUtxo = iTx.multisigRegimeUtxo,
              tokenNames = iTx.tokenNames,
              tallyFeeAllowance = tallyFeeAllowance,
              cardanoInfo = cardanoInfo,
              votingDuration = votingDuration
            )

            ftxRecipe = FallbackTx.Recipe(
              treasuryUtxoSpent = iTx.treasuryProduced,
              // Time checks are done for the whole sequence later on, see down below.
              validityStart = fallbackValidityStartSlot
            )
            expectedFallbackTx <- FallbackTx
                .build(ftxConfig, ftxRecipe)
                .left
                .map(FallbackTxBuildError(_))

            _ <-
                if expectedFallbackTx.tx == fallbackTx then Right(())
                else
                    Left(
                      FallbackTxMismatch(
                        expected = expectedFallbackTx,
                        actual = fallbackTx
                      )
                    )

            // Check validity ranges are correct and match each other
            // Silence period is respected: fallbackTx.validityStart -initializationTx.ttl > txTiming.
            expectedFallbackValidityStart: Slot =
                (iTx.validityEnd + txTiming.silenceDuration).toSlot

            // TODO: Should this be in the fallback parser?
            _ <-
                if fallbackValidityStartSlot == expectedFallbackValidityStart
                then Right(())
                else
                    Left(
                      TTLValidityStartGapError(
                        expectedFallbackValidityStart,
                        fallbackValidityStartSlot
                      )
                    )

        } yield InitializationTxSeq(initializationTx = iTx, fallbackTx = expectedFallbackTx)
    }

    /* OUTLINE:
         The HMRW utxo is of a fixed size -- it has no datum and a fixed amount of tokens.
         However, those tokens depend on the size of the vote utxos and collateral utxos that are in the fallback
         transaction.

         Thus, there is a pseudo-dependency for building the initialization transaction -- we want to know the
         size of the outputs of the fallback tx, but we don't actually need to construct the full fallback transaction.
         We only need to construct its transaction outputs.

         The two options I thought of for doing this was:

         - (1) Make a fallback "partial result" that returns the ada amount needed for the HRMW utxo and a callback to
           finish the transaction.
         - (2) Duplicate a little bit of work and construct the utxos in this builder as well as in the fallback tx
           builder.

          I settled on (2) because it was the quickest to get running, while simplifying the fallback tx builder and
           allowing it to be reused in the settlement tx sequence builder with identical semantics (just pass in the
           treasury).
     */
    object Builder {

        def build(args: Args, config: Config): Either[Error, InitializationTxSeq] = {
            val tokenNames = CIP67.HeadTokenNames(args.spentUtxos.seedUtxo.input)
            val disputeResolutionAddress = ShelleyAddress(
              network = config.cardanoInfo.network,
              payment = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash),
              delegation = Null
            )

            import tokenNames.*

            // ===================================
            // Head Native Script
            // ===================================

            // Construct head native script directly from the list of peers
            val hms = HeadMultisigScript(config.peerKeys)

            // ===================================
            // Init Treasury
            // ===================================
            val initTreasuryDatum = MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum

            // ===================================
            // Vote Utxos
            // ===================================
            def mkVoteToken(amount: Long): MultiAsset = MultiAsset(
              SortedMap(
                (
                  hms.policyId,
                  SortedMap((voteTokenName, amount))
                )
              )
            )

            def mkVoteUtxo(datum: Data): TransactionOutput = Babbage(
              address = disputeResolutionAddress,
              value = Value(config.tallyFeeAllowance, mkVoteToken(1)),
              datumOption = Some(Inline(datum)),
              scriptRef = None
            ).ensureMinAda(config.cardanoInfo.protocolParams)

            val initDefaultVoteUtxo: TransactionOutput = mkVoteUtxo(
              VD.default(initTreasuryDatum.commit).toData
            )

            val peerVoteUtxos: NonEmptyList[TransactionOutput] = {
                val datums = VD(
                  NonEmptyList.fromListUnsafe(
                    hms.requiredSigners.map(x => PubKeyHash(x.hash)).toList
                  )
                )
                datums.map(datum => mkVoteUtxo(datum.toData))
            }

            // ===================================
            // Collateral utxos
            // ===================================

            def collateralUtxos: NonEmptyList[TransactionOutput] = {
                NonEmptyList.fromListUnsafe(
                  hms.requiredSigners
                      .map(es =>
                          Babbage(
                            address = ShelleyAddress(
                              network = config.cardanoInfo.network,
                              payment = ShelleyPaymentPart.Key(es.hash),
                              delegation = Null
                            ),
                            value = Value(config.tallyFeeAllowance),
                            datumOption = None,
                            scriptRef = None
                          ).ensureMinAda(config.cardanoInfo.protocolParams)
                      )
                      .toList
                )
            }

            // ===================================
            // Validity ranges
            // ===================================
            val initializationTxValidityEnd =
                config.startTime + config.txTiming.minSettlementDuration +
                    config.txTiming.inactivityMarginDuration

            val fallbackTxValidityStart =
                initializationTxValidityEnd + config.txTiming.silenceDuration

            // ===================================
            // Init Tx
            // ===================================

            val multisigRegimeWitnessCoin: Coin =
                Coin(
                  maxNonPlutusTxFee(config.cardanoInfo.protocolParams).value
                      + initDefaultVoteUtxo.value.coin.value
                      + peerVoteUtxos.map(_.value.coin.value).toList.sum
                      + collateralUtxos.map(_.value.coin.value).toList.sum
                )

            val initializationTxConfig = InitializationTx.Config(
              peerKeys = config.peerKeys,
              cardanoInfo = config.cardanoInfo,
              txTiming = config.txTiming,
              startTime = config.startTime,
              tokenNames = tokenNames
            )

            val initializationTxRecipe = InitializationTx.Recipe(
              hmrwCoin = multisigRegimeWitnessCoin,
              spentUtxos = args.spentUtxos,
              initialTreasury = args.initialTreasury,
              changePP = args.initializationTxChangePP
            )

            for {
                initializationTx <- InitializationTx
                    .build(initializationTxConfig, initializationTxRecipe)
                    .left
                    .map(InitializationTxError(_))

                fallbackConfig = FallbackTx.Config(
                  headMultisigScript = hms,
                  tallyFeeAllowance = config.tallyFeeAllowance,
                  multisigRegimeUtxo = initializationTx.multisigRegimeUtxo,
                  tokenNames = initializationTx.tokenNames,
                  cardanoInfo = config.cardanoInfo,
                  votingDuration = config.votingDuration
                )

                fallbackTxRecipe = FallbackTx.Recipe(
                  treasuryUtxoSpent = initializationTx.treasuryProduced,
                  validityStart = fallbackTxValidityStart.toSlot
                )

                fallbackTx <- FallbackTx
                    .build(fallbackConfig, fallbackTxRecipe)
                    .left
                    .map(FallbackTxError(_))

            } yield InitializationTxSeq(initializationTx, fallbackTx)
        }

        // TODO: Make the individual builders actually throw these (typed) errors
        sealed trait Error extends Throwable

        case class FallbackPRError(e: SomeBuildError) extends Error
        case class InitializationTxError(e: SomeBuildError) extends Error
        case class FallbackTxError(e: SomeBuildError) extends Error

        // TODO: this is getting cumbersome, review
        final case class Args(
            spentUtxos: InitializationTx.SpentUtxos,
            initialTreasury: Value,
            initializationTxChangePP: ShelleyPaymentPart,
        )
    }
}
