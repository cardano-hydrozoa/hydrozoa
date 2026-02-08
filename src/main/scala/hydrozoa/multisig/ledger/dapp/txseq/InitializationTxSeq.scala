package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.*
import hydrozoa.multisig.ledger.dapp.tx.{Metadata as _, *}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

final case class InitializationTxSeq(initializationTx: InitializationTx, fallbackTx: FallbackTx)

object InitializationTxSeq {
    export InitializationTxSeqOps.{Build, Parse}
}

private object InitializationTxSeqOps {
    type Config = HeadConfig.Preinit.Section

    object Build {
        enum Error extends Throwable {
            case FallbackPRError(e: SomeBuildError)
            case InitializationTxError(e: (SomeBuildError, String))
            case FallbackTxError(e: SomeBuildError)
        }
    }

    final case class Build(config: Config) {
        import Build.*
        import Build.Error.*

        def result: Either[Error, InitializationTxSeq] = for {
            initializationTx <- InitializationTx
                .Build(config)
                .result
                .left
                .map(InitializationTxError(_))

            fallbackTx <- FallbackTx
                .Build(config)(
                  config.txTiming.newFallbackStartTime(config.headStartTime),
                  initializationTx.treasuryProduced,
                  initializationTx.multisigRegimeProduced,
                )
                .result
                .left
                .map(FallbackTxError(_))
        } yield InitializationTxSeq(initializationTx, fallbackTx)
    }

    object Parse {
        type ParseErrorOr[A] = Either[Error, A]

        enum Error {
            case InitializationTxParseError(wrapped: InitializationTx.Parse.Error)
            case FallbackTxBuildError(wrapped: SomeBuildError)
            case FallbackTxMismatch(expected: FallbackTx, actual: Transaction)
            case FallbackTxValidityStartIsMissing
            case FallbackTxValidityStartError(
                lowerPossible: Slot,
                upperPossible: Slot,
                actual: Slot
            )
            case TTLValidityStartGapError(difference: Slot, actual: Slot)
        }

    }

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
      * @param config
      * @param transactionSequence
      * @param resolvedUtxos
      * @return
      */
    final case class Parse(config: Config)(
        transactionSequence: (Transaction, Transaction),
        resolvedUtxos: ResolvedUtxos
    ) {
        import Parse.*
        import Parse.Error.*

        def result: ParseErrorOr[InitializationTxSeq] = {

            val initializationTx = transactionSequence._1
            val fallbackTx = transactionSequence._2

            for {
                iTx <- InitializationTx
                    .Parse(config = config)(
                      tx = initializationTx,
                      resolvedUtxos = resolvedUtxos
                    )
                    .result
                    .left
                    .map(InitializationTxParseError(_))

                expectedFallbackValidityStart: Slot =
                    (iTx.validityEnd + config.txTiming.silenceDuration).toSlot

                fallbackValidityStartSlot <- fallbackTx.body.value.validityStartSlot
                    .toRight(FallbackTxValidityStartIsMissing)
                    .map(Slot.apply)

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

                expectedFallbackTx <- FallbackTx
                    .Build(config)(
                      config.txTiming.newFallbackStartTime(config.headStartTime),
                      iTx.treasuryProduced,
                      iTx.multisigRegimeProduced
                    )
                    .result
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
            } yield InitializationTxSeq(initializationTx = iTx, fallbackTx = expectedFallbackTx)
        }
    }
}
