package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.*
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.{Metadata as _, *}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos

final case class InitializationTxSeq(initializationTx: InitializationTx, fallbackTx: FallbackTx)

object InitializationTxSeq {
    type Config = HeadConfig.Preinit.Section

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
      * @param config
      * @param transactionSequence
      * @param resolvedUtxos
      * @return
      */
    def parse(
        config: Config,
        transactionSequence: (Transaction, Transaction),
        resolvedUtxos: ResolvedUtxos,
    ): Either[ParseError, InitializationTxSeq] = {
        given ProtocolVersion = config.cardanoProtocolVersion

        val initializationTx = transactionSequence._1
        val fallbackTx = transactionSequence._2

        for {
            iTx <- InitializationTx
                .parse(
                  config = config,
                  tx = initializationTx,
                  resolvedUtxos = resolvedUtxos
                )
                .left
                .map(InitializationTxParseError(_))

            fallbackValidityStartSlot <- fallbackTx.body.value.validityStartSlot
                .toRight(FallbackTxValidityStartIsMissing)
                .map(Slot.apply)

            expectedFallbackTx <- FallbackTx
                .build(
                  config = config,
                  validityStartTime = config.txTiming.newFallbackStartTime(config.headStartTime),
                  treasuryUtxoSpent = iTx.treasuryProduced,
                  multisigRegimeUtxo = iTx.multisigRegimeUtxo
                )
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
                (iTx.validityEnd + config.txTiming.silenceDuration).toSlot
            // ^ preview       ^ mainnet
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

        def build(config: Config): Either[Error, InitializationTxSeq] = {
            // ===================================
            // Head Native Script
            // ===================================

            // Construct head native script directly from the list of peers
            val hms = HeadMultisigScript(config.headPeerVKeys)

            // ===================================
            // Init Tx
            // ===================================
            for {
                initializationTx <- InitializationTx
                    .build(config)
                    .left
                    .map(InitializationTxError(_))

                fallbackTx <- FallbackTx
                    .build(
                      config = config,
                      validityStartTime =
                          config.txTiming.newFallbackStartTime(config.headStartTime),
                      treasuryUtxoSpent = initializationTx.treasuryProduced,
                      multisigRegimeUtxo = initializationTx.multisigRegimeUtxo,
                    )
                    .left
                    .map(FallbackTxError(_))

            } yield InitializationTxSeq(initializationTx, fallbackTx)
        }

        // TODO: Make the individual builders actually throw these (typed) errors
        sealed trait Error extends Throwable

        case class FallbackPRError(e: SomeBuildError) extends Error
        case class InitializationTxError(e: SomeBuildError) extends Error
        case class FallbackTxError(e: SomeBuildError) extends Error
    }
}
