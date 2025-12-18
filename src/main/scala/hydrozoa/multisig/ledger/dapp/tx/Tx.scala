package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.PosixTime
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import scala.Function.const
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.{PlutusScriptEvaluator, Slot, Transaction, Utxo}
import scalus.cardano.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.{Environment, SomeBuildError, TransactionBuilder}
import sourcecode.*

trait Tx {
    def tx: Transaction
}

trait HasValidityStartSlot:
    def validityStart: Slot

trait HasTtlSlot {
    def ttl: Slot
}

// This is used for some Args.
// TODO: Find a better place for it.
trait HasTtlTime {
    def ttl: PosixTime
}

object Tx {

    /** A result that includes additional information besides the built transaction.
      *
      * @tparam T
      *   The type of built transaction.
      */
    trait AugmentedResult[T] {
        def transaction: T
    }

    trait Builder {
        def config: Builder.Config

        final def finish(
            txBuilderContext: TransactionBuilder.Context
        ): Either[SomeBuildError, TransactionBuilder.Context] =
            // Try to build, balance, and validate the resulting transaction
            txBuilderContext
                .finalizeContext(
                  protocolParams = config.env.protocolParams,
                  diffHandler = ChangeOutputDiffHandler(
                    protocolParams = config.env.protocolParams,
                    changeOutputIdx = 0
                  ).changeOutputDiffHandler,
                  evaluator = config.evaluator,
                  validators = config.validators
                )
    }

    object Builder {
        type BuildErrorOr[A] = Either[(SomeBuildError, String), A]

        extension [E, A](either: Either[E, A])
            /** Augment an Either with a string on the Left, including source locations. Defaults to
              * only providing source location.
              */
            def explain(
                mkString: E => String = ((_: E) => "")
            )(implicit line: Line, file: File, enclosing: Enclosing): Either[(E, String), A] =
                either.left.map(e =>
                    (e, s"[${file.value}:${line.value} in ${enclosing.value}] " + s"${mkString(e)}")
                )

            /** Like `explain`, but only taking a string */
            def explainConst(
                string: String
            )(implicit line: Line, file: File, enclosing: Enclosing): Either[(E, String), A] =
                either.explain(const(string))

        extension [E, A](augmentedEither: Either[(E, String), A])
            def explainReplace(
                string: String
            )(implicit line: Line, file: File, enclosing: Enclosing): Either[(E, String), A] = {
                val oldEither = augmentedEither.left.map(_._1)
                oldEither.explainConst(string)
            }

            def explainModify(
                modifyString: String => String
            )(implicit line: Line, file: File, enclosing: Enclosing): Either[(E, String), A] = {
                augmentedEither.left.map(t => (t._1, modifyString(t._2)))
            }

            def explainAppendConst(
                string: String
            )(implicit line: Line, file: File, enclosing: Enclosing): Either[(E, String), A] = {
                augmentedEither.explainModify(
                  _ + s";\n[${file.value}:${line.value} in ${enclosing.value}] $string"
                )
            }

        trait HasCtx {
            def ctx: TransactionBuilder.Context
        }

        final case class Config(
            headNativeScript: HeadMultisigScript,
            headNativeScriptReferenceInput: Utxo,
            tokenNames: TokenNames,
            env: Environment,
            evaluator: PlutusScriptEvaluator,
            validators: Seq[Validator]
        ) {
            lazy val headAddress: ShelleyAddress = headNativeScript.mkAddress(env.network)
        }

        object Incremental {

            /** Replace an [[InvalidTransactionSizeException]] with some other value.
              *
              * @param err
              *   The error to replace.
              * @param replacement
              *   The replacement value, provided as a lazy argument.
              * @tparam A
              *   The type of the replacement value, usually inferred by Scala.
              * @return
              */
            final def replaceInvalidSizeException[A](
                err: SomeBuildError,
                replacement: => A
            ): Either[SomeBuildError, A] = {
                err match
                    case SomeBuildError.ValidationError(ve, ctx) =>
                        ve match {
                            case _: InvalidTransactionSizeException =>
                                Right(replacement)
                            case _ => Left(err)
                        }
                    case _ => Left(err)
            }
        }
    }
}

trait HasResolvedUtxos {
    def resolvedUtxos: ResolvedUtxos
}

/** TODO: This should be derived from Hydrozoa parameters.
  *
  * TODO: move around?
  *
  * Peter and I determined that the settlement tx duration should be:
  *   - Long enough that the settlement tx is still unexpired whenever we need to resubmit it due to
  *     rollbacks.
  *   - Short enough that: We don't need to push forward the fallback start too frequently with
  *     empty major blocks.
  *   - Whenever we have too many deposits and some get deferred for absorption in future blocks, we
  *     aren't forced to decide to never-absorb them because they've been deferred for too long.
  *
  * Therefore, I think a reasonable settlement tx duration should be approximately the Cardano
  * security parameter (~12h on average), and the deposit absorption window should be longer (~24h).
  *
  * Other parameter values:
  *   - Deposit maturity duration ~= 30min to 1h,
  *   - Deposit expiry margin ~= 5 min
  *   - (Fallback N start - Settlement N TTL) ~= 24h
  *
  * The reason we measure time duration in real units is that slot length is different for different
  * networks.
  *
  * @param minSettlementDuration
  *   Minimal length of a settlement (finalization) tx's validity range.
  * @param majorBlockTimeout
  *   The minimal frequency of major blocks (in case no activity happens).
  * @param silencePeriod
  *   A fixed-time gap between concurrent txs (i.e. fallback N and settlement/finalization N+1) to
  *   prevent contention, typically a small value like 5 min.
  * @param initializationFallbackDeviation
  *   Since it doesn't make sense to ask users to specify exact the same TTL/validity start slot for
  *   txs in the initialization sequence that we may calculate based on the time the initialization
  *   request was received, we need to allow some deviation which is defined by that parameter. The
  *   rule is that the specified value in the txs should stay in the [calculatedTime -
  *   initializationFallbackDeviation; calculatedTime + initializationFallbackDeviation].
  */
final case class TxTiming(
    minSettlementDuration: FiniteDuration,
    majorBlockTimeout: FiniteDuration,
    silencePeriod: FiniteDuration,
    initializationFallbackDeviation: FiniteDuration
)

object TxTiming:
    val default = TxTiming(1.day, 2.hours, 5.minutes, 10.minutes)
