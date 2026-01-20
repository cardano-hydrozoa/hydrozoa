package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.config.HeadConfig.Fields.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import monocle.Lens
import scala.Function.const
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.Transaction
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.{ChangeOutputDiffHandler, SomeBuildError, TransactionBuilder}
import sourcecode.*

trait Tx[Self <: Tx[Self]] { self: Self =>
    def tx: Transaction

    /** Lens for accessing the transaction field. Implementations should use:
      * `override val txLens: Lens[ConcreteType, Transaction] = Focus[ConcreteType](_.tx)`
      * Unfortunately this can't be generalized since Focus requires a concrete type.
      */
    def txLens: Lens[Self, Transaction]

    /** This excludes the lens from equality. */
    override def equals(obj: Any): Boolean = obj match {
        case that: Tx[?] =>
            this.tx == that.tx
        case _ => false
    }
}

trait HasValidityStart:
    def validityStart: QuantizedInstant

trait HasValidityEnd {
    def validityEnd: QuantizedInstant
}

object Tx {
    type Serialized = Array[Byte]

    /** A result that includes additional information besides the built transaction.
      *
      * @tparam T
      *   The type of built transaction.
      */
    trait AugmentedResult[T] {
        def transaction: T
    }

    trait Builder {
        def config: Tx.Builder.Config

        extension (txbc: Tx.Builder.Config) {
            def headAddress: ShelleyAddress =
                txbc.headMultisigScript.mkAddress(txbc.cardanoInfo.network)
        }

        final def finish(
            txBuilderContext: TransactionBuilder.Context
        ): Either[SomeBuildError, TransactionBuilder.Context] =
            // Try to build, balance, and validate the resulting transaction
            txBuilderContext
                .finalizeContext(
                  protocolParams = config.cardanoInfo.protocolParams,
                  diffHandler = ChangeOutputDiffHandler(
                    protocolParams = config.cardanoInfo.protocolParams,
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

        type Config =
            HasHeadMultisigScript & HasMultisigRegimeUtxo &
                // FIXME: TokenNames can be derived from the other fields, but I'm not
                // sure of the best way to make it accessible given what we're coming from
                HasTokenNames & HasCardanoInfo &
                // NOTE: I'm debating whether or not HasValidators should be part of the config.
                // We don't necessarily want the validators passed around the same to each transaction, but everything else
                // in this config DOES stay static.
                HasValidators & HasEvaluator &
                // FIXME: Head address can be derived from the other fields, but I'm not sure
                // the best way to make it accessible given what we're coming from
                HasHeadAddress

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

trait HasValidators {
    def validators: Seq[Validator]
}
