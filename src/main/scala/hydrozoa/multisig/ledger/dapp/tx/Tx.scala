package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import monocle.Lens
import scala.Function.const
import scalus.cardano.ledger.Transaction
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{AllInputsMustBeInUtxoValidator, EmptyInputsValidator, FeesOkValidator, InputsAndReferenceInputsDisjointValidator, MissingOrExtraScriptHashesValidator, OutputsHaveNotEnoughCoinsValidator, OutputsHaveTooBigValueStorageSizeValidator, OutsideForecastValidator, OutsideValidityIntervalValidator, TransactionSizeValidator, ValueNotConservedUTxOValidator}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.{SomeBuildError, TransactionBuilder}
import sourcecode.*

trait Tx[Self <: Tx[Self]] extends HasResolvedUtxos { self: Self =>
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
    object Validators {

        val nonSigningValidators: Seq[Validator] =
            // These validators are all the ones from the CardanoMutator that could be checked on an unsigned transaction
            List(
              EmptyInputsValidator,
              InputsAndReferenceInputsDisjointValidator,
              AllInputsMustBeInUtxoValidator,
              ValueNotConservedUTxOValidator,
              // VerifiedSignaturesInWitnessesValidator,
              // MissingKeyHashesValidator
              MissingOrExtraScriptHashesValidator,
              TransactionSizeValidator,
              FeesOkValidator,
              OutputsHaveNotEnoughCoinsValidator,
              OutputsHaveTooBigValueStorageSizeValidator,
              OutsideValidityIntervalValidator,
              OutsideForecastValidator
            )

        val nonSigningNonValidityChecksValidators: Seq[Validator] = nonSigningValidators
            .filterNot(_.isInstanceOf[OutsideValidityIntervalValidator.type])
    }

    type Serialized = Array[Byte]

    /** A result that includes additional information besides the built transaction.
      *
      * @tparam T
      *   The type of built transaction.
      */
    trait AugmentedResult[T] {
        def transaction: T
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
