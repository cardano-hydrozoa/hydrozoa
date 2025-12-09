package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import scala.Function.const
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.{PlutusScriptEvaluator, Transaction, Utxo}
import scalus.cardano.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.{Environment, SomeBuildError, TransactionBuilder}
import sourcecode.*

trait Tx {
    def tx: Transaction
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
