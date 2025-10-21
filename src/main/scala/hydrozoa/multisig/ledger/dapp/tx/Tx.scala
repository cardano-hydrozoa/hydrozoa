package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.lib.tx.{SomeBuildError, TransactionBuilder, TransactionUnspentOutput}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.Transaction
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.Environment
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler

trait Tx {
    def tx: Transaction
}

object Tx {
    /** A result that includes additional information besides the built transaction.
      * 
      * @tparam T The type of built transaction.
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
                    evaluator = config.env.evaluator,
                    validators = config.validators
                )
    }

    object Builder {
        type BuildErrorOr[A] = Either[SomeBuildError, A]
        
        trait HasCtx {
            def ctx: TransactionBuilder.Context
        }
        
        final case class Config(
            headNativeScript: HeadMultisigScript,
            headNativeScriptReferenceInput: TransactionUnspentOutput,
            env: Environment,
            validators: Seq[Validator]
        ) {
            lazy val headAddress: ShelleyAddress = headNativeScript.mkAddress(env.network)
        }

        object Incremental {
            /** Replace an [[InvalidTransactionSizeException]] with some other value.
              *
              * @param err
              * The error to replace.
              * @param replacement
              * The replacement value, provided as a lazy argument.
              * @tparam A
              * The type of the replacement value, usually inferred by Scala.
              * @return
              */
            final def replaceInvalidSizeException[A](
                err: SomeBuildError,
                replacement: => A
            ): Either[SomeBuildError, A] = {
                err match
                    case SomeBuildError.ValidationError(ve) =>
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
