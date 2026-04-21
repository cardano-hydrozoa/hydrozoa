package hydrozoa.multisig.ledger.l1.tx

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.multisig.ledger.l1.tx.Tx.SignatureError.InvalidSignature
import monocle.{Focus, Lens}
import scala.Function.const
import scalus.cardano.ledger.TransactionException.InvalidTransactionSizeException
import scalus.cardano.ledger.TransactionWitnessSet.given
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{AllInputsMustBeInUtxoValidator, EmptyInputsValidator, FeesOkValidator, InputsAndReferenceInputsDisjointValidator, MissingOrExtraScriptHashesValidator, OutputsHaveNotEnoughCoinsValidator, OutputsHaveTooBigValueStorageSizeValidator, OutsideForecastValidator, OutsideValidityIntervalValidator, TransactionSizeValidator, ValueNotConservedUTxOValidator}
import scalus.cardano.ledger.{TaggedSortedSet, Transaction, TransactionWitnessSet, VKeyWitness}
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.{SomeBuildError, TransactionBuilder, keepRawL}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.{ByteString, platform}
import scalus.|>
import sourcecode.*

// FIXME: This trait and parts of the companion object are applicable to the rulebased regime.
//   Lets move it out
trait Tx[Self <: Tx[Self]] extends HasResolvedUtxos { self: Self =>

    /** A human-readable name, primarily for error reporting
      */
    // TODO: Is there a performant way to do this automatically?
    def transactionFamily: String

    def tx: Transaction

    /** Lens for accessing the transaction field. Implementations should use:
      * `override val txLens: Lens[ConcreteType, Transaction] = Focus[ConcreteType](_.tx)`
      * Unfortunately this can't be generalized since Focus requires a concrete type.
      */
    protected def txLens: Lens[Self, Transaction]

    /** This excludes the lens from equality. */
    override def equals(obj: Any): Boolean = obj match {
        case that: Tx[?] =>
            this.tx == that.tx
        case _ => false
    }

    /** @return
      *   - Invalid[SignatureError.InvalidSignature] if the verification key and signing key don't
      *     match
      *   - The signed `Tx` otherwise
      */
    final def signTx(
        signingKey: SigningKey,
        verificationKey: VerificationKey
    ): Validated[InvalidSignature[Self], Self] = {

        val signature = platform.signEd25519(signingKey, tx.id)
        addSignatures(Set(VKeyWitness(vkey = verificationKey, signature = signature))).leftMap(
          nel => nel.head
        )
    }

    /** Validates and adds the given witnesses to the transaction
      * @param vkw
      * @return
      *   - Invalid[NonEmptyList[SignatureError.InvalidSignature]] if any signatures (including
      *     existing signatures) are incorrect
      *   - Valid[Transaction] with the signatures applied otherwise
      */
    final def addSignatures(
        vkw: Set[VKeyWitness]
    ): ValidatedNel[InvalidSignature[Self], Self] =
        val signed = this |> txLens
            .andThen(Focus[Transaction](_.witnessSetRaw))
            .andThen(keepRawL())
            .andThen(Focus[TransactionWitnessSet](_.vkeyWitnesses))
            .modify((tss: TaggedSortedSet[VKeyWitness]) => TaggedSortedSet(tss.toSet ++ vkw))

        signed.validateSignatures match {
            case Valid(_)   => Valid(signed)
            case Invalid(e) => Invalid(e)
        }

    /** Checks the transaction to ensure that all signatures in VKeyWitnesses are valid
      */
    final def validateSignatures: ValidatedNel[InvalidSignature[Self], Unit] =
        tx.witnessSetRaw.value.vkeyWitnesses.toSet.filter(witness =>
            !platform.verifyEd25519Signature(witness.vkey, tx.id, witness.signature)
        ) match {
            case invalidWitnesses if invalidWitnesses.nonEmpty =>
                Invalid(
                  NonEmptyList.fromListUnsafe(
                    invalidWitnesses.toList.map(Tx.SignatureError.InvalidSignature(_, self))
                  )
                )
            case _ => Valid(())
        }

    /** Checks that:
      *   - The given transaction's body matches this transaction's body.
      *   - The given transaction is properly multisigned by all head peers.
      *   - Checks all signatures (including ones not from the head peers -- eventually this will
      *     include the coil peers)
      * Then:
      *   - Takes all the given transaction's signatures and applies it to this Tx[A]
      */
    final def validateAndAddMultiSignatures(
        headPeers: HeadPeers,
        otherTx: Transaction
    ): ValidatedNel[Tx.SignatureError[Self], Self] = {
        // The Transaction type isn't very type safe... I think it's best to compare the bodies directly rather
        // than just hashes.
        val bodiesMatch =
            if tx.body == otherTx.body
            then Valid(this)
            else Invalid(NonEmptyList.one(Tx.SignatureError.TransactionBodyMismatch(otherTx, self)))

        val containsHeadPeerWitnesses = {
            val witnessVKeys = otherTx.witnessSetRaw.value.vkeyWitnesses.toSet.map(_.vkey)
            val errors: List[Tx.SignatureError[Self]] = headPeers.headPeerVKeys
                .filter(vKey => !witnessVKeys.contains(vKey))
                .map(Tx.SignatureError.MissingSignature(_, self))

            if errors.isEmpty
            then Valid(this)
            else Invalid(NonEmptyList.fromListUnsafe(errors))
        }

        val addedSignatures = addSignatures(otherTx.witnessSetRaw.value.vkeyWitnesses.toSet)

        // It's a little bit clunky, but order matters here. We are right-biased in the fold, so addedSignatures must
        // come last
        List(bodiesMatch, containsHeadPeerWitnesses, addedSignatures).foldLeft(
          Valid(this): ValidatedNel[Tx.SignatureError[Self], Self]
        ) {
            case (Valid(v1), Valid(v2))     => Valid(v2)
            case (Valid(_), e @ Invalid(_)) => e
            case (e @ Invalid(_), Valid(_)) => e
            case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2.toList)
        }

    }

}

object Tx {
    enum SignatureError[T <: Tx[T]] extends Throwable:
        case MissingSignature(vkey: VerificationKey, tx: T)
        case InvalidSignature(witness: VKeyWitness, tx: T)
        case TransactionBodyMismatch(otherTx: Transaction, tx: T)

        override def getMessage: String = this match {
            case MissingSignature(vKey, tx) =>
                s"Missing multisig witness for verification key $vKey in ${tx.transactionFamily}: ${tx.tx}"
            case InvalidSignature(witness, tx) =>
                s"Invalid multisig witness $witness for ${tx.transactionFamily}: ${tx.tx}"
            case TransactionBodyMismatch(otherTx: Transaction, thisTx) =>
                s"${thisTx.transactionFamily} bodies don't match; cannot add signatures" +
                    s"\n\t This transaction: $thisTx" +
                    s"\n\t Other transaction: $otherTx"
        }

    enum Type:
        case Deposit, Fallback, Finalization, Initialization, Refund, Rollout, Settlement

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

    type Serialized = ByteString

    /** A result that includes additional information besides the built transaction.
      *
      * @tparam T
      *   The type of built transaction.
      */
    trait AugmentedResult[T] {
        def transaction: T
    }

    object Builder {

        /** Builder-related functions across multiple builders use this type for their returning
          * value.
          *
          * @tparam Result
          *   the type of result
          * @tparam TxError
          *   the type of tx-specific errors
          */
        // TODO: swap params
        type BuilderResult[Result, TxError] = Either[(SomeBuildError | TxError, String), Result]
        //                                            ^ tx builder     ^ tx-     ^ additional
        //                                              error            specific  info
        //                                                               error

        /** If a particular transaction doesn't use custom errors, these type aliases may come in
          * handy.
          */
        type BuilderResultSimple[Result] = BuilderResult[Result, Void]
        type SomeBuildErrorOnly = SomeBuildError | Void

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
              * It comes useful in incremental builders when adding payouts/deposit one by one until
              * it fires - returning the previous state not the error.
              *
              * @param err
              *   The error to replace.
              * @param replacement
              *   The replacement value, provided as a lazy argument.
              * @tparam A
              *   The type of the replacement value, usually inferred by Scala.
              * @tparam TxError
              *   The concrete type of tx-specific error.
              * @return
              */
            final def replaceInvalidSizeException[A, TxError](
                err: SomeBuildError | TxError,
                replacement: => A
            ): Either[SomeBuildError | TxError, A] = {
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
