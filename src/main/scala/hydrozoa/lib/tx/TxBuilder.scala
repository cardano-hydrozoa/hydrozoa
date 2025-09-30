package hydrozoa.lib.tx

/*
 This module contains declarative transaction building types and utilities
 ported from purescript-cardano-transaction-builder.
 */

import cats.*
import cats.data.*
import cats.implicits.*
import hydrozoa.lib.tx
import hydrozoa.lib.tx.InputAction.{ReferenceInput, SpendInput}
import hydrozoa.lib.tx.TxBuildError.RedeemerIndexingInternalError
import hydrozoa.{datumOption, emptyTransaction, txBodyL}
import io.bullet.borer.Cbor
import monocle.syntax.all.*
import monocle.{Focus, Lens}
import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.GovAction.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.|>

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

// ============================================================================
// TransactionBuilderStep
// ============================================================================

sealed trait TransactionBuilderStep:
    /** A "best effort" method to determine which additional signers should be added or removed from
      * the context and/or the TransactionBody.requiredSigners field.
      *
      * This method is "unsafe" because it will return an empty set if we can't figure out what you
      * mean. For example, if you try to spend a script UTxO but don't pass a witness.
      *
      * TODO: Make a safe version with better error handling
      *
      * TODO: This could be a function `TransactionBuilderStep => Set[ExpectedSigner]`, but I made
      * it a method instead and sort of regret it.
      */
    val additionalSignersUnsafe: Set[ExpectedSigner]

object TransactionBuilderStep {
    case class SpendOutput(
        utxo: TransactionUnspentOutput,
        /** Pass None for pubkey outputs only */
        witness: Option[OutputWitness]
    ) extends TransactionBuilderStep:
        override val additionalSignersUnsafe: Set[ExpectedSigner] = {
            witness match {
                case Some(ns: OutputWitness.NativeScriptOutput) =>
                    ns.additionalSigners.map(ExpectedSigner(_))
                case Some(ps: OutputWitness.PlutusScriptOutput) =>
                    ps.additionalSigners.map(ExpectedSigner(_))
                case None =>
                    (utxo.output.address match {
                        case ShelleyAddress(_, payment: ShelleyPaymentPart.Key, _) =>
                            Set(payment.hash)
                        // Note: in the "safe" version, this case should probably return a left
                        // It means that we've passed a pubkey output, but without a Shelley key address.
                        // Question: can we get anything useful from byron addresses?
                        case _ => Set.empty
                    }).map(ExpectedSigner(_))
            }

        }

    case class Pay(output: TransactionOutput) extends TransactionBuilderStep:
        // Note: this should be the same in the safe version. Creating an output does
        // not trigger any scripts or pkh checks.
        override val additionalSignersUnsafe: Set[ExpectedSigner] = Set.empty

    case class MintAsset(
        scriptHash: ScriptHash,
        assetName: AssetName,
        amount: Long,
        witness: CredentialWitness
    ) extends TransactionBuilderStep:
        override val additionalSignersUnsafe: Set[ExpectedSigner] =
            witness match {
                case ns: CredentialWitness.NativeScriptCredential =>
                    ns.additionalSigners.map(ExpectedSigner(_))
                case ps: CredentialWitness.PlutusScriptCredential =>
                    ps.additionalSigners.map(ExpectedSigner(_))

            }

    case class IssueCertificate(
        cert: Certificate,
        witness: Option[CredentialWitness]
    ) extends TransactionBuilderStep:
        override val additionalSignersUnsafe: Set[ExpectedSigner] = {
            witness match {
                case Some(ns: CredentialWitness.NativeScriptCredential) =>
                    ns.additionalSigners.map(ExpectedSigner(_))
                case Some(ps: CredentialWitness.PlutusScriptCredential) =>
                    ps.additionalSigners.map(ExpectedSigner(_))
                case None =>
                    // FIXME: I'm not sure if this is correct at all. And some of these can definitely be pool key
                    //  hashes, so I'm not sure if the cast is valid? Do all of the hashes need to sign in order
                    //  unlock the certificate action(s)?
                    //  If the pool hashes do need to sign, then the type of `ExpectedSigner.signer` should be relaxed.
                    cert.keyHashes
                        .map(_.asInstanceOf[AddrKeyHash])
                        .map(ExpectedSigner(_))
            }
        }

    case class WithdrawRewards(
        stakeCredential: StakeCredential,
        amount: Coin,
        witness: Option[CredentialWitness]
    ) extends TransactionBuilderStep:
        override val additionalSignersUnsafe: Set[ExpectedSigner] =
            witness match {
                case Some(ns: CredentialWitness.NativeScriptCredential) =>
                    ns.additionalSigners.map(ExpectedSigner(_))
                case Some(ps: CredentialWitness.PlutusScriptCredential) =>
                    ps.additionalSigners.map(ExpectedSigner(_))
                case None =>
                    stakeCredential.credential.keyHashOption match {
                        case Some(kh) => Set(kh).map(ExpectedSigner(_))
                        // Note: in the safe version, this should return a left.
                        // It means we've passed a PKH credential, but can't extract the key
                        case None => Set.empty
                    }

            }

    case class SubmitProposal(
        proposal: ProposalProcedure,
        witness: Option[CredentialWitness]
    ) extends TransactionBuilderStep:

        // Note (dragospe, 2025-09-27): I'm not sure if this is correct.
        // From a brief look at the spec, it looks like placing a gov action on chain requires a deposit,
        // but that deposit itself does not get unlocked via the "SubmitProposal" step.
        // The "ParameterChange" and "TreasuryWithdrawals" proposal types can refer to "guardrails scripts";
        // the signatures related to these should be obtained from the witness field.
        // As far as I can tell, there is no possiblity of having a individual pubkey signature directly
        // tied to a proposal submission without a script requiring it.
        override val additionalSignersUnsafe: Set[ExpectedSigner] = {
            val signers = witness match {
                case Some(ns: CredentialWitness.NativeScriptCredential) =>
                    ns.additionalSigners.map(ExpectedSigner(_))
                case Some(ps: CredentialWitness.PlutusScriptCredential) =>
                    ps.additionalSigners.map(ExpectedSigner(_))
                // Note: This case is PROBABLY correct -- I think this is just the default when
                // there is no guardrails script. I think we keep this as-is in the safe version
                case None => Set.empty
            }
            signers
        }

    case class SubmitVotingProcedure(
        voter: Voter,
        votes: Map[GovActionId, VotingProcedure],
        witness: Option[CredentialWitness]
    ) extends TransactionBuilderStep:
        // Note (dragospe, 2025-09-27): I'm not sure if this is correct.
        // I believe that if voter.keyHashOption is Some(kh), the kh should be the sole additional signer.
        // Otherwise, the script hash must correspond to the credential witness.
        override val additionalSignersUnsafe: Set[ExpectedSigner] = {
            witness match {
                case Some(ns: CredentialWitness.NativeScriptCredential) =>
                    ns.additionalSigners.map(ExpectedSigner(_))
                case Some(ps: CredentialWitness.PlutusScriptCredential) =>
                    ps.additionalSigners.map(ExpectedSigner(_))
                case None =>
                    voter.keyHashOption match {
                        // Note: in the "safe" version, this should return left.
                        // It is again the case where we've passed a PKH voter
                        // (indicated by `witness == None`), but no
                        // key hash could be extracted.
                        case None     => Set.empty
                        case Some(kh) => Set(kh).map(ExpectedSigner(_))
                    }
            }

        }

    case class ModifyAuxData(f: Option[AuxiliaryData] => Option[AuxiliaryData])
        extends TransactionBuilderStep:
        // This should stay the same in the safe version.
        override val additionalSignersUnsafe: Set[ExpectedSigner] = Set.empty
}

// ============================================================================
// OutputWitness
// ============================================================================

/** -- | `OutputWitness` is used to provide the evidence needed to consume an -- | output. It must
  * correspond to a `TransactionUnspentOutput` address' -- | payment credential to unlock it.
  */

sealed trait OutputWitness

/** `OutputWitness` is used to provide the evidence needed to consume an output. It must correspond
  * to a `TransactionUnspentOutput` address' payment credential to unlock it.
  *
  * It also contains the signatures expected (for native scripts) or required (for plutus scripts)
  * to be used in fee calculation.
  */
object OutputWitness {
    case class NativeScriptOutput(
        witness: ScriptWitness[Script.Native]
    ) extends OutputWitness:
        val additionalSigners: Set[AddrKeyHash] = witness.additionalSigners

    case class PlutusScriptOutput(
        witness: ScriptWitness[PlutusScript],
        redeemer: Data,
        datum: Option[DatumWitness]
    ) extends OutputWitness:
        val additionalSigners: Set[AddrKeyHash] = witness.additionalSigners
}

// ============================================================================
// CredentialWitness
// ============================================================================

/*
-- | `CredentialWitness` is used to provide the evidence needed to perform
-- | operations on behalf of a credential, which include:
-- |
-- | - Minting
-- | - Certificate witnessing
-- | - Rewards withdrawal
-- |
-- | Unlike `OutputWitness`, it does not include a `DatumWitness`, because
-- | minting policies and stake scripts do not have a datum.
 */

sealed trait CredentialWitness:
    val additionalSigners: Set[AddrKeyHash]

object CredentialWitness {
    case class NativeScriptCredential(
        witness: ScriptWitness[Script.Native]
    ) extends CredentialWitness:
        override final val additionalSigners: Set[AddrKeyHash] = witness.additionalSigners
    case class PlutusScriptCredential(
        // N.B.: Changed from upstream, we have 3 distinct script types
        witness: ScriptWitness[PlutusScript],
        redeemer: Data
    ) extends CredentialWitness:
        override final val additionalSigners: Set[AddrKeyHash] = witness.additionalSigners
}

// ============================================================================
// ScriptWitness
// ============================================================================

/*
-- | Gives the user options for specifying everything needed to unlock an action guarded by a script, including
-- | - Spending a UTxO located at an address with a ScriptHash payment credential.
-- | - Witnessing credential operations requiring a script hash
-- | - Witnessing a mint
-- | - Witnessing a rewards withdrawal for a script hash credential
-- |
-- |  The two constructors behave as follows:
-- | - `ScriptValue` contains a script for the witness set.
-- |
-- | - `ScriptReference` contains a CIP-31 reference input where the inline script should be available at, and a flag to either spend the referenced input or just reference it.
 */
sealed trait ScriptWitness[+A]:
    val additionalSigners: Set[AddrKeyHash]

object ScriptWitness {
    case class ScriptValue[A](script: A, additionalSigners: Set[AddrKeyHash])
        extends ScriptWitness[A]
    case class ScriptReference(
        input: TransactionInput,
        action: InputAction,
        additionalSigners: Set[AddrKeyHash]
    ) extends ScriptWitness[Nothing]
}

// ============================================================================
// InputAction
// ============================================================================

/*
-- | Inputs can be referenced or spent in a transaction (See CIP-31).
-- | Inline datums (CIP-32) and reference scripts (CIP-33) contained within
-- | transaction outputs become visible to the script context of the
-- | transaction, regardless of whether the output is spent or just
-- | referenced. This data type lets the developer specify, which
-- | action to perform with an input.
 */

sealed trait InputAction

object InputAction {
    case object ReferenceInput extends InputAction
    case object SpendInput extends InputAction
}

/*
-- | Depending on `RefInputAction` value, we either want to spend a reference
-- | UTxO, or just reference it.
 */
private def inputActionToLens(
    inputAction: InputAction
): monocle.PLens[TransactionBody, TransactionBody, TaggedOrderedSet[
  scalus.cardano.ledger.TransactionInput
], TaggedOrderedSet[scalus.cardano.ledger.TransactionInput]] =
    inputAction match {
        case ReferenceInput => Focus[TransactionBody](_.referenceInputs)
        case SpendInput     => Focus[TransactionBody](_.inputs)
    }

// ============================================================================
// DatumWitness
// ============================================================================

/*
-- | Datums in UTxOs can be stored in two forms: inline datums or datum hashes.
-- | When there's a hash, we need to provide a datum corresponding to this hash,
-- | which can be done by either providing the value literally, or using a
-- | reference input where it is stored inline.
 */
sealed trait DatumWitness

object DatumWitness {
    case class DatumValue(datum: Data) extends DatumWitness
    case class DatumReference(
        input: TransactionInput,
        action: InputAction
    ) extends DatumWitness
}

// ===========================================================
// Expected Witness Type
// ===========================================================

sealed trait ExpectedWitnessType[A <: OutputWitness | CredentialWitness] {
    def explain: String
}

object ExpectedWitnessType {
    case class ScriptHashWitness[A <: OutputWitness | CredentialWitness](witness: A)
        extends ExpectedWitnessType[A]:
        override def explain: String = "ScriptHash"
    case class PubKeyHashWitness[A <: OutputWitness | CredentialWitness]()
        extends ExpectedWitnessType[A]:
        override def explain: String = "PubKeyHash"
}

// NOTE (Peter, 2025-09-23): this comes from  https://github.com/mlabs-haskell/purescript-cardano-types/blob/master/src/Cardano/Types/TransactionUnspentOutput.purs
// FIXME: change to utxo: (TransactionInput, TransactionOutput)
case class TransactionUnspentOutput(input: TransactionInput, output: TransactionOutput)

// NOTE (Peter, 2025-09-23): this comes from https://github.com/mlabs-haskell/purescript-cardano-types/blob/master/src/Cardano/Types/StakeCredential.purs
case class StakeCredential(credential: Credential)

// ============================================================================
// Errors
// ============================================================================

sealed trait TxBuildError:
    def explain: String

object TxBuildError {
    case class WrongSpendWitnessType(utxo: TransactionUnspentOutput) extends TxBuildError {
        override def explain: String =
            "`OutputWitness` is incompatible with the given output." +
                s" The output does not contain a datum: $utxo"
    }

    case class IncorrectDatumHash(
        utxo: TransactionUnspentOutput,
        datum: Data,
        datumHash: DataHash
    ) extends TxBuildError {
        override def explain
            : String = "You provided a `DatumWitness` with a datum that does not match the datum hash present in a transaction output.\n " +
            s" Datum: $datum (CBOR: ${ByteString.fromArray(Cbor.encode(datum).toByteArray).toHex})\n  " +
            s" Datum hash: ${ByteString.fromArray(Cbor.encode(datumHash).toByteArray)}\n  " +
            s"UTxO: $utxo"
    }

    case class IncorrectScriptHash(
        script: Either[Script.Native, PlutusScript],
        hash: ScriptHash
    ) extends TxBuildError {
        override def explain: String = script match {
            case Left(nativeScript) =>
                s"Provided script hash ($hash) does not match the provided native script ($nativeScript)"
            case Right(plutusScript) =>
                s"Provided script hash ($hash) does not match the provided Plutus script ($plutusScript)"
        }
    }

    case class WrongOutputType(
        expectedType: ExpectedWitnessType[OutputWitness],
        utxo: TransactionUnspentOutput
    ) extends TxBuildError {
        override def explain: String =
            "The UTxO you provided requires no witness, because the payment credential of the address is a `PubKeyHash`. " +
                s"UTxO: $utxo"
    }

    case class WrongCredentialType(
        action: CredentialAction,
        expectedType: ExpectedWitnessType[CredentialWitness],
        cred: Credential
    ) extends TxBuildError {
        override def explain: String =
            s"${action.explain} ($action) requires a ${expectedType.explain} witness: $cred"
    }

    case class DatumWitnessNotProvided(utxo: TransactionUnspentOutput) extends TxBuildError {
        override def explain: String =
            "The UTxO you are trying to spend contains a datum hash. " +
                s"A matching `DatumWitness` is required. Use `getDatumByHash`. UTxO: $utxo"
    }

    case class UnneededDatumWitness(utxo: TransactionUnspentOutput, witness: DatumWitness)
        extends TxBuildError {
        override def explain: String = "You've provided an optional `DatumWitness`," +
            " but the output you are spending already contains an inline datum (not just a datum hash)." +
            s" You should omit the provided datum witness. You provided: $witness for the UTxO: $utxo"
    }

    case class UnneededDeregisterWitness(
        stakeCredential: StakeCredential,
        witness: CredentialWitness
    ) extends TxBuildError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, " +
                "but the stake credential you are trying to issue a deregistering certificate for " +
                "is a PubKeyHash credential. You should omit the provided credential witness for this " +
                s"credential: $stakeCredential. Provided witness: $witness"
    }

    case class UnneededSpoVoteWitness(cred: Credential, witness: CredentialWitness)
        extends TxBuildError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, but the corresponding Voter is " +
                "SPO (Stake Pool Operator). You should omit the provided credential witness " +
                s"for this credential: $cred. Provided witness: $witness"
    }

    case class UnneededProposalPolicyWitness(
        proposal: ProposalProcedure,
        witness: CredentialWitness
    ) extends TxBuildError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, but the corresponding proposal" +
                " does not need to validate against the proposal policy. You should omit the " +
                s"provided credential witness for this proposal: $proposal. Provided witness: $witness"
    }

    case class RedeemerIndexingError(redeemer: Redeemer) extends TxBuildError {
        override def explain: String =
            s"Redeemer indexing error. Problematic redeemer that does not have a valid index: $redeemer"
    }

    case class RedeemerIndexingInternalError(
        tx: Transaction,
        steps: Seq[TransactionBuilderStep]
    ) extends TxBuildError {
        override def explain: String =
            s"Internal redeemer indexing error. Please report as bug: $bugTrackerUrl\nDebug info: " +
                s"Transaction: $tx, steps: ${steps
                        .mkString(", ")}"
    }

    private val bugTrackerUrl: String = "https://github.com/cardano-hydrozoa/hydrozoa/issues"

    case class WrongNetworkId(address: Address) extends TxBuildError {
        override def explain: String =
            "The following `Address` that was specified in one of the UTxOs has a `NetworkId`" +
                s" different from the one `TransactionBody` has: $address"
    }

    case object NoTransactionNetworkId extends TxBuildError {
        override def explain: String =
            "You are editing a transaction without a `NetworkId` set. To create a `RewardAddress`," +
                " a NetworkId is needed: set it in the `TransactionBody`"
    }
}

// ============================================================================
//  Credential Action
// ============================================================================

sealed trait CredentialAction:
    def explain: String

object CredentialAction {
    case class StakeCert(cert: Certificate) extends CredentialAction:
        override def explain: String = "This stake certificate"
    // TODO: should be address: StakeAddress?
    case class Withdrawal(address: Address) extends CredentialAction:
        override def explain: String = "This stake rewards withdrawal"
    case class Minting(scriptHash: PolicyId) extends CredentialAction:
        override def explain: String = "This mint"
    case class Voting(voter: Voter) extends CredentialAction:
        override def explain: String = "This voting procedure"
    case class Proposing(proposal: ProposalProcedure) extends CredentialAction:
        override def explain: String = "This voting proposal"
}

// ============================================================================
// The builder
// ============================================================================

case class Context(
    transaction: Transaction,
    redeemers: Seq[DetachedRedeemer],
    networkId: Option[Int],
    expectedSigners: Set[ExpectedSigner]
) {

    /** Add additional signers to the transaction, silently dropping any that point to invalid
      * components
      */
    def addSigners(additionalSigners: Set[ExpectedSigner]): Context = {
        this.copy(expectedSigners = this.expectedSigners ++ additionalSigners)
    }
}

// ==============================================================
// Transaction Builder
// ==============================================================

/** An "ExpectedSigner" signs for a pubkey or native script transaction component. Their signature
  * is declared as _expected_ on the final transaction, and thus taken into account when calculating
  * fees.
  */
case class ExpectedSigner(
    val signer: AddrKeyHash
)

// type BuilderM a = StateT Context (Except TxBuildError) a
private type BuilderM[A] = StateT[[X] =>> Either[TxBuildError, X], Context, A]

object TransactionBuilder:
    // Will drop signers silently
    private val unsafeCtxBodyL: Lens[Context, TransactionBody] =
        Focus[Context](_.transaction).andThen(txBodyL)

    // Will drop signers silently
    private val unsafeCtxWitnessL: Lens[Context, TransactionWitnessSet] =
        Focus[Context](_.transaction).refocus(_.witnessSet)

    // Add signers to the Context's transaction for the given step
    def addSignersFromStep(step: TransactionBuilderStep): BuilderM[Unit] =
        StateT.modify[[X] =>> Either[TxBuildError, X], Context](
          _.addSigners(step.additionalSignersUnsafe)
        )

    /** Recursively calculate the minAda for UTxO.
      *
      * @param candidateOutput
      *   The initial output
      * @param params
      *   Protocol params (for minAda calculation)
      * @param update
      *   A function that takes the calculated minAda for the [[candidateOutput]] and modifies the
      *   output to calculate the new minAda. By default, it is [[replaceAdaUpdate]]
      * @return
      *   An output that has the [[update]] function applied to it until the minAda condition is
      *   satisfied for the UTxO
      */
    @tailrec
    def setMinAda(
        candidateOutput: Babbage,
        params: ProtocolParams,
        update: (Coin, Babbage) => Babbage = replaceAdaUpdate
    ): Babbage = {
        val minAda = MinCoinSizedTransactionOutput(Sized(candidateOutput), params)
        //    println(s"Current candidate output value: ${candidateOutput.value.coin};" +
        //        s" minAda required for current candidate output: $minAda; " +
        //        s" size of current candidate output: ${Sized(candidateOutput.asInstanceOf[TransactionOutput]).size}")
        if minAda <= candidateOutput.value.coin
        then candidateOutput
        else setMinAda(update(minAda, candidateOutput), params, update)
    }

    /** An update function for use with calcMinAda. It replaces the output's coin with the given
      * coin.
      *
      * @param coin
      * @param to
      * @return
      */
    def replaceAdaUpdate(coin: Coin, to: Babbage): Babbage =
        to.focus(_.value.coin).replace(coin)

    def buildTransaction(
        steps: Seq[TransactionBuilderStep]
    ): Either[TxBuildError, (Transaction, Set[ExpectedSigner])] =
        modifyTransaction(emptyTransaction, Set.empty, steps)

    def modifyTransaction(
        tx: Transaction,
        expectedSigners: Set[ExpectedSigner],
        steps: Seq[TransactionBuilderStep]
    ): Either[TxBuildError, (Transaction, Set[ExpectedSigner])] =
        for {
            context <- for {
                editableTransaction <- TransactionConversion
                    .toEditableTransactionSafe(tx)
                    .left
                    .map(TxBuildError.RedeemerIndexingError(_))
            } yield Context(
              transaction = editableTransaction.transaction,
              redeemers = editableTransaction.redeemers,
              networkId = editableTransaction.transaction.body.value.networkId,
              expectedSigners = expectedSigners
            )
            // This modifies the transaction field of the context
            eiCtx <- processConstraints(steps).run(context).map(_._1)
            // This modifies the expectedSigners
            eiCtx2 <- steps.traverse_(addSignersFromStep).run(eiCtx).map(_._1)

            res <- TransactionConversion.fromEditableTransactionSafe(
              EditableTransaction(
                transaction = eiCtx2.transaction,
                redeemers = eiCtx2.redeemers.toVector
              )
            ) match {
                case None    => Left(RedeemerIndexingInternalError(tx, steps))
                case Some(x) => Right(x)
            }
        } yield (res, eiCtx2.expectedSigners)

    def processConstraints(steps: Seq[TransactionBuilderStep]): BuilderM[Unit] =
        steps.traverse_(processConstraint)

    def processConstraint(step: TransactionBuilderStep): BuilderM[Unit] = step match {
        case step @ TransactionBuilderStep.SpendOutput(utxo, spendWitness) =>
            for {
                _ <- assertNetworkId(utxo.output.address)
                _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](
                  // Add Input
                  unsafeCtxBodyL
                      .refocus(_.inputs)
                      .modify(inputs => TaggedOrderedSet.from(pushUnique(utxo.input, inputs.toSeq)))
                )
                _ <- useSpendWitness(utxo, spendWitness)
            } yield ()

        case TransactionBuilderStep.Pay(output) =>
            for {
                _ <- assertNetworkId(output.address)
                _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](
                  unsafeCtxBodyL
                      .refocus(_.outputs)
                      // Intentionally not using pushUnique: we can create multiple outputs of the same shape
                      .modify(outputs => outputs.toSeq :+ Sized(output))
                )
            } yield ()
        case TransactionBuilderStep.MintAsset(scriptHash, assetName, amount, mintWitness) =>
            useMintAssetWitness(scriptHash, assetName, amount, mintWitness)
        case TransactionBuilderStep.IssueCertificate(cert, witness) =>
            for {
                _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](
                  unsafeCtxBodyL
                      .refocus(_.certificates)
                      .modify(certificates =>
                          TaggedSet.from(pushUnique(cert, certificates.toIndexedSeq))
                      )
                )
                _ <- useCertificateWitness(cert, witness)
            } yield ()
        case TransactionBuilderStep.WithdrawRewards(stakeCredential, amount, witness) =>
            useWithdrawRewardsWitness(stakeCredential, amount, witness)
        case TransactionBuilderStep.SubmitProposal(proposal, witness) =>
            for {
                _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](
                  unsafeCtxBodyL
                      .refocus(_.proposalProcedures)
                      .modify(proposals =>
                          TaggedOrderedSet.from(pushUnique(proposal, proposals.toSeq))
                      )
                )
                _ <- useProposalWitness(proposal, witness)
            } yield ()
        case TransactionBuilderStep.SubmitVotingProcedure(voter, votes, witness) =>
            for {
                _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](
                  unsafeCtxBodyL
                      .refocus(_.votingProcedures)
                      .modify(procedures => {
                          val currentProcedures = procedures
                              .map(_.procedures)
                              .getOrElse(
                                SortedMap.empty[Voter, SortedMap[GovActionId, VotingProcedure]]
                              )
                          Some(
                            VotingProcedures(
                              currentProcedures + (voter -> SortedMap.from(votes))
                            )
                          )
                      })
                )
                _ <- useVotingProcedureWitness(voter, witness)
            } yield ()
        case TransactionBuilderStep.ModifyAuxData(f) =>
            StateT.modify[[X] =>> Either[TxBuildError, X], Context](
              Focus[Context](_.transaction)
                  .refocus(_.auxiliaryData)
                  .modify(f(_))
            )
    }

    /** -- | Ensures uniqueness of an element pushUnique :: forall a. Ord a => a -> Array a -> Array
      * a pushUnique x xs = nub $ xs <> [ x ]
      */
    def pushUnique[A](elem: A, seq: Seq[A]): Seq[A] =
        seq.appended(elem).distinct

    /** Ensure that the network id of the address matches the network id of the builder context */
    def assertNetworkId(addr: Address): BuilderM[Unit] =
        for {
            // NOTE from dragospe, 2025-09-23: I have no idea why I need such verbose type annotations.
            // Sorry.
            context: Context <- StateT.get[[X] =>> Either[TxBuildError, X], Context]

            // NOTE, from dragospe 2025-09-23: In the purescript version, getNetworkId
            // is forced to be total. See https://github.com/mlabs-haskell/purescript-cardano-types/blob/348fbbefa8bec5050e8492f5a9201ac5bb17c9d9/src/Cardano/Types/Address.purs#L93-L95
            // I do the same here for conformance, otherwise I'm not sure what to do with the leftover case.
            // But I don't know if this is sensible.
            addrNetworkId = addr.getNetwork.get.value.toInt
            _: Unit <- context.networkId match {
                case None => StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
                case Some(ctxNetworkId) =>
                    if addrNetworkId != ctxNetworkId
                    then
                        StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Unit](
                          Left(TxBuildError.WrongNetworkId(addr))
                        )
                    else StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
            }
        } yield ()

    /** Tries to modify the transaction to make it consume a given output. Uses a `SpendWitness` to
      * try to satisfy spending requirements.
      */
    def useSpendWitness(
        utxo: TransactionUnspentOutput,
        mbWitness: Option[OutputWitness]
    ): BuilderM[Unit] = {
        mbWitness match {
            case None =>
                assertOutputType(ExpectedWitnessType.PubKeyHashWitness[OutputWitness](), utxo)
            case Some(witness @ OutputWitness.NativeScriptOutput(nsWitness)) =>
                for {
                    _ <- assertOutputType(
                      ExpectedWitnessType.ScriptHashWitness[OutputWitness](witness),
                      utxo
                    )
                    res <- useNativeScriptWitness(nsWitness)
                } yield res
            case Some(
                  witness @ OutputWitness.PlutusScriptOutput(
                    plutusScriptWitness,
                    redeemerDatum,
                    mbDatumWitness
                  )
                ) =>
                for {
                    _ <- assertOutputType(
                      ExpectedWitnessType.ScriptHashWitness[OutputWitness](witness),
                      utxo
                    )
                    _ <- usePlutusScriptWitness(plutusScriptWitness)
                    detachedRedeemer = DetachedRedeemer(
                      redeemerDatum,
                      RedeemerPurpose.ForSpend(utxo.input)
                    )
                    _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](ctx =>
                        ctx.focus(_.redeemers).modify(r => pushUnique(detachedRedeemer, r))
                    )
                    _ <- useDatumWitnessForUtxo(utxo, mbDatumWitness)
                } yield ()
        }
    }

    def assertOutputType(
        expectedType: ExpectedWitnessType[OutputWitness],
        utxo: TransactionUnspentOutput
    ): BuilderM[Unit] =
        for {
            mbCredential <-
                (for {
                    // Extract the credential, assuming its a shelley address
                    // Upstream, this is `getPaymentCredential`
                    // https://github.com/mlabs-haskell/purescript-cardano-types/blob/348fbbefa8bec5050e8492f5a9201ac5bb17c9d9/src/Cardano/Types/Address.purs#L97C42-L102
                    cred: Credential <-
                        utxo.output.address match {
                            case address: ShelleyAddress =>
                                address.payment match {
                                    case s: ShelleyPaymentPart.Script =>
                                        OptionT.some[BuilderM](Credential.ScriptHash(s.hash))
                                    case pkh: ShelleyPaymentPart.Key =>
                                        OptionT.some[BuilderM](Credential.KeyHash(pkh.hash))
                                }
                            case _ => OptionT.none[BuilderM, Credential]
                        }
                    _ <- expectedType match {
                        case ExpectedWitnessType.ScriptHashWitness(witness) =>
                            for {
                                scriptHash <- OptionT.fromOption[BuilderM](cred.scriptHashOption)
                                _ <- OptionT.liftF[BuilderM, Unit](
                                  assertScriptHashMatchesOutputWitness(scriptHash, witness)
                                )
                            } yield ()
                        case ExpectedWitnessType.PubKeyHashWitness() =>
                            for {
                                _ <- OptionT.fromOption[BuilderM](cred.keyHashOption)
                            } yield ()
                    }
                } yield ()).value
            res <-
                if mbCredential.isEmpty
                then
                    StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Unit](
                      Left(TxBuildError.WrongOutputType(expectedType, utxo))
                    )
                else StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
        } yield res

    def assertScriptHashMatchesOutputWitness(
        scriptHash: ScriptHash,
        witness: OutputWitness
    ): BuilderM[Unit] =
        for {
            _ <- witness match {
                case OutputWitness.PlutusScriptOutput(
                      ScriptWitness.ScriptValue(plutusScript, _),
                      _,
                      _
                    ) =>
                    assertScriptHashMatchesScript(scriptHash, Right(plutusScript))
                case OutputWitness.NativeScriptOutput(ScriptWitness.ScriptValue(nativeScript, _)) =>
                    assertScriptHashMatchesScript(scriptHash, Left(nativeScript))
                case _ =>
                    StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
            }
        } yield ()

    def assertScriptHashMatchesScript(
        scriptHash: ScriptHash,
        eiScript: Either[Script.Native, PlutusScript]
    ): BuilderM[Unit] = {
        val computedHash = eiScript match {
            case Left(nativeScript)  => nativeScript.scriptHash
            case Right(plutusScript) => plutusScript.scriptHash
        }

        if (scriptHash != computedHash) {
            StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Unit](
              Left(TxBuildError.IncorrectScriptHash(eiScript, scriptHash))
            )
        } else {
            StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
        }
    }

    def useNativeScriptWitness(scriptWitness: ScriptWitness[Script.Native]): BuilderM[Unit] =
        scriptWitness match {
            case ScriptWitness.ScriptValue(ns, expectedSigners) =>
                StateT.modify(
                  // Add the native script to the witness set
                  unsafeCtxWitnessL
                      .refocus(_.nativeScripts)
                      .modify(s => pushUnique(ns, s.toList).toSet)
                )
            case ScriptWitness.ScriptReference(input, inputAction, expectedSigners) =>
                StateT.modify(
                  unsafeCtxBodyL
                      .andThen(inputActionToLens(inputAction))
                      .modify(s => TaggedOrderedSet.from(pushUnique(input, s.toSeq)))
                )
        }

    def usePlutusScriptWitness(
        scriptWitness: ScriptWitness[PlutusScript]
    ): BuilderM[Unit] =
        for {
            _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](
              Focus[Context](_.transaction)
                  .andThen(txBodyL)
                  .refocus(_.requiredSigners)
                  .modify((s: TaggedOrderedSet[AddrKeyHash]) =>
                      TaggedOrderedSet.from(s.toSortedSet ++ scriptWitness.additionalSigners)
                  )
            )
            _ <- scriptWitness match {
                case ScriptWitness.ScriptValue(ps: PlutusScript, _) =>
                    ps match {
                        case (v1: Script.PlutusV1) =>
                            StateT.modify[[X] =>> Either[TxBuildError, X], Context](
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV1Scripts)
                                  .modify(s => Set.from(pushUnique(v1, s.toSeq)))
                            )
                        case (v2: Script.PlutusV2) =>
                            StateT.modify[[X] =>> Either[TxBuildError, X], Context](
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV2Scripts)
                                  .modify(s => Set.from(pushUnique(v2, s.toSeq)))
                            )
                        case (v3: Script.PlutusV3) =>
                            StateT.modify[[X] =>> Either[TxBuildError, X], Context](
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV3Scripts)
                                  .modify(s => Set.from(pushUnique(v3, s.toSeq)))
                            )
                    }
                case ScriptWitness.ScriptReference(input, action, _) =>
                    StateT.modify[[X] =>> Either[TxBuildError, X], Context](
                      unsafeCtxBodyL
                          .andThen(inputActionToLens(action))
                          .modify(s => TaggedOrderedSet.from(pushUnique(input, s.toSeq)))
                    )
            }
        } yield ()

    /** Tries to modify the transaction state to make it consume a given script output. Uses a
      * `DatumWitness` if the UTxO datum is provided as a hash.
      */
    def useDatumWitnessForUtxo(
        utxo: TransactionUnspentOutput,
        mbDatumWitness: Option[DatumWitness]
    ): BuilderM[Unit] =
        for {
            _ <- utxo.output.datumOption match {
                // script outputs must have a datum
                case None =>
                    StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Unit](
                      Left(TxBuildError.WrongSpendWitnessType(utxo))
                    )
                // if the datum is inline, we don't need to attach it as witness
                case Some(DatumOption.Inline(_)) =>
                    mbDatumWitness match {
                        case Some(datumWitness) =>
                            StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Unit](
                              Left(TxBuildError.UnneededDatumWitness(utxo, datumWitness))
                            )
                        case None =>
                            StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
                    }
                // if the datum is provided as hash
                case Some(DatumOption.Hash(datumHash)) =>
                    mbDatumWitness match {
                        // Error if the datum witness was not provided
                        case None =>
                            StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Unit](
                              Left(TxBuildError.DatumWitnessNotProvided(utxo))
                            )
                        // Datum as a value
                        //
                        // Check if the provided datum hash matches the output datum hash and if so
                        // add the datum to the witness set.
                        case Some(DatumWitness.DatumValue(providedDatum)) =>
                            // TODO: is that correct? Upstream Data.dataHash extension?
                            val computedHash: DataHash =
                                DataHash.fromByteString(blake2b_224(serialiseData(providedDatum)))
                            if (datumHash == computedHash) {
                                //
                                StateT.modify[[X] =>> Either[TxBuildError, X], Context](
                                  unsafeCtxWitnessL
                                      .refocus(_.plutusData)
                                      .modify(plutusData =>
                                          KeepRaw.apply(
                                            TaggedSet.from(
                                              pushUnique(
                                                KeepRaw.apply(providedDatum),
                                                plutusData.value.toIndexedSeq
                                              )
                                            )
                                          )
                                      )
                                )
                            } else {
                                StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Unit](
                                  Left(
                                    TxBuildError.IncorrectDatumHash(utxo, providedDatum, datumHash)
                                  )
                                )
                            }
                        // Add reference input without checking (expensive UTxO lookup).
                        //
                        // If a reference input is provided, we just attach it without
                        // checking (doing that would require looking up the utxo).
                        //
                        // We COULD require the user to provide the output to do an additional
                        // check, but we don't, because otherwise the contents of the ref output
                        // do not matter (i.e., they are not needed for balancing).
                        //
                        // UTxO lookups are quite expensive, so it's best to not require more
                        // of them than strictly necessary.
                        case Some(DatumWitness.DatumReference(input, inputAction)) =>
                            StateT.modify[[X] =>> Either[TxBuildError, X], Context](
                              unsafeCtxBodyL
                                  .andThen(inputActionToLens(inputAction))
                                  .modify(s => TaggedOrderedSet.from(pushUnique(input, s.toSeq)))
                            )
                    }
            }
        } yield ()

    // ============================================================================
    // Minting
    // ============================================================================

    def useMintAssetWitness(
        scriptHash: ScriptHash,
        assetName: AssetName,
        amount: Long,
        witness: CredentialWitness
    ): BuilderM[Unit] =
        for {
            _ <- useCredentialWitness(
              CredentialAction.Minting(scriptHash),
              Credential.ScriptHash(scriptHash),
              Some(witness)
            )
            _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](ctx => {
                val currentMint = ctx |> unsafeCtxBodyL.refocus(_.mint).get
                val thisMint = MultiAsset.asset(scriptHash, assetName, amount)

                val newMint = currentMint match {
                    case None           => Some(thisMint)
                    case Some(existing) => Some(existing + thisMint)
                }
                ctx |> unsafeCtxBodyL.refocus(_.mint).replace(newMint.map(Mint.apply))
            })
        } yield ()

    // ============================================================================
    // useCredentialWitness
    // ============================================================================

    def useCredentialWitness(
        credAction: CredentialAction,
        cred: Credential,
        mbWitness: Option[CredentialWitness]
    ): BuilderM[Unit] =
        for {
            _ <- mbWitness match {
                case None =>
                    assertCredentialType(credAction, ExpectedWitnessType.PubKeyHashWitness(), cred)
                case Some(
                      witness @ CredentialWitness.NativeScriptCredential(nsWitness)
                    ) =>
                    for {
                        _ <- assertCredentialType(
                          credAction,
                          ExpectedWitnessType.ScriptHashWitness(witness),
                          cred
                        )
                        _ <- useNativeScriptWitness(nsWitness)
                    } yield ()
                case Some(
                      witness @ CredentialWitness.PlutusScriptCredential(
                        plutusScriptWitness,
                        redeemerDatum
                      )
                    ) =>
                    for {
                        _ <- assertCredentialType(
                          credAction,
                          ExpectedWitnessType.ScriptHashWitness(witness),
                          cred
                        )
                        _ <- usePlutusScriptWitness(plutusScriptWitness)
                        _ <- {
                            val redeemer = DetachedRedeemer(
                              datum = redeemerDatum,
                              purpose = credAction match {
                                  case CredentialAction.Withdrawal(address) =>
                                      val stakeAddress = address match {
                                          case ShelleyAddress(_, _, _)           => ??? // FIXME:
                                          case stakeAddress @ StakeAddress(_, _) => stakeAddress
                                          case ByronAddress(_)                   => ??? // FIXME:
                                      }
                                      RedeemerPurpose.ForReward(
                                        RewardAccount(stakeAddress)
                                      )
                                  case CredentialAction.StakeCert(cert) =>
                                      RedeemerPurpose.ForCert(cert)
                                  case CredentialAction.Minting(scriptHash) =>
                                      RedeemerPurpose.ForMint(scriptHash)
                                  case CredentialAction.Voting(voter) =>
                                      RedeemerPurpose.ForVote(voter)
                                  case CredentialAction.Proposing(proposal) =>
                                      RedeemerPurpose.ForPropose(proposal)
                              }
                            )
                            StateT.modify[[X] =>> Either[TxBuildError, X], Context](ctx =>
                                ctx.focus(_.redeemers)
                                    .modify(redeemers => pushUnique(redeemer, redeemers))
                            )
                        }
                    } yield ()
            }
        } yield ()

    // ============================================================================
    // IssueCertificate
    // ============================================================================

    def useCertificateWitness(
        cert: Certificate,
        mbWitness: Option[CredentialWitness]
    ): BuilderM[Unit] =
        cert match {
            // FIXME: verify
            case Certificate.UnregCert(credential, _) =>
                for {
                    _ <- (credential, mbWitness) match {
                        case (Credential.KeyHash(_), Some(witness)) =>
                            StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Unit](
                              Left(
                                TxBuildError.UnneededDeregisterWitness(
                                  StakeCredential(credential),
                                  witness
                                )
                              )
                            )
                        case (Credential.KeyHash(_), None) =>
                            StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
                        case (Credential.ScriptHash(_), None) =>
                            StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Unit](
                              Left(
                                TxBuildError.WrongCredentialType(
                                  CredentialAction.StakeCert(cert),
                                  ExpectedWitnessType.PubKeyHashWitness(),
                                  credential
                                )
                              )
                            )
                        case (Credential.ScriptHash(scriptHash), Some(witness)) =>
                            assertScriptHashMatchesCredentialWitness(scriptHash, witness)
                    }
                    _ <- useCredentialWitness(
                      CredentialAction.StakeCert(cert),
                      credential,
                      mbWitness
                    )
                } yield ()
            case Certificate.StakeDelegation(credential, _) =>
                useCredentialWitness(CredentialAction.StakeCert(cert), credential, mbWitness)
            // FIXME: verify
            case Certificate.RegCert(_, _) =>
                StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
            case Certificate.PoolRegistration(_, _, _, _, _, _, _, _, _) =>
                StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
            case Certificate.PoolRetirement(_, _) =>
                StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
            case Certificate.VoteDelegCert(credential, _) =>
                useCredentialWitness(CredentialAction.StakeCert(cert), credential, mbWitness)
            case Certificate.StakeVoteDelegCert(credential, _, _) =>
                useCredentialWitness(CredentialAction.StakeCert(cert), credential, mbWitness)
            case Certificate.StakeRegDelegCert(credential, _, _) =>
                useCredentialWitness(CredentialAction.StakeCert(cert), credential, mbWitness)
            case Certificate.VoteRegDelegCert(credential, _, _) =>
                useCredentialWitness(CredentialAction.StakeCert(cert), credential, mbWitness)
            case Certificate.StakeVoteRegDelegCert(credential, _, _, _) =>
                useCredentialWitness(CredentialAction.StakeCert(cert), credential, mbWitness)
            case Certificate.AuthCommitteeHotCert(_, _) =>
                StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](()) // not supported
            case Certificate.ResignCommitteeColdCert(_, _) =>
                StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](()) // not supported
            case Certificate.RegDRepCert(credential, _, _) =>
                useCredentialWitness(CredentialAction.StakeCert(cert), credential, mbWitness)
            case Certificate.UnregDRepCert(credential, _) =>
                useCredentialWitness(CredentialAction.StakeCert(cert), credential, mbWitness)
            case Certificate.UpdateDRepCert(credential, _) =>
                useCredentialWitness(CredentialAction.StakeCert(cert), credential, mbWitness)
        }

    // ============================================================================
    // WithdrawRewards
    // ============================================================================

    def useWithdrawRewardsWitness(
        stakeCredential: StakeCredential,
        amount: Coin,
        witness: Option[CredentialWitness]
    ): BuilderM[Unit] =
        for {
            networkId <- StateT.get[[X] =>> Either[TxBuildError, X], Context].flatMap { ctx =>
                ctx.networkId match {
                    case Some(netId) =>
                        StateT.pure[[X] =>> Either[TxBuildError, X], Context, Int](netId)
                    case None =>
                        StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Int](
                          Left(TxBuildError.NoTransactionNetworkId)
                        )
                }
            }
            // Convert Int to Network and Credential to StakePayload
            network = if (networkId == 0) Network.Testnet else Network.Mainnet
            rewardAccount = stakeCredential.credential match {
                case Credential.KeyHash(keyHash) =>
                    // Convert AddrKeyHash to StakeKeyHash - they're likely the same underlying type?
                    val stakeKeyHash = keyHash.asInstanceOf[StakeKeyHash]
                    val stakeAddress = StakeAddress(network, StakePayload.Stake(stakeKeyHash))
                    RewardAccount(stakeAddress)
                case Credential.ScriptHash(scriptHash) =>
                    val stakeAddress = StakeAddress(network, StakePayload.Script(scriptHash))
                    RewardAccount(stakeAddress)
            }

            _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](
              unsafeCtxBodyL
                  .refocus(_.withdrawals)
                  .modify(withdrawals => {
                      val currentWithdrawals = withdrawals.map(_.withdrawals).getOrElse(Map.empty)
                      Some(
                        Withdrawals(
                          SortedMap.from(currentWithdrawals + (rewardAccount -> amount))
                        )
                      )
                  })
            )

            _ <- useCredentialWitness(
              CredentialAction.Withdrawal(rewardAccount.address),
              stakeCredential.credential,
              witness
            )
        } yield ()

    // ============================================================================
    // SubmitProposal
    // ============================================================================

    def useProposalWitness(
        proposal: ProposalProcedure,
        mbWitness: Option[CredentialWitness]
    ): BuilderM[Unit] = {
        def getPolicyHash(govAction: GovAction): Option[ScriptHash] = govAction match {
            case GovAction.ParameterChange(_, _, policyHash)  => policyHash
            case GovAction.TreasuryWithdrawals(_, policyHash) => policyHash
            case _                                            => None
        }

        (getPolicyHash(proposal.govAction), mbWitness) match {
            case (None, Some(witness)) =>
                StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Unit](
                  Left(TxBuildError.UnneededProposalPolicyWitness(proposal, witness))
                )
            case (Some(policyHash), witness) =>
                useCredentialWitness(
                  CredentialAction.Proposing(proposal),
                  Credential.ScriptHash(policyHash),
                  witness
                )
            case (None, None) =>
                StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
        }
    }

    def useVotingProcedureWitness(
        voter: Voter,
        mbWitness: Option[CredentialWitness]
    ): BuilderM[Unit] =
        for {
            cred <- voter match {
                case Voter.StakingPoolKey(poolKeyHash) =>
                    val credential = Credential.KeyHash(poolKeyHash)
                    mbWitness match {
                        case Some(witness) =>
                            StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Credential](
                              Left(TxBuildError.UnneededSpoVoteWitness(credential, witness))
                            )
                        case None =>
                            StateT.pure[[X] =>> Either[TxBuildError, X], Context, Credential](
                              credential
                            )
                    }
                case Voter.ConstitutionalCommitteeHotKey(credential) =>
                    StateT.pure[[X] =>> Either[TxBuildError, X], Context, Credential](
                      Credential.KeyHash(credential)
                    )
                case Voter.ConstitutionalCommitteeHotScript(scriptHash) =>
                    StateT.pure[[X] =>> Either[TxBuildError, X], Context, Credential](
                      Credential.ScriptHash(scriptHash)
                    )
                case Voter.DRepKey(credential) =>
                    StateT.pure[[X] =>> Either[TxBuildError, X], Context, Credential](
                      Credential.KeyHash(credential)
                    )
                case Voter.DRepScript(scriptHash) =>
                    StateT.pure[[X] =>> Either[TxBuildError, X], Context, Credential](
                      Credential.ScriptHash(scriptHash)
                    )
            }
            _ <- useCredentialWitness(CredentialAction.Voting(voter), cred, mbWitness)
        } yield ()

    def assertCredentialType(
        action: CredentialAction,
        expectedType: ExpectedWitnessType[CredentialWitness],
        cred: Credential
    ): BuilderM[Unit] =
        for {
            _ <- {
                val wrongCredErr = TxBuildError.WrongCredentialType(action, expectedType, cred)
                expectedType match {
                    case ExpectedWitnessType.ScriptHashWitness(witness) =>
                        for {
                            scriptHash <- cred.scriptHashOption match {
                                case Some(hash) =>
                                    StateT
                                        .pure[[X] =>> Either[TxBuildError, X], Context, ScriptHash](
                                          hash
                                        )
                                case None =>
                                    StateT.liftF[[X] =>> Either[
                                      TxBuildError,
                                      X
                                    ], Context, ScriptHash](
                                      Left(wrongCredErr)
                                    )
                            }
                            _ <- assertScriptHashMatchesCredentialWitness(scriptHash, witness)
                        } yield ()
                    case ExpectedWitnessType.PubKeyHashWitness() =>
                        cred.keyHashOption match {
                            case Some(_) =>
                                StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
                            case None =>
                                StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Unit](
                                  Left(wrongCredErr)
                                )
                        }
                }
            }
        } yield ()

    def assertScriptHashMatchesCredentialWitness(
        scriptHash: ScriptHash,
        witness: CredentialWitness
    ): BuilderM[Unit] =
        for {
            _ <- witness match {
                case CredentialWitness.NativeScriptCredential(
                      ScriptWitness.ScriptValue(nativeScript, _)
                    ) =>
                    assertScriptHashMatchesScript(scriptHash, Left(nativeScript))
                case CredentialWitness.PlutusScriptCredential(
                      ScriptWitness.ScriptValue(plutusScript, _),
                      _
                    ) =>
                    assertScriptHashMatchesScript(scriptHash, Right(plutusScript))
                case _ =>
                    StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
            }
        } yield ()
