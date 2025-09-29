package hydrozoa.lib.tx

import hydrozoa.{emptyTransaction, txBodyL}
import hydrozoa.lib.tx.TransactionWithSigners.{ValidSignersPartition, mkUnsafe}
import monocle.Focus.{focus, refocus}
import monocle.Lens
import scalus.cardano.ledger.{AddrKeyHash, Certificate, PolicyId, ProposalProcedure, TaggedOrderedSet, Transaction, TransactionInput, Voter, Withdrawals}
import scalus.|>

/*
Pubkey: know what signature is needed and why
Plutus script: know what signature are needed, don't know why 
Native script: don't know what signatures are needed or why
*/



/** A transaction paired with the expected (and required) signers. This is necessary for accurate
  * fee calculation.
  *
  * If the number of expected signers is too small compared to the number of actual signers, the
  * transaction should fail with an insufficient fee error.
  *
  * If the number of expected signers is too large compared to the number of actual signers, the
  * transaction may pass, but the fee will be over-paid.
  *
  * Note: this class is opaque. It is not (or should not be) possible to have [[ExpectedSigner]]s or
  * [[RequiredSigner]]s that point to invalid transaction components.
  */
case class TransactionWithSigners private (tx: Transaction, expectedSigners: Set[ExpectedSigner]):
    // Developers note: Do NOT use `new TransactionWithSigners(...)`. There are invariants that are
    // established from the `apply` and `mkUnsafe` smart constructors that must be maintained.
    // Specifically:
    //
    // 1.) "Expected Signers Resolve": For all signers appearing in the [[expectedSigners]] field, the
    // corresponding signer.signerPurpose must exist in the transaction.
    //
    // 2.) "No dangling required signers": For all signers appearing in the requiredSigners of the transaction body,
    // there must be a corresponding entry in the expectedSigners field.

    /** Add additional signers to the transaction, silently dropping any that point to invalid
      * components
      */
    def addSigners(additionalSigners: Set[ExpectedSigner]): TransactionWithSigners = {
        mkUnsafe(this.tx, this.expectedSigners ++ additionalSigners)
    }

    /** Add additional signers to the transaction, returning a Left with all invalid signatures if
      * any exist.
      * @param additionalSigners
      * @return
      */
    def addSignersSafe(
        additionalSigners: Set[ExpectedSigner]
    ): Either[
      // NOTE: Should be a non-empty set, but it seems like that has to come from a SortedSet,
      // and I don't yet know how to handle Ordering instances in scala.
      ValidSignersPartition,
      TransactionWithSigners
    ] = {
        TransactionWithSigners.mk(this.tx, this.expectedSigners ++ additionalSigners)
    }

object TransactionWithSigners:
    val empty: TransactionWithSigners = TransactionWithSigners.mkUnsafe(emptyTransaction, Set.empty)

    case class ValidSignersPartition(
        /** Signers that have a signingPurpose that point to a valid transaction component */
        resolvableExpectedSigners: Set[ExpectedSigner],
        /** Signers that have a signingPurpose that does not point to a valid transaction component
          */
        danglingExpectedSigners: Set[ExpectedSigner],
        /** key hashes in the requiredSigners field of the transaction body that can be resolved to
          * one or more resolvable expected signers
          */
        resolvableRequiredSigners: Set[AddrKeyHash],
        /** key hashes in the requiredSigners field of the transaction body that cannot be resolved
          * to any resolvable expected signer.
          */
        danglingRequiredSigners: Set[AddrKeyHash]
    )

    private def partitionValidSignatures(
        tx: Transaction,
        expectedSigners: Set[ExpectedSigner]
    ): ValidSignersPartition = {
        // Invariant 1: Ensure that all expected signers resolve to real transaction components.
        val (resolvableExpectedSigners, danglingExpectedSigners) = expectedSigners.foldLeft(
          (Set.empty[ExpectedSigner], Set.empty[ExpectedSigner])
        )((acc, signer) =>
            if signer.signerPurpose.isIn(tx) then acc.focus(_._1).modify(_ + signer)
            else acc.focus(_._2).modify(_ + signer)
        )

        // Invariant 2: ensure that all requiredSigners in the tx body refer to signatures in the valid expected signers
        // set
        val actualRequiredSigners: Set[AddrKeyHash] =
            (tx |> txBodyL.refocus(_.requiredSigners).get).toSortedSet
        val validRequiredHashes: Set[AddrKeyHash] =
            resolvableExpectedSigners
                .filter(_.isInstanceOf[RequiredSigner])
                .map(_.signer)
        val (resolvableRequiredSigners, danglingRequiredSigners) =
            actualRequiredSigners.partition(validRequiredHashes.contains)

        ValidSignersPartition(
          resolvableExpectedSigners = resolvableExpectedSigners,
          danglingExpectedSigners = danglingExpectedSigners,
          resolvableRequiredSigners = resolvableRequiredSigners,
          danglingRequiredSigners = danglingRequiredSigners
        )
    }

    /** Create a transaction with expected signers, returning a Left if any dangling expected or
      * required signers are present.
      * @param tx
      *   a transaction
      * @param expectedSigners
      *   a set of expected signers for this transaction
      * @return
      *   Left(invalidSigners) or Right(transactionWithExpectedAndRequiredSigners)
      */
    def mk(
        tx: Transaction,
        expectedSigners: Set[ExpectedSigner]
    ): Either[ValidSignersPartition, TransactionWithSigners] = {
        val signersPartition = partitionValidSignatures(tx, expectedSigners)
        if signersPartition.danglingRequiredSigners.isEmpty && signersPartition.danglingExpectedSigners.isEmpty
        then {
            val txWithRS = tx |> txBodyL
                .refocus(_.requiredSigners)
                .replace(
                  TaggedOrderedSet.from(signersPartition.resolvableRequiredSigners)
                )

            Right(TransactionWithSigners(txWithRS, signersPartition.resolvableExpectedSigners))
        } else Left(signersPartition)
    }

    /** Silently drops any dangling expected or required signers
      * @param tx
      * @param expectedSigners
      * @return
      */
    def mkUnsafe(tx: Transaction, expectedSigners: Set[ExpectedSigner]): TransactionWithSigners = {
        val signersPartition = partitionValidSignatures(tx, expectedSigners)

        val txWithRS = tx |> txBodyL
            .refocus(_.requiredSigners)
            .replace(TaggedOrderedSet.from(signersPartition.resolvableRequiredSigners))
        TransactionWithSigners(txWithRS, signersPartition.resolvableExpectedSigners)
    }

    /** Lens using [[mkUnsafe]].
      *
      * This is safe to use only if you're modifying parts of the transaction that do not correspond
      * to signatures. It will silently drop signatures that have been made invalid as a result of
      * modifying the transaction.
      */
    val txwsTxUnsafeL: Lens[TransactionWithSigners, Transaction] = {
        val getter: TransactionWithSigners => Transaction = (txws => txws.tx)
        val setter: Transaction => TransactionWithSigners => TransactionWithSigners = {
            (tx => txws => mkUnsafe(tx = tx, expectedSigners = txws.expectedSigners))
        }
        Lens(getter)(setter)
    }

    /** Lens using [[mkUnsafe]].
      *
      * This will silently drop signatures that are passed to the setter, but that do not correspond
      * to valid signature components
      */
    val txwsSignersUnsafeL: Lens[TransactionWithSigners, Set[ExpectedSigner]] = {
        val getter: TransactionWithSigners => Set[ExpectedSigner] = _.expectedSigners
        val setter: Set[ExpectedSigner] => TransactionWithSigners => TransactionWithSigners =
            (es => txws => mkUnsafe(tx = txws.tx, expectedSigners = es))
        Lens(getter)(setter)
    }



// ===========================================================================
// ExpectedSigner
// ===========================================================================

/** An "ExpectedSigner" signs for a pubkey or native script transaction component. Their signature
 * is declared as _expected_ on the final transaction, and thus taken into account when calculating
 * fees.
 */
class ExpectedSigner(
                        val signer: AddrKeyHash,
                        val signerPurpose: SignerPurpose
                    )

/** A "RequiredSigner" signs for a plutus script transaction component. Their signature is both
 * _expected_ in the final transaction and set as _required_ in the transaction body.
 */
case class RequiredSigner(
                             override val signer: AddrKeyHash,
                             override val signerPurpose: SignerPurpose
                         ) extends ExpectedSigner(signer, signerPurpose)

// ============================================================================
// SignerPurpose
// ============================================================================

/** Contains a value that a Signer corresponds to.
 */
sealed trait SignerPurpose:
    /** returns true if this signer purpose is valid for the given transaction */
    // TODO: Needs tests.
    def isIn(tx: Transaction): Boolean

object SignerPurpose {
    // QUESTION: do we need another case class for addition an additional expected signer without being
    // tied to any particular transaction action?
    case class ForSpend(input: TransactionInput) extends SignerPurpose:
        override def isIn(tx: Transaction): Boolean = tx.body.value.inputs.toSeq.contains(input)
    case class ForMint(policyId: PolicyId) extends SignerPurpose:
        override def isIn(tx: Transaction): Boolean = tx.body.value.mint match {
            case None    => false
            case Some(m) => m.assets.contains(policyId)
        }
    // N.B.: In the redeemer version, this is a RewardAddress (which is just a stake credential
    // paired with a network). For signing, I change this because I don't think the
    // network is relevant -- we would need to pass the tx/context to [[additionalSignersUnsafe]]
    // in order to create the reward address.
    case class ForReward(stakeCredential: StakeCredential) extends SignerPurpose:
        override def isIn(tx: Transaction): Boolean = tx.body.value.withdrawals
            .getOrElse(Withdrawals.empty)
            .withdrawals
            .keys
            .toSeq
            .contains(stakeCredential)
    case class ForCert(certificate: Certificate) extends SignerPurpose:
        override def isIn(tx: Transaction): Boolean =
            tx.body.value.certificates.toIndexedSeq.contains(certificate)
    case class ForVote(voter: Voter) extends SignerPurpose:
        override def isIn(tx: Transaction): Boolean =
            tx.body.value.votingProcedures match {
                case Some(voters) => voters.procedures.keys.toSeq.contains(voter)
                case None         => false
            }
    case class ForPropose(proposal: ProposalProcedure) extends SignerPurpose:
        override def isIn(tx: Transaction): Boolean =
            tx.body.value.proposalProcedures.toSeq.contains(proposal)
}