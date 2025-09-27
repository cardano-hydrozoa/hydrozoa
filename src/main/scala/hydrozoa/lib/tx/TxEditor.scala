package hydrozoa.lib.tx

/*
 Transaction editing utilities with automatic redeemer re-indexing.

 According to the ledger spec, redeemers, which are stored in a
 `TransactionWitnessSet`, contain pointers to various transaction parts.
 The pointers are just numbers corresponding to indices in arrays.

 For example, a redeemer for spending a UTxO locked at a script address
 contains an index of the corresponding input. It's not very convenient
 because of the need to keep the indices correct while modifying the transaction.

 For example, if a new mint is added, all mint redeemer indices may have to
 be updated. This module automates these updates by providing a better API
 for modifying transactions that lets the developer abstract away from the indices.

 The main functions are `editTransaction` and `editTransactionSafe`.
 */

import cats.implicits.*
import hydrozoa.lib.tx.TransactionWithSigners.txwsUnsafeL
import hydrozoa.{emptyTransaction, lib}
import monocle.Lens
import monocle.Monocle.{focus, refocus}
import scalus.builtin.Data
import scalus.cardano.ledger.*
import scalus.|>

// ============================================================================
// DetachedRedeemer
// ============================================================================

/** Redeemer that was detached from a transaction. Contains just enough info for it to be
  * re-attached again, if a transaction needs a redeemer for some action.
  */
case class DetachedRedeemer(
    datum: Data,
    purpose: RedeemerPurpose
)

// ============================================================================
// RedeemerPurpose
// ============================================================================

/** Contains a value that a redeemer corresponds to. Allows finding a redeemer index, given a
  * transaction contains the value.
  */
sealed trait RedeemerPurpose

object RedeemerPurpose {
    case class ForSpend(input: TransactionInput) extends RedeemerPurpose
    case class ForMint(scriptHash: ScriptHash) extends RedeemerPurpose
    case class ForReward(rewardAddress: RewardAccount) extends RedeemerPurpose
    case class ForCert(certificate: Certificate) extends RedeemerPurpose
    case class ForVote(voter: Voter) extends RedeemerPurpose
    case class ForPropose(proposal: ProposalProcedure) extends RedeemerPurpose
}

object RedeemerPurposeUtils {
    def redeemerPurposeToRedeemerTag(purpose: RedeemerPurpose): RedeemerTag = purpose match {
        case _: RedeemerPurpose.ForSpend   => RedeemerTag.Spend
        case _: RedeemerPurpose.ForMint    => RedeemerTag.Mint
        case _: RedeemerPurpose.ForReward  => RedeemerTag.Reward
        case _: RedeemerPurpose.ForCert    => RedeemerTag.Cert
        case _: RedeemerPurpose.ForPropose => RedeemerTag.Proposing
        case _: RedeemerPurpose.ForVote    => RedeemerTag.Voting
    }
}

// ============================================================================
// RedeemersContext
// ============================================================================

/** Contains parts of a transaction that are needed for redeemer processing.
  */
case class RedeemersContext(
    inputs: Vector[TransactionInput],
    mintingPolicyHashes: Vector[ScriptHash],
    rewardAddresses: Vector[RewardAccount],
    certs: Vector[Certificate],
    proposals: Vector[ProposalProcedure],
    voters: Vector[Voter]
)

// ============================================================================
// RedeemersContext utilities
// ============================================================================

object RedeemersContext {
    def fromTransaction(tx: Transaction): RedeemersContext = {
        val body = tx.body.value
        RedeemersContext(
          inputs = body.inputs.toSeq.toVector,
          mintingPolicyHashes = body.mint.map(_.assets.keys.toVector).getOrElse(Vector.empty),
          rewardAddresses = body.withdrawals.getOrElse(Withdrawals.empty).withdrawals.keys.toVector,
          /*
          TODO: shouldn't this be TaggedOrderedSet?
          /** Certificates for delegation, stake operations, etc. */
          certificates: TaggedSet[Certificate] = TaggedSet.empty,
           */
          certs = body.certificates.toIndexedSeq.toVector,
          proposals = body.proposalProcedures.toSeq.toVector,
          voters = body.votingProcedures match {
              case Some(voters) =>
                  voters.procedures.keys.toVector
              case None => Vector.empty
          }
        )
    }
}

// ============================================================================
// Redeemer attachment/detachment
// ============================================================================

object RedeemerManagement {
    def detachRedeemer(ctx: RedeemersContext, redeemer: Redeemer): Option[DetachedRedeemer] = {
        val index = redeemer.index
        val purposeOpt = redeemer.tag match
            case RedeemerTag.Spend =>
                ctx.inputs.lift(index).map(RedeemerPurpose.ForSpend.apply)
            case RedeemerTag.Mint =>
                ctx.mintingPolicyHashes.lift(index).map(RedeemerPurpose.ForMint.apply)
            case RedeemerTag.Reward =>
                ctx.rewardAddresses.lift(index).map(RedeemerPurpose.ForReward.apply)
            case RedeemerTag.Cert =>
                ctx.certs.lift(index).map(RedeemerPurpose.ForCert.apply)
            case RedeemerTag.Proposing =>
                ctx.proposals.lift(index).map(RedeemerPurpose.ForPropose.apply)
            case RedeemerTag.Voting =>
                ctx.voters.lift(index).map(RedeemerPurpose.ForVote.apply)

        purposeOpt.map(purpose => DetachedRedeemer(redeemer.data, purpose))
    }

    def attachRedeemer(ctx: RedeemersContext, detached: DetachedRedeemer): Option[Redeemer] = {
        val (tag, indexOpt) = detached.purpose match {
            case RedeemerPurpose.ForSpend(input) =>
                (RedeemerTag.Spend, ctx.inputs.indexOf(input))
            case RedeemerPurpose.ForMint(scriptHash) =>
                (RedeemerTag.Mint, ctx.mintingPolicyHashes.indexOf(scriptHash))
            case RedeemerPurpose.ForReward(rewardAddress) =>
                (RedeemerTag.Reward, ctx.rewardAddresses.indexOf(rewardAddress))
            case RedeemerPurpose.ForCert(certificate) =>
                (RedeemerTag.Cert, ctx.certs.indexOf(certificate))
            case RedeemerPurpose.ForPropose(proposal) =>
                (RedeemerTag.Proposing, ctx.proposals.indexOf(proposal))
            case RedeemerPurpose.ForVote(voter) =>
                (RedeemerTag.Voting, ctx.voters.indexOf(voter))
        }

        if (indexOpt >= 0) {
            Some(
              Redeemer(
                tag = tag,
                index = indexOpt,
                data = detached.datum,
                exUnits = ExUnits.zero
              )
            )
        } else {
            None
        }
    }

    def attachRedeemers(
        ctx: RedeemersContext,
        detached: Vector[DetachedRedeemer]
    ): Either[DetachedRedeemer, Vector[Redeemer]] = {
        detached.traverse(redeemer => attachRedeemer(ctx, redeemer).toRight(redeemer))
    }
}
// ===========================================================================
// ExpectedSigner
// ===========================================================================

case class ExpectedSigner(
    expectedSigner: AddrKeyHash,
    signerPurpose: SignerPurpose
)

// ============================================================================
// SignerPurpose
// ============================================================================

/** Contains a value that a Signer corresponds to.
  */
sealed trait SignerPurpose:
    /** returns true if this signer purpose is valid for the given transaction */
    def isIn(tx: Transaction): Boolean

object SignerPurpose {
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

// ============================================================================
// EditableTransaction
// ============================================================================

/** A transaction with redeemers detached.
  */
case class EditableTransaction(
    transaction: TransactionWithSigners,
    redeemers: Vector[DetachedRedeemer]
)

/** A transaction paired with the expected signers. This is necessary for accurate fee calculation
  *
  * If the number of expected signers is too small compared to the number of actual signers, the
  * transaction should fail with an insufficient fee error.
  *
  * If the number of expected signers is too large compared to the number of actual signers, the
  * transaction may pass, but the fee will be over-paid.
  *
  * Note: this class is opaque. It is not (or should not be) possible to have expectedSigners that
  * point to invalid transaction components
  */

case class TransactionWithSigners private (tx: Transaction, expectedSigners: Set[ExpectedSigner]):
    /** Modify a TransactionWithSigners, dropping any signatures that point to components that have
      * been removed.
      *
      * @param f
      * @return
      */
    def modify(f: TransactionWithSigners => TransactionWithSigners): TransactionWithSigners = {
        val modifiedTx = f(this)
        val signersPartition = TransactionWithSigners.partitionValidSignatures(
          modifiedTx.tx,
          modifiedTx.expectedSigners
        )
        new TransactionWithSigners(modifiedTx.tx, signersPartition.validSigners)
    }

    /** Add additional signers to the transaction, silently dropping any that point to invalid
      * componenets
      */
    def addSigners(additionalSigners: Set[ExpectedSigner]): TransactionWithSigners = {
        val signersPartition =
            TransactionWithSigners.partitionValidSignatures(this.tx, additionalSigners)
        new TransactionWithSigners(this.tx, signersPartition._1)
    }

    /** Safely modify a TransactionWithSigners, returning a Left if any signatures point to
      * components that have been removed.
      *
      * @param f
      * @return
      */
    def modifySafe(
        f: TransactionWithSigners => TransactionWithSigners
    ): Either[
      // NOTE: Should be a non-empty set, but it seems like that has to come from a SortedSet,
      // and I don't yet know how to handle Ordering instances in scala.
      Set[ExpectedSigner],
      TransactionWithSigners
    ] = {
        val modifiedTx = f(this)
        val signersPartition = TransactionWithSigners.partitionValidSignatures(
          modifiedTx.tx,
          modifiedTx.expectedSigners
        )
        if signersPartition.invalidSigners.isEmpty
        then Right(new TransactionWithSigners(modifiedTx.tx, signersPartition.validSigners))
        else Left(signersPartition.invalidSigners)
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
      Set[ExpectedSigner],
      TransactionWithSigners
    ] = {
        val signersPartition = TransactionWithSigners.partitionValidSignatures(
          this.tx,
          additionalSigners
        )
        if signersPartition.invalidSigners.isEmpty
        then Right(new TransactionWithSigners(this.tx, signersPartition.validSigners))
        else Left(signersPartition.invalidSigners)
    }

object TransactionWithSigners:
    val empty: TransactionWithSigners = TransactionWithSigners.mkUnsafe(emptyTransaction, Set.empty)
    private case class SignersPartition(
        validSigners: Set[ExpectedSigner],
        invalidSigners: Set[ExpectedSigner]
    )

    private def partitionValidSignatures(
        tx: Transaction,
        expectedSigners: Set[ExpectedSigner]
    ): SignersPartition =
        expectedSigners.foldLeft(SignersPartition(Set.empty, Set.empty))((acc, signer) =>
            if signer.signerPurpose.isIn(tx) then acc.focus(_.validSigners).modify(_ + signer)
            else acc.focus(_.invalidSigners).modify(_ + signer)
        )

    /** Create a transaction with expected signers, returning a Left if any signers are not present.
      * @param tx
      *   a transaction
      * @param expectedSigners
      *   a set of expected signers for this transaction
      * @return
      *   Left(invalidSigners) or Right(transactionWithSigners)
      */
    def apply(
        tx: Transaction,
        expectedSigners: Set[ExpectedSigner]
    ): Either[Set[ExpectedSigner], TransactionWithSigners] = {
        val signersPartition = partitionValidSignatures(tx, expectedSigners)
        if signersPartition.invalidSigners.isEmpty
        then Right(new TransactionWithSigners(tx, signersPartition.validSigners))
        else Left(signersPartition.invalidSigners)
    }

    /** Silently drops any signers that point to invalid transaction components
      * @param tx
      * @param expectedSigners
      * @return
      */
    def mkUnsafe(tx: Transaction, expectedSigners: Set[ExpectedSigner]): TransactionWithSigners = {
        val signersPartition = partitionValidSignatures(tx, expectedSigners)
        new TransactionWithSigners(tx, signersPartition.validSigners)
    }

    /** Lens using [[mkUnsafe]].
      *
      * This is safe to use only if you're modifying parts of the transaction that do not correspond
      * to signatures.
      */
    val txwsUnsafeL: Lens[TransactionWithSigners, Transaction] = {
        val getter: TransactionWithSigners => Transaction = (txws => txws.tx)
        val setter: Transaction => TransactionWithSigners => TransactionWithSigners = {
            (tx => txws => mkUnsafe(tx = tx, expectedSigners = txws.expectedSigners))
        }
        Lens(getter)(setter)
    }

// ============================================================================
// Transaction conversion functions
// ============================================================================

object TransactionConversion {

    /** Detach transaction redeemers. Leaves invalid redeemers in the transaction's witness set, and
      * places the valid ones alongside the transaction.
      */
    def toEditableTransaction(tx: TransactionWithSigners): EditableTransaction = {
        val ctx = RedeemersContext.fromTransaction(tx.tx)
        val witnessSet = tx.tx.witnessSet

        val (validRedeemers, invalidRedeemers) = {
            witnessSet.redeemers.map(_.value) match {
                case None => (Seq.empty, Seq.empty)
                case Some(rs) =>
                    rs.toSeq.partition(redeemer =>
                        RedeemerManagement.detachRedeemer(ctx, redeemer).isDefined
                    )
            }
        }

        val redeemers = validRedeemers.flatMap(RedeemerManagement.detachRedeemer(ctx, _)).toVector
        val updatedWitnessSet = witnessSet.copy(redeemers =
            if invalidRedeemers.isEmpty then None
            else Some(KeepRaw.apply(Redeemers.from(invalidRedeemers)))
        )

        val updatedTx =
            tx |> txwsUnsafeL.refocus(_.witnessSet).replace(updatedWitnessSet)

        EditableTransaction(updatedTx, redeemers)
    }

    /** Detach transaction redeemers. Removes redeemers from the witness set and places them
      * alongside the transaction. Fails if there are redeemers that do not point to anything.
      */
    def toEditableTransactionSafe(
        tx: TransactionWithSigners
    ): Either[Redeemer, EditableTransaction] = {
        val ctx = RedeemersContext.fromTransaction(tx.tx)

        for {
            redeemers <- tx.tx.witnessSet.redeemers match {
                case None => Right(Vector.empty)
                case Some(rs) =>
                    rs.value.toSeq
                        .traverse { redeemer =>
                            RedeemerManagement.detachRedeemer(ctx, redeemer).toRight(redeemer)
                        }
            }

            updatedTx = tx |> txwsUnsafeL
                .refocus(_.witnessSet)
                .replace(TransactionWitnessSet.empty)
        } yield EditableTransaction(updatedTx, redeemers.toVector)
    }

    /** Re-attach transaction redeemers. Fails if there are detached redeemers that are not valid
      * (do not point to anything in the transaction).
      */
    def fromEditableTransactionSafe(
        editable: EditableTransaction
    ): Option[TransactionWithSigners] = {
        val ctx = RedeemersContext.fromTransaction(editable.transaction.tx)

        RedeemerManagement.attachRedeemers(ctx, editable.redeemers) match {
            case Left(_) => None
            case Right(attachedRedeemers) =>
                val currentWitnessSet = editable.transaction.tx.witnessSet
                val invalidRedeemers =
                    currentWitnessSet.redeemers.map(_.value.toSeq.toVector).getOrElse(Vector.empty)
                val allRedeemers = (invalidRedeemers ++ attachedRedeemers).distinct
                val updatedWitnessSet =
                    currentWitnessSet.copy(redeemers =
                        if allRedeemers.isEmpty then None
                        else Some(KeepRaw.apply(Redeemers.from(allRedeemers)))
                    )
                val updatedTx = editable.transaction |> txwsUnsafeL
                    .refocus(_.witnessSet)
                    .replace(updatedWitnessSet)
                Some(updatedTx)
        }
    }

    /** Re-attach transaction redeemers. Silently drops detached redeemers that are not valid.
      */
    def fromEditableTransaction(editable: EditableTransaction): TransactionWithSigners = {
        val ctx = RedeemersContext.fromTransaction(editable.transaction.tx)
        val attachedRedeemers =
            editable.redeemers.flatMap(RedeemerManagement.attachRedeemer(ctx, _))

        val currentWitnessSet = editable.transaction.tx.witnessSet
        val invalidRedeemers =
            currentWitnessSet.redeemers.map(_.value.toSeq).getOrElse(Seq.empty)

        val allRedeemers = (invalidRedeemers ++ attachedRedeemers).distinct
        val updatedWitnessSet =
            currentWitnessSet.copy(redeemers =
                if allRedeemers.isEmpty then None
                else Some(KeepRaw.apply(Redeemers.from(allRedeemers)))
            )
        val updatedTx = editable.transaction |> txwsUnsafeL
            .refocus(_.witnessSet)
            .replace(updatedWitnessSet)
        updatedTx
    }
}

// ============================================================================
// Main transaction editing functions
// ============================================================================

object TransactionEditor {

    /** Edit a transaction, ensuring proper handling of redeemers.
      *
      * You can insert or delete inputs, certificates, mints or reward withdrawals: regardless of
      * the changes, the redeemers will be re-indexed to point to the correct transaction
      * components.
      *
      *   - If you add any new redeemers, and they point to the transaction components correctly,
      *     they are guaranteed to have correct indices in the output tx.
      *
      *   - If some component that has a redeemer pointing to it is removed, the corresponding
      *     redeemer will be removed as well from the resulting transaction.
      *
      * @param f
      *   endomorphism
      * @param tx
      *   source transaction
      * @return
      *   target transaction
      */
    def editTransaction(
        f: (TransactionWithSigners) => (TransactionWithSigners)
    )(tx: TransactionWithSigners): TransactionWithSigners = {
        val editableTx = TransactionConversion.toEditableTransaction(tx)
        val processedTransaction = editableTx.transaction.modify(f)
        val newEditableTx = TransactionConversion.toEditableTransaction(processedTransaction)
        val editedTx = editableTx.copy(
          transaction = processedTransaction,
          redeemers = (editableTx.redeemers ++ newEditableTx.redeemers).distinct
        )
        TransactionConversion.fromEditableTransaction(editedTx)
    }

    /** Like `editTransaction`, but fails if:
      *   - the input transaction's redeemers have invalid `index` pointers
      *   - the resulting transaction's redeemers have invalid `index` pointers
      *
      * The first problematic redeemer will be returned as an error value.
      *
      * @param f
      *   endomorphism
      * @param tx
      *   source transaction
      * @return
      *   target transaction
      */
    def editTransactionSafe(
        f: TransactionWithSigners => TransactionWithSigners
    )(
        tx: TransactionWithSigners
    ): Either[Redeemer | Set[ExpectedSigner], TransactionWithSigners] = {
        for {
            editableTx <- TransactionConversion.toEditableTransactionSafe(tx)
            processedTx <- editableTx.transaction.modifySafe(f)
            newEditableTx <- TransactionConversion.toEditableTransactionSafe(processedTx)
            editedTx = editableTx.copy(
              transaction = processedTx,
              redeemers = (editableTx.redeemers ++ newEditableTx.redeemers).distinct
            )
            // Not using the safe variant: we want to drop stale redeemers
            result = TransactionConversion.fromEditableTransaction(editedTx)
        } yield result
    }
}
