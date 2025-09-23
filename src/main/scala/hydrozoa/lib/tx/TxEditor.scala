package hydrozoa.lib.tx

/*
-- | According to the ledger spec, redeemers that are stored in a
-- | `TransactionWitnessSet`, contain pointers to various transaction parts.
-- | The pointers are just numbers corresponding to indices in arrays.
-- |
-- | For example, a redeemer for spending a UTxO locked at a script address
-- | contains an index of the corresponding input of the transaction in the
-- | list of transaction body inputs.
-- |
-- | This API is not very convenient for the developers, because of the need to
-- | keep the indices correct while modifying the transaction.
-- |
-- | For example, if a new mint is added, all mint redeemer indices may have to
-- | be updated.
-- |
-- | This module automates these updates by providing a better API for modifying
-- | transactions, that lets the developer abstract away from the indices.
-- |
-- | The main functions are `editTransaction` and `editTransactionSafe`
 */

import cats.implicits.*
import scalus.builtin.Data
import scalus.cardano.ledger.*

import scala.collection.immutable.SortedMap

// ============================================================================
// DetachedRedeemer
// ============================================================================

/*
-- | Redeemer that was detached from a transaction.
-- | Contains just enough info for it to be re-attached again, if a
-- | transaction needs a redeemer for some action.
type DetachedRedeemer =
  { datum :: RedeemerDatum
  , purpose :: RedeemerPurpose
  }
 */

case class DetachedRedeemer(
    datum: Data,
    purpose: RedeemerPurpose
)

// ============================================================================
// RedeemerPurpose
// ============================================================================

/*
-- | Contains a value some redeemer corresponds to.
-- |
-- | Allows finding a redeemer index, given a transaction that contains the
-- | component the redeemer points to.
data RedeemerPurpose
  = ForSpend TransactionInput
  | ForMint ScriptHash
  | ForReward RewardAddress
  | ForCert Certificate
  | ForVote Voter
  | ForPropose VotingProposal

derive instance Generic RedeemerPurpose _
derive instance Eq RedeemerPurpose
derive instance Ord RedeemerPurpose

instance Show RedeemerPurpose where
  show = genericShow
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

// ============================================================================
// RedeemerPurpose utilities
// ============================================================================

/*
-- | Ignore the value that the redeemer points to and take just the tag.
redeemerPurposeToRedeemerTag :: RedeemerPurpose -> RedeemerTag
redeemerPurposeToRedeemerTag = case _ of
  ForSpend _ -> Spend
  ForMint _ -> Mint
  ForReward _ -> Reward
  ForCert _ -> Cert
  ForPropose _ -> Propose
  ForVote _ -> Vote
 */

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

/*
-- | Contains parts of a transaction that are important for redeemer processing.
-- | Used to avoid re-computing.
type RedeemersContext =
  { inputs :: Array TransactionInput
  , mintingPolicyHashes :: Array ScriptHash
  , rewardAddresses :: Array RewardAddress
  , certs :: Array Certificate
  , proposals :: Array VotingProposal
  , voters :: Array Voter
  }
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
// EditableTransaction
// ============================================================================

/*
-- | A transaction with redeemers detached.
type EditableTransaction =
  { transaction :: Transaction
  , redeemers :: Array DetachedRedeemer
  }
 */

case class EditableTransaction(
    transaction: Transaction,
    redeemers: Vector[DetachedRedeemer]
)

// ============================================================================
// RedeemersContext utilities
// ============================================================================

/*
mkRedeemersContext :: Transaction -> RedeemersContext
mkRedeemersContext (Transaction { body: TransactionBody txBody }) =
  { inputs: Set.toUnfoldable $ Set.fromFoldable txBody.inputs
  , mintingPolicyHashes:
      Set.toUnfoldable $ Map.keys $ unwrap $ fromMaybe
        (wrap Map.empty)
        txBody.mint
  , rewardAddresses: Set.toUnfoldable $ Map.keys $ txBody.withdrawals
  , certs: txBody.certs
  , proposals: txBody.votingProposals
  , voters: Set.toUnfoldable $ Map.keys $ unwrap txBody.votingProcedures
  }
 */

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
          voters = body.votingProcedures
              .getOrElse(VotingProcedures(SortedMap.empty))
              .procedures
              .keys
              .toVector
        )
    }
}

// ============================================================================
// Redeemer attachment/detachment
// ============================================================================

/*
detachRedeemer :: RedeemersContext -> Redeemer -> Maybe DetachedRedeemer
detachRedeemer ctx (Redeemer { tag, index, data: datum, exUnits: _ }) = do
  indexInt <- BigNum.toInt index
  purpose <- case tag of
    Spend ->
      ForSpend <$> Array.index ctx.inputs indexInt
    Mint ->
      ForMint <$> Array.index ctx.mintingPolicyHashes indexInt
    Reward ->
      ForReward <$> Array.index ctx.rewardAddresses indexInt
    Cert ->
      ForCert <$> Array.index ctx.certs indexInt
    Propose ->
      ForPropose <$> Array.index ctx.proposals indexInt
    Vote ->
      ForVote <$> Array.index ctx.voters indexInt
  pure { datum, purpose }
 */

object RedeemerManagement {
    def detachRedeemer(ctx: RedeemersContext, redeemer: Redeemer): Option[DetachedRedeemer] = {
        val index = redeemer.index.toInt
        val purposeOpt = redeemer.tag match {
            case RedeemerTag.Spend =>
                ctx.inputs.lift(index).map(RedeemerPurpose.ForSpend)
            case RedeemerTag.Mint =>
                ctx.mintingPolicyHashes.lift(index).map(RedeemerPurpose.ForMint)
            case RedeemerTag.Reward =>
                ctx.rewardAddresses.lift(index).map(RedeemerPurpose.ForReward)
            case RedeemerTag.Cert =>
                ctx.certs.lift(index).map(RedeemerPurpose.ForCert)
            case RedeemerTag.Proposing =>
                ctx.proposals.lift(index).map(RedeemerPurpose.ForPropose)
            case RedeemerTag.Voting =>
                ctx.voters.lift(index).map(RedeemerPurpose.ForVote)
        }

        purposeOpt.map(purpose => DetachedRedeemer(redeemer.data, purpose))
    }

    /*
attachRedeemer :: RedeemersContext -> DetachedRedeemer -> Maybe Redeemer
attachRedeemer ctx { purpose, datum } = do
  { tag, index } <- case purpose of
    ForSpend input -> findIndex (eq input) ctx.inputs <#> \index ->
      { tag: Spend, index }
    ForMint mps -> findIndex (eq mps) ctx.mintingPolicyHashes <#> \index ->
      { tag: Mint, index }
    ForReward addr -> findIndex (eq addr) ctx.rewardAddresses <#> \index ->
      { tag: Reward, index }
    ForCert cert -> findIndex (eq cert) ctx.certs <#> \index ->
      { tag: Cert, index }
    ForPropose proposal -> findIndex (eq proposal) ctx.proposals <#> \index ->
      { tag: Propose, index }
    ForVote voter -> findIndex (eq voter) ctx.voters <#> \index ->
      { tag: Vote, index }
  pure $
    Redeemer
      { tag, index: BigNum.fromInt index, data: datum, exUnits: ExUnits.empty }
     */

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

    /*
attachRedeemers
  :: RedeemersContext
  -> Array DetachedRedeemer
  -> Either DetachedRedeemer (Array Redeemer)
attachRedeemers ctx redeemers = do
  for redeemers \redeemer -> note redeemer $ attachRedeemer ctx redeemer
     */

    def attachRedeemers(
        ctx: RedeemersContext,
        detached: Vector[DetachedRedeemer]
    ): Either[DetachedRedeemer, Vector[Redeemer]] = {
        detached.traverse(redeemer => attachRedeemer(ctx, redeemer).toRight(redeemer))
    }
}

// ============================================================================
// Transaction conversion functions
// ============================================================================

/*
-- | Detach transaction redeemers.
-- | Leaves invalid redeemers in the transaction's witness set, and
-- | places the valid ones alongside the transaction.
toEditableTransaction :: Transaction -> EditableTransaction
toEditableTransaction tx =
  let
    ctx = mkRedeemersContext tx
    { yes: validRedeemers
    , no: invalidRedeemers
    } = partitionWith (detachRedeemer ctx) $
      tx ^. _witnessSet <<< _redeemers
  in
    { transaction: tx # _witnessSet <<< _redeemers .~ invalidRedeemers
    , redeemers: validRedeemers
    }
  where
  partitionWith
    :: forall a b
     . (a -> Maybe b)
    -> Array a
    -> { no :: Array a, yes :: Array b }
  partitionWith f =
    map (\x -> note x (f x)) >>> \arr ->
      { no: arr # map blush # catMaybes
      , yes: arr # map hush # catMaybes
      }
 */

object TransactionConversion {
    def toEditableTransaction(tx: Transaction): EditableTransaction = {
        val ctx = RedeemersContext.fromTransaction(tx)
        val witnessSet = tx.witnessSet
        val (validRedeemers, invalidRedeemers) =
            witnessSet.redeemers.map(_.value).getOrElse(Redeemers.from(Seq.empty)).toSeq.partition {
                redeemer =>
                    RedeemerManagement.detachRedeemer(ctx, redeemer).isDefined
            }

        val detached = validRedeemers.flatMap(RedeemerManagement.detachRedeemer(ctx, _)).toVector
        val updatedWitnessSet = witnessSet.copy(redeemers =
            if invalidRedeemers.isEmpty then None
            else Some(KeepRaw.apply(Redeemers.from(invalidRedeemers)))
        )
        val updatedTx = tx.copy(witnessSet = updatedWitnessSet)

        EditableTransaction(updatedTx, detached)
    }

    /*
-- | Detach transaction redeemers. Removes redeemers from the witness set and
-- | places them alongside the transaction.
-- |
-- | Fails if there are redeemers that do not point to anything.
toEditableTransactionSafe :: Transaction -> Either Redeemer EditableTransaction
toEditableTransactionSafe tx = do
  let
    ctx = mkRedeemersContext tx
  redeemers <- for (tx ^. _witnessSet <<< _redeemers) \redeemer ->
    note redeemer $ detachRedeemer ctx redeemer
  pure $
    { transaction: tx # _witnessSet <<< _redeemers .~ []
    , redeemers
    }
     */

    // TODO: fix
    def toEditableTransactionSafe(tx: Transaction): Either[Redeemer, EditableTransaction] = ???
    // {
    //    val ctx = RedeemersContext.fromTransaction(tx)
    //    val witnessSet = tx.witnessSet
    //
    //     val detachedResult = witnessSet.redeemers.toVector.traverse { redeemer =>
    //        RedeemerManagement.detachRedeemer(ctx, redeemer).toRight(redeemer)
    //     }
    //
    //    detachedResult.map { detached =>
    //        val updatedWitnessSet = witnessSet.copy(redeemers = IndexedSeq.empty)
    //        val updatedTx = tx.copy(witnessSet = Sized(updatedWitnessSet))
    //        EditableTransaction(updatedTx, detached)
    //    }
    // }

    /*
-- | Re-attach transaction redeemers.
-- | Fails if there are detached redeemers that are not valid (do not point
-- | to anything in the transaction).
fromEditableTransactionSafe :: EditableTransaction -> Maybe Transaction
fromEditableTransactionSafe { transaction, redeemers } = do
  let
    ctx = mkRedeemersContext transaction
  attachedRedeemers <- for redeemers $ attachRedeemer ctx
  pure $ transaction # _witnessSet <<< _redeemers
    %~ (nub <<< append attachedRedeemers)
     */

    // TODO: fix
    def fromEditableTransactionSafe(editable: EditableTransaction): Option[Transaction] = ???
    // {
    //    val ctx = RedeemersContext.fromTransaction(editable.transaction)
    //
    //    RedeemerManagement.attachRedeemers(ctx, editable.redeemers) match {
    //        case Left(_) => None
    //        case Right(attachedRedeemers) =>
    //            val currentWitnessSet = editable.transaction.witnessSet
    //            val allRedeemers = (currentWitnessSet.redeemers ++ attachedRedeemers).distinct
    //            val updatedWitnessSet =
    //                currentWitnessSet.copy(redeemers = allRedeemers.toIndexedSeq)
    //            val updatedTx = editable.transaction.copy(witnessSet = Sized(updatedWitnessSet))
    //            Some(updatedTx)
    //    }
    // }

    /*
-- | Re-attach transaction redeemers.
-- | Silently drops detached redeemers that are not valid.
fromEditableTransaction :: EditableTransaction -> Transaction
fromEditableTransaction { transaction, redeemers } =
  let
    ctx = mkRedeemersContext transaction
    attachedRedeemers = catMaybes $ redeemers <#> attachRedeemer ctx
  in
    transaction # _witnessSet <<< _redeemers
      %~ (nub <<< append attachedRedeemers)
     */

    // TODO: fix
    def fromEditableTransaction(editable: EditableTransaction): Transaction = ???
    // {
    //    val ctx = RedeemersContext.fromTransaction(editable.transaction)
    //    val attachedRedeemers =
    //        editable.redeemers.flatMap(RedeemerManagement.attachRedeemer(ctx, _))
    //
    //    val currentWitnessSet = editable.transaction.witnessSet
    //    val allRedeemers = (currentWitnessSet.redeemers ++ attachedRedeemers).distinct
    //    val updatedWitnessSet = currentWitnessSet.copy(redeemers = allRedeemers.toIndexedSeq)
    //    val updatedTx = editable.transaction.copy(witnessSet = Sized(updatedWitnessSet))
    //    updatedTx
    // }
}

// ============================================================================
// Main transaction editing functions
// ============================================================================

/*
-- | Edit a transaction, ensuring proper handling of redeemers.
-- |
-- | You can insert or delete inputs, certificates, mints or reward withdrawals:
-- | regardless of the changes, the redeemers will be re-indexed to point to
-- | the correct transaction components.
-- |
-- | - If you add any new redeemers and they point to the transaction components
-- | correctly, they are guaranteed to have correct indices in the output tx.
-- |
-- | - If some component that has a redeemer pointing to it is removed,
-- | the corresponding redeemer will be removed as well from the resulting
-- | transaction.
editTransaction :: (Transaction -> Transaction) -> (Transaction -> Transaction)
editTransaction f tx =
  let
    editableTx = toEditableTransaction tx
    processedTransaction = f editableTx.transaction
    { redeemers: newValidRedeemers } = toEditableTransaction
      processedTransaction
    editedTx = editableTx
      { transaction = processedTransaction
      , redeemers = nub $ editableTx.redeemers <> newValidRedeemers
      }
  in
    fromEditableTransaction editedTx
 */

object TransactionEditor {
    def editTransaction(f: Transaction => Transaction)(tx: Transaction): Transaction = {
        val editableTx = TransactionConversion.toEditableTransaction(tx)
        val processedTransaction = f(editableTx.transaction)
        val newEditableTx = TransactionConversion.toEditableTransaction(processedTransaction)
        val editedTx = editableTx.copy(
          transaction = processedTransaction,
          redeemers = (editableTx.redeemers ++ newEditableTx.redeemers).distinct
        )
        TransactionConversion.fromEditableTransaction(editedTx)
    }

    /*
-- | Like `editTransaction`, but fails if:
-- |
-- | - the input transaction's redeemers have invalid `index` pointers
-- | - the resulting transaction's redeemers have invalid `index` pointers
-- |
-- | The first problematic redeemer will be returned as error value.
editTransactionSafe
  :: (Transaction -> Transaction)
  -> (Transaction -> Either Redeemer Transaction)
editTransactionSafe f tx = do
  editableTx <- toEditableTransactionSafe tx
  let
    tx' = f editableTx.transaction
  { redeemers: newValidRedeemers } <- toEditableTransactionSafe tx'
  let
    editedTx = editableTx
      { transaction = tx'
      , redeemers = nub $ editableTx.redeemers <> newValidRedeemers
      }
  -- not using the safe variant: we *want* to drop stale redeemers
  pure $ fromEditableTransaction editedTx
     */

    def editTransactionSafe(
        f: Transaction => Transaction
    )(tx: Transaction): Either[Redeemer, Transaction] = {
        for {
            editableTx <- TransactionConversion.toEditableTransactionSafe(tx)
            processedTx = f(editableTx.transaction)
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
