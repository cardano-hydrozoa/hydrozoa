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
module Cardano.Transaction.Edit
  ( editTransaction
  , editTransactionSafe
  , toEditableTransactionSafe
  , toEditableTransaction
  , fromEditableTransactionSafe
  , fromEditableTransaction
  , RedeemerPurpose(ForSpend, ForMint, ForReward, ForCert, ForPropose, ForVote)
  , redeemerPurposeToRedeemerTag
  , DetachedRedeemer
  , EditableTransaction
  , RedeemersContext
  , attachRedeemer
  , detachRedeemer
  , mkRedeemersContext
  , attachRedeemers
  ) where

import Prelude

import Cardano.Types
  ( Certificate
  , Redeemer(Redeemer)
  , RedeemerTag(Mint, Spend, Reward, Cert, Propose, Vote)
  , RewardAddress
  , ScriptHash
  , Transaction(Transaction)
  , TransactionBody(TransactionBody)
  , TransactionInput
  , _redeemers
  , _witnessSet
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.ExUnits as ExUnits
import Cardano.Types.RedeemerDatum (RedeemerDatum)
import Cardano.Types.Voter (Voter)
import Cardano.Types.VotingProposal (VotingProposal)
import Data.Array (catMaybes, findIndex, nub)
import Data.Array as Array
import Data.Either (Either, blush, hush, note)
import Data.Generic.Rep (class Generic)
import Data.Lens ((%~), (.~), (^.))
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (for)

-- | Redeemer that was detached from a transaction.
-- | Contains just enough info for it to be re-attached again, if a
-- | transaction needs a redeemer for some action.
type DetachedRedeemer =
  { datum :: RedeemerDatum
  , purpose :: RedeemerPurpose
  }

-- | Contains a value some redeemer corresponds to.
-- |
-- | Allows to find a redeemer index, given a transaction that contains the
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

-- | Ignore the value that the redeemer points to and take just the tag.
redeemerPurposeToRedeemerTag :: RedeemerPurpose -> RedeemerTag
redeemerPurposeToRedeemerTag = case _ of
  ForSpend _ -> Spend
  ForMint _ -> Mint
  ForReward _ -> Reward
  ForCert _ -> Cert
  ForPropose _ -> Propose
  ForVote _ -> Vote

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

attachRedeemers
  :: RedeemersContext
  -> Array DetachedRedeemer
  -> Either DetachedRedeemer (Array Redeemer)
attachRedeemers ctx redeemers = do
  for redeemers \redeemer -> note redeemer $ attachRedeemer ctx redeemer

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

-- | A transaction with redeemers detached.
type EditableTransaction =
  { transaction :: Transaction
  , redeemers :: Array DetachedRedeemer
  }

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
