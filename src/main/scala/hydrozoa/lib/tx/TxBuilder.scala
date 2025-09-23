package hydrozoa.lib.tx

import scalus.builtin.Data
import scalus.cardano.ledger.*

// ============================================================================
// TransactionBuilderStep
// ============================================================================

/*
data TransactionBuilderStep
  = SpendOutput TransactionUnspentOutput (Maybe OutputWitness)
  | Pay TransactionOutput
  | MintAsset ScriptHash AssetName Int.Int CredentialWitness
  | IssueCertificate Certificate (Maybe CredentialWitness)
  | WithdrawRewards StakeCredential Coin (Maybe CredentialWitness)
  | SubmitProposal VotingProposal (Maybe CredentialWitness)
  | SubmitVotingProcedure Voter (Map GovernanceActionId VotingProcedure)
      (Maybe CredentialWitness)

derive instance Generic TransactionBuilderStep _
derive instance Eq TransactionBuilderStep
instance Show TransactionBuilderStep where
  show = genericShow
 */

sealed trait TransactionBuilderStep

object TransactionBuilderStep {
    case class SpendOutput(
        utxo: (TransactionInput, TransactionOutput),
        witness: Option[OutputWitness]
    ) extends TransactionBuilderStep

    case class Pay(output: TransactionOutput) extends TransactionBuilderStep

    case class MintAsset(
        scriptHash: ScriptHash,
        assetName: AssetName,
        amount: Long,
        witness: CredentialWitness
    ) extends TransactionBuilderStep

    case class IssueCertificate(
        cert: Certificate,
        witness: Option[CredentialWitness]
    ) extends TransactionBuilderStep

    case class WithdrawRewards(
        stakeCredential: Credential,
        amount: Coin,
        witness: Option[CredentialWitness]
    ) extends TransactionBuilderStep

    case class SubmitProposal(
        proposal: ProposalProcedure,
        witness: Option[CredentialWitness]
    ) extends TransactionBuilderStep

    case class SubmitVotingProcedure(
        voter: Voter,
        votes: Map[GovActionId, VotingProcedure],
        witness: Option[CredentialWitness]
    ) extends TransactionBuilderStep
}

// ============================================================================
// OutputWitness
// ============================================================================

/*
-- | `OutputWitness` is used to provide the evidence needed to consume an
-- | output. It must correspond to a `TransactionUnspentOutput` address'
-- | payment credential to unlock it.
data OutputWitness
  = NativeScriptOutput (ScriptWitness NativeScript)
  | PlutusScriptOutput (ScriptWitness PlutusScript) RedeemerDatum
      (Maybe DatumWitness)

derive instance Generic OutputWitness _
derive instance Eq OutputWitness
instance Show OutputWitness where
  show = genericShow
 */

sealed trait OutputWitness

object OutputWitness {
    case class NativeScriptOutput(witness: ScriptWitness[Script.Native]) extends OutputWitness
    case class PlutusScriptOutput(
        witness: ScriptWitness[PlutusScript],
        redeemer: Data,
        datum: Option[DatumWitness]
    ) extends OutputWitness
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
data CredentialWitness
  = NativeScriptCredential (ScriptWitness NativeScript)
  | PlutusScriptCredential (ScriptWitness PlutusScript) RedeemerDatum

derive instance Eq CredentialWitness
derive instance Generic CredentialWitness _
instance Show CredentialWitness where
  show = genericShow
 */

sealed trait CredentialWitness

object CredentialWitness {
    case class NativeScriptCredential(witness: ScriptWitness[Script.Native])
        extends CredentialWitness
    case class PlutusScriptCredential(
        witness: ScriptWitness[PlutusScript],
        redeemer: Data
    ) extends CredentialWitness
}

// ============================================================================
// ScriptWitness
// ============================================================================

/*
-- | Gives the user options for specifying everything needed to unlock an action guarded by a script, including
-- | - Spending a UTxO located at an address with a ScriptHash payment credential.
-- | - Witnessing credential operations requiring a script hash
-- | - Witnessing  a mint 
-- | - Witnessing a rewards withdrawal for a script hash credential
-- | 
-- |  The two constructors behave as follows:
-- | - `ScriptValue` contains a script for the witness set.
-- |
-- | - `ScriptReference` contains a CIP-31 reference input where the inline script should be available at, and a flag to either spend the referenced input or just reference it.


derive instance Show a => Generic (ScriptWitness a) _
derive instance Eq a => Eq (ScriptWitness a)
instance Show a => Show (ScriptWitness a) where
  show = genericShow
 */

sealed trait ScriptWitness[+A]

object ScriptWitness {
    case class ScriptValue[A](script: A) extends ScriptWitness[A]
    case class ScriptReference(
        input: TransactionInput,
        action: RefInputAction
    ) extends ScriptWitness[Nothing]
}

// ============================================================================
// RefInputAction
// ============================================================================

/*
-- | Inputs can be referenced or spent in a transaction (See CIP-31).
-- | Inline datums (CIP-32) and reference scripts (CIP-33) contained within
-- | transaction outputs become visible to the script context of the
-- | transaction, regardless of whether the output is spent or just
-- | referenced. This data type lets the developer to specify, which
-- | action to perform with a referenced input.
data RefInputAction
  = ReferenceInput
  | SpendInput

derive instance Generic RefInputAction _
derive instance Eq RefInputAction
instance Show RefInputAction where
  show = genericShow
 */

sealed trait RefInputAction

object RefInputAction {
    case object ReferenceInput extends RefInputAction
    case object SpendInput extends RefInputAction
}

// ============================================================================
// DatumWitness
// ============================================================================

/*
-- | Datums in UTxOs can be stored in two forms: inline datums or datum hashes.
-- | When there's a hash, we need to provide a datum corresponding to this hash,
-- | which can be done by either providing the value literally, or using a
-- | reference input where it is stored inline.
data DatumWitness
  = DatumValue PlutusData
  | DatumReference TransactionInput RefInputAction

derive instance Generic DatumWitness _
derive instance Eq DatumWitness
instance Show DatumWitness where
  show = genericShow
 */

sealed trait DatumWitness

object DatumWitness {
    case class DatumValue(datum: Data) extends DatumWitness
    case class DatumReference(
        input: TransactionInput,
        action: RefInputAction
    ) extends DatumWitness
}

/*
module Cardano.Transaction.Builder
  ( TransactionBuilderStep
      ( SpendOutput
      , Pay
      , MintAsset
      , IssueCertificate
      , WithdrawRewards
      , SubmitProposal
      , SubmitVotingProcedure
      )
  , OutputWitness(NativeScriptOutput, PlutusScriptOutput)
  , CredentialWitness(NativeScriptCredential, PlutusScriptCredential)
  , ScriptWitness(ScriptValue, ScriptReference)
  , DatumWitness(DatumValue, DatumReference)
  , RefInputAction(ReferenceInput, SpendInput)
  , TxBuildError
      ( WrongSpendWitnessType
      , IncorrectDatumHash
      , IncorrectScriptHash
      , WrongOutputType
      , WrongCredentialType
      , DatumWitnessNotProvided
      , UnneededDatumWitness
      , UnneededDeregisterWitness
      , UnneededSpoVoteWitness
      , UnneededProposalPolicyWitness
      , RedeemerIndexingError
      , RedeemerIndexingInternalError
      , WrongNetworkId
      , NoTransactionNetworkId
      )
  , ExpectedWitnessType(ScriptHashWitness, PubKeyHashWitness)
  , CredentialAction
      ( StakeCert
      , Withdrawal
      , Minting
      , Voting
      , Proposing
      )
  , buildTransaction
  , modifyTransaction
  , explainTxBuildError
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Transaction.Edit
  ( DetachedRedeemer
  , RedeemerPurpose(ForCert, ForReward, ForSpend, ForMint, ForVote, ForPropose)
  , fromEditableTransactionSafe
  , toEditableTransactionSafe
  )
import Cardano.Types
  ( Address
  , AssetName
  , Coin
  , DataHash
  , GovernanceAction(ChangePParams, TreasuryWdrl)
  , NativeScript
  , NetworkId
  , PlutusData
  , PlutusScript
  , Redeemer
  , RewardAddress
  , StakeCredential(StakeCredential)
  , Transaction
  , TransactionBody
  , _certs
  , _inputs
  , _mint
  , _networkId
  , _outputs
  , _referenceInputs
  , _withdrawals
  )
import Cardano.Types.Address (getNetworkId, getPaymentCredential)
import Cardano.Types.Certificate
  ( Certificate
      ( StakeRegistration
      , StakeDeregistration
      , StakeDelegation
      , PoolRegistration
      , PoolRetirement
      , VoteDelegCert
      , StakeVoteDelegCert
      , StakeRegDelegCert
      , VoteRegDelegCert
      , StakeVoteRegDelegCert
      , AuthCommitteeHotCert
      , ResignCommitteeColdCert
      , RegDrepCert
      , UnregDrepCert
      , UpdateDrepCert
      )
  )
import Cardano.Types.Credential
  ( Credential(PubKeyHashCredential, ScriptHashCredential)
  )
import Cardano.Types.Credential as Credential
import Cardano.Types.DataHash as PlutusData
import Cardano.Types.GovernanceActionId (GovernanceActionId)
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash, OutputDatum))
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.RedeemerDatum (RedeemerDatum)
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.StakeCredential (StakeCredential)
import Cardano.Types.StakePubKeyHash (StakePubKeyHash)
import Cardano.Types.Transaction (_body, _witnessSet)
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionBody (_votingProcedures, _votingProposals)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput, _address, _datum)
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput
  , _output
  )
import Cardano.Types.TransactionWitnessSet
  ( _nativeScripts
  , _plutusData
  , _plutusScripts
  )
import Cardano.Types.Voter (Voter(Cc, Drep, Spo))
import Cardano.Types.VotingProcedure (VotingProcedure)
import Cardano.Types.VotingProposal (VotingProposal)
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.State (StateT, modify_, runStateT)
import Control.Monad.State.Trans (gets)
import Control.Monad.Trans.Class (lift)
import Data.Array (nub)
import Data.Bifunctor (lmap)
import Data.ByteArray (byteArrayToHex)
import Data.Either (Either(Left, Right), either, note)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view, (%=), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse_)
import Data.Tuple (snd)
import Record (merge)
import Type.Proxy (Proxy(Proxy))

data TransactionBuilderStep
  = SpendOutput TransactionUnspentOutput (Maybe OutputWitness)
  | Pay TransactionOutput
  | MintAsset ScriptHash AssetName Int.Int CredentialWitness
  | IssueCertificate Certificate (Maybe CredentialWitness)
  | WithdrawRewards StakeCredential Coin (Maybe CredentialWitness)
  | SubmitProposal VotingProposal (Maybe CredentialWitness)
  | SubmitVotingProcedure Voter (Map GovernanceActionId VotingProcedure)
      (Maybe CredentialWitness)

derive instance Generic TransactionBuilderStep _
derive instance Eq TransactionBuilderStep
instance Show TransactionBuilderStep where
  show = genericShow

-- | `OutputWitness` is used to provide the evidence needed to consume an
-- | output. It must correspond to a `TransactionUnspentOutput` address'
-- | payment credential to unlock it.
data OutputWitness
  = NativeScriptOutput (ScriptWitness NativeScript)
  | PlutusScriptOutput (ScriptWitness PlutusScript) RedeemerDatum
      (Maybe DatumWitness)

derive instance Generic OutputWitness _
derive instance Eq OutputWitness
instance Show OutputWitness where
  show = genericShow

-- | `CredentialWitness` is used to provide the evidence needed to perform
-- | operations on behalf of a credential, which include:
-- |
-- | - Minting
-- | - Certificate witnessing
-- | - Rewards withdrawal
-- |
-- | Unlike `OutputWitness`, it does not include a `DatumWitness`, because
-- | minting policies and stake scripts do not have a datum.
data CredentialWitness
  = NativeScriptCredential (ScriptWitness NativeScript)
  | PlutusScriptCredential (ScriptWitness PlutusScript) RedeemerDatum

derive instance Eq CredentialWitness
derive instance Generic CredentialWitness _
instance Show CredentialWitness where
  show = genericShow

-- | Gives the user options for specifying everything needed to spend an UTxO
-- | located at an address with a ScriptHash payment credential.
-- |
-- | - `ScriptValue` contains a script for the witness set.
-- |
-- | - `ScriptReference` contains a CIP-31 reference input where the inline script should be available at, and a flag to either spend the referenced input or just reference it.
data ScriptWitness a
  = ScriptValue a
  | ScriptReference TransactionInput RefInputAction

derive instance Show a => Generic (ScriptWitness a) _
derive instance Eq a => Eq (ScriptWitness a)
instance Show a => Show (ScriptWitness a) where
  show = genericShow

-- | Inputs can be referenced or spent in a transaction (See CIP-31).
-- | Inline datums (CIP-32) and reference scripts (CIP-33) contained within
-- | transaction outputs become visible to the script context of the
-- | transaction, regardless of whether the output is spent or just
-- | referenced. This data type lets the developer to specify, which
-- | action to perform with a referenced input.
data RefInputAction
  = ReferenceInput
  | SpendInput

derive instance Generic RefInputAction _
derive instance Eq RefInputAction
instance Show RefInputAction where
  show = genericShow

-- | Datums in UTxOs can be stored in two forms: inline datums or datum hashes.
-- | When there's a hash, we need to provide a datum corresponding to this hash,
-- | which can be done by either providing the value literally, or using a
-- | reference input where it is stored inline.
data DatumWitness
  = DatumValue PlutusData
  | DatumReference TransactionInput RefInputAction

derive instance Generic DatumWitness _
derive instance Eq DatumWitness
instance Show DatumWitness where
  show = genericShow

data StakeWitness
  = PubKeyHashStakeWitness StakePubKeyHash
  | PlutusScriptStakeWitness (ScriptWitness PlutusScript)
  | NativeScriptStakeWitness (ScriptWitness NativeScript)

derive instance Generic StakeWitness _
instance Show StakeWitness where
  show = genericShow

type Context =
  { transaction :: Transaction
  , redeemers :: Array DetachedRedeemer
  , networkId :: Maybe NetworkId
  }

_transaction
  :: Lens' Context Transaction
_transaction = prop (Proxy :: Proxy "transaction")

_redeemers
  :: Lens' Context (Array DetachedRedeemer)
_redeemers = prop (Proxy :: Proxy "redeemers")

data ExpectedWitnessType witness
  = ScriptHashWitness witness
  | PubKeyHashWitness

derive instance Generic (ExpectedWitnessType witness) _
derive instance Eq witness => Eq (ExpectedWitnessType witness)
instance Show witness => Show (ExpectedWitnessType witness) where
  show = genericShow

explainExpectedWitnessType :: forall a. ExpectedWitnessType a -> String
explainExpectedWitnessType (ScriptHashWitness _) = "ScriptHash"
explainExpectedWitnessType PubKeyHashWitness = "PubKeyHash"

data CredentialAction
  = StakeCert Certificate
  | Withdrawal RewardAddress
  | Minting ScriptHash
  | Voting Voter
  | Proposing VotingProposal

derive instance Generic CredentialAction _
derive instance Eq CredentialAction
instance Show CredentialAction where
  show = genericShow

explainCredentialAction :: CredentialAction -> String
explainCredentialAction (StakeCert _) = "This stake certificate"
explainCredentialAction (Withdrawal _) = "This stake rewards withdrawal"
explainCredentialAction (Minting _) = "This mint"
explainCredentialAction (Voting _) = "This voting procedure"
explainCredentialAction (Proposing _) = "This voting proposal"

data TxBuildError
  = WrongSpendWitnessType TransactionUnspentOutput
  | IncorrectDatumHash TransactionUnspentOutput PlutusData DataHash
  | IncorrectScriptHash (Either NativeScript PlutusScript) ScriptHash
  | WrongOutputType (ExpectedWitnessType OutputWitness) TransactionUnspentOutput
  | WrongCredentialType CredentialAction (ExpectedWitnessType CredentialWitness)
      Credential
  | DatumWitnessNotProvided TransactionUnspentOutput
  | UnneededDatumWitness TransactionUnspentOutput DatumWitness
  | UnneededDeregisterWitness StakeCredential CredentialWitness
  | UnneededSpoVoteWitness Credential CredentialWitness
  | UnneededProposalPolicyWitness VotingProposal CredentialWitness
  | RedeemerIndexingError Redeemer
  | RedeemerIndexingInternalError Transaction (Array TransactionBuilderStep)
  | WrongNetworkId Address
  | NoTransactionNetworkId

derive instance Generic TxBuildError _
derive instance Eq TxBuildError
instance Show TxBuildError where
  show = genericShow

explainTxBuildError :: TxBuildError -> String
explainTxBuildError (WrongSpendWitnessType utxo) =
  "`OutputWitness` is incompatible with the given output. The output does not contain a datum: "
    <> show utxo
explainTxBuildError (IncorrectDatumHash utxo datum datumHash) =
  "You provided a `DatumWitness` with a datum that does not match the datum hash present in a transaction output.\n  Datum: "
    <> show datum
    <> " (CBOR: "
    <> byteArrayToHex (unwrap $ encodeCbor datum)
    <> ")\n  Datum hash: "
    <> byteArrayToHex (unwrap $ encodeCbor datumHash)
    <> "\n  UTxO: "
    <> show utxo
explainTxBuildError (IncorrectScriptHash (Left nativeScript) hash) =
  "Provided script hash (" <> show hash
    <> ") does not match the provided native script ("
    <> show nativeScript
    <> ")"
explainTxBuildError (IncorrectScriptHash (Right plutusScript) hash) =
  "Provided script hash (" <> show hash
    <> ") does not match the provided Plutus script ("
    <> show plutusScript
    <> ")"
explainTxBuildError (WrongOutputType (ScriptHashWitness _) utxo) =
  "The UTxO you provided requires no witness, because the payment credential of the address is a `PubKeyHash`. UTxO: "
    <> show
      utxo
explainTxBuildError (WrongOutputType PubKeyHashWitness utxo) =
  "The UTxO you provided requires a `ScriptHash` witness to unlock, because the payment credential of the address is a `ScriptHash`. UTxO: "
    <>
      show utxo
explainTxBuildError (WrongCredentialType operation expWitnessType cred) =
  explainCredentialAction operation <> " (" <> show operation <> ") requires a "
    <> explainExpectedWitnessType expWitnessType
    <> " witness: "
    <> show cred
explainTxBuildError (DatumWitnessNotProvided utxo) =
  "The UTxO you are trying to spend contains a datum hash. A matching `DatumWitness` is required. Use `getDatumByHash`. UTxO: "
    <> show utxo
explainTxBuildError (UnneededDatumWitness utxo witness) =
  "You've provided an optional `DatumWitness`, but the output you are spending already contains an inline datum (not just a datum hash). You should omit the provided datum witness. You provided: "
    <> show witness
    <> " for the UTxO: "
    <> show utxo
explainTxBuildError (UnneededDeregisterWitness stakeCredential witness) =
  "You've provided an optional `CredentialWitness`, but the stake credential you are trying to issue a deregistering certificate for is a PubKeyHash credential. You should omit the provided credential witness for this credential: "
    <> show stakeCredential
    <> ". Provided witness: "
    <> show witness
explainTxBuildError (UnneededSpoVoteWitness cred witness) =
  "You've provided an optional `CredentialWitness`, but the corresponding Voter is SPO (Stake Pool Operator). You should omit the provided credential witness for this credential: "
    <> show cred
    <> ". Provided witness: "
    <> show witness
explainTxBuildError (UnneededProposalPolicyWitness proposal witness) =
  "You've provided an optional `CredentialWitness`, but the corresponding proposal does not need to validate against the proposal policy. You should omit the provided credential witness for this proposal: "
    <> show proposal
    <> ". Provided witness: "
    <> show witness
explainTxBuildError (RedeemerIndexingError redeemer) =
  "Redeemer indexing error. Problematic redeemer that does not have a valid index: "
    <> show redeemer
explainTxBuildError (RedeemerIndexingInternalError tx steps) =
  "Internal redeemer indexing error. Please report as bug: " <> bugTrackerUrl
    <> "\nDebug info: Transaction: "
    <> show tx
    <> ", steps: "
    <> show steps
explainTxBuildError (WrongNetworkId address) =
  "The following `Address` that was specified in one of the UTxOs has a `NetworkId` different from the one `TransactionBody` has: "
    <> show address
explainTxBuildError NoTransactionNetworkId =
  "You are editing a transaction without a `NetworkId` set. To create a `RewardAddress`, a NetworkId is needed: set it in the `TransactionBody`"

type BuilderM a = StateT Context (Except TxBuildError) a

buildTransaction
  :: Array TransactionBuilderStep
  -> Either TxBuildError Transaction
buildTransaction =
  modifyTransaction Transaction.empty

modifyTransaction
  :: Transaction
  -> Array TransactionBuilderStep
  -> Either TxBuildError Transaction
modifyTransaction tx steps = do
  context <- do
    editableTransaction <- lmap RedeemerIndexingError $
      toEditableTransactionSafe tx
    pure $ merge editableTransaction
      { networkId: editableTransaction.transaction ^. _body <<< _networkId }
  let
    eiCtx = map snd
      $ runExcept
      $ flip runStateT context
      $ processConstraints steps
  eiCtx >>= \({ redeemers, transaction }) ->
    note (RedeemerIndexingInternalError tx steps) $
      fromEditableTransactionSafe { redeemers, transaction }

processConstraints :: Array TransactionBuilderStep -> BuilderM Unit
processConstraints = traverse_ processConstraint

processConstraint :: TransactionBuilderStep -> BuilderM Unit
processConstraint = case _ of
  SpendOutput utxo spendWitness -> do
    assertNetworkId $ utxo ^. _output <<< _address
    _transaction <<< _body <<< _inputs
      %= pushUnique (unwrap utxo).input
    useSpendWitness utxo spendWitness
  Pay output -> do
    assertNetworkId $ output ^. _address
    _transaction <<< _body <<< _outputs
      -- intentionally not using pushUnique: we can
      -- create multiple outputs of the same shape
      %= flip append [ output ]
  MintAsset scriptHash assetName amount mintWitness ->
    useMintAssetWitness scriptHash assetName amount mintWitness
  IssueCertificate cert witness -> do
    _transaction <<< _body <<< _certs %= pushUnique cert
    useCertificateWitness cert witness
  WithdrawRewards stakeCredential amount witness ->
    useWithdrawRewardsWitness stakeCredential amount witness
  SubmitProposal proposal witness -> do
    _transaction <<< _body <<< _votingProposals
      %= pushUnique proposal
    useProposalWitness proposal witness
  SubmitVotingProcedure voter votes witness -> do
    _transaction <<< _body <<< _votingProcedures <<< _Newtype
      %= Map.insert voter votes
    useVotingProcedureWitness voter witness

assertNetworkId :: Address -> BuilderM Unit
assertNetworkId addr = do
  mbNetworkId <- gets _.networkId
  let
    addrNetworkId = getNetworkId addr
  case mbNetworkId of
    Nothing -> pure unit
    Just networkId -> do
      unless (networkId == addrNetworkId) do
        throwError (WrongNetworkId addr)

assertOutputType
  :: ExpectedWitnessType OutputWitness
  -> TransactionUnspentOutput
  -> BuilderM Unit
assertOutputType expectedType utxo = do
  mbCredential <- runMaybeT do
    cred <- MaybeT $ pure $ getPaymentCredential (utxo ^. _output <<< _address)
      <#> unwrap
    case expectedType of
      ScriptHashWitness witness -> do
        scriptHash <- MaybeT $ pure $ Credential.asScriptHash cred
        lift $ assertScriptHashMatchesOutputWitness scriptHash witness
        pure unit
      PubKeyHashWitness ->
        MaybeT $ pure $ Credential.asPubKeyHash cred $> unit
  unless (isJust mbCredential) do
    throwError $ WrongOutputType expectedType utxo

assertCredentialType
  :: CredentialAction
  -> ExpectedWitnessType CredentialWitness
  -> Credential
  -> BuilderM Unit
assertCredentialType action expectedType cred = do
  let wrongCredErr = WrongCredentialType action expectedType cred
  case expectedType of
    ScriptHashWitness witness -> do
      scriptHash <- liftMaybe wrongCredErr $ Credential.asScriptHash cred
      assertScriptHashMatchesCredentialWitness scriptHash witness
    PubKeyHashWitness ->
      maybe (throwError wrongCredErr) (const (pure unit)) $
        Credential.asPubKeyHash cred

useMintAssetWitness
  :: ScriptHash -> AssetName -> Int.Int -> CredentialWitness -> BuilderM Unit
useMintAssetWitness scriptHash assetName amount witness = do
  useCredentialWitness
    (Minting scriptHash)
    (ScriptHashCredential scriptHash)
    (Just witness)
  mbMint <- gets $ view $ _transaction <<< _body <<< _mint
  let
    thisMint = Mint.singleton scriptHash assetName amount
  newMint <- case mbMint of
    Nothing -> pure $ Just thisMint
    Just mint -> pure $ Mint.union mint thisMint
  modify_ $ _transaction <<< _body <<< _mint .~ newMint

assertScriptHashMatchesCredentialWitness
  :: ScriptHash
  -> CredentialWitness
  -> BuilderM Unit
assertScriptHashMatchesCredentialWitness scriptHash witness =
  traverse_ (assertScriptHashMatchesScript scriptHash) $
    case witness of
      PlutusScriptCredential (ScriptValue plutusScript) _ -> Just
        (Right plutusScript)
      NativeScriptCredential (ScriptValue nativeScript) -> Just
        (Left nativeScript)
      _ -> Nothing

assertScriptHashMatchesOutputWitness
  :: ScriptHash
  -> OutputWitness
  -> BuilderM Unit
assertScriptHashMatchesOutputWitness scriptHash witness =
  traverse_ (assertScriptHashMatchesScript scriptHash) $
    case witness of
      PlutusScriptOutput (ScriptValue plutusScript) _ _ -> Just
        (Right plutusScript)
      NativeScriptOutput (ScriptValue nativeScript) -> Just
        (Left nativeScript)
      _ -> Nothing

assertScriptHashMatchesScript
  :: ScriptHash
  -> Either NativeScript PlutusScript
  -> BuilderM Unit
assertScriptHashMatchesScript scriptHash eiScript = do
  let hash = either NativeScript.hash PlutusScript.hash eiScript
  unless (scriptHash == hash) do
    throwError $ IncorrectScriptHash eiScript scriptHash

useVotingProcedureWitness :: Voter -> Maybe CredentialWitness -> BuilderM Unit
useVotingProcedureWitness voter mbWitness = do
  cred <- case voter of
    Spo poolKeyHash -> do
      let cred = PubKeyHashCredential poolKeyHash
      case mbWitness of
        Just witness -> throwError $ UnneededSpoVoteWitness cred witness
        Nothing -> pure cred
    Cc cred -> pure cred
    Drep cred -> pure cred
  useCredentialWitness (Voting voter) cred mbWitness

useProposalWitness :: VotingProposal -> Maybe CredentialWitness -> BuilderM Unit
useProposalWitness proposal mbWitness =
  case getPolicyHash (unwrap proposal).govAction, mbWitness of
    Nothing, Just witness ->
      throwError $ UnneededProposalPolicyWitness proposal witness
    Just policyHash, witness ->
      useCredentialWitness (Proposing proposal)
        (ScriptHashCredential policyHash)
        witness
    Nothing, Nothing ->
      pure unit
  where
  getPolicyHash :: GovernanceAction -> Maybe ScriptHash
  getPolicyHash = case _ of
    ChangePParams action -> (unwrap action).policyHash
    TreasuryWdrl action -> (unwrap action).policyHash
    _ -> Nothing

useCertificateWitness :: Certificate -> Maybe CredentialWitness -> BuilderM Unit
useCertificateWitness cert mbWitness =
  case cert of
    StakeDeregistration stakeCred -> do
      let cred = unwrap stakeCred
      case stakeCred, mbWitness of
        StakeCredential (PubKeyHashCredential _), Just witness ->
          throwError $ UnneededDeregisterWitness stakeCred witness
        StakeCredential (PubKeyHashCredential _), Nothing -> pure unit
        StakeCredential (ScriptHashCredential _), Nothing ->
          throwError $ WrongCredentialType (StakeCert cert) PubKeyHashWitness
            cred
        StakeCredential (ScriptHashCredential scriptHash), Just witness ->
          assertScriptHashMatchesCredentialWitness scriptHash witness
      useCredentialWitness (StakeCert cert) cred mbWitness
    StakeDelegation stakeCred _ ->
      useCredentialWitness (StakeCert cert) (unwrap stakeCred) mbWitness
    StakeRegistration _ -> pure unit
    PoolRegistration _ -> pure unit
    PoolRetirement _ -> pure unit
    VoteDelegCert stakeCred _ ->
      useCredentialWitness (StakeCert cert) (unwrap stakeCred) mbWitness
    StakeVoteDelegCert stakeCred _ _ ->
      useCredentialWitness (StakeCert cert) (unwrap stakeCred) mbWitness
    StakeRegDelegCert stakeCred _ _ ->
      useCredentialWitness (StakeCert cert) (unwrap stakeCred) mbWitness
    VoteRegDelegCert stakeCred _ _ ->
      useCredentialWitness (StakeCert cert) (unwrap stakeCred) mbWitness
    StakeVoteRegDelegCert stakeCred _ _ _ ->
      useCredentialWitness (StakeCert cert) (unwrap stakeCred) mbWitness
    AuthCommitteeHotCert _ -> pure unit -- not supported
    ResignCommitteeColdCert _ _ -> pure unit -- not supported
    RegDrepCert drepCred _ _ ->
      useCredentialWitness (StakeCert cert) drepCred mbWitness
    UnregDrepCert drepCred _ ->
      useCredentialWitness (StakeCert cert) drepCred mbWitness
    UpdateDrepCert drepCred _ ->
      useCredentialWitness (StakeCert cert) drepCred mbWitness

useCredentialWitness
  :: CredentialAction
  -> Credential
  -> Maybe CredentialWitness
  -> BuilderM Unit
useCredentialWitness credAction cred mbWitness =
  case mbWitness of
    Nothing ->
      assertCredentialType credAction PubKeyHashWitness cred
    Just witness@(NativeScriptCredential nsWitness) -> do
      assertCredentialType credAction (ScriptHashWitness witness) cred
      useNativeScriptWitness nsWitness
    Just witness@(PlutusScriptCredential plutusScriptWitness redeemerDatum) ->
      do
        assertCredentialType credAction (ScriptHashWitness witness) cred
        usePlutusScriptWitness plutusScriptWitness
        let
          redeemer =
            { purpose: case credAction of
                Withdrawal rewardAddress -> ForReward rewardAddress
                StakeCert cert -> ForCert cert
                Minting scriptHash -> ForMint scriptHash
                Voting voter -> ForVote voter
                Proposing proposal -> ForPropose proposal
            -- ForSpend is not possible: for that we use OutputWitness
            , datum: redeemerDatum
            }
        _redeemers %= pushUnique redeemer

useWithdrawRewardsWitness
  :: StakeCredential -> Coin -> Maybe CredentialWitness -> BuilderM Unit
useWithdrawRewardsWitness stakeCredential amount witness = do
  networkId <- gets _.networkId >>=
    maybe (throwError NoTransactionNetworkId) pure
  let
    rewardAddress =
      { networkId
      , stakeCredential
      }
  _transaction <<< _body <<< _withdrawals %=
    Map.insert rewardAddress amount
  useCredentialWitness (Withdrawal rewardAddress) (unwrap stakeCredential)
    witness

-- | Tries to modify the transaction to make it consume a given output.
-- | Uses a `SpendWitness` to try to satisfy spending requirements.
useSpendWitness
  :: TransactionUnspentOutput -> Maybe OutputWitness -> BuilderM Unit
useSpendWitness utxo = case _ of
  Nothing -> do
    assertOutputType PubKeyHashWitness utxo
  Just witness@(NativeScriptOutput nsWitness) -> do
    assertOutputType (ScriptHashWitness witness) utxo
    -- attach the script
    useNativeScriptWitness nsWitness
  Just
    witness@
      (PlutusScriptOutput plutusScriptWitness redeemerDatum mbDatumWitness) ->
    do
      assertOutputType (ScriptHashWitness witness) utxo
      -- attach the script
      usePlutusScriptWitness plutusScriptWitness
      -- attach the datum
      useDatumWitnessForUtxo utxo mbDatumWitness
      -- attach the redeemer
      let
        uiRedeemer =
          { purpose: ForSpend (unwrap utxo).input
          , datum: redeemerDatum
          }
      _redeemers %= pushUnique uiRedeemer

usePlutusScriptWitness :: ScriptWitness PlutusScript -> BuilderM Unit
usePlutusScriptWitness =
  case _ of
    ScriptValue ps -> do
      _transaction <<< _witnessSet <<< _plutusScripts
        %= pushUnique ps
    ScriptReference input action -> do
      _transaction <<< _body <<< refInputActionToLens action
        %= pushUnique input

useNativeScriptWitness :: ScriptWitness NativeScript -> BuilderM Unit
useNativeScriptWitness =
  case _ of
    ScriptValue ns -> do
      _transaction <<< _witnessSet <<< _nativeScripts
        %= pushUnique ns
    ScriptReference refInput refInputAction -> do
      _transaction <<< _body <<< refInputActionToLens refInputAction
        %= pushUnique refInput

-- | Tries to modify the transaction state to make it consume a given script output.
-- | Uses a `DatumWitness` if the UTxO datum is provided as a hash.
useDatumWitnessForUtxo
  :: TransactionUnspentOutput -> Maybe DatumWitness -> BuilderM Unit
useDatumWitnessForUtxo utxo mbDatumWitness = do
  case utxo ^. _output <<< _datum of
    -- script outputs must have a datum
    Nothing -> throwError $ WrongSpendWitnessType utxo
    -- if the datum is inline, we don't need to attach it as witness
    Just (OutputDatum _providedDatum) -> do
      case mbDatumWitness of
        Just datumWitness ->
          throwError $ UnneededDatumWitness utxo datumWitness
        Nothing -> pure unit
    -- if the datum is provided as hash,
    Just (OutputDatumHash datumHash) ->
      case mbDatumWitness of
        -- if the datum witness was not provided, look the datum up
        Nothing -> do
          throwError $ DatumWitnessNotProvided utxo
        -- if the datum was provided, check it's hash. if it matches the one
        -- specified in the output, use that datum.
        Just (DatumValue providedDatum)
          | datumHash == PlutusData.hashPlutusData providedDatum -> do
              _transaction <<< _witnessSet <<< _plutusData
                %= pushUnique providedDatum
        -- otherwise, fail
        Just (DatumValue providedDatum) -> do
          throwError $ IncorrectDatumHash utxo providedDatum datumHash
        -- If a reference input is provided, we just attach it without
        -- checking (doing that would require looking up the utxo)
        --
        -- We COULD require the user to provide the output to do an additional
        -- check, but we don't, because otherwise the contents of the ref output
        -- do not matter (i.e. they are not needed for balancing).
        -- UTxO lookups are quite expensive, so it's best to not require more
        -- of them than strictly necessary.
        Just (DatumReference datumWitnessRef refInputAction) -> do
          _transaction <<< _body <<< refInputActionToLens refInputAction
            %= pushUnique datumWitnessRef

-- | Depending on `RefInputAction` value, we either want to spend a reference
-- | UTxO, or just reference it.
refInputActionToLens
  :: RefInputAction
  -> Lens' TransactionBody (Array TransactionInput)
refInputActionToLens =
  case _ of
    ReferenceInput -> _referenceInputs
    SpendInput -> _inputs

-- | Ensures uniqueness of an element
pushUnique :: forall a. Ord a => a -> Array a -> Array a
pushUnique x xs = nub $ xs <> [ x ]

bugTrackerUrl :: String
bugTrackerUrl =
  "https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/issues"
 */
