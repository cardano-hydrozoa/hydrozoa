package hydrozoa.lib.tx

/*
 This module contains declarative transaction building types and utilities
 ported from purescript-cardano-transaction-builder.
 */

import cats.*
import cats.data.*
import cats.implicits.*
import hydrozoa.datumOption
import hydrozoa.emptyTransaction
import hydrozoa.lib.tx.InputAction.ReferenceInput
import hydrozoa.lib.tx.InputAction.SpendInput
import hydrozoa.lib.tx.TxBuildError.RedeemerIndexingInternalError
import io.bullet.borer.{Cbor, Encoder}
import monocle.syntax.all.*
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.Builtins.serialiseData
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.cardano.address
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.serialization.cbor.Cbor as ScalusCbor

import scala.collection.immutable.SortedMap

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
        utxo: TransactionUnspentOutput,
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

    case class ModifyAuxData(f: Option[AuxiliaryData] => Option[AuxiliaryData])
        extends TransactionBuilderStep
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
        witness: ScriptWitness[Script.PlutusV1 | Script.PlutusV2 | Script.PlutusV3],
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
        // N.B.: Changed from upstream, we have 3 distinct script types
        witness: ScriptWitness[Script.PlutusV1 | Script.PlutusV2 | Script.PlutusV3],
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
-- | - Witnessing a mint
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
        action: InputAction
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
        action: InputAction
    ) extends DatumWitness
}

//sealed trait StakeWitness
//
//object StakeWitness {
//    case class PubKeyHashStakeWitness(pkh: StakeKeyHash) extends StakeWitness
//    case class PlutusScriptStakeWitness(scriptWitness: ScriptWitness[PlutusScript])
//    case class NativeScriptStakeWitness(scriptWitness: ScriptWitness[Script.Native])
//}

// ===========================================================
// Expected Witness Type
// ===========================================================

/*
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
 */

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
case class TransactionUnspentOutput(input: TransactionInput, output: TransactionOutput)

// NOTE (Peter, 2025-09-23): this comes from https://github.com/mlabs-haskell/purescript-cardano-types/blob/master/src/Cardano/Types/StakeCredential.purs
case class StakeCredential(credential: Credential)

// ============================================================================
// Errors
// ============================================================================

/*
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
 */

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

/*
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
 */

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

/*
type Context =
    { transaction :: Transaction
    , redeemers :: Array DetachedRedeemer
    , networkId :: Maybe NetworkId
    }
 */
case class Context(
    transaction: Transaction,
    redeemers: Seq[DetachedRedeemer],
    networkId: Option[Int]
)

// type BuilderM a = StateT Context (Except TxBuildError) a
type BuilderM[A] = StateT[[X] =>> Either[TxBuildError, X], Context, A]

object TransactionBuilder:
    /*
    buildTransaction
      :: Array TransactionBuilderStep
      -> Either TxBuildError Transaction
    buildTransaction =
      modifyTransaction Transaction.empty
     */
    def buildTransaction(steps: Seq[TransactionBuilderStep]): Either[TxBuildError, Transaction] =
        modifyTransaction(emptyTransaction, steps)

    /*
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
     */
    def modifyTransaction(
        tx: Transaction,
        steps: Seq[TransactionBuilderStep]
    ): Either[TxBuildError, Transaction] =

        def refreshAllKeepRaw(self: Transaction): Transaction = {
            self
                .focus(_.body.raw)
                .replace(ScalusCbor.encode(self.body.value))
                .focus(_.witnessSet.plutusData.raw)
                .replace(ScalusCbor.encode(self.witnessSet.plutusData.value))
            // FIXME: witnessSet.plutusData.value.toIndexedSeq
            // FIXME: witnessSet.redeemers
        }

        for {
            context <- for {
                editableTransaction <- TransactionConversion
                    .toEditableTransactionSafe(tx)
                    .left
                    .map(TxBuildError.RedeemerIndexingError(_))
            } yield Context(
              transaction = editableTransaction.transaction,
              redeemers = editableTransaction.redeemers,
              networkId = editableTransaction.transaction.body.value.networkId
            )
            eiCtx <- processConstraints(steps).run(context).map(_._1)

            res <- TransactionConversion.fromEditableTransactionSafe(
              EditableTransaction(
                transaction = eiCtx.transaction,
                redeemers = eiCtx.redeemers.toVector
              )
            ) match {
                case None    => Left(RedeemerIndexingInternalError(tx, steps))
                case Some(x) => Right(x)
            }
        } yield refreshAllKeepRaw(res)

    /*
    processConstraints :: Array TransactionBuilderStep -> BuilderM Unit
    processConstraints = traverse_ processConstraint
     */
    def processConstraints(steps: Seq[TransactionBuilderStep]): BuilderM[Unit] =
        steps.traverse_(processConstraint)

    /*
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
     */
    def processConstraint(step: TransactionBuilderStep): BuilderM[Unit] = step match {
        case TransactionBuilderStep.SpendOutput(utxo, spendWitness) =>
            for {
                _ <- assertNetworkId(utxo.output.address)
                _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](ctx =>
                    ctx.focus(_.transaction.body.value.inputs)
                        .modify(inputs =>
                            TaggedOrderedSet.from(pushUnique(utxo.input, inputs.toSeq))
                        )
                )
                _ <- useSpendWitness(utxo, spendWitness)
            } yield ()
        /*
      Pay output -> do
        assertNetworkId $ output ^. _address
        _transaction <<< _body <<< _outputs
          -- intentionally not using pushUnique: we can
          -- create multiple outputs of the same shape
          %= flip append [ output ]
         */
        case TransactionBuilderStep.Pay(output) =>
            for {
                _ <- assertNetworkId(output.address)
                _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](ctx =>
                    ctx.focus(_.transaction.body.value.outputs)
                        // Intentionally not using pushUnique: we can create multiple outputs of the same shape
                        .modify(outputs => outputs.toSeq :+ Sized(output))
                )
            } yield ()
        case TransactionBuilderStep.MintAsset(scriptHash, assetName, amount, mintWitness) =>
            useMintAssetWitness(scriptHash, assetName, amount, mintWitness)
        case TransactionBuilderStep.IssueCertificate(cert, witness) =>
            for {
                _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](ctx =>
                    ctx.focus(_.transaction.body.value.certificates)
                        .modify(certificates =>
                            TaggedSet.from(pushUnique(cert, certificates.toIndexedSeq))
                        )
                )
                _ <- useCertificateWitness(cert, witness)
            } yield ()
        case TransactionBuilderStep.WithdrawRewards(stakeCredential, amount, witness) =>
            useWithdrawRewardsWitness(StakeCredential(stakeCredential), amount, witness)
        case TransactionBuilderStep.SubmitProposal(proposal, witness) =>
            for {
                _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](ctx =>
                    ctx.focus(_.transaction.body.value.proposalProcedures)
                        .modify(proposals =>
                            TaggedOrderedSet.from(pushUnique(proposal, proposals.toSeq))
                        )
                )
                _ <- useProposalWitness(proposal, witness)
            } yield ()
        case TransactionBuilderStep.SubmitVotingProcedure(voter, votes, witness) =>
            for {
                _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](ctx =>
                    ctx.focus(_.transaction.body.value.votingProcedures)
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
            StateT.modify[[X] =>> Either[TxBuildError, X], Context](ctx =>
                ctx.focus(_.transaction.auxiliaryData).modify(f(_))
            )

    }

    /** -- | Ensures uniqueness of an element pushUnique :: forall a. Ord a => a -> Array a -> Array
      * a pushUnique x xs = nub $ xs <> [ x ]
      */
    def pushUnique[A](elem: A, seq: Seq[A]): Seq[A] =
        seq.appended(elem).distinct

    /*
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
     */
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
                    _ <- useDatumWitnessForUtxo(utxo, mbDatumWitness)
                } yield ()
        }
    }

    /*
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
     */
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

    /*
    assertScriptHashMatchesOutputWitness scriptHash witness =
      traverse_ (assertScriptHashMatchesScript scriptHash) $
        case witness of
          PlutusScriptOutput (ScriptValue plutusScript) _ _ -> Just
            (Right plutusScript)
          NativeScriptOutput (ScriptValue nativeScript) -> Just
            (Left nativeScript)
          _ -> Nothing

     */
    def assertScriptHashMatchesOutputWitness(
        scriptHash: ScriptHash,
        witness: OutputWitness
    ): BuilderM[Unit] =
        for {
            _ <- witness match {
                case OutputWitness.PlutusScriptOutput(
                      ScriptWitness.ScriptValue(plutusScript),
                      _,
                      _
                    ) =>
                    assertScriptHashMatchesScript(scriptHash, Right(plutusScript))
                case OutputWitness.NativeScriptOutput(ScriptWitness.ScriptValue(nativeScript)) =>
                    assertScriptHashMatchesScript(scriptHash, Left(nativeScript))
                case _ =>
                    StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
            }
        } yield ()

    /*
    assertScriptHashMatchesScript
      :: ScriptHash
      -> Either NativeScript PlutusScript
      -> BuilderM Unit
    assertScriptHashMatchesScript scriptHash eiScript = do
      let hash = either NativeScript.hash PlutusScript.hash eiScript
      unless (scriptHash == hash) do
        throwError $ IncorrectScriptHash eiScript scriptHash
     */
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

    /*
    useNativeScriptWitness :: ScriptWitness NativeScript -> BuilderM Unit
    useNativeScriptWitness =
      case _ of
        ScriptValue ns -> do
          _transaction <<< _witnessSet <<< _nativeScripts
            %= pushUnique ns
        ScriptReference refInput refInputAction -> do
          _transaction <<< _body <<< refInputActionToLens refInputAction
            %= pushUnique refInput
     */
    def useNativeScriptWitness(scriptWitness: ScriptWitness[Script.Native]): BuilderM[Unit] =
        scriptWitness match {
            case ScriptWitness.ScriptValue(ns) =>
                StateT.modify(ctx =>
                    ctx.focus(_.transaction.witnessSet.nativeScripts)
                        .modify(s => pushUnique(ns, s.toList).toSet)
                )
            case ScriptWitness.ScriptReference(input, inputAction) =>
                StateT.modify(ctx =>
                    inputAction match {
                        case ReferenceInput =>
                            ctx.focus(_.transaction.body.value.referenceInputs)
                                .modify(s => TaggedOrderedSet.from(pushUnique(input, s.toSeq)))
                        case SpendInput =>
                            ctx.focus(_.transaction.body.value.inputs)
                                .modify(s => TaggedOrderedSet.from(pushUnique(input, s.toSeq)))
                    }
                )
        }

    /*
    usePlutusScriptWitness :: ScriptWitness PlutusScript -> BuilderM Unit
    usePlutusScriptWitness =
      case _ of
        ScriptValue ps -> do
          _transaction <<< _witnessSet <<< _plutusScripts
            %= pushUnique ps
        ScriptReference input action -> do
          _transaction <<< _body <<< refInputActionToLens action
            %= pushUnique input
     */
    def usePlutusScriptWitness(
        scriptWitness: ScriptWitness[Script.PlutusV1 | Script.PlutusV2 | Script.PlutusV3]
    ): BuilderM[Unit] =
        scriptWitness match {
            case ScriptWitness.ScriptValue(ps: Script.PlutusV1) =>
                StateT.modify(ctx =>
                    ctx.focus(_.transaction.witnessSet.plutusV1Scripts)
                        .modify(s => Set.from(pushUnique(ps, s.toSeq)))
                )
            case ScriptWitness.ScriptValue(ps: Script.PlutusV2) =>
                StateT.modify(ctx =>
                    ctx.focus(_.transaction.witnessSet.plutusV2Scripts)
                        .modify(s => Set.from(pushUnique(ps, s.toSeq)))
                )
            case ScriptWitness.ScriptValue(ps: Script.PlutusV3) =>
                StateT.modify(ctx =>
                    ctx.focus(_.transaction.witnessSet.plutusV3Scripts)
                        .modify(s => Set.from(pushUnique(ps, s.toSeq)))
                )
            case ScriptWitness.ScriptReference(input, action) =>
                StateT.modify(ctx =>
                    action match {
                        case ReferenceInput =>
                            ctx.focus(_.transaction.body.value.referenceInputs)
                                .modify(s => TaggedOrderedSet.from(pushUnique(input, s.toSeq)))
                        case SpendInput =>
                            ctx.focus(_.transaction.body.value.inputs)
                                .modify(s => TaggedOrderedSet.from(pushUnique(input, s.toSeq)))
                    }
                )
        }

    // FIXME: Is that the case indeed?
    // NOTE, from dragospe 2025-09-23: we have a wider variety of options here than purescript, since we
    // have both Shelley and Babbage outputs

    /*
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
                    %= pushUnique `providedDatum`
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
     */

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
                                StateT.modify[[X] =>> Either[TxBuildError, X], Context](ctx =>
                                    ctx.focus(_.transaction.witnessSet.plutusData)
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
                            StateT.modify[[X] =>> Either[TxBuildError, X], Context](ctx =>
                                inputAction match {
                                    case InputAction.ReferenceInput =>
                                        ctx.focus(_.transaction.body.value.referenceInputs)
                                            .modify(s =>
                                                TaggedOrderedSet.from(pushUnique(input, s.toSeq))
                                            )
                                    case InputAction.SpendInput =>
                                        ctx.focus(_.transaction.body.value.inputs)
                                            .modify(s =>
                                                TaggedOrderedSet.from(pushUnique(input, s.toSeq))
                                            )
                                }
                            )
                    }
            }
        } yield ()

    // ============================================================================
    // Minting
    // ============================================================================

    /*
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
     */
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
                val currentMint = ctx.transaction.body.value.mint
                val thisMint = MultiAsset.asset(scriptHash, assetName, amount)

                val newMint = currentMint match {
                    case None           => Some(thisMint)
                    case Some(existing) => Some(existing + thisMint)
                }
                ctx.focus(_.transaction.body.value.mint).replace(newMint.map(Mint.apply))
            })
        } yield ()

    // ============================================================================
    // useCredentialWitness
    // ============================================================================

    /*
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
     */
    def useCredentialWitness(
        credAction: CredentialAction,
        cred: Credential,
        mbWitness: Option[CredentialWitness]
    ): BuilderM[Unit] =
        for {
            _ <- mbWitness match {
                case None =>
                    assertCredentialType(credAction, ExpectedWitnessType.PubKeyHashWitness(), cred)
                case Some(witness @ CredentialWitness.NativeScriptCredential(nsWitness)) =>
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

    /*
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
     */
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

    /*
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
     */
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

            _ <- StateT.modify[[X] =>> Either[TxBuildError, X], Context](ctx =>
                ctx.focus(_.transaction.body.value.withdrawals)
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

    /*
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
     */
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

    /*
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
     */
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

    /*
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
     */
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

    /*
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
     */
    def assertScriptHashMatchesCredentialWitness(
        scriptHash: ScriptHash,
        witness: CredentialWitness
    ): BuilderM[Unit] =
        for {
            _ <- witness match {
                case CredentialWitness.NativeScriptCredential(
                      ScriptWitness.ScriptValue(nativeScript)
                    ) =>
                    assertScriptHashMatchesScript(scriptHash, Left(nativeScript))
                case CredentialWitness.PlutusScriptCredential(
                      ScriptWitness.ScriptValue(plutusScript),
                      _
                    ) =>
                    assertScriptHashMatchesScript(scriptHash, Right(plutusScript))
                case _ =>
                    StateT.pure[[X] =>> Either[TxBuildError, X], Context, Unit](())
            }
        } yield ()

    /*
    -- | Depending on `RefInputAction` value, we either want to spend a reference
    -- | UTxO, or just reference it.
    refInputActionToLens
            :: RefInputAction
            -> Lens' TransactionBody (Array TransactionInput)
    refInputActionToLens =
        case _ of
        ReferenceInput -> _referenceInputs
        SpendInput -> _inputs

     */
    // def refInputActionToLens(
    //    inputAction: InputAction
    // ): monocle.PLens[TransactionBody, TransactionBody, TaggedOrderedSet[
    //  scalus.cardano.ledger.TransactionInput
    // ], TaggedOrderedSet[scalus.cardano.ledger.TransactionInput]] =
    //    inputAction match {
    //        case ReferenceInput => Focus[TransactionBody](_.referenceInputs)
    //        case SpendInput     => Focus[TransactionBody](_.inputs)
    //    }
