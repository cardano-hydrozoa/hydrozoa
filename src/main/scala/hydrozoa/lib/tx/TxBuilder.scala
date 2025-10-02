package hydrozoa.lib.tx

/** This module contains declarative transaction building types and utilities ported from
  * purescript-cardano-transaction-builder with some modifications and additions.
  *   - The main entry-point: [[TransactionBuilder.build]]
  *   - The definition of steps: [[TransactionBuilderStep]]
  */

import cats.*
import cats.data.*
import cats.implicits.*
import hydrozoa.*
import hydrozoa.lib.optics.>>>
import hydrozoa.lib.tx
import hydrozoa.lib.tx.TxBuildError.{
    CannotExtractSignatures,
    RedeemerIndexingInternalError,
    Unimplemented,
    WrongCredentialType
}
import io.bullet.borer.Cbor
import monocle.*
import monocle.syntax.all.*
import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, *}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.GovAction.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{STS, UtxoEnv, Context as SContext, State as SState}
import scalus.cardano.ledger.txbuilder.wip.DiffHandler
import scalus.cardano.ledger.txbuilder.{LowLevelTxBuilder, TxBalancingError}
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.|>

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

// ============================================================================
// Network Extensions
// ============================================================================

extension (network: Network)
    /** Convert Network to the corresponding integer network ID */
    def toNetworkId: Int = network match
        case Network.Testnet => 0
        case Network.Mainnet => 1

object NetworkExtensions:
    /** Convert integer network ID to Network */
    def fromNetworkId(networkId: Int): Option[Network] = networkId match
        case 0 => Some(Network.Testnet)
        case 1 => Some(Network.Mainnet)
        case _ => None

// ============================================================================
// TransactionBuilderStep
// ============================================================================

sealed trait TransactionBuilderStep

object TransactionBuilderStep {

    case class SpendOutput(
        utxo: TransactionUnspentOutput,
        /** Pass None for pubkey outputs only */
        witness: Option[OutputWitness]
    ) extends TransactionBuilderStep

    case class Pay(output: TransactionOutput) extends TransactionBuilderStep

    case class MintAsset(
        scriptHash: ScriptHash,
        assetName: AssetName,
        amount: Long,
        witness: CredentialWitness
    ) extends TransactionBuilderStep

    /** Add an output as a reference. The reason that action is represented as a step is that
      * reference utxos should be added to the context.
      * @param utxo
      *   any utxo
      */
    case class ReferenceOutput(utxo: TransactionUnspentOutput) extends TransactionBuilderStep

    /** Add a utxo as a collateral input. Utxo should contain ada only and be controlled by a key,
      * not a script. If you need set collateral outputs ot `totalCollateral` field, please use
      * optics.
      */
    case class AddCollateral(
        utxo: TransactionUnspentOutput
    ) extends TransactionBuilderStep

    case class IssueCertificate(
        cert: Certificate,
        witness: Option[CredentialWitness]
    ) extends TransactionBuilderStep

    case class WithdrawRewards(
        stakeCredential: StakeCredential,
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

/** `OutputWitness` is used to provide the evidence needed to consume an output. It must correspond
  * to a `TransactionUnspentOutput` address' payment credential to unlock it.
  *
  * It also contains the signatures expected (for native scripts) or required (for plutus scripts)
  * to be used in fee calculation.
  */
sealed trait OutputWitness

object OutputWitness {
    case class NativeScriptOutput(
        witness: ScriptWitness[Script.Native]
    ) extends OutputWitness

    case class PlutusScriptOutput(
        witness: ScriptWitness[PlutusScript],
        redeemer: Data,
        datum: Option[DatumWitness]
    ) extends OutputWitness

}

// ============================================================================
// CredentialWitness
// ============================================================================

/** `CredentialWitness` is used to provide the evidence needed to perform operations on behalf of a
  * credential, which include:
  *
  * \- Minting \- Certificate witnessing \- Rewards withdrawal
  *
  * Unlike `OutputWitness`, it does not include a `DatumWitness`, because minting policies and stake
  * scripts do not have a datum.
  */
sealed trait CredentialWitness

object CredentialWitness {
    case class NativeScriptCredential(
        witness: ScriptWitness[Script.Native]
    ) extends CredentialWitness
    case class PlutusScriptCredential(
        witness: ScriptWitness[PlutusScript],
        redeemer: Data
    ) extends CredentialWitness
}

// ============================================================================
// ScriptWitness
// ============================================================================

/** Gives the user options for specifying everything needed to unlock an action guarded by a script,
  * including:
  *   - Spending a UTxO located at an address with a ScriptHash payment credential.
  *   - Witnessing credential operations requiring a script hash
  *   - Witnessing a mint
  *   - Witnessing a rewards withdrawal for a script hash credential
  *   - Any additional signers required to unlock the script, such as for a plutus or native
  *     multisig
  *
  * The two constructors behave as follows:
  *   - `ScriptValue` contains a script for the witness set.
  *
  *   - `ScriptReferenceSpent` includes a step to spend a utxo where a CIP-33 reference script
  *     should be available at.
  *
  *   - `ScriptReferenceReferenced` wraps a UTxO that carrying a CIP-33 reference script, where the
  *     provided UTxO will be referenced according to CIP-31.
  */
sealed trait ScriptWitness[+A]:
    val additionalSigners: Set[ExpectedSigner]

object ScriptWitness {
    case class ScriptValue[A](script: A, additionalSigners: Set[ExpectedSigner])
        extends ScriptWitness[A]

    // The case when a UTxO carrying a reference script necessary for the transaction is spent
    // (added to the "inputs" field of the transaction body)
    case class ScriptReferenceSpent(
        spendStep: TransactionBuilderStep.SpendOutput,
        additionalSigners: Set[ExpectedSigner]
    ) extends ScriptWitness[Nothing]

    // The case when a UTxO carrying a reference script necessary for the transaction is referenced
    // (added to the "referenceInputs" field of the transaction body)
    case class ScriptReferenceReferenced(
        referenceStep: TransactionBuilderStep.ReferenceOutput,
        additionalSigners: Set[ExpectedSigner]
    ) extends ScriptWitness[Nothing]

}

// ============================================================================
// DatumWitness
// ============================================================================

/** Datums in UTxOs can be stored in two forms: inline datums or datum hashes. When there's a hash,
  * we need to provide a datum corresponding to this hash, which can be done by either providing the
  * value literally, or using a reference input where it is stored inline (the latter is not
  * supported, since we haven't seen in the wild - you can work with the datum of a reference input
  * deirectly, so we decided to remove that. Please open an issue if you need it.
  */
sealed trait DatumWitness

object DatumWitness {
    case class DatumValue(datum: Data) extends DatumWitness
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

// TODO: itd be nice to make this opaque and only return from chain queries
// NOTE (Peter, 2025-09-23): this comes from  https://github.com/mlabs-haskell/purescript-cardano-types/blob/master/src/Cardano/Types/TransactionUnspentOutput.purs
case class TransactionUnspentOutput(input: TransactionInput, output: TransactionOutput):
    def toTuple: (TransactionInput, TransactionOutput) = (input, output)

object TransactionUnspentOutput:
    def apply(utxo: (TransactionInput, TransactionOutput)): TransactionUnspentOutput =
        TransactionUnspentOutput(utxo._1, utxo._2)

// NOTE (Peter, 2025-09-23): this comes from https://github.com/mlabs-haskell/purescript-cardano-types/blob/master/src/Cardano/Types/StakeCredential.purs
case class StakeCredential(credential: Credential)

// ============================================================================
// Errors
// ============================================================================

sealed trait TxBuildError:
    def explain: String

object TxBuildError {
    case class Unimplemented(description: String) extends TxBuildError {
        override def explain: String = s"$description is not yet implemented. If you need it, " +
            s"submit a request at $bugTrackerUrl."
    }

    case class CollateralNotPubKey(utxo : TransactionUnspentOutput) extends TxBuildError{
        override def explain: String = s"The UTxO passed as a collateral input is not a PubKey UTxO. UTxO: $utxo"
    }

    // TODO: This error could probably be improved.
    case class CannotExtractSignatures(step: TransactionBuilderStep) extends TxBuildError {
        override def explain: String =
            s"Could not extract signatures via _.additionalSigners from $step"
    }

    case class DatumIsMissing(utxo: TransactionUnspentOutput) extends TxBuildError {
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

    case class CollateralWithTokens(
        utxo: TransactionUnspentOutput
    ) extends TxBuildError {
        override def explain: String =
            "The UTxO you provided as a collateral must contain only ada. " +
                s"UTxO: $utxo"
    }

    case class ReferenceScriptNotProvided(scriptHash: ScriptHash, utxo: TransactionUnspentOutput)
        extends TxBuildError {
        override def explain: String =
            s"The UTxO you provide needs to carry a reference script matching $scriptHash. UTxO: $utxo"
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

// ==============================================================
// Transaction Builder
// ==============================================================

/** An [[AddrKeyHash]] that is expected to sign some [[Transaction]].
  *
  * The purpose for signing is not presently tracked. For a sketch, see commit
  * https://github.com/cardano-hydrozoa/hydrozoa/commit/1a8c9c73fbfb33e79456a0a8b9f08688ef39b749.
  */
case class ExpectedSigner(hash: AddrKeyHash)

object TransactionBuilder:

    /** Builder is a state monad over Context.
      * @tparam A
      */
    private type BuilderM[A] = StateT[[X] =>> Either[TxBuildError, X], Context, A]

    // Helpers to cut down on type signature noise
    def pure0[A] = StateT.pure[[X] =>> Either[TxBuildError, X], Context, A]
    def liftF0[A] = StateT.liftF[[X] =>> Either[TxBuildError, X], Context, A]
    def modify0 = StateT.modify[[X] =>> Either[TxBuildError, X], Context]

    /** An opaque context in which the builder operates. TODO: make a class, remove toTuple()
      *
      * @param transaction
      * @param redeemers
      * @param network
      * @param expectedSigners
      * @param resolvedUtxos
      */
    case class Context private[TransactionBuilder] (
        transaction: Transaction,
        redeemers: Seq[DetachedRedeemer],
        network: Network,
        expectedSigners: Set[ExpectedSigner],
        resolvedUtxos: Set[TransactionUnspentOutput]
    ) {

        /** Extract tupled information from a Context. This method is provided to avoid breaking
          * opacity while making it easier to check for equality in testing.
          */
        val toTuple: (
            Transaction,
            Seq[DetachedRedeemer],
            Network,
            Set[ExpectedSigner],
            Set[TransactionUnspentOutput]
        ) = (
          this.transaction,
          this.redeemers,
          this.network,
          this.expectedSigners,
          this.resolvedUtxos
        )

        /** Add additional signers to the transaction, silently dropping any that point to invalid
          * components
          */
        def addSigners(additionalSigners: Set[ExpectedSigner]): Context = {
            this |> Focus[Context](_.expectedSigners).modify(_ ++ additionalSigners)
        }

        /** ensure that all transaction outputs in the context have min ada */
        def setMinAdaAll(protocolParams: ProtocolParams): Context = {
            // @Ilia we also need to set the min ADA on the collateral return output
            this |> unsafeCtxBodyL
                .refocus(_.outputs)
                .modify(os =>
                    os.map((to: Sized[TransactionOutput]) =>
                        Sized(setMinAda(to.value, protocolParams))
                    )
                )
        }

        /** balance the transaction in a context, adding and removing mock signatures where
          * necessary.
          */
        def balance(
            // @Ilia leave comment about not messing with inputs, etc. If your diff handler
            // adds or removes components needing signatures, the fees won't be calculated correctly.
            // It also won't update .resolvedUtxos.
            // @Ilia Wrap this so that we can only modify the transaction outputs. Basically inject
            // a (Coin, Set[TransactionOutput]) => Either[TxBalancingError, Set[TransactionOutput]]
            // into a DiffHandler
            diffHandler: DiffHandler,
            protocolParams: ProtocolParams,
            evaluator: PlutusScriptEvaluator
        ): Either[TxBalancingError, Context] = {
            val withVKeys: Transaction = addDummyVKeys(this.expectedSigners.size, this.transaction)
            for {
                balanced <- LowLevelTxBuilder.balanceFeeAndChange(
                  initial = withVKeys,
                  diffHandler = diffHandler,
                  protocolParams = protocolParams,
                  resolvedUtxo = this.getUtxo,
                  evaluator = evaluator
                )
                withoutVKeys = removeDummyVKeys(this.expectedSigners.size, this.transaction)

            } yield Context(
              transaction = withoutVKeys,
              redeemers = this.redeemers,
              network = this.network,
              expectedSigners = this.expectedSigners,
              resolvedUtxos = this.resolvedUtxos
            )
        }

        /** Conversion help to Scalus [[Utxo]] */
        private def getUtxo: UTxO = Map.from(this.resolvedUtxos.map(utxo => (utxo._1, utxo._2)))

        /** validate a context according so a set of ledger rules */
        def validate(
            validators: Seq[Validator],
            protocolParams: ProtocolParams
        ): Either[TransactionException, Context] = {
            val certState = CertState.empty
            val context = SContext(
              this.transaction.body.value.fee,
              UtxoEnv(1L, protocolParams, certState, network)
            )
            val state = SState(this.getUtxo, certState)
            validators
                .map(_.validate(context, state, this.transaction))
                .collectFirst { case l: Left[?, ?] => l.value }
                .toLeft(this)
        }

        /** set min ada, balance, and validate a context */
        // @Ilia consider putting PP, evaluator, and validators, into the parameters for the transaction builder class
        def finalizeContext(
            protocolParams: ProtocolParams,
            diffHandler: DiffHandler,
            evaluator: PlutusScriptEvaluator,
            validators: Seq[Validator]
        ): Either[TransactionException | TxBalancingError, Context] =
            for {
                balancedCtx <- this
                    .setMinAdaAll(protocolParams)
                    .balance(diffHandler, protocolParams, evaluator)
                validatedCtx <- balancedCtx.validate(validators, protocolParams)
            } yield validatedCtx
    }

    object Context:
        def empty(networkId: Network) = Context(
          transaction = emptyTransaction,
          redeemers = Seq.empty,
          network = networkId,
          expectedSigners = Set.empty,
          resolvedUtxos = Set.empty
        )

    // Will drop signers silently
    private val unsafeCtxBodyL: Lens[Context, TransactionBody] = {
        Focus[Context](_.transaction) >>> txBodyL
    }

    // Will drop signers silently
    private val unsafeCtxWitnessL: Lens[Context, TransactionWitnessSet] =
        Focus[Context](_.transaction).refocus(_.witnessSet)

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
    def setMinAda[TO <: TransactionOutput](
        candidateOutput: TO,
        params: ProtocolParams,
        update: (Coin, TO) => TO = replaceAdaUpdate
    ): TO = {
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
    // TODO: try to make it polymorphic
    def replaceAdaUpdate(coin: Coin, to: TransactionOutput): TransactionOutput =
        to match {
            case s: TransactionOutput.Shelley => s.focus(_.value.coin).replace(coin)
            case b: TransactionOutput.Babbage => b.focus(_.value.coin).replace(coin)
        }

    /** Build a transaction from scratch, starting with an "empty" transaction and no signers. */
    def build(
        network: Network,
        steps: Seq[TransactionBuilderStep]
    ): Either[TxBuildError, Context] =
        modify(Context.empty(network), steps)

    /** Modify a transaction within a context. */
    def modify(
        ctx: Context,
        steps: Seq[TransactionBuilderStep]
    ): Either[TxBuildError, Context] = {
        val modifyBuilderM: BuilderM[Unit] = {

            for {
                _ <- processConstraints(steps)
                ctx0 <- StateT.get
                res <- liftF0(
                  TransactionConversion.fromEditableTransactionSafe(
                    EditableTransaction(
                      transaction = ctx0.transaction,
                      redeemers = ctx0.redeemers.toVector
                    )
                  ) match {
                      case None    => Left(RedeemerIndexingInternalError(ctx.transaction, steps))
                      case Some(x) => Right(x)
                  }
                )
                // Replace the transaction in the context, keeping the rest
                _ <- modify0(Focus[Context](_.transaction).replace(res))
            } yield ()
        }
        modifyBuilderM.run(ctx).map(_._1)
    }

    def processConstraints(steps: Seq[TransactionBuilderStep]): BuilderM[Unit] =
        steps.traverse_(processConstraint)

    def processConstraint(step: TransactionBuilderStep): BuilderM[Unit] = step match {
        case step @ TransactionBuilderStep.SpendOutput(utxo, spendWitness) =>
            // Note (dragospe, 2025-10-02): this gets very clunky.
            // assertOutputType has to ensure that the types match between the witness provided and the
            // utxo provided.
            //
            // Then, in the case of a pubkey input, we have to add the expected signers
            // directly.
            //
            // In the case of a script witness, we have to handle up to six different situations, depending on whether the
            // script:
            //    - is provided directly (add to witness set, depending on the 4 possible script types)
            //    - included via the spending of a UTxO carrying a CIP-33 ref script (process the spend step for the utxo)
            //    - inlcuded via the reference of a utxo carrying a CIP-33 ref script (add to context's _.resolvedUtxos
            //       and txbody's _.referenceInputs)
            //
            // in any case, we also need to update the context's _.expectedSigners for the script. In the case of
            // plutus scripts only, we need to also updated the _.requiredSigners of the transaction body
            //
            // The Valid routes are:
            //
            //  (Spend 1) PubKey Utxo => No Witness:
            //      - add pkh to ES
            //      - add spent utxo to inputs
            //      - add spent utxo to resolvedUtxos
            //  (2) NS Utxo => Direct NS Witness:
            //      - and NS to transaction witnessSet.nativeScripts
            //      - add additional signers in witness to ES
            //      - add spent utxo to resolvedUtxos
            //      - add spent utxo to inputs
            //  (3) NS Utxo => Spend NS Witness:
            //      - spend witness utxo (recur on SpendOutput step)
            //      - add additional signers in witness to ES
            //      - add utxo to resolvedUtxos
            //      - add spent utxo to inputs
            //  (4) NS Utxo => Ref NS Witness;
            //      - add spent utxo to inputs
            //      - add ref utxo to resolvedUtxos
            //      - add additional signers for witness to ES
            //      - reference witness utxo (recur on ReferenceOutput step)
            //  (5) PS Utxo => Direct PS Witness
            //      - add script to witnessSet.plutusV{1 | 2 | 3}Scripts
            //      - add additional signers in witness to ES
            //      - add additional signers to required signers
            //      - add spent utxo to resolvedUtxos
            //      - add spent utxo to inputs
            //  (6) PS Utxo => Spend PS Witness
            //      - spend witness utxo (recur on SpendOutput step)
            //      - add additional signers for witness to ES
            //      - add additional signers to required signers
            //      - add spent utxo to resolvedUtxos
            //      - add spent utxo to inputs
            //  (7) PS Utxo => Ref PS Witness
            //      - add spent utxo to inputs
            //      - add spent utxo to resolved utxos
            //      - add additional signers to required signers
            //      - add additional signers for witness to ES
            //      - reference witness utxo (recur on ReferenceOutput step)
            //
            //  Notes:
            //    - Every case needs to add the spent `utxo` to resolvedUtxos and to inputs.
            //      This is done in the top level of this function.
            //    - Every case needs to add the expected signers, but they do this in different ways
            //      depending on whether the OutputWitness is None or Member.
            //    - cases 2 and 5 differ only on where the script needs to be added to
            //    - cases 3 and 6 match
            //    - cases 4 and 7 match
            //
            //  The invalid routes include things like s
            //
            // PubKey Utxo => Direct NS Witness
            // NS Utxo => No Witness
            // PS UTXO => Spend NS Witness
            // etc.

            for {
                _ <- assertNetworkId(utxo.output.address)
                _ <- modify0(
                  // Add Input
                  unsafeCtxBodyL
                      .refocus(_.inputs)
                      .modify(inputs => TaggedOrderedSet.from(pushUnique(utxo.input, inputs.toSeq)))
                  // Add utxo to resolved utxos
                      <<< Focus[Context](_.resolvedUtxos).modify(_ + utxo)
                )
                _ <- useSpendWitness(utxo, spendWitness)
            } yield ()

        case TransactionBuilderStep.Pay(output) =>
            for {
                _ <- assertNetworkId(output.address)
                _ <- modify0(
                  unsafeCtxBodyL
                      .refocus(_.outputs)
                      // Intentionally not using pushUnique: we can create multiple outputs of the same shape
                      .modify(outputs => outputs.toSeq :+ Sized(output))
                )
            } yield ()
        case TransactionBuilderStep.MintAsset(scriptHash, assetName, amount, mintWitness) =>
            useMintAssetWitness(scriptHash, assetName, amount, mintWitness)
        case TransactionBuilderStep.ReferenceOutput(utxo) =>
            for {
                _ <- assertNetworkId(utxo.output.address)

                _ <- modify0(
                  // Add the referenced utxo to the context's resolvedUtxos
                  Focus[Context](_.resolvedUtxos)
                      .modify(_ + utxo)
                      >>>
                          // Add the referenced utxo id to the tx body
                          unsafeCtxBodyL
                              .refocus(_.referenceInputs)
                              .modify(inputs =>
                                  TaggedOrderedSet.from(pushUnique(utxo.input, inputs.toSeq))
                              )
                )
            } yield ()
        case TransactionBuilderStep.AddCollateral(utxo) =>
            for {
                _ <- assertNetworkId(utxo.output.address)
                _ <- assertAdaOnlyPubkeyUtxo(utxo)

                _ <- modify0(
                  // Add the collateral utxo to the context's resolvedUtxos
                  Focus[Context](_.resolvedUtxos)
                      .modify(_ + utxo)
                      >>>
                          // Add the collateral utxo to the tx body
                          unsafeCtxBodyL
                              .refocus(_.collateralInputs)
                              .modify(inputs =>
                                  TaggedOrderedSet.from(pushUnique(utxo.input, inputs.toSeq))
                              )
                )
            } yield ()
        case TransactionBuilderStep.IssueCertificate(cert, witness) =>
            for {
                _ <- modify0(
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
                _ <- modify0(
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
                _ <- modify0(
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
            modify0(
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
            addrNetwork = addr.getNetwork.get
            res <-
                if context.network != addrNetwork
                then
                    liftF0(
                      Left(TxBuildError.WrongNetworkId(addr))
                    )
                else pure0(())
        } yield res

    /** Ensure that the output is a pubkey output containing only ada. */
    def assertAdaOnlyPubkeyUtxo(utxo: TransactionUnspentOutput): BuilderM[Unit] =
        for {
            _ <-
                if !utxo.output.value.assets.isEmpty
                then
                    liftF0(
                      Left(TxBuildError.CollateralWithTokens(utxo))
                    )
                else pure0(())
            addr = utxo.output.address
            _ <- addr.keyHashOption.match {
                case Some(_: AddrKeyHash) => pure0(())
                case _ => liftF0(Left(TxBuildError.CollateralNotPubKey(utxo)))
            }
        } yield ()

    /** Tries to modify the transaction to make it consume a given output and add the requisite
      * signature(s) to the Context's _.expectedSigners. Uses a `SpendWitness` to try to satisfy
      * spending requirements.
      */
    def useSpendWitness(
        utxo: TransactionUnspentOutput,
        mbWitness: Option[OutputWitness]
    ): BuilderM[Unit] = {

        val ewt = ExpectedWitnessType.PubKeyHashWitness[OutputWitness]()
        for {
            _ <- mbWitness match {
                // Case Spend 1: Pubkey input: add the pubkey hash to ctx.expectedSigners
                case None =>
                    for {
                        _ <- assertOutputType(
                          ExpectedWitnessType.PubKeyHashWitness[OutputWitness](),
                          utxo
                        )
                        keyHash <- liftF0(utxo.output.address match {
                            case sa: ShelleyAddress =>
                                sa.payment match {
                                    case kh: ShelleyPaymentPart.Key => Right(kh.hash)
                                    case _: ShelleyPaymentPart.Script =>
                                        Left(TxBuildError.WrongOutputType(ewt, utxo))
                                }
                            case _ => Left(TxBuildError.WrongOutputType(ewt, utxo))
                        })
                        _ <- modify0(
                          Focus[Context](_.expectedSigners)
                              .modify(_ + ExpectedSigner(keyHash))
                        )
                    } yield ()

                // Native Script: Assure the output type is correct
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
                        _ <- modify0(ctx =>
                            ctx.focus(_.redeemers).modify(r => pushUnique(detachedRedeemer, r))
                        )
                        _ <- useDatumWitnessForUtxo(utxo, mbDatumWitness)
                    } yield ()
            }
        } yield ()
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
                    liftF0(
                      Left(TxBuildError.WrongOutputType(expectedType, utxo))
                    )
                else pure0(())
        } yield res

    // FIXME: this has signifcant overlap with AssertScriptHashMatchesCredentialWitness.
    // We can factor it out by extracting a script from both witness types.
    def assertScriptHashMatchesOutputWitness(
        scriptHash: ScriptHash,
        outputWitness: OutputWitness
    ): BuilderM[Unit] =
        for {
            // Basically 6 cases: plutus or native, times direct, spent, or referenced
            script: Either[Script.Native, PlutusScript] <- outputWitness match {

                // Direct native sript
                case OutputWitness.NativeScriptOutput(
                      ScriptWitness.ScriptValue(script, _)
                    ) =>
                    pure0(Left(script))
                // Direct plutus script
                case OutputWitness.PlutusScriptOutput(
                      ScriptWitness.ScriptValue(script, _),
                      _, _
                    ) =>
                    pure0(Right(script))
                case otherwise =>
                    // grab a utxo carrying a CIP-33 ref script
                    val utxo: TransactionUnspentOutput = otherwise match {
                        // Spent Utxo with CIP-33 Native Script ref
                        case OutputWitness.NativeScriptOutput(
                              ScriptWitness.ScriptReferenceSpent(spendStep, _)
                            ) =>
                            spendStep.utxo
                        // Spent Utxo with CIP-33 Plutus script ref
                        case OutputWitness.PlutusScriptOutput(
                              ScriptWitness.ScriptReferenceSpent(spendStep, _),
                              _, _
                            ) =>
                            spendStep.utxo
                        // Referenced Utxo with a CIP-33 Native script ref
                        case OutputWitness.NativeScriptOutput(
                              ScriptWitness.ScriptReferenceReferenced(referenceStep, _)
                            ) =>
                            referenceStep.utxo
                        // Referenced Utxo with a CIP-33 Plutus script ref
                        case OutputWitness.PlutusScriptOutput(
                              ScriptWitness.ScriptReferenceReferenced(referenceStep, _),
                              _, _
                            ) =>
                            referenceStep.utxo
                    }
                    utxo.output.match {
                        case b: Babbage =>
                            b.scriptRef match {
                                // Needed a script ref, but couldn't get one
                                case None =>
                                    liftF0(
                                      Left(
                                        TxBuildError.ReferenceScriptNotProvided(scriptHash, utxo)
                                      )
                                    )
                                case Some(s) =>
                                    s.script match {
                                        case ps: PlutusScript  => pure0(Right(ps))
                                        case ns: Script.Native => pure0(Left(ns))
                                    }
                            }

                        // Needed a script ref, but couldn't get one (not a babbage output)
                        case _ =>
                            liftF0(Left(TxBuildError.ReferenceScriptNotProvided(scriptHash, utxo)))
                    }

            }
            _ <- assertScriptHashMatchesScript(scriptHash, script)
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
            liftF0(
              Left(TxBuildError.IncorrectScriptHash(eiScript, scriptHash))
            )
        } else {
            pure0(())
        }
    }

    /** To use a native script, we have 3 options:
      *
      * @param scriptWitness
      * @return
      */
    def useNativeScriptWitness(scriptWitness: ScriptWitness[Script.Native]): BuilderM[Unit] =
        for {
            // Spend Cases 2, 3, 4
            _ <- modify0(
              Focus[Context](_.expectedSigners).modify(_ ++ scriptWitness.additionalSigners)
            )

            _ <- scriptWitness match {
                // Spend case 2
                case ScriptWitness.ScriptValue(ns, _) =>
                    modify0(
                      // Add the native script to the witness set
                      unsafeCtxWitnessL
                          .refocus(_.nativeScripts)
                          .modify(s => pushUnique(ns, s.toList).toSet)
                    )
                // Spend case 3
                case ScriptWitness.ScriptReferenceSpent(spendStep, _) =>
                    processConstraint(spendStep)
                // Spend case 4
                case ScriptWitness.ScriptReferenceReferenced(referenceStep, _) =>
                    processConstraint(referenceStep)
            }
        } yield ()

    def usePlutusScriptWitness(
        scriptWitness: ScriptWitness[PlutusScript]
    ): BuilderM[Unit] =
        for {
            // Spend cases 5, 6, and 7
            // Add script's additional signers to txBody.requiredSigners
            _ <- modify0(
              Focus[Context](_.transaction)
                  .andThen(txBodyL)
                  .refocus(_.requiredSigners)
                  .modify((s: TaggedOrderedSet[AddrKeyHash]) =>
                      TaggedOrderedSet.from(
                        s.toSortedSet ++ scriptWitness.additionalSigners.map(_.hash)
                      )
                  )
            )
            // Spend case 5, 6, and 7: Add script's additional signers to context.expectedSigners
            _ <- modify0(
              Focus[Context](_.expectedSigners).modify(_ ++ scriptWitness.additionalSigners)
            )

            // Spend case 5
            _ <- scriptWitness match {
                case ScriptWitness.ScriptValue(ps: PlutusScript, _) =>
                    // Add the script value to the appropriate field
                    ps match {
                        case (v1: Script.PlutusV1) =>
                            modify0(
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV1Scripts)
                                  .modify(s => Set.from(pushUnique(v1, s.toSeq)))
                            )
                        case (v2: Script.PlutusV2) =>
                            modify0(
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV2Scripts)
                                  .modify(s => Set.from(pushUnique(v2, s.toSeq)))
                            )
                        case (v3: Script.PlutusV3) =>
                            modify0(
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV3Scripts)
                                  .modify(s => Set.from(pushUnique(v3, s.toSeq)))
                            )
                    }
                case ScriptWitness.ScriptReferenceSpent(spendStep, _) =>
                    processConstraint(spendStep)
                case ScriptWitness.ScriptReferenceReferenced(referenceStep, _) =>
                    processConstraint(referenceStep)
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
                    liftF0(
                      Left(TxBuildError.DatumIsMissing(utxo))
                    )
                // if the datum is inline, we don't need to attach it as witness
                case Some(DatumOption.Inline(_)) =>
                    mbDatumWitness match {
                        case Some(datumWitness) =>
                            liftF0(
                              Left(TxBuildError.UnneededDatumWitness(utxo, datumWitness))
                            )
                        case None =>
                            pure0(())
                    }
                // if the datum is provided as hash
                case Some(DatumOption.Hash(datumHash)) =>
                    mbDatumWitness match {
                        // Error if the datum witness was not provided
                        case None =>
                            liftF0(
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
                                modify0(
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
                                liftF0(
                                  Left(
                                    TxBuildError.IncorrectDatumHash(utxo, providedDatum, datumHash)
                                  )
                                )
                            }
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
            _ <- modify0(ctx => {
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
                // Pubkey credential witness: add to expected signers
                case None =>
                    for {
                        _ <- assertCredentialType(
                          credAction,
                          ExpectedWitnessType.PubKeyHashWitness(),
                          cred
                        )
                        // Add key hash to expected signers
                        _ <- cred match {
                            case Credential.KeyHash(keyHash) =>
                                modify0(
                                  Focus[Context](_.expectedSigners)
                                      .modify(_ + ExpectedSigner(keyHash))
                                )
                            case _ =>
                                liftF0(
                                  Left(
                                    TxBuildError.WrongCredentialType(
                                      credAction,
                                      ExpectedWitnessType.PubKeyHashWitness(),
                                      cred
                                    )
                                  )
                                )
                        }
                    } yield ()
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
                            modify0(ctx =>
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
                            liftF0(
                              Left(
                                TxBuildError.UnneededDeregisterWitness(
                                  StakeCredential(credential),
                                  witness
                                )
                              )
                            )
                        case (Credential.KeyHash(_), None) =>
                            pure0(())
                        case (Credential.ScriptHash(_), None) =>
                            liftF0(
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
                pure0(())
            case Certificate.PoolRegistration(_, _, _, _, _, _, _, _, _) =>
                pure0(())
            case Certificate.PoolRetirement(_, _) =>
                pure0(())
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
                pure0(()) // not supported
            case Certificate.ResignCommitteeColdCert(_, _) =>
                pure0(()) // not supported
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
            ctx <- StateT.get

            rewardAccount = stakeCredential.credential match {
                case Credential.KeyHash(keyHash) =>
                    // Convert AddrKeyHash to StakeKeyHash - they're likely the same underlying type?
                    val stakeKeyHash = keyHash.asInstanceOf[StakeKeyHash]
                    val stakeAddress = StakeAddress(ctx.network, StakePayload.Stake(stakeKeyHash))
                    RewardAccount(stakeAddress)
                case Credential.ScriptHash(scriptHash) =>
                    val stakeAddress = StakeAddress(ctx.network, StakePayload.Script(scriptHash))
                    RewardAccount(stakeAddress)
            }

            _ <- modify0(
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
                pure0(())
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
                            liftF0(
                              Left(TxBuildError.UnneededSpoVoteWitness(credential, witness))
                            )
                        case None =>
                            pure0(
                              credential
                            )
                    }
                case Voter.ConstitutionalCommitteeHotKey(credential) =>
                    pure0(
                      Credential.KeyHash(credential)
                    )
                case Voter.ConstitutionalCommitteeHotScript(scriptHash) =>
                    pure0(
                      Credential.ScriptHash(scriptHash)
                    )
                case Voter.DRepKey(credential) =>
                    pure0(
                      Credential.KeyHash(credential)
                    )
                case Voter.DRepScript(scriptHash) =>
                    pure0(
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
                                pure0(())
                            case None =>
                                StateT.liftF[[X] =>> Either[TxBuildError, X], Context, Unit](
                                  Left(wrongCredErr)
                                )
                        }
                }
            }
        } yield ()

    // FIXME  this has signifcant overlap with AssertScriptHashMatchesOutputWitness.
    //     We can factor it out by extracting a script from both witness types.
    def assertScriptHashMatchesCredentialWitness(
        scriptHash: ScriptHash,
        credWitness: CredentialWitness
    ): BuilderM[Unit] =
        for {
            // Basically 6 cases: plutus or native, times direct, spent, or referenced
            script: Either[Script.Native, PlutusScript] <- credWitness match {

                // Direct native sript
                case CredentialWitness.NativeScriptCredential(
                      ScriptWitness.ScriptValue(script, _)
                    ) =>
                    pure0(Left(script))
                // Direct plutus script
                case CredentialWitness.PlutusScriptCredential(
                      ScriptWitness.ScriptValue(script, _),
                      _
                    ) =>
                    pure0(Right(script))
                case otherwise =>
                    // grab a utxo carrying a CIP-33 ref script
                    val utxo: TransactionUnspentOutput = otherwise match {
                        // Spent Utxo with CIP-33 Native Script ref
                        case CredentialWitness.NativeScriptCredential(
                              ScriptWitness.ScriptReferenceSpent(spendStep, _)
                            ) =>
                            spendStep.utxo
                        // Spent Utxo with CIP-33 Plutus script ref
                        case CredentialWitness.PlutusScriptCredential(
                              ScriptWitness.ScriptReferenceSpent(spendStep, _),
                              _
                            ) =>
                            spendStep.utxo
                        // Referenced Utxo with a CIP-33 Native script ref
                        case CredentialWitness.NativeScriptCredential(
                              ScriptWitness.ScriptReferenceReferenced(referenceStep, _)
                            ) =>
                            referenceStep.utxo
                        // Referenced Utxo with a CIP-33 Plutus script ref
                        case CredentialWitness.PlutusScriptCredential(
                              ScriptWitness.ScriptReferenceReferenced(referenceStep, _),
                              _
                            ) =>
                            referenceStep.utxo
                    }
                    utxo.output.match {
                        case b: Babbage =>
                            b.scriptRef match {
                                // Needed a script ref, but couldn't get one
                                case None =>
                                    liftF0(
                                      Left(
                                        TxBuildError.ReferenceScriptNotProvided(scriptHash, utxo)
                                      )
                                    )
                                case Some(s) =>
                                    s.script match {
                                        case ps: PlutusScript  => pure0(Right(ps))
                                        case ns: Script.Native => pure0(Left(ns))
                                    }
                            }

                        // Needed a script ref, but couldn't get one (not a babbage output)
                        case _ =>
                            liftF0(Left(TxBuildError.ReferenceScriptNotProvided(scriptHash, utxo)))
                    }

            }
            _ <- assertScriptHashMatchesScript(scriptHash, script)
        } yield ()
