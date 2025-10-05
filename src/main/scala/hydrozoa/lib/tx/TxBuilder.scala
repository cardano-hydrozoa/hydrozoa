package hydrozoa.lib.tx

/** This module contains declarative transaction building types and utilities ported from
  * purescript-cardano-transaction-builder with some modifications and additions.
  *   - The main entry-point: [[TransactionBuilder.build]]
  *   - The definition of steps: [[TransactionBuilderStep]]
  */

import hydrozoa.*
import hydrozoa.lib.optics.>>>
import hydrozoa.lib.tx
import hydrozoa.lib.tx.TxBuildError.*
import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, *}
import scalus.cardano.ledger.GovAction.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{STS, UtxoEnv, Context as SContext, State as SState}
import scalus.cardano.ledger.txbuilder.wip.DiffHandler
import scalus.cardano.ledger.txbuilder.{LowLevelTxBuilder, TxBalancingError}
import scalus.cardano.ledger.utils.{AllResolvedScripts, MinCoinSizedTransactionOutput}
import scalus.|>
import cats.*
import cats.data.*
import cats.implicits.*
import hydrozoa.lib.tx.Datum.DatumValue

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import io.bullet.borer.Cbor
import monocle.*
import monocle.syntax.all.*

// -----------------------------------------------------------------------------
// Tx Builder steps
// -----------------------------------------------------------------------------

sealed trait TransactionBuilderStep

/** Steps to build the transaction:
  *   - generally non-commutative, so the order matters
  *   - fail-fast
  */
object TransactionBuilderStep {

    /** Spend a utxo - utxos should be unique. TODO: add tests. If spending witness uses a reference
      * utxo for a script, that should be added beforehand with [[ReferenceOutput]] or
      * [[SpendOutput]] step (the latter is rare).
      *
      *   - Endos (StateT effect):
      *     - Must add utxo to resolved utxos
      *     - Must add utxo.input to txBody inputs
      *     - Must add expected signers:
      *       - If pubkey, pubkey signer
      *       - If plutus/native scripts, additional signers from script witness
      *     - Must add plutus data for plutus scripts (datums, redeemers)
      *     - (...)
      *   - Asserts (Either effect):
      *     - Must have coherent networkId
      *     - Must have coherent witness (script has must match script addr)
      *     - (...)
      *
      * case class SpendOutpuytSpec( spentOutputNotReferenced : SpentOutputNotReferencedProof,
      * spentOutputNotAlreadySpent : SpentOutputNotAlreadySpentProof, (...))
      *
      * case class SpendOutputRecipe (utxo: TransactionUnspentOutput, witness: Option[OutputWitness]
      *
      * spendOutput(recipe : SpendOutputRecipe) = BuilderM[SpendOutputSpec]
      *
      * N.B.: Using the an update monad (https://chrispenner.ca/posts/update-monad) would be more
      * sutiable than state. There are a restricted set endomorphisms on the Context that we would
      * want to restrict to ensure invariants are satisfied.
      */
    case class SpendOutput(
        utxo: TransactionUnspentOutput,
        /** Pass None for pubkey outputs only */
        witness: PubKeyWitness.type | NativeScriptWitness | ThreeArgumentPlutusScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep

    /** Send some funds/data to an address. Multiple identical steps are acceptable. */
    case class SendOutput(output: TransactionOutput) extends TransactionBuilderStep

    /** Mint/burn tokens using a native/plutus script. */
    case class MintAsset(
        scriptHash: ScriptHash,
        assetName: AssetName,
        amount: Long,
        witness: NativeScriptWitness | TwoArgumentPlutusScriptWitness
    ) extends TransactionBuilderStep

    /** Add a [[TransactionUnspentOutput]] as a CIP-31 reference input. Doesn't allow the same utxo
      * being referenced several times. The reason that action is represented as a step is that
      * reference utxos should be added to the context and also may be required to create a
      * [[WitnessForSpend]].
      *
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
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep

    case class WithdrawRewards(
        stakeCredential: StakeCredential,
        amount: Coin,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep

    case class SubmitProposal(
        proposal: ProposalProcedure,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep

    case class SubmitVotingProcedure(
        voter: Voter,
        votes: Map[GovActionId, VotingProcedure],
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness =
            PubKeyWitness
    ) extends TransactionBuilderStep

    case class ModifyAuxData(f: Option[AuxiliaryData] => Option[AuxiliaryData])
        extends TransactionBuilderStep
}

// -----------------------------------------------------------------------------
// Witness
// -----------------------------------------------------------------------------

/** A witness to conduct an authorized operation on-chain. This could be spending an input, minting,
  * rewarding, governance ops, certificate ops, etc.
  *
  * The only ways to do this as of writing (2025-10-03) are
  *   - PubKey
  *   - Native Script
  *   - Plutus Script
  *
  * The types include all additional data required to authorize the operation.
  */
sealed trait Witness

case object PubKeyWitness extends Witness

case class NativeScriptWitness(
    scriptSource: ScriptSource[Script.Native],
    additionalSigners: Set[ExpectedSigner]
) extends Witness

// For operations that only take a redeemer and script context
case class TwoArgumentPlutusScriptWitness(
    scriptSource: ScriptSource[PlutusScript],
    redeemer: Data,
    additionalSigners: Set[ExpectedSigner]
) extends Witness

// For operations that take a datum, redeemer, and script context
case class ThreeArgumentPlutusScriptWitness(
    scriptSource: ScriptSource[PlutusScript],
    redeemer: Data,
    datum: Datum,
    additionalSigner: Set[ExpectedSigner]
) extends Witness

// -----------------------------------------------------------------------------
// ScriptSource
// -----------------------------------------------------------------------------

/** Specifes how the transaction should find the source code for the script.
  */
sealed trait ScriptSource[+A <: PlutusScript | Script.Native]

object ScriptSource {

    /** Contains a script itself, will be included to the witness set. */
    case class NativeValue(script: Script.Native) extends ScriptSource[Script.Native]

    /** Tries to use a CIP-33 reference script or a script manually passed in the builder. */
    case object NativeAttached extends ScriptSource[Script.Native]

    case class PlutusValue(script: PlutusScript) extends ScriptSource[PlutusScript]

    case object PlutusAttached extends ScriptSource[PlutusScript]
}

// -----------------------------------------------------------------------------
// Datum
// -----------------------------------------------------------------------------

/** Datums in UTxOs can be stored in two forms: inline datums or datum hashes. When there's a hash,
  * we need to provide a datum corresponding to this hash, which can be done by either providing the
  * value literally, or using a reference input where it is stored inline (the latter is not
  * supported, since we haven't seen it in the wild - you can work with the datum of a reference
  * input deirectly, so we decided to remove that. Please open an issue if you need it.
  */

sealed trait Datum

object Datum {
    case object DatumInlined extends Datum
    case class DatumValue(datum: Data) extends Datum
}

// -----------------------------------------------------------------------------
// ExpectedSigner
// -----------------------------------------------------------------------------

/** An [[AddrKeyHash]] that is expected to sign some [[Transaction]].
  *
  * The purpose for signing is not presently tracked. For a sketch, see commit
  * https://github.com/cardano-hydrozoa/hydrozoa/commit/1a8c9c73fbfb33e79456a0a8b9f08688ef39b749.
  *
  * TODO: shall we use [[AddrKeyHash]] in the steps?
  */
case class ExpectedSigner(hash: AddrKeyHash)

// -----------------------------------------------------------------------------
// Transaction Builder
// -----------------------------------------------------------------------------

/** Represents different types for credentil actions (except spending which goes separetely).
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

/** Represents a type of [[Witness]], i.e., whether it's a key-based or a script-based one.
  */
sealed trait ExpectedWitnessType[A <: Witness] {
    def explain: String
}

object ExpectedWitnessType {
    case class ScriptHashWitness[A <: Witness](witness: A) extends ExpectedWitnessType[A]:
        override def explain: String = "ScriptHash"

    case class PubKeyHashWitness[A <: Witness]() extends ExpectedWitnessType[A]:
        override def explain: String = "PubKeyHash"
}

trait HasWitnessKind[A]:
    def witnessKind: WitnessKind

enum WitnessKind:
    case KeyBased
    case ScriptBased

object HasWitnessKind:
    given HasWitnessKind[PubKeyWitness.type] with
        override def witnessKind = WitnessKind.KeyBased

    given HasWitnessKind[NativeScriptWitness] with
        override def witnessKind = WitnessKind.ScriptBased

    given HasWitnessKind[TwoArgumentPlutusScriptWitness] with
        override def witnessKind = WitnessKind.ScriptBased

    given HasWitnessKind[ThreeArgumentPlutusScriptWitness] with
        override def witnessKind = WitnessKind.ScriptBased

object TransactionBuilder:

    /** Builder is a state monad over Context. */
    private type BuilderM[A] = StateT[[X] =>> Either[TxBuildError, X], Context, A]

    // Helpers to cut down on type signature noise
    private def pure0[A] = StateT.pure[[X] =>> Either[TxBuildError, X], Context, A]
    private def liftF0[A] = StateT.liftF[[X] =>> Either[TxBuildError, X], Context, A]
    private def modify0 = StateT.modify[[X] =>> Either[TxBuildError, X], Context]
    private def get0 = StateT.get[[X] =>> Either[TxBuildError, X], Context]

    /** A wrapper around a UTxO set that prevents adding conflicting pairs */
    case class ResolvedUtxos private (utxos: UTxO) {

        /**   - If the UTxO does not exist in the map, add it.
          *   - If the UTxO exists in the map with a different output associated, return None
          *   - If the UTxO exists in the map with the same output, return the map unmodified
          */
        def addUtxo(utxo: TransactionUnspentOutput): Option[ResolvedUtxos] =
            utxos.get(utxo.input) match {
                case None => Some(ResolvedUtxos(utxos + utxo.toTuple))
                case Some(existingOutput) =>
                    if existingOutput == utxo.output
                    then Some(ResolvedUtxos(utxos))
                    else None
            }

        /** Tries to add multiple UTxOs, returning invalid additions. See [[addUtxo]] */
        def addUtxos(
            utxos: Seq[TransactionUnspentOutput]
        ): Either[Seq[TransactionUnspentOutput], ResolvedUtxos] = {
            val res: (Seq[TransactionUnspentOutput], ResolvedUtxos) =
                utxos.foldLeft((Seq.empty[TransactionUnspentOutput], this))((acc, utxo) =>
                    acc._2.addUtxo(utxo) match {
                        case Some(newResolved) => (acc._1, newResolved)
                        case None              => (acc._1.appended(utxo), acc._2)
                    }
                )
            if res._1.isEmpty
            then Right(res._2)
            else Left(res._1)
        }
    }

    object ResolvedUtxos:
        val empty: ResolvedUtxos = ResolvedUtxos(Map.empty)
        def apply(utxos: UTxO): ResolvedUtxos = new ResolvedUtxos(utxos)

    /** An opaque context in which the builder operates.
      *
      * TODO: make a class, remove toTuple()
      */
    case class Context private[TransactionBuilder] (
        transaction: Transaction,
        redeemers: Seq[DetachedRedeemer],
        network: Network,
        expectedSigners: Set[ExpectedSigner],
        /** Invariants:
          *   - The union of transaction.body.value.inputs, transaction.body.value.referenceInputs,
          *     and transaction.body.value.collateralInputs must exactly match resolvedUtxos.inputs
          */
        resolvedUtxos: ResolvedUtxos
    ) {

        /** Extract tupled information from a Context. This method is provided to avoid breaking
          * opacity while making it easier to check for equality in testing.
          */
        val toTuple: (
            Transaction,
            Seq[DetachedRedeemer],
            Network,
            Set[ExpectedSigner],
            ResolvedUtxos
        ) = (
          this.transaction,
          this.redeemers,
          this.network,
          this.expectedSigners,
          this.resolvedUtxos
        )

        /** Add additional signers to the Context.
          */
        def addSigners(additionalSigners: Set[ExpectedSigner]): Context = {
            this |> Focus[Context](_.expectedSigners).modify(_ ++ additionalSigners)
        }

        /** Ensure that all transaction outputs in the context have min ada. */
        def setMinAdaAll(protocolParams: ProtocolParams): Context = {
            this |> unsafeCtxBodyL
                .refocus(_.outputs)
                .modify(os =>
                    os.map((to: Sized[TransactionOutput]) =>
                        Sized(setMinAda(to.value, protocolParams))
                    )
                )
        }

        /** Balance the transaction in a context, adding and removing mock signatures where
          * necessary.
          */
        def balance(
            // TODO: @Ilia leave comment about not messing with inputs, etc. If your diff handler
            // adds or removes components needing signatures, the fees won't be calculated correctly.
            // It also won't update .resolvedUtxos.
            // TODO: @Ilia Wrap this so that we can only modify the transaction outputs. Basically inject
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
        def getUtxo: UTxO = this.resolvedUtxos.utxos

        /** Validate a context according so a set of ledger rules */
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

        /** Set min ada, balance, and validate a context. TODO: @Ilia consider putting PP,
          * evaluator, and validators, into the parameters for the transaction builder class
          */
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
          resolvedUtxos = ResolvedUtxos.empty
        )

        // Tries to add the output to the resolved utxo set, throwing an error if
        // the input is already mapped to another output
        def addResolvedUtxo(utxo: TransactionUnspentOutput): BuilderM[Unit] =
            for {
                ctx <- StateT.get
                mbNewUtxos = ctx.resolvedUtxos.addUtxo(utxo)
                _ <- mbNewUtxos match {
                    case None =>
                        liftF0(
                          Left(
                            TxBuildError.ResolvedUtxosIncoherence(
                              input = utxo.input,
                              existingOutput = ctx.resolvedUtxos.utxos(utxo.input),
                              incoherentOutput = utxo.output
                            )
                          )
                        )
                    case Some(utxos) => modify0(Focus[Context](_.resolvedUtxos).replace(utxos))
                }
            } yield ()

    private val unsafeCtxBodyL: Lens[Context, TransactionBody] = {
        Focus[Context](_.transaction) >>> txBodyL
    }

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
      */
    // TODO: try to make it polymorphic
    private def replaceAdaUpdate(coin: Coin, to: TransactionOutput): TransactionOutput =
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
                // TODO: Keeping detached redeemers might not work as expected, add test
                _ <- modify0(Focus[Context](_.transaction).replace(res))
            } yield ()
        }
        modifyBuilderM.run(ctx).map(_._1)
    }

    trait HasBuilderEffect[A]:
        def runEffect(): BuilderM[Unit]

    private def processConstraints(steps: Seq[TransactionBuilderStep]): BuilderM[Unit] =
        steps.traverse_(processConstraint)

    private def processConstraint(step: TransactionBuilderStep): BuilderM[Unit] = step match {

        case step @ TransactionBuilderStep.SpendOutput(utxo, witness) =>
            for {
                _ <- assertNetworkId(utxo.output.address)
                _ <- modify0(
                  // Add Input
                  unsafeCtxBodyL
                      .refocus(_.inputs)
                      .modify(inputs =>
                          TaggedOrderedSet.from(appendDistinct(utxo.input, inputs.toSeq))
                      )
                )
                // Add to resolvedUtxos
                _ <- Context.addResolvedUtxo(utxo)
                // Handle to witness
                _ <- useOutputWitness(witness, utxo)
            } yield ()

        case TransactionBuilderStep.SendOutput(output) =>
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
            useMint(scriptHash, assetName, amount, mintWitness)

        case TransactionBuilderStep.ReferenceOutput(utxo) =>
            for {
                _ <- assertNetworkId(utxo.output.address)

                _ <- modify0(
                  // Add the referenced utxo id to the tx body
                  unsafeCtxBodyL
                      .refocus(_.referenceInputs)
                      .modify(inputs =>
                          TaggedOrderedSet.from(appendDistinct(utxo.input, inputs.toSeq))
                      )
                )

                _ <- Context.addResolvedUtxo(utxo)
            } yield ()

        case TransactionBuilderStep.AddCollateral(utxo) =>
            for {
                _ <- assertNetworkId(utxo.output.address)
                _ <- assertAdaOnlyPubkeyUtxo(utxo)
                _ <- Context.addResolvedUtxo(utxo)
                _ <- modify0(
                  // Add the collateral utxo to the tx body
                  unsafeCtxBodyL
                      .refocus(_.collateralInputs)
                      .modify(inputs =>
                          TaggedOrderedSet.from(appendDistinct(utxo.input, inputs.toSeq))
                      )
                )
            } yield ()
        case TransactionBuilderStep.IssueCertificate(cert, witness) =>
            for {
                _ <- modify0(
                  unsafeCtxBodyL
                      .refocus(_.certificates)
                      .modify(certificates =>
                          TaggedSet.from(appendDistinct(cert, certificates.toIndexedSeq))
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
                          TaggedOrderedSet.from(appendDistinct(proposal, proposals.toSeq))
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

    // -------------------------------------------------------------------------
    // SpendOutput step
    // -------------------------------------------------------------------------

    /** Tries to modify the transaction to make it consume a given output and add the requisite
      * signature(s) to the Context's _.expectedSigners. Uses an [[WitnessForSpend]] to try to
      * satisfy spending requirements.
      */
    def useOutputWitness(
        witness: PubKeyWitness.type | NativeScriptWitness | ThreeArgumentPlutusScriptWitness,
        utxo: TransactionUnspentOutput
    ): BuilderM[Unit] =
        witness match {
            // Case 1: Key-locked input
            // Add the pubkey hash to ctx.expectedSigners
            case _: PubKeyWitness.type => {
                //val pkhw = ExpectedWitnessType.PubKeyHashWitness[PubKeyWitness.type]()
                for {
                    // Extract the key hash, erroring if not a Shelley PKH address
                    keyHash <- liftF0(utxo.output.address match {
                        case sa: ShelleyAddress =>
                            sa.payment match {
                                case kh: ShelleyPaymentPart.Key => Right(kh.hash)
                                case _: ShelleyPaymentPart.Script =>
                                    Left(TxBuildError.WrongOutputType(WitnessKind.KeyBased, utxo))
                            }
                        case _ => Left(TxBuildError.WrongOutputType(WitnessKind.KeyBased, utxo))
                    })
                    // Add the Key hash to expectedSigners
                    _ <- modify0(
                      Focus[Context](_.expectedSigners)
                          .modify(_ + ExpectedSigner(keyHash))
                    )
                } yield ()
            }
            // Case 2: Native script-locked input
            // Ensure the hash matches the witness, handle the output components, defer to witness handling
            case nativeScriptWitness: NativeScriptWitness => {
                // val shw = ExpectedWitnessType.ScriptHashWitness[WitnessForSpend](outputWitness)
                for {
                    // Extract the script hash, throwing an error on anything but a Shelley Script Addrress
                    scriptHash <- liftF0(utxo.output.address match {
                        case sa: ShelleyAddress =>
                            sa.payment match {
                                case s: ShelleyPaymentPart.Script => Right(s.hash)
                                case _: ShelleyPaymentPart.Key =>
                                    Left(TxBuildError.WrongOutputType(???, utxo))

                            }
                        case _ => Left(TxBuildError.WrongOutputType(???, utxo))
                    })

                    _ <- assertScriptHashMatchesSource(scriptHash, nativeScriptWitness.scriptSource)

                    _ <- useNativeScript(
                      nativeScriptWitness.scriptSource,
                      nativeScriptWitness.additionalSigners
                    )
                } yield ()
            }

            // Case 3: Plutus script-locked input
            // Ensure the hash matches the witness, handle the output components, defer to witness handling
            case plutusWitness: ThreeArgumentPlutusScriptWitness => {
                // val shw = ExpectedWitnessType.ScriptHashWitness[WitnessForSpend](outputWitness)
                for {
                    // TODO: Factor out
                    // Extract the script hash, throwing an error on anything but a Shelley Script Addrress
                    scriptHash <- liftF0(utxo.output.address match {
                        case sa: ShelleyAddress =>
                            sa.payment match {
                                case s: ShelleyPaymentPart.Script => Right(s.hash)
                                case _: ShelleyPaymentPart.Key =>
                                    Left(TxBuildError.WrongOutputType(???, utxo))

                            }
                        case _ => Left(TxBuildError.WrongOutputType(???, utxo))
                    })

                    _ <- assertScriptHashMatchesSource(scriptHash, plutusWitness.scriptSource)

                    _ <- usePlutusScript(plutusWitness.scriptSource, plutusWitness.additionalSigner)

                    detachedRedeemer = DetachedRedeemer(
                      plutusWitness.redeemer,
                      RedeemerPurpose.ForSpend(utxo.input)
                    )
                    _ <- modify0(ctx =>
                        ctx.focus(_.redeemers)
                            .modify(r => appendDistinct(detachedRedeemer, r))
                    )
                    _ <- useDatum(utxo, plutusWitness.datum)
                } yield ()
            }
        }

    def useNativeScript(
        scriptWitness: ScriptSource[Script.Native],
        additionalSigners: Set[ExpectedSigner]
    ): BuilderM[Unit] =
        for {
            // Regardless of how the witness is passed, add the additional signers
            _ <- modify0(
              Focus[Context](_.expectedSigners).modify(_ ++ additionalSigners)
            )

            _ <- scriptWitness match {
                case ScriptSource.NativeValue(ns) =>
                    modify0(
                      // Add the native script to the witness set
                      unsafeCtxWitnessL
                          .refocus(_.nativeScripts)
                          .modify(s => appendDistinct(ns, s.toList).toSet)
                    )
                // Script should already be attached, see [[assertAttachedScriptExists]]
                // case ScriptSource.NativeAttachedScript(_) => pure0(())
                // FIXME: Should not happen
                case ScriptSource.PlutusValue(_) => ???
                //case ScriptSource.PlutusAttachedScript => ???
            }
        } yield ()

    def usePlutusScript(
        scriptWitness: ScriptSource[PlutusScript],
        additionalSigners: Set[ExpectedSigner]
    ): BuilderM[Unit] =
        for {
            // Add script's additional signers to txBody.requiredSigners
            _ <- modify0(
              (Focus[Context](_.transaction) >>> txBodyL)
                  .refocus(_.requiredSigners)
                  .modify((s: TaggedOrderedSet[AddrKeyHash]) =>
                      TaggedOrderedSet.from(
                        s.toSortedSet ++ additionalSigners.map(_.hash)
                      )
                  )
            )

            // Add to expected signers
            _ <- modify0(
              Focus[Context](_.expectedSigners).modify(_ ++ additionalSigners)
            )

            _ <- scriptWitness match {
                case ScriptSource.PlutusValue(ps: PlutusScript) =>
                    // Add the script value to the appropriate field
                    ps match {
                        case (v1: Script.PlutusV1) =>
                            modify0(
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV1Scripts)
                                  .modify(s => Set.from(appendDistinct(v1, s.toSeq)))
                            )
                        case (v2: Script.PlutusV2) =>
                            modify0(
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV2Scripts)
                                  .modify(s => Set.from(appendDistinct(v2, s.toSeq)))
                            )
                        case (v3: Script.PlutusV3) =>
                            modify0(
                              unsafeCtxWitnessL
                                  .refocus(_.plutusV3Scripts)
                                  .modify(s => Set.from(appendDistinct(v3, s.toSeq)))
                            )
                    }
                // Script should already be attached, see [[assertAttachedScriptExists]]
                case ScriptSource.PlutusAttached => pure0(())
            }
        } yield ()

    /** Tries to modify the transaction state to make it consume a given script output. Uses a
      * `DatumWitness` if the UTxO datum is provided as a hash. TODO: used only once in
      * useOutputWitness
      */
    def useDatum(
        utxo: TransactionUnspentOutput,
        datum: Datum
    ): BuilderM[Unit] =
        for {
            _ <- utxo.output.datumOption match {
                case None =>
                    liftF0(
                      Left(TxBuildError.DatumIsMissing(utxo))
                    )
                case Some(DatumOption.Inline(_)) =>
                    datum match {
                        case Datum.DatumInlined => pure0(())
                        case Datum.DatumValue(_) =>
                            liftF0(
                              Left(TxBuildError.DatumValueForUtxoWithInlineDatum(utxo, datum))
                            )
                    }
                case Some(DatumOption.Hash(datumHash)) =>
                    datum match {
                        case Datum.DatumInlined =>
                            liftF0(Left(TxBuildError.DatumWitnessNotProvided(utxo)))
                        case Datum.DatumValue(providedDatum) =>
                            // TODO: is that correct? Upstream Data.dataHash extension?
                            val computedHash: DataHash =
                                DataHash.fromByteString(blake2b_224(serialiseData(providedDatum)))

                            if (datumHash == computedHash) {
                                modify0(
                                  unsafeCtxWitnessL
                                      .refocus(_.plutusData)
                                      .modify(plutusData =>
                                          KeepRaw.apply(
                                            TaggedSet.from(
                                              appendDistinct(
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

    // -------------------------------------------------------------------------
    // MintAsset step
    // -------------------------------------------------------------------------

    def useMint(
        scriptHash: ScriptHash,
        assetName: AssetName,
        amount: Long,
        witness: NativeScriptWitness | TwoArgumentPlutusScriptWitness
    ): BuilderM[Unit] =
        for {
            _ <- useCredentialWitness(
              CredentialAction.Minting(scriptHash),
              Credential.ScriptHash(scriptHash),
              witness
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

    // -------------------------------------------------------------------------
    // AddCollateral step
    // -------------------------------------------------------------------------

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
                case _                    => liftF0(Left(TxBuildError.CollateralNotPubKey(utxo)))
            }
        } yield ()

    // -------------------------------------------------------------------------
    // IssueCertificate step
    // -------------------------------------------------------------------------

    def useCertificateWitness(
        cert: Certificate,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness
    ): BuilderM[Unit] = cert match {
        // FIXME: verify
        case Certificate.UnregCert(credential, _) =>
            for {
                _ <- (credential, witness) match {
                    // Credential.KeyHash
                    case (Credential.KeyHash(_), PubKeyWitness) => pure0(())
                    case (Credential.KeyHash(_), witness: TwoArgumentPlutusScriptWitness) =>
                        liftF0(
                          Left(
                            TxBuildError.UnneededDeregisterWitness(
                              StakeCredential(credential),
                              witness
                            )
                          )
                        )
                    case (Credential.KeyHash(_), witness: NativeScriptWitness) =>
                      liftF0(
                        Left(
                          TxBuildError.UnneededDeregisterWitness(
                            StakeCredential(credential),
                            witness
                          )
                        )
                      )
                    // Credential.ScriptHash
                    case (Credential.ScriptHash(_), PubKeyWitness) =>
                        liftF0(
                          Left(
                            TxBuildError.WrongCredentialType(
                              CredentialAction.StakeCert(cert),
                              WitnessKind.KeyBased,
                              credential
                            )
                          )
                        )
                    case (Credential.ScriptHash(scriptHash), witness: TwoArgumentPlutusScriptWitness) =>
                        assertScriptHashMatchesSource(scriptHash, witness.scriptSource)
                    case (Credential.ScriptHash(scriptHash), witness: NativeScriptWitness) =>
                        assertScriptHashMatchesSource(scriptHash, witness.scriptSource)
                }
                _ <- useCredentialWitness(
                  CredentialAction.StakeCert(cert),
                  credential,
                  witness
                )
            } yield ()
        case Certificate.StakeDelegation(credential, _) =>
            useCredentialWitness(CredentialAction.StakeCert(cert), credential, witness)
        // FIXME: verify
        case Certificate.RegCert(_, _) =>
            pure0(())
        case Certificate.PoolRegistration(_, _, _, _, _, _, _, _, _) =>
            pure0(())
        case Certificate.PoolRetirement(_, _) =>
            pure0(())
        case Certificate.VoteDelegCert(credential, _) =>
            useCredentialWitness(CredentialAction.StakeCert(cert), credential, witness)
        case Certificate.StakeVoteDelegCert(credential, _, _) =>
            useCredentialWitness(CredentialAction.StakeCert(cert), credential, witness)
        case Certificate.StakeRegDelegCert(credential, _, _) =>
            useCredentialWitness(CredentialAction.StakeCert(cert), credential, witness)
        case Certificate.VoteRegDelegCert(credential, _, _) =>
            useCredentialWitness(CredentialAction.StakeCert(cert), credential, witness)
        case Certificate.StakeVoteRegDelegCert(credential, _, _, _) =>
            useCredentialWitness(CredentialAction.StakeCert(cert), credential, witness)
        case Certificate.AuthCommitteeHotCert(_, _) =>
            pure0(()) // not supported
        case Certificate.ResignCommitteeColdCert(_, _) =>
            pure0(()) // not supported
        case Certificate.RegDRepCert(credential, _, _) =>
            useCredentialWitness(CredentialAction.StakeCert(cert), credential, witness)
        case Certificate.UnregDRepCert(credential, _) =>
            useCredentialWitness(CredentialAction.StakeCert(cert), credential, witness)
        case Certificate.UpdateDRepCert(credential, _) =>
            useCredentialWitness(CredentialAction.StakeCert(cert), credential, witness)
     }

    // -------------------------------------------------------------------------
    // WithdrawRewards step
    // -------------------------------------------------------------------------

    def useWithdrawRewardsWitness(
        stakeCredential: StakeCredential,
        amount: Coin,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness
    ): BuilderM[Unit] =
        for {
            ctx <- StateT.get[[X] =>> Either[TxBuildError, X], Context]

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

    // -------------------------------------------------------------------------
    // SubmitProposal step
    // -------------------------------------------------------------------------

    def useProposalWitness(
          proposal: ProposalProcedure,
          witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness
    ): BuilderM[Unit] = {
        def getPolicyHash(govAction: GovAction): Option[ScriptHash] = govAction match {
            case GovAction.ParameterChange(_, _, policyHash)  => policyHash
            case GovAction.TreasuryWithdrawals(_, policyHash) => policyHash
            case _                                            => None
        }

        getPolicyHash(proposal.govAction) match {
            case None =>
                pure0(())
            case Some(policyHash) =>
                useCredentialWitness(
                  CredentialAction.Proposing(proposal),
                  Credential.ScriptHash(policyHash),
                  witness
                )
        }
    }

    // -------------------------------------------------------------------------
    // SubmitVotingProcedure step
    // -------------------------------------------------------------------------

    def useVotingProcedureWitness(
         voter: Voter,
         witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness
    ): BuilderM[Unit] =
        for {
            cred <- voter match {
                case Voter.StakingPoolKey(poolKeyHash) =>
                    val credential = Credential.KeyHash(poolKeyHash)
                    witness match {
                        case _: PubKeyWitness.type => pure0(credential)
                        case witness: TwoArgumentPlutusScriptWitness =>
                            liftF0(
                              Left(TxBuildError.UnneededSpoVoteWitness(credential, witness))
                            )
                        case witness: NativeScriptWitness =>
                            liftF0(
                              Left(TxBuildError.UnneededSpoVoteWitness(credential, witness))
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
            _ <- useCredentialWitness(CredentialAction.Voting(voter), cred, witness)
        } yield ()

    // -------------------------------------------------------------------------
    // Common functions for CredentialWitness
    // -------------------------------------------------------------------------

    def useCredentialWitness(
        credAction: CredentialAction,
        cred: Credential,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness
    ): BuilderM[Unit] =
     for {
        _ <- witness match {
            // Pubkey credential witness: add to expected signers
            case PubKeyWitness =>

                for {
                    _ <- assertCredentialType(
                      credAction,
                      PubKeyWitness,
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
                                  WitnessKind.KeyBased,
                                  cred
                                )
                              )
                            )
                    }
                } yield ()
            case witness: NativeScriptWitness =>
                for {
                    _ <- assertCredentialType(
                      credAction,
                      witness,
                      cred
                    )
                    _ <- useNativeScript(witness.scriptSource, witness.additionalSigners)
                } yield ()
            case witness: TwoArgumentPlutusScriptWitness =>
                for {
                    _ <- assertCredentialType(
                      credAction,
                      witness,
                      cred
                    )
                    _ <- usePlutusScript(witness.scriptSource,  witness.additionalSigners)
                    _ <- {
                        val detachedRedeemer = DetachedRedeemer(
                          datum = witness.redeemer,
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
                                .modify(redeemers => appendDistinct(detachedRedeemer, redeemers))
                        )
                    }
                } yield ()
        }
     } yield ()

    def assertCredentialType[A <: PubKeyWitness.type | NativeScriptWitness | TwoArgumentPlutusScriptWitness ](
        action: CredentialAction,
        witness: A,
        cred: Credential
     )(using hwk: HasWitnessKind[A]): BuilderM[Unit] = {

       val wrongCredErr = TxBuildError.WrongCredentialType(action, hwk.witnessKind, cred)

        val result: BuilderM[Unit] = witness match {
            case PubKeyWitness =>
                cred.keyHashOption match {
                    case Some(_) => pure0(())
                    case None => liftF0(Left(wrongCredErr))
                }

            case witness: NativeScriptWitness =>
                for {
                    scriptHash <- cred.scriptHashOption match {
                        case Some(hash) => pure0(hash)
                        case None => liftF0(Left(wrongCredErr))
                    }
                    _ <- assertScriptHashMatchesSource(scriptHash, witness.scriptSource)
                } yield ()

            case witness: TwoArgumentPlutusScriptWitness =>
                for {
                    scriptHash <- cred.scriptHashOption match {
                        case Some(hash) => pure0(hash)
                        case None => liftF0(Left(wrongCredErr))
                    }
                    _ <- assertScriptHashMatchesSource(scriptHash, witness.scriptSource)
                } yield ()
        }
        result
    }

    /** Given a script hash, check the context to ensure that a script matching the given script
      * hash is attached to the transaction either as a CIP-33 ref script or in the witness set
      */
    def assertAttachedScriptExists(scriptHash: ScriptHash): BuilderM[Unit] =
        for {
            ctx <- StateT.get
            resolvedScripts <- liftF0(
              AllResolvedScripts
                  .allResolvedScripts(
                    ctx.transaction,
                    ctx.resolvedUtxos.utxos
                  )
                  .left
                  .map(_ => TxBuildError.ScriptResolutionError)
            )
            _ <-
                if resolvedScripts.map(_.scriptHash).contains(scriptHash)
                then pure0(())
                else
                    liftF0(
                      Left(
                        TxBuildError.AttachedScriptNotFound(scriptHash)
                      )
                    )
        } yield ()

    /** Assert that the given script hash either matches the script provided directly, or is
      * otherwise already attached to the transaction as a CIP-33 script or as a pre-existing
      * witness.
      * @param neededScriptHash
      * @param scriptWitness
      * @return
      */
    def assertScriptHashMatchesSource(
        neededScriptHash: ScriptHash,
        scriptWitness: ScriptSource[PlutusScript | Script.Native]
    ): BuilderM[Unit] =
        scriptWitness match {
            case ScriptSource.NativeValue(script) =>
                assertScriptHashMatchesScript(neededScriptHash, script)

            case ScriptSource.NativeAttached => ??? //assertAttachedScriptExists(neededScriptHash)
            case ScriptSource.PlutusValue(script) =>
                assertScriptHashMatchesScript(neededScriptHash, script)
            case ScriptSource.PlutusAttached => ???
        }

    def assertScriptHashMatchesScript(
        scriptHash: ScriptHash,
        script: Script
    ): BuilderM[Unit] = {
        val computedHash = script match {
            case nativeScript: Script.Native => nativeScript.scriptHash
            case plutusScript: PlutusScript  => plutusScript.scriptHash
        }

        if (scriptHash != computedHash) {
            liftF0(
              Left(TxBuildError.IncorrectScriptHash(script, scriptHash))
            )
        } else {
            pure0(())
        }
    }

    // -------------------------------------------------------------------------
    // Common assertions
    // -------------------------------------------------------------------------

    /** Ensure that the network id of the address matches the network id of the builder context.
      */
    def assertNetworkId(addr: Address): BuilderM[Unit] =
        for {
            context: Context <- StateT.get
            addrNetwork <- addr.getNetwork match
                case Some(network) => pure0(network)
                case None =>
                    liftF0(
                      Left(TxBuildError.ByronAddressesNotSupported(addr))
                    )
            res <-
                if context.network != addrNetwork
                then
                    liftF0(
                      Left(TxBuildError.WrongNetworkId(addr))
                    )
                else pure0(())
        } yield ()

// -------------------------------------------------------------------------
// Errors
// -------------------------------------------------------------------------
sealed trait TxBuildError:
    def explain: String

object TxBuildError {
    case class Unimplemented(description: String) extends TxBuildError {
        override def explain: String = s"$description is not yet implemented. If you need it, " +
            s"submit a request at $bugTrackerUrl."
    }

    case class ResolvedUtxosIncoherence(
        input: TransactionInput,
        existingOutput: TransactionOutput,
        incoherentOutput: TransactionOutput
    ) extends TxBuildError {
        override def explain: String =
            "The context's resolvedUtxos already contain an input associated with a different output." +
                s"\nInput: $input" +
                s"\nExisting Output: $existingOutput" +
                s"\nIncoherent Output: $incoherentOutput"
    }

    case class CollateralNotPubKey(utxo: TransactionUnspentOutput) extends TxBuildError {
        override def explain: String =
            s"The UTxO passed as a collateral input is not a PubKey UTxO. UTxO: $utxo"
    }

    // TODO: This error could probably be improved.
    case class CannotExtractSignatures(step: TransactionBuilderStep) extends TxBuildError {
        override def explain: String =
            s"Could not extract signatures via _.additionalSigners from $step"
    }

    case class DatumIsMissing(utxo: TransactionUnspentOutput) extends TxBuildError {
        override def explain: String =
            "Given witness to spend an output requires a datum that is missing: $utxo"
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
        script: Script,
        hash: ScriptHash
    ) extends TxBuildError {
        override def explain: String = script match {
            case nativeScript: Script.Native =>
                s"Provided script hash ($hash) does not match the provided native script ($nativeScript)"
            case plutusScript: PlutusScript =>
                s"Provided script hash ($hash) does not match the provided Plutus script ($plutusScript)"
        }
    }

    case class WrongOutputType(
        expectedType: WitnessKind,
        utxo: TransactionUnspentOutput
    ) extends TxBuildError {
        override def explain: String =
            "The UTxO you provided requires no witness, because the payment credential of the address is a `PubKeyHash`. " +
                s"UTxO: $utxo"
    }

    case class WrongCredentialType(
        action: CredentialAction,
        expectedType: WitnessKind,
        cred: Credential
    ) extends TxBuildError {
        override def explain: String =
            s"${action.explain} ($action) requires a ${expectedType} witness: $cred"
    }

    // TODO: no reason to have TransactionUnspentOutput here
    case class DatumWitnessNotProvided(utxo: TransactionUnspentOutput) extends TxBuildError {
        override def explain: String =
            "The output you are trying to spend contains a datum hash, you need to provide " +
                s"a `DatumValue`, output: $utxo"
    }

    case class DatumValueForUtxoWithInlineDatum(utxo: TransactionUnspentOutput, datum: Datum)
        extends TxBuildError {
        override def explain: String =
            "You can't provide datum value for a utxo with inlined datum: " +
                s"You tried to provide: $datum for the UTxO: $utxo"
    }

    case class UnneededDeregisterWitness(
        stakeCredential: StakeCredential,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness
    ) extends TxBuildError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, " +
                "but the stake credential you are trying to issue a deregistering certificate for " +
                "is a PubKeyHash credential. You should omit the provided credential witness for this " +
                s"credential: $stakeCredential. Provided witness: $witness"
    }

    case class UnneededSpoVoteWitness(cred: Credential, witness: TwoArgumentPlutusScriptWitness | NativeScriptWitness)
        extends TxBuildError {
        override def explain: String =
            "You've provided an optional `CredentialWitness`, but the corresponding Voter is " +
                "SPO (Stake Pool Operator). You should omit the provided credential witness " +
                s"for this credential: $cred. Provided witness: $witness"
    }

    case class UnneededProposalPolicyWitness(
        proposal: ProposalProcedure,
        witness: PubKeyWitness.type | TwoArgumentPlutusScriptWitness | NativeScriptWitness
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

    case class AttachedScriptNotFound(scriptHash: ScriptHash) extends TxBuildError {
        override def explain: String =
            s"No witness or ref/spent output is found for script matching $scriptHash." +
                "Note that the builder steps are not commutative: you must attach the script " +
                "before using an AttachedScript ScriptWitness."
    }

    case class ByronAddressesNotSupported(address: Address) extends TxBuildError {
        override def explain: String =
            s"Byron addresses are not supported: $address."
    }

    // We can't really return meaningful information, because scalus doesn't
    // provide it. See [[assertAttachedScriptExists]]
    case object ScriptResolutionError extends TxBuildError {
        override def explain: String =
            "An error was returned when trying to resolve scripts for the transaction."
    }
}

// -------------------------------------------------------------------------
// auxiliary types, extensions, helpers
// -------------------------------------------------------------------------

// TODO: itd be nice to make this opaque and only return from chain queries
// NOTE (Peter, 2025-09-23): this comes from
// https://github.com/mlabs-haskell/purescript-cardano-types/blob/master/src/Cardano/Types/TransactionUnspentOutput.purs
case class TransactionUnspentOutput(input: TransactionInput, output: TransactionOutput):
    def toTuple: (TransactionInput, TransactionOutput) = (input, output)

object TransactionUnspentOutput:
    def apply(utxo: (TransactionInput, TransactionOutput)): TransactionUnspentOutput =
        TransactionUnspentOutput(utxo._1, utxo._2)

// NOTE (Peter, 2025-09-23): this comes from https://github.com/mlabs-haskell/purescript-cardano-types/blob/master/src/Cardano/Types/StakeCredential.purs
case class StakeCredential(credential: Credential)

extension (network: Network)
    def toNetworkId: Int = network match
        case Network.Testnet  => 0
        case Network.Mainnet  => 1
        case Network.Other(b) => b.toInt

object NetworkExtensions:
    /** Convert integer network ID to Network */
    def fromNetworkId(networkId: Int): Option[Network] = networkId match
        case 0                      => Some(Network.Testnet)
        case 1                      => Some(Network.Mainnet)
        case v if v >= 2 && v <= 15 => Some(Network.Other(v.toByte))
        case _                      => None

/** Append en element to sequence and return distinct values only, preservin the order.element.
  */
def appendDistinct[A](elem: A, seq: Seq[A]): Seq[A] =
    seq.appended(elem).distinct
