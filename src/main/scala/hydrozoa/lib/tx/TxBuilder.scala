package hydrozoa.lib.tx

/** This module contains declarative transaction building types and utilities ported from
  * purescript-cardano-transaction-builder with some modifications and additions.
  *   - The main entry-point: [[TransactionBuilder.build]]
  *   - The definition of steps: [[TransactionBuilderStep]]
  */

import hydrozoa._
import hydrozoa.lib.optics.>>>
import hydrozoa.lib.tx
import hydrozoa.lib.tx.TxBuildError.{
    CannotExtractSignatures,
    RedeemerIndexingInternalError,
    Unimplemented,
    WrongCredentialType
}

import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, _}
import scalus.cardano.ledger.GovAction._
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger._
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{Context => SContext, STS, State => SState, UtxoEnv}
import scalus.cardano.ledger.txbuilder.wip.DiffHandler
import scalus.cardano.ledger.txbuilder.{LowLevelTxBuilder, TxBalancingError}
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.|>

import cats._
import cats.data._
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import io.bullet.borer.Cbor
import monocle._
import monocle.syntax.all._

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
      */
    case class SpendOutput(
        utxo: TransactionUnspentOutput,
        /** Pass None for pubkey outputs only */
        witness: Option[OutputWitness] = None
    ) extends TransactionBuilderStep

    /** Send some funds/data to an address. Multiple identical steps are acceptable. */
    case class Pay(output: TransactionOutput) extends TransactionBuilderStep

    /** Mint/burn tokens using a native/plutus script. */
    case class MintAsset(
        scriptHash: ScriptHash,
        assetName: AssetName,
        amount: Long,
        witness: CredentialWitness
    ) extends TransactionBuilderStep

    /** Add a utxo as a reference one. Doesn't allow the same utxo being references several times.
      * The reason that action is represented as a step is that reference utxos should be added to
      * the context and also may be required to create a [[OutputWitness]].
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

// -----------------------------------------------------------------------------
// OutputWitness
// -----------------------------------------------------------------------------

/** [[OutputWitness]] is used to provide the evidence needed to consume an output. It must
  * correspond to a [[TransactionUnspentOutput]] address' payment credential to unlock it.
  *
  * It also contains the signatures expected (for native scripts) or required (for plutus scripts)
  * to be used in fee calculation.
  *
  * // TODO: rename to SpendWitness
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

    extension (self: OutputWitness)
        def scriptWitness: ScriptWitness[_] = self match {
            case NativeScriptOutput(witness)       => witness
            case PlutusScriptOutput(witness, _, _) => witness
        }
}

// -----------------------------------------------------------------------------
// CredentialWitness
// -----------------------------------------------------------------------------

/** [[CredentialWitness]] is used to provide the evidence needed to perform operations on behalf of
  * a credential, which include:
  *
  *   - Minting
  *   - Certificate witnessing
  *   - Rewards withdrawal
  *
  * Unlike [[OutputWitness]], it does not include a [[DatumWitness]], because minting policies and
  * stake scripts do not take a datum.
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

    extension (self: CredentialWitness)
        def scriptWitness: ScriptWitness[_] = self match {
            case NativeScriptCredential(witness)    => witness
            case PlutusScriptCredential(witness, _) => witness
        }
}

// -----------------------------------------------------------------------------
// ScriptWitness
// -----------------------------------------------------------------------------

/** Gives the user options for specifying everything needed to unlock an action guarded by a script,
  * including:
  *   - Spending a UTxO located at an address with a ScriptHash payment credential.
  *   - Witnessing credential operations requiring a script hash
  *   - Witnessing a mint
  *   - Witnessing a rewards withdrawal for a script hash credential
  *   - Any additional signers required to unlock the script, such as for a plutus or native
  *     multisig
  *
  * TODO: rename ScriptSource?
  */
sealed trait ScriptWitness[+A]:
    val additionalSigners: Set[ExpectedSigner]

object ScriptWitness {

    /** Contains a script itself, will be included to the witness set. */
    case class ScriptValue[A](script: A, additionalSigners: Set[ExpectedSigner])
        extends ScriptWitness[A]

    /** Tries to use a CIP-33 reference script from a referenced utxo - see ReferenceUtxo step. */
    case class ScriptReference(
        utxoId: TransactionInput,
        additionalSigners: Set[ExpectedSigner]
    ) extends ScriptWitness[Nothing]

    /** Like [[ScriptReference]] but uses a spent utxo rather then a referenced utxo - see
      * SepndOutput step.
      */
    case class ScriptReferenceSpent(
        utxoId: TransactionInput,
        additionalSigners: Set[ExpectedSigner]
    ) extends ScriptWitness[Nothing]

}

// -----------------------------------------------------------------------------
// DatumWitness
// -----------------------------------------------------------------------------

/** Datums in UTxOs can be stored in two forms: inline datums or datum hashes. When there's a hash,
  * we need to provide a datum corresponding to this hash, which can be done by either providing the
  * value literally, or using a reference input where it is stored inline (the latter is not
  * supported, since we haven't seen it in the wild - you can work with the datum of a reference
  * input deirectly, so we decided to remove that. Please open an issue if you need it.
  */
sealed trait DatumWitness

object DatumWitness {
    case class DatumValue(datum: Data) extends DatumWitness
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

/** Represents a witness type, i.e., key-based or script-based for possible kinds of witnesses -
  * [[OutputWitness]] or [[CredentialWitness]].
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

        /** Add additional signers to the transaction. FIXME: , silently dropping any that point to
          * invalid components.
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
        def getUtxo: UTxO = Map.from(this.resolvedUtxos.map(utxo => (utxo._1, utxo._2)))

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
          resolvedUtxos = Set.empty
        )

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
      *
      * @param coin
      * @param to
      * @return
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
                  // Add utxo to resolved utxos
                      <<< Focus[Context](_.resolvedUtxos).modify(_ + utxo)
                )
                // Handle to witness
                _ <- useOutputWitness(witness, utxo)
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
                                  TaggedOrderedSet.from(appendDistinct(utxo.input, inputs.toSeq))
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
      * signature(s) to the Context's _.expectedSigners. Uses an [[OutputWitness]] to try to satisfy
      * spending requirements.
      */
    def useOutputWitness(
        mbWitness: Option[OutputWitness],
        utxo: TransactionUnspentOutput
    ): BuilderM[Unit] = {

        // Checks that output is locked at the address that corresponds to witness type.
        def assertAddressMathesWitness(
            expectedType: ExpectedWitnessType[OutputWitness],
            utxo: TransactionUnspentOutput
        ): BuilderM[Unit] =
            for {
                mbRes <-
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
                                    scriptHash <- OptionT.fromOption[BuilderM](
                                      cred.scriptHashOption
                                    )
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
                    if mbRes.isEmpty
                    then
                        liftF0(
                          Left(TxBuildError.WrongOutputType(expectedType, utxo))
                        )
                    else pure0(())
            } yield res
        end assertAddressMathesWitness

        for {
            _ <- mbWitness match {
                // Case Spend 1: Pubkey input: add the pubkey hash to ctx.expectedSigners
                case None =>
                    val pkhw = ExpectedWitnessType.PubKeyHashWitness[OutputWitness]()
                    for {
                        _ <- assertAddressMathesWitness(pkhw, utxo)
                        keyHash <- liftF0(utxo.output.address match {
                            case sa: ShelleyAddress =>
                                sa.payment match {
                                    case kh: ShelleyPaymentPart.Key => Right(kh.hash)
                                    case _: ShelleyPaymentPart.Script =>
                                        Left(TxBuildError.WrongOutputType(pkhw, utxo))
                                }
                            case _ => Left(TxBuildError.WrongOutputType(pkhw, utxo))
                        })
                        _ <- modify0(
                          Focus[Context](_.expectedSigners)
                              .modify(_ + ExpectedSigner(keyHash))
                        )
                    } yield ()

                // Native Script: Assure the output type is correct
                case Some(witness @ OutputWitness.NativeScriptOutput(nsWitness)) =>
                    for {
                        _ <- assertAddressMathesWitness(
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
                        _ <- assertAddressMathesWitness(
                          ExpectedWitnessType.ScriptHashWitness[OutputWitness](witness),
                          utxo
                        )
                        _ <- usePlutusScriptWitness(plutusScriptWitness)
                        detachedRedeemer = DetachedRedeemer(
                          redeemerDatum,
                          RedeemerPurpose.ForSpend(utxo.input)
                        )
                        _ <- modify0(ctx =>
                            ctx.focus(_.redeemers).modify(r => appendDistinct(detachedRedeemer, r))
                        )
                        _ <- useDatumWitnessForUtxo(utxo, mbDatumWitness)
                    } yield ()
            }
        } yield ()
    }

    // TODO: this has signifcant overlap with `assertScriptHashMatchesCredentialWitness`.
    //     We can factor it out by extracting a script from both witness types.
    //     One obstacle is that the script witness uses type parameter that is erased.
    //     So I was able to factor only the last part
    def assertScriptHashMatchesOutputWitness(
        scriptHash: ScriptHash,
        outputWitness: OutputWitness
    ): BuilderM[Unit] =
        for {
            ctx <- StateT.get[[X] =>> Either[TxBuildError, X], Context]

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
                      _,
                      _
                    ) =>
                    pure0(Right(script))
                case otherwise =>
                    assertReferencedScriptIsProvided(scriptHash, outputWitness.scriptWitness)
            }
            _ <- assertScriptHashMatchesScript(scriptHash, script)
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

    // -------------------------------------------------------------------------
    // WithdrawRewards step
    // -------------------------------------------------------------------------

    def useWithdrawRewardsWitness(
        stakeCredential: StakeCredential,
        amount: Coin,
        witness: Option[CredentialWitness]
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

    // -------------------------------------------------------------------------
    // SubmitVotingProcedure step
    // -------------------------------------------------------------------------

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

    // -------------------------------------------------------------------------
    // Common functions for CredentialWitness
    // -------------------------------------------------------------------------

    def useCredentialWitness(
        credAction: CredentialAction,
        cred: Credential,
        mbWitness: Option[CredentialWitness]
    ): BuilderM[Unit] =
        for {
            _ <- mbWitness match {
                // Pubkey credential witness: add to expected signers
                case None =>
                    val pkhw: ExpectedWitnessType[CredentialWitness] =
                        ExpectedWitnessType.PubKeyHashWitness()
                    for {
                        _ <- assertCredentialType(
                          credAction,
                          pkhw,
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
                                      pkhw,
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
                                    .modify(redeemers => appendDistinct(redeemer, redeemers))
                            )
                        }
                    } yield ()
            }
        } yield ()

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
                          .modify(s => appendDistinct(ns, s.toList).toSet)
                    )
                // Spend case 4
                case ScriptWitness.ScriptReference(_, _) =>
                    // TODO: anything?
                    // processConstraint(referenceStep)
                    pure0(())
                // Spend case 3
                case ScriptWitness.ScriptReferenceSpent(_, _) =>
                    // TODO: anything?
                    // processConstraint(spendStep)
                    pure0(())
            }
        } yield ()

    def usePlutusScriptWitness(
        scriptWitness: ScriptWitness[PlutusScript]
    ): BuilderM[Unit] =
        for {
            // Spend cases 5, 6, and 7
            // Add script's additional signers to txBody.requiredSigners
            _ <- modify0(
              (Focus[Context](_.transaction) >>> txBodyL)
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
                case ScriptWitness.ScriptReference(_, _) =>
                    // TODO: shall we do something?
                    // processConstraint(referenceStep)
                    pure0(())

                case ScriptWitness.ScriptReferenceSpent(_, _) =>
                    // TODO: shall we do something?
                    // processConstraint(spendStep)
                    pure0(())
            }
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

    // TODO:  this has signifcant overlap with `assertScriptHashMatchesOutputWitness`.
    //     We can factor it out by extracting a script from both witness types.
    //     One obstacle is that the script witness uses type parameter that is erased.
    //     So I was able to factor only the last part
    def assertScriptHashMatchesCredentialWitness(
        scriptHash: ScriptHash,
        credentialWitness: CredentialWitness
    ): BuilderM[Unit] =
        for {
            ctx <- StateT.get[[X] =>> Either[TxBuildError, X], Context]

            // Basically 6 cases: plutus or native, times direct, spent, or referenced
            script: Either[Script.Native, PlutusScript] <- credentialWitness match {

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
                    assertReferencedScriptIsProvided(scriptHash, credentialWitness.scriptWitness)
            }
            _ <- assertScriptHashMatchesScript(scriptHash, script)
        } yield ()

    def assertReferencedScriptIsProvided(
        scriptHash: ScriptHash,
        scriptWitness: ScriptWitness[_]
    ): BuilderM[Either[Script.Native, PlutusScript]] =
        for {
            ctx <- StateT.get[[X] =>> Either[TxBuildError, X], Context]

            mbCip33output <- pure0(scriptWitness match
                case ScriptWitness.ScriptValue(_, _) => None
                case ScriptWitness.ScriptReference(utxoId, _) =>
                    if ctx.transaction.body.value.referenceInputs.toSeq.contains(utxoId)
                    then ctx.getUtxo.get(utxoId)
                    else None
                case ScriptWitness.ScriptReferenceSpent(utxoId, _) =>
                    if ctx.transaction.body.value.inputs.toSeq.contains(utxoId)
                    then ctx.getUtxo.get(utxoId)
                    else None
            )

            script <- mbCip33output match {
                case Some(output) =>
                    output match {
                        case b: Babbage =>
                            b.scriptRef match {
                                // Needed a script ref, but couldn't get one
                                case None =>
                                    liftF0(
                                      Left(
                                        TxBuildError.ReferenceScriptNotProvided(scriptHash)
                                      )
                                    )
                                case Some(s) =>
                                    s.script match {
                                        case ps: PlutusScript  => pure0(Right(ps))
                                        case ns: Script.Native => pure0(Left(ns))
                                    }
                            }
                        // Non-babbage utxo can't have a CIP-33 scripts
                        case _ =>
                            liftF0(Left(TxBuildError.ReferenceScriptNotProvided(scriptHash)))
                    }
                case None =>
                    liftF0(Left(TxBuildError.ReferenceScriptNotProvided(scriptHash)))
            }
        } yield script

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
        } yield res

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

    case class ReferenceScriptNotProvided(scriptHash: ScriptHash) extends TxBuildError {
        override def explain: String =
            s"No ref/spent output is found for script matching $scriptHash."
    }

    case class ByronAddressesNotSupported(address: Address) extends TxBuildError {
        override def explain: String =
            s"Byron addresses are not supported: $address."
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
