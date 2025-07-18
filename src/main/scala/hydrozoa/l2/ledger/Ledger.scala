package hydrozoa.l2.ledger

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{
    Piper,
    decodeBech32AddressL2,
    decodeHex,
    encodeHex,
    plutusAddressAsL2,
    txHash
}
import hydrozoa.l1.multisig.state.depositDatum
import hydrozoa.l1.rulebased.onchain.scalar.Scalar as ScalusScalar
import hydrozoa.l2.commitment.infG2Point
import hydrozoa.l2.ledger
import hydrozoa.l2.ledger.*
import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.Data.toData
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, ByteString}
import scalus.cardano.address.Address.Shelley
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context, STS, State, UtxoEnv}
import scalus.cardano.ledger.{
    AddrKeyHash,
    AuxiliaryData,
    AuxiliaryDataHash,
    CertState,
    Coin,
    DatumOption,
    KeepRaw,
    Redeemers,
    Script,
    ScriptDataHash,
    ScriptRef,
    Sized,
    SlotConfig,
    SlotNo,
    Transaction,
    TransactionBody,
    TransactionException,
    TransactionInput,
    TransactionOutput,
    TransactionWitnessSet,
    UTxO,
    VKeyWitness,
    Value
}
import scalus.ledger.api.v2.OutputDatum.NoOutputDatum
import scalus.ledger.api.v3
import scalus.ledger.babbage.ProtocolParams
import scalus.prelude.List.{Cons, asScala, asScalus}
import scalus.prelude.Option.{None as SNone, Some as SSome}
import scalus.prelude.crypto.bls12_381.G1
import scalus.prelude.{AssocMap, List as SList, Option as SOption, given}
import supranational.blst.{P1, P2, Scalar}

import java.math.BigInteger
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/*
We want to create a STS that is valid for the L2 ledger. The L2 is "nearly isomorphic", in the sense that
it is essentially a "subtype" of the L1 ledger. Where possible, we want to re-use L1 types -- our serialized types
should be cardano-compatible.

There are three approaches (not necessarily mutually exclusive) that we can use: "best effort", "validating rules",
or "l2 parsing".

As a concrete example, addresses in the L2 ledger should be shelley addresses and should not have delegation parts.

- The "best-effort" approach would accept transactions with Shelley addresses containing delegation parts, but ignores
  them.
  - This has the benefit of being simple, but seems risky. We would have to ensure that if we are given
    two address, a1 and a2, that only differ in that one has a delegation part and one does not, that all possible
    applications of the L2 ledger rules on a STS involving a1 are identical to a2. I think its trivial to construct a
    counter-example: a plutus script that fails if a delegation part is set. It therefore seems better if we explicitly
    disallow things we don't want.
  - This is approach is rejected in section 3.3 of the spec.

- The "validating" approach would add one or more ledger rules that tries to enumerate all places where an L1
  address could appear in the input /or output/ of the L2 STS.
  - The first minor issue with this is that we would need to run validation on the result of the ledger rules. This
    is because the output of one state transition is the input of the next: if a state transition _produced_
    an address with a delegation part set, then the next state transition would fail to validate.
    - But in practice, the output of the ledger rules are either Unit (for validation rules) or a UTxO Set + CertState.
      These types are simple enough that we could validate them easily.
  - The second issue is that it will be more error-prone to validate. I'll conjecture that the _only_ definition of
    "full and correct validation" gives a result that is identical to round-trippping a parse, but the completeness of
    the parse can be type-checked.
    - For example, we'd have to validate ALL of the places in the L1 `Transaction` type where an L1 Address can appear.
      We can do this by careful examination, but that careful examination is essentially manual type-checking.
  - This is the approach stipulated in the spec

- The "parsing" approach would first parse an L1 address into an "AddressL2" type that is incapable of representing
  non-shelley addresses, or shelley addresses with delegation parts. When using ledger rules written for L1 (we can
  call these "foreign" rules), we would lift and lower the arguments of the rules, and only consider the rules
  successfully applied if the results parsed. "Native" rules could be written directly for our L2 sub-types.
  - This has the drawback that we'd be doing a lot of lifting and lowering, since we'll want to use L1 types on-the-wire,
    with helper utilities, and with the majority (if not all) of the ledger rules.
  - It also means constructing and maintaining a number of types.
  - It'd be semantically equivalent to the validating approach if correctly implemented; but probably less error-prone
    to develop, easier to test, and possibly heavier at runtime.

If we look the `CardanoMutator` from scalus (applying all of the ledger validation and transit rules), our approach 
would look like this in comparison:

```
object HydrozoaMutator extends STSL2.Mutator {
    override final type Error = TransactionException

    override def transit(context: ContextL2, state: StateL2, event: EventL2): Result = {
        val contextL1 : Context = ContextL2.toL1(context)
        val stateL1 : State = StateL2.toL1(state)
        val eventL1 : Event = EventL2.toL1(event)

        // N.B.: this is basically the Either monad
        for
            ////////////////////////
            // "Foreign" rules

            // Validations don't mutate anything, so we just use the injected l1 equivalents 
            _ <- EmptyInputsValidator.validate(contextL1, stateL1, eventL1)
            _ <- InputsAndReferenceInputsDisjointValidator.validate(contextL1, stateL1, eventL1)
           (...) // More validations
            _ <- OutputsHaveTooBigValueStorageSizeValidator.validate(contextL1, stateL1, eventL1)
            _ <- OutsideValidityIntervalValidator.validate(contextL1, stateL1, eventL1)
            _ <- OutsideForecastValidator.validate(contextL1, stateL1, eventL1)
            
            // Now we get to transits: the output of one transit becomes the input of the next
            state <- RemoveInputsFromUtxoMutator.transit(contextL1, stateL1, eventL1) 
            (...)
            state <- AddOutputsToUtxoMutator.transit(context, state, event)
            
            ///////////////////////////////
            // Now we parse back down to the L2 state and apply "native" rules
            stateL2 <- StateL2.fromL1(stateL2)
             
            _ <- L2BalanceValidator.validate(context, stateL2, event)
            stateL2 <- SomeOtherCustomLedgerRule.transit(context, stateL2, event)

            // And ensure that lifting again round-trips as a final check on our parsing
            _ <- if StateL2.toL1(stateL2) != state then throw TransactionException(...) else ()
        yield stateL2
    }
}
```

------------------------------------------------------------------------------------------------------------------------

(This is my original take on the parsing approach. The `validateL1` functions described and implemented below are
essentially what would become individual `STS.Validator` ledger rules if we went for the validating approach instead)

Objective: Create a Layer 2 state transition system (STSL2) modeled after the scalus STS, and provide functions to
translate between them. This will allow us to re-use the majority of the scalus ledger code, while adding a single
 additional ledger rule that ensures the L1 STS can round-trip to and from the L2 STS.

Each declared type should have an injection (in the mathematical sense) to the L1 types as a `toL1` method,
and a canonical projection (again, in the mathematical sense) from the L1 types.
The projections should be considered as "parsing", and should include "checked" and "unchecked" versions, such that
the following diagram commutes (morally; the fromRight is partial):

              validateL1
     L1 type ------------> Either[err|L1 type]
       ^    \                |
       |     \ fromL1        |  map(unsafeFromL1(_))
       |      \              |
       |       \             v
       |         -----------> Either[err|L2 type]
       |                          |
       |--------------------------|
              toL1 . fromRight



i.e., the projection and injection should round-trip, and `checked == validate(_).map(unchecked(_))`. The validation
must ensure that the extraneous fields of the L1 types do not contain meaningful data; for example, we don't support
byron addresses or delegation parts in shelley addresses, so the projection from an L1 address must return Left if
either is condition isn't satisfied. "Meaningful", in this sense, means that the round-tripping of the data
doesn't affect the observable behavior of transitions -- loosely something like:

                                 transitionL2
(ContextL2, StateL2, EventL2)  --------------> (ContextL2, StateL2, EventL2)
       |                                                    |
       |                                                    |
       | trimap(_.toL1)                                     | tripmap(_.toL1)
       |                                                    |
       |                                                    |
       v                                                    v
(Context, State, Event) -----------------------> (Context, State, Event)
                            transitionL2.toL1   

or maybe the other way around(?):

                              transition
(Context, State, Event)  ---------------------> (Context, State, Event)
       |                                                    |
       |                                                    |
       | trimap(uncheckedProj(_))                           | trimap(uncheckedProj(_)
       |                                                    |
       |                                                    |
       v                                                    v
(ContextL2, StateL2, EventL2) -----------------------> (ContextL2, StateL2, EventL2)
                              transition.toL2


Basically, we want to ensure that our ledger can directly use the corresponding L1 state transitions in a way
that doesn't introduce any unintended side-effects; and that the translation from L1 to L2 and back encodes our additional
ledger rules.
 */

// This trait applies to companion objects for L2 types. It defines the corresponding L1 type as an associated type
// and provides the injection/projection functions.

trait L1Codec[L2] {
    type L1
    def toL1(l2: L2): L1
    type ProjectionError
    def unsafeFromL1(l1: L1): L2

    // TODO: the validation function currently terminates early with a Left. We should probably accumulate validation
    // errors.
    def validateL1(l1: L1): Either[ProjectionError, L1]
    final def fromL1(l1: L1): Either[ProjectionError, L2] = validateL1(l1).map(this.unsafeFromL1)
}

// This differs from the L1 Context as follows:
// - No fees
case class ContextL2(
    env: UtxoEnvL2,
    slotConfig: SlotConfig = SlotConfig.Mainnet
)

object ContextL2 extends L1Codec {
    override type L1 = Context
    override def toL1(l2: ContextL2): Context =
        Context(fee = Coin(0L), env = UtxoEnvL2.toL1(l2.env), slotConfig = l2.slotConfig)

    override type ProjectionError = String
    override def unsafeFromL1(c: Context): ContextL2 = ContextL2(
      env = UtxoEnvL2.unsafeFromL1(c.env),
      slotConfig = c.slotConfig
    )

    override def validate(l1: Context): Either[String, Context] = l1 match {
        case ctx if (l1.fee.value != 0L) => Left("Fee is not equal to 0")
        // FIXME: performs the validation twice -- whats the proper move here?
        case ctx if UtxoEnvL2.validateL1(l1.env).isLeft =>
            Left(s"UtxoEnv invalid: ${UtxoEnvL2.validateL1(l1.env)}")
        case ctx => Right(ctx)
    }
}

// This differs from the L1 STS as follows:
// - No certState
// - The UTxO set is specifically XXX (babbage?) UTxOs
case class StateL2(
    utxo: UTxOL2 = Map.empty
)
object StateL2 extends L1Codec {
    override type L1 = State
    // Generates the corresponding L1 state with an empty CertState
    override def toL1(l2: StateL2): State = {
        State(utxo = l2.utxo.map((k, v) => (k, v.toL1)), certState = CertState.empty)

    }
    override def unsafeFromL1(l1: State): StateL2 = {
        StateL2(utxo = l1.utxo.map((k, v) => (k, TransactionOutputL2.unsafeFromL1(v))))
    }

    override type ProjectionError = String

    // TODO: Thread through the underlying errors
    override def validateL1(l1: State): Either[String, State] =
        if l1.utxo.forall((ti, to) => TransactionOutputL2.validateL1(to).isRight)
        then Right(l1)
        else Left("UTxO map contains transaction outputs that cannot be projected to L2")
}

// This differs from an L1 address as follows:
// - Only Shelley-style addresses are supported
// - Delegation is not supported.
case class AddressL2(
    network: Network,
    payment: ShelleyPaymentPart
)
object AddressL2 extends L1Codec {
    override type L1 = Address
    // Creates the corresponding L1 shelley address with a null delegation
    def toL1(l2: AddressL2): Address = Shelley(
      ShelleyAddress(network = l2.network, payment = l2.payment, delegation = Null)
    )

    override def unsafeFromL1(l1: Address): AddressL2 = {
        val shelley = l1.asInstanceOf[ShelleyAddress]
        AddressL2(network = shelley.network, payment = shelley.payment)
    }

    override type ProjectionError = String

    override def validateL1(l1: Address): Either[String, Address] = l1 match {
        case shelley: ShelleyAddress =>
            if shelley.delegation != Null then Left("Address has a delegation, but shouldn't")
            else Right(shelley)
        case _ => Left("Address is not shelley")
    }
}

// Differs from the L1 Transaction Output in that:
// - Only babbage-style outputs are allowed
// - L2 transaction outputs can only contian Ada
// - Datums, if present, must be inline
// - Only native scripts or v3 plutus scripts allowed in the script ref
case class TransactionOutputL2(
    address: AddressL2,
    value: Coin,
    datumOption: Option[Inline] = None,
    scriptRef: Option[ScriptRefL2] = None
)
object TransactionOutputL2 extends L1Codec {
    override type L1 = TransactionOutput
    override def toL1(l2: TransactionOutputL2): TransactionOutput = Babbage(
      address = AddressL2.toL1(l2.address),
      value = Value(l2.value),
      datumOption = l2.datumOption,
      scriptRef = l2.scriptRef.map(_.toL1)
    )
    override def unsafeFromL1(l1: TransactionOutput): TransactionOutputL2 = {
        // Unchecked, so this will throw
        val babbage = l1.asInstanceOf[Babbage]

        TransactionOutputL2(
          address = AddressL2.unsafeFromL1(babbage.address),
          value = babbage.value.coin,
          datumOption = babbage.datumOption.map(_.asInstanceOf[Inline]),
          scriptRef = l1.scriptRef.map(ScriptRefL2.unsafeFromL1(_))
        )
    }

    override type ProjectionError = String

    // Datum must either be inline or absent
    // TODO: Maybe this should be its own DatumOptionL2 class?
    private def isValidDatum(dat: Option[DatumOption]): Boolean = dat match {
        case Some(i: Inline) => true
        case None            => true
        case _               => false
    }

    override def validateL1(l1: TransactionOutput): Either[ProjectionError, TransactionOutput] =
        l1 match {
            case to if !(l1.isInstanceOf[Babbage]) =>
                Left("Transaction output is not a Babbage output")
            case to if !isValidDatum((to.asInstanceOf[Babbage].datumOption)) =>
                Left(
                  "Transaction output has a datum, but it is not inline"
                )
            case to
                if !(to
                    .asInstanceOf[Babbage]
                    .scriptRef
                    .forall(sr => ScriptRefL2.validateL1(sr).isRight)) =>
                Left("Transaction output has invalid script ref")

        }
}

// Differs from the L1 script ref in that:
// - Only native or v3 plutus scripts are allowed
case class ScriptRefL2(script: Script.Native | Script.PlutusV3)
object ScriptRefL2 extends L1Codec {
    type L1 = ScriptRef
    // A simple injection
    def toL1(l2: ScriptRefL2): ScriptRef = ScriptRef(l2.script)

    override def unsafeFromL1(l1: ScriptRef): ScriptRefL2 = l1.script match {
        case ns: Script.Native    => ScriptRefL2(ns)
        case pv3: Script.PlutusV3 => ScriptRefL2(pv3)
    }

    override type ProjectionError = String

    override def validateL1(l1: ScriptRef): Either[String, ScriptRef] = l1.script match {
        case ns: Script.Native    => Right(l1)
        case pv3: Script.PlutusV3 => Right(l1)
        case _ => Left("Script ref contains a script that is not Native or PlutusV2")
    }
}

// A UTxO map specifically for babbage transaction
// TODO: This should probably be a case class, but is implmeneted as  type alias upstrem
type UTxOL2 = Map[TransactionInput, TransactionOutputL2]

// Differs from L1 Utxo as follows:
// - No cert state
case class UtxoEnvL2(slot: SlotNo, params: ProtocolParams)
object UtxoEnvL2 extends L1Codec {
    override type L1 = UtxoEnv
    // Creates the corresponding L1 UTxO env with an empty cert state
    override def toL1(l2: UtxoEnvL2): UtxoEnv =
        UtxoEnv(slot = l2.slot, params = l2.params, certState = CertState.empty)

    override def unsafeFromL1(l1: UtxoEnv): UtxoEnvL2 = UtxoEnvL2(slot = l1.slot, params = l1.params)

    override type ProjectionError = String
    override def validateL1(l1: UtxoEnv): Either[ledger.UtxoEnvL2.ProjectionError, UtxoEnv] = {
        if l1.certState != CertState.empty then Left("CertState not empty") else Right(l1)
    }

}

// Differs from L1 transaction in the type of the body and witness set
// and that we don't do a KeepRaw on the L2 TxBody (do we need to? Are we ever sending it in CBOR?)
case class TransactionL2(
    body: TransactionBodyL2,
    witnessSet: TransactionWitnessSetL2,
    isValid: Boolean,
    auxiliaryData: Option[AuxiliaryData] = None
)
object TransactionL2 extends L1Codec {
    override type L1 = Transaction

    override def toL1(l2: TransactionL2): Transaction = Transaction(
      body = KeepRaw(TransactionBodyL2.toL1(l2.body)),
      witnessSet = TransactionWitnessSetL2.toL1(l2.witnessSet),
      isValid = l2.isValid,
      auxiliaryData = l2.auxiliaryData
    )

    override def unsafeFromL1(l1: Transaction): TransactionL2 = TransactionL2(
      body = TransactionBodyL2.unsafeFromL1(l1.body.value),
      witnessSet = TransactionWitnessSetL2.unsafeFromL1(l1.witnessSet),
      isValid = l1.isValid,
      auxiliaryData = l1.auxiliaryData
    )

    override type ProjectionError = String

    override def validateL1(l1: Transaction): Either[String, Transaction] = ???

}

// Differs from the L1 Tx Body as follows:
// - The following omissions from the current spec (commit e2ef186, 2025-07-08 version):
//   - No certificates, withdrawals, mints, voting_procedures, proposal_procedures, current_treasury_value,
//     or treasury_donation
// - Omitting fields related to fees and collateral (see private discussion at
//      https://discord.com/channels/@me/1387084765173121175/1389956276208926852;
//    rationale being that someone can quit consensus if scripts keep failing)

case class TransactionBodyL2(
    /** Transaction inputs to spend */
    inputs: Set[TransactionInput],

    /** Transaction outputs to create */
    outputs: IndexedSeq[TransactionOutputL2],

    /** Time-to-live (TTL) - transaction is invalid after this slot */
    ttl: Option[Long] = None,

    /** Auxiliary data hash */
    auxiliaryDataHash: Option[AuxiliaryDataHash] = None,

    /** Transaction validity start (transaction is invalid before this slot) */
    validityStartSlot: Option[Long] = None,

    /** Script data hash */
    scriptDataHash: Option[ScriptDataHash] = None,

    /** Required signers */
    requiredSigners: Set[AddrKeyHash] = Set.empty,

    /** Network ID */
    networkId: Option[Int] = None,

    /** Reference inputs */
    referenceInputs: Set[TransactionInput] = Set.empty
):

    /** Validate network ID if present */
    require(
      networkId.forall(id => id == 0 || id == 1),
      "Network ID must be 0 (testnet) or 1 (mainnet)"
    )
object TransactionBodyL2 extends L1Codec {
    override type L1 = TransactionBody

    // Sets fee to 0 and uses the defaults for the L1 transaction body
    override def toL1(l2: TransactionBodyL2): TransactionBody = TransactionBody(
      inputs = l2.inputs,
      outputs = l2.outputs.map(s => Sized(TransactionOutputL2.toL1(s.value))),
      fee = Coin(0L),
      ttl = l2.ttl,
      auxiliaryDataHash = l2.auxiliaryDataHash,
      validityStartSlot = l2.validityStartSlot,
      scriptDataHash = l2.scriptDataHash,
      requiredSigners = l2.requiredSigners,
      networkId = l2.networkId,
      referenceInputs = l2.referenceInputs
    )

    override def unsafeFromL1(l1: L1): TransactionBodyL2 = TransactionBodyL2(
      inputs = l1.inputs,
      outputs = l1.outputs.map(sto => TransactionOutputL2.unsafeFromL1(sto.value)),
      ttl = l1.ttl,
      auxiliaryDataHash = l1.auxiliaryDataHash,
      validityStartSlot = l1.validityStartSlot,
      scriptDataHash = l1.scriptDataHash,
      requiredSigners = l1.requiredSigners,
      networkId = l1.networkId,
      referenceInputs = l1.referenceInputs
    )

}

// Differs from the L1 witness set by excluding:
// - data witnesses (all datums are inline)
// - Plutus v1 and v2 scripts
// - bootstrap witnesses
case class TransactionWitnessSetL2(
    /** VKey witnesses */
    vkeyWitnesses: Set[VKeyWitness] = Set.empty,

    /** Native scripts */
    nativeScripts: Set[Script.Native] = Set.empty,

    /** Redeemers */
    redeemers: Option[KeepRaw[Redeemers]] = None,

    /** Plutus V3 scripts */
    plutusV3Scripts: Set[Script.PlutusV3] = Set.empty
)
object TransactionWitnessSetL2 extends L1Codec {
    override type L1 = TransactionWitnessSet

    // uses the defaults for the L1 witness set
    override def toL1: TransactionWitnessSet = TransactionWitnessSet(
      vkeyWitnesses = this.vkeyWitnesses,
      nativeScripts = this.nativeScripts,
      redeemers = this.redeemers,
      plutusV3Scripts = this.plutusV3Scripts
    )
}

sealed trait STSL2 {
    final type ContextL2 = hydrozoa.l2.ledger.ContextL2
    final type StateL2 = hydrozoa.l2.ledger.StateL2
    final type EventL2 = TransactionL2
    type Value
    type Error <: TransactionException
    final type Result = Either[Error, Value]

    def apply(context: ContextL2, state: StateL2, event: EventL2): Result

    protected final def failure(error: Error): Result = Left(error)
}


object STSL2 {
    trait Validator extends STSL2 {
        override final type Value = Unit

        def validate(context: ContextL2, state: StateL2, event: EventL2): Result

        override final def apply(context: ContextL2, state: StateL2, event: EventL2): Result =
            validate(context, state, event)

        protected final val success: Result = Right(())
    }

    trait Mutator extends STSL2 {
        override final type Value = StateL2

        def transit(context: ContextL2, state: StateL2, event: EventL2): Result

        override final def apply(context: ContextL2, state: StateL2, event: EventL2): Result =
            transit(context, state, event)

        protected final def success(state: StateL2): Result = Right(state)
    }
}


/*

// TODO: this module uses the Bloxbean dep directly

/** --------------------------------------------------------------------------------------------- L2
  * Genesis
  * ---------------------------------------------------------------------------------------------
  */

// TODO: can be simplified, since inputs and outputs represent the same things
case class L2Genesis(
    depositUtxos: List[(UtxoId[L1], Output[L1])],
    outputs: List[OutputL2]
) derives CanEqual:
    def volume(): Long = outputs.map(_.coins).sum.toLong

object L2Genesis:
    def apply(ds: List[(UtxoId[L1], Output[L1])]): L2Genesis =
        L2Genesis(
          ds,
          ds.map((_, o) =>
              val datum = depositDatum(o) match
                  case Some(datum) => datum
                  case None =>
                      throw RuntimeException("deposit UTxO doesn't contain a proper datum")
              Output.apply(datum.address |> plutusAddressAsL2, o.coins, o.tokens)
          ).toList
        )

private def mkCardanoTxForL2Genesis(genesis: L2Genesis): TxL2 =

    val depositInputs = genesis.depositUtxos.map { (utxoId, _) =>
        TransactionInput.builder
            .transactionId(utxoId.txId.hash)
            .index(utxoId.outputIx.ix)
            .build
    }

    val virtualOutputs = genesis.outputs.map { output =>
        TransactionOutput.builder
            .address(output.address.bech32)
            .value(Value.builder.coin(output.coins.bigInteger).build)
            .build
    }

    val body = TransactionBody.builder
        .inputs(depositInputs.toList.asJava)
        .outputs(virtualOutputs.asJava)
        .build

    val tx = Transaction.builder.era(Era.Conway).body(body).build
    TxL2(tx.serialize)

def calculateGenesisHash(genesis: L2Genesis): TxId =
    val cardanoTx = mkCardanoTxForL2Genesis(genesis)
    txHash(cardanoTx)

def mkGenesisOutputs(genesis: L2Genesis, genesisHash: TxId): UtxoSetL2 =
    val utxoDiff = genesis.outputs.zipWithIndex
        .map(output =>
            val txIn = UtxoIdL2(genesisHash, TxIx(output._2.toChar))
            val txOut = Output[L2](output._1.address.asL2, output._1.coins, output._1.tokens)
            (txIn, txOut)
        )
    UtxoSet.apply(utxoDiff.toMap)

/** --------------------------------------------------------------------------------------------- L2
  * Transaction
  * ---------------------------------------------------------------------------------------------
  */

case class L2Transaction(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[UtxoIdL2],
    outputs: List[OutputNoTokens[L2]]
):
    def volume(): Long = outputs.map(_.coins).sum.toLong

/** @param l2Tx
  * @return
  */
private def mkCardanoTxForL2Transaction(l2Tx: L2Transaction): TxL2 =

    val virtualInputs = l2Tx.inputs.map { input =>
        TransactionInput.builder
            .transactionId(input._1.hash)
            .index(input._2.ix.intValue)
            .build
    }

    val virtualOutputs = l2Tx.outputs.map { output =>
        TransactionOutput.builder
            .address(output.address.bech32)
            .value(Value.builder.coin(output.coins.bigInteger).build)
            .build
    }

    val body = TransactionBody.builder
        .inputs(virtualInputs.asJava)
        .outputs(virtualOutputs.asJava)
        .build

    val tx = Transaction.builder.era(Era.Conway).body(body).build
    TxL2(tx.serialize)

// TODO: this arguably can be considered as a ledger's function
def calculateTxHash(tx: L2Transaction): TxId =
    val cardanoTx = mkCardanoTxForL2Transaction(tx)
    val txId = txHash(cardanoTx)
    txId

/** --------------------------------------------------------------------------------------------- L2
  * Withdrawal
  * ---------------------------------------------------------------------------------------------
  */

case class L2Withdrawal(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[UtxoIdL2]
)

/** @param withdrawal
  * @return
  */
private def mkCardanoTxForL2Withdrawal(withdrawal: L2Withdrawal): TxL2 =

    val virtualInputs = withdrawal.inputs.map { input =>
        TransactionInput.builder
            .transactionId(input._1.hash)
            .index(input._2.ix.intValue)
            .build
    }

    val body = TransactionBody.builder
        .inputs(virtualInputs.asJava)
        .build

    val tx = Transaction.builder.era(Era.Conway).body(body).build
    Tx[L2](tx.serialize)

// TODO: this arguably can be considered as a ledger's function
def calculateWithdrawalHash(withdrawal: L2Withdrawal): TxId =
    val cardanoTx = mkCardanoTxForL2Withdrawal(withdrawal)
    val txId = txHash(cardanoTx)
    txId

/** Now just a bunch of functions
  */
object SimpleL2Ledger:

    // UTxO conversions between Hydrozoa types and Ledger types
    def liftOutputRef(UtxoIdL2: UtxoIdL2): v3.TxOutRef =
        val sTxId = v3.TxId(ByteString.fromHex(UtxoIdL2.txId.hash))
        val sTxIx = BigInt(UtxoIdL2.outputIx.ix)
        v3.TxOutRef(sTxId, sTxIx)

    def unliftOutputRef(outputRef: v3.TxOutRef): UtxoIdL2 =
        UtxoIdL2(TxId(outputRef.id.hash.toHex), TxIx(outputRef.idx.toChar))

    def liftOutput(output: OutputL2): v3.TxOut =
        val address = decodeBech32AddressL2(output.address)
        val value = v3.Value.lovelace(output.coins)
        v3.TxOut(address = address, value = value)

    def unliftOutput(output: v3.TxOut): Output[L2] =
        val SSome(e) = AssocMap.get(output.value)(ByteString.empty)
        val SSome(coins) = AssocMap.get(e)(ByteString.empty)
        Output[L2](plutusAddressAsL2(output.address).asL2, coins, emptyTokens)

    def liftUtxoSet(utxoSet: Map[UtxoIdL2, OutputL2]): Map[v3.TxOutRef, v3.TxOut] =
        utxoSet.map(_.bimap(liftOutputRef, liftOutput))

    // This is not private, since it's used by MBT suite
    def unliftUtxoSet(utxosSetOpaque: Map[v3.TxOutRef, v3.TxOut]): Map[UtxoIdL2, OutputL2] =
        utxosSetOpaque.map(_.bimap(unliftOutputRef, unliftOutput))

    // TODO: this will be gone as soon as we get a setup ceremony up and running.
    val tau = Scalar(BigInteger("42"))

    def mkDummySetupG2(n: Int): SList[P2] = {
        val setup =
            (1 to n + 1).map(i =>
                P2.generator().dup().mult(tau.dup().mul(Scalar(BigInteger(i.toString))))
            )
        SList.Cons(P2.generator(), setup.toList.asScalus)
    }

    def mkDummySetupG1(n: Int): SList[P1] = {
        val setup =
            (1 to n + 1).map(i =>
                P1.generator().dup().mult(tau.dup().mul(Scalar(BigInteger(i.toString))))
            )
        SList.Cons(P1.generator(), setup.toList.asScalus)
    }

    // The implementation
    class SimpleL2LedgerClass:

        private val log = Logger(getClass)

        type LedgerTransaction = L2Transaction | L2Withdrawal

        type SubmissionError = String

        private type UtxosSetOpaqueMutable = mutable.Map[v3.TxOutRef, v3.TxOut]
        private val activeState: UtxosSetOpaqueMutable = mutable.Map.empty

        def isEmpty: Boolean = activeState.isEmpty

        def getUtxosActive: Map[v3.TxOutRef, v3.TxOut] = activeState.clone.toMap

        def getUtxosActiveCommitment: IArray[Byte] = {
            val elemsRaw = activeState.clone.toList
                .map(e => blake2b_224(serialiseData(e.toData)).toHex)
                .asScalus
            log.info(s"utxos active hashes raw: $elemsRaw")

            val elems = activeState.clone.toList
                .map(e => Scalar().from_bendian(blake2b_224(serialiseData(e.toData)).bytes))
                .asScalus
            log.info(s"utxos active hashes: ${elems.map(e => BigInt.apply(e.to_bendian()))}")

            val setup = mkDummySetupG2(elems.length.toInt)

            val setupBS = setup.map(e => BLS12_381_G2_Element.apply(e).toCompressedByteString)
            setupBS.foreach(println)

            val commitmentPoint = getG2Commitment(setup, elems)
            val commitment = IArray.unsafeFromArray(commitmentPoint.compress())
            log.info(s"Commitment: ${(encodeHex(commitment))}")
            commitment
        }

        def getState: UtxoSetL2 =
            UtxoSet[L2](unliftUtxoSet(activeState.clone().toMap))

        def replaceUtxosActive(activeState: Map[v3.TxOutRef, v3.TxOut]): Unit =
            this.activeState.clear()
            this.activeState.addAll(activeState)

        def addGenesisUtxos(utxoSet: UtxoSetL2): Unit =
            liftUtxoSet(utxoSet.utxoMap) |> this.activeState.addAll

        def getOutput(utxoId: UtxoIdL2): OutputL2 =
            activeState(utxoId |> liftOutputRef) |> unliftOutput

        def flushAndGetState: UtxoSetL2 =
            val ret = activeState.clone()
            activeState.clear()
            UtxoSet[L2](ret.toMap.map((k, v) => (unliftOutputRef(k), unliftOutput(v))))

        def submit(
            event: LedgerTransaction
        ): Either[(TxId, SubmissionError), (TxId, (UtxoSetL2, UtxoSetL2))] =
            event match
                case tx: L2Transaction        => submitTransaction(tx)
                case withdrawal: L2Withdrawal => submitWithdrawal(withdrawal)

        private val emptyUtxoSet = UtxoSet[L2](Map.empty[UtxoIdL2, Output[L2]])

        private def submitTransaction(tx: L2Transaction) =

            def checkSumInvariant(
                inputs: List[v3.TxOut],
                outputs: List[v3.TxOut]
            ): Boolean =
                val before: v3.Value =
                    inputs.map(_.value).fold(v3.Value.zero)(v3.Value.plus)
                val after: v3.Value =
                    outputs.map(_.value).fold(v3.Value.zero)(v3.Value.plus)
                before == after

            val txId = calculateTxHash(tx)

            resolveInputs(tx.inputs) match
                case Left(extraneous) =>
                    Left(txId, s"Extraneous utxos in transaction $txId: $extraneous")
                case Right(oldUtxos) =>
                    // Outputs
                    val newUtxos = tx.outputs.zipWithIndex.map(output =>
                        val txIn = liftOutputRef(UtxoIdL2(txId, TxIx(output._2.toChar)))
                        val txOut = liftOutput(Output.apply(output._1))
                        (txIn, txOut)
                    )

                    val (inputRefs, inputs, _) = oldUtxos.unzip3

                    if !checkSumInvariant(inputs, newUtxos.map(_._2)) then
                        Left(txId, s"Sum invariant is not hold for tx $txId")
                    else
                        // FIXME: atomicity
                        inputRefs.foreach(activeState.remove)
                        newUtxos.foreach(activeState.put.tupled)

                        Right((txId, (emptyUtxoSet, emptyUtxoSet)))

        private def submitWithdrawal(withdrawal: L2Withdrawal) =
            val txId = calculateWithdrawalHash(withdrawal)

            resolveInputs(withdrawal.inputs) match
                case Left(extraneous) => Left(txId, s"Extraneous utxos in withdrawal: $extraneous")
                case Right(resolved) =>
                    val (withdrawnRefs, withdrawnOutputs, (withdrawnPub)) = resolved.unzip3
                    val (withdrawnRefsPub, withdrawnOutputsPub) = withdrawnPub.unzip
                    // FIXME: atomicity
                    withdrawnRefs.foreach(activeState.remove)

                    Right(
                      txId,
                      (
                        emptyUtxoSet,
                        UtxoSet[L2](withdrawnRefsPub.zip(withdrawnOutputsPub).toMap)
                      )
                    )

        /** Tries to resolve output refs.
          *
          * @param inputs
          *   output refs to resolve
          * @return
          *   Left if
          */
        private def resolveInputs(
            inputs: List[UtxoIdL2]
        ): Either[List[UtxoIdL2], List[(v3.TxOutRef, v3.TxOut, (UtxoIdL2, Output[L2]))]] =
            inputs
                .map { e =>
                    val outputRefInt = liftOutputRef(e)
                    activeState.get(outputRefInt) match
                        case Some(output) => Right(outputRefInt, output, (e, unliftOutput(output)))
                        case None         => Left(e)
                }
                .partitionMap(identity) match
                case (Nil, resolved) => Right(resolved)
                case (extraneous, _) => Left(extraneous)

        def toLedgerTransaction(tx: L2Transaction | L2Withdrawal): LedgerTransaction = tx

/*
 * Multiply a list of n coefficients that belong to a binomial each to get a final polynomial of degree n+1
 * Example: for (x+2)(x+3)(x+5)(x+7)(x+11)=x^5 + 28 x^4 + 288 x^3 + 1358 x^2 + 2927 x + 2310
 */
def getFinalPoly(binomial_poly: SList[Scalar]): SList[Scalar] = {
    binomial_poly
        .foldLeft(SList.single(new Scalar(BigInteger("1")))): (acc, term) =>
            // We need to clone the whole `acc` since `mul` mutates it
            // and final adding gets mutated `shiftedPoly`
            val shiftedPoly: SList[Scalar] =
                SList.Cons(Scalar(BigInteger("0")), acc.map(_.dup))
            val multipliedPoly = acc.map(s => s.mul(term)).appended(Scalar(BigInteger("0")))
            SList.map2(shiftedPoly, multipliedPoly)((l, r) => l.add(r))
}

// TODO: use multi-scalar multiplication
def getG2Commitment(
    setup: SList[P2],
    subset: SList[Scalar]
): P2 = {
    val subsetInG2 =
        SList.map2(getFinalPoly(subset), setup): (sb, st) =>
            st.mult(sb)

    val zero = infG2Point
    require(zero.is_inf())

    subsetInG2.foldLeft(zero.dup()): (a, b) =>
        a.add(b)
}

@main
def dumpSetupG1(): Unit = {
    val setup = SimpleL2Ledger.mkDummySetupG1(5)
    val setupBS = setup.map(e => BLS12_381_G1_Element.apply(e).toCompressedByteString)
    setupBS.foreach(println)

    //    println(encodeHex(IArray.unsafeFromArray(P1.generator().compress())))
    //    println(G1.generator.toCompressedByteString)
}
*/