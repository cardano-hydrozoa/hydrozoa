// These types are temporary bridging between hydrozoa types and scalus types.
// Eventually, we want to move exclusively to the scalus types
// TODO: Not tests for this module yet

package hydrozoa.infra.transitionary

import hydrozoa.infra.toBloxbean
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.infra.{Piper, addressToBloxbean, plutusAddressAsL2, toEither}
import hydrozoa.{
    AddressBech,
    AnyLayer,
    NativeScript,
    Output,
    TokenName,
    Tokens,
    TxAny,
    TxId,
    TxIx,
    TxL1,
    UtxoId,
    UtxoSet,
    hydrozoaL2Network,
    networkL1static,
    Network as HNetwork,
    PolicyId as HPolicyId
}
import io.bullet.borer.Cbor
import scalus.bloxbean.Interop
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.*
import scalus.cardano.address.Address.Shelley
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.address.ShelleyDelegationPart.{Key, Null}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.BloxbeanToLedgerTranslation.toLedgerValue
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.Transaction.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import scalus.ledger.api
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.StakingCredential.StakingHash
import scalus.ledger.api.v2.OutputDatum.{NoOutputDatum, OutputDatum}
import scalus.ledger.api.{v1, v3}
import scalus.prelude.Option
import scalus.{ledger, prelude}

import scala.collection.immutable.SortedMap

val emptyTxBody: TransactionBody = TransactionBody(
  inputs = Set.empty,
  outputs = IndexedSeq.empty,
  fee = Coin(0),
  ttl = None,
  certificates = Set.empty,
  withdrawals = None,
  auxiliaryDataHash = None,
  validityStartSlot = None,
  mint = None,
  scriptDataHash = None,
  collateralInputs = Set.empty,
  requiredSigners = Set.empty,
  networkId = None,
  collateralReturnOutput = None,
  totalCollateral = None,
  referenceInputs = Set.empty,
  votingProcedures = None,
  proposalProcedures = Set.empty,
  currentTreasuryValue = None,
  donation = None
)

val emptyTransaction: Transaction =
    Transaction(
      body = KeepRaw(emptyTxBody),
      witnessSet = TransactionWitnessSet.empty,
      isValid = false,
      auxiliaryData = None
    )

extension [L <: AnyLayer](utxo: UtxoId[L]) {
    def toScalus: TransactionInput =
        TransactionInput(
          transactionId = Hash(ByteString.fromHex(utxo.txId.hash)),
          index = utxo.outputIx.ix
        )
}

extension [L <: hydrozoa.AnyLayer](address: AddressBech[L]) {
    def toScalus: Address = Address.fromBech32(address.bech32)
}

extension (p: HPolicyId) {
    def toScalus: PolicyId = {
        Hash(ByteString.fromHex(p.policyId))
    }
}

extension (p: PolicyId) {
    def toHydrozoa: HPolicyId = {
        HPolicyId(p.toHex)
    }
}

extension (tn: TokenName) {
    // Token Name comes prepended with a 0x; we drop it
    def toScalus: AssetName = {
        AssetName(ByteString.fromHex(tn.tokenNameHex.drop(2)))
    }
}

extension (an: AssetName) {
    def toHydrozoa: TokenName = TokenName(s"0x${an.bytes.toHex}")
}

extension (ma: MultiAsset) {
    def toHydrozoa: Tokens =
        ma.map((k, v) => (k.toHydrozoa, v.map((k1, v1) => (k1.toHydrozoa, BigInt(v1)))))
}

def htokensToMultiAsset(tokens: Tokens): MultiAsset = {
    Map.from(
      tokens.map((cs, tnAndQ) =>
          (cs.toScalus, Map.from(tnAndQ.map((tn, q) => (tn.toScalus, q.toLong))))
      )
    )

}

extension [L <: hydrozoa.AnyLayer](output: Output[L]) {
    def toScalus: TransactionOutput = {

        TransactionOutput(
          address = output.address.toScalus,
          value = Value(
            coin = Coin(output.coins.toLong),
            multiAsset = htokensToMultiAsset(output.tokens)
          ),
          datumOption = output.mbInlineDatum match {
              case Some(d) =>
                  Some(
                    // NEEDS REVIEW (Peter, 2025-07-11): I don't know the encoding, I'm assuming this is correct
                    Inline(toData(ByteString.fromHex(d)))
                  )
              case None => None
          }
        )
    }
}

extension [HF, P](hash: Hash[HF, P]) {
    def toIArray: IArray[Byte] =
        IArray.from(hash.bytes)
}

extension (native: Native) {
    def toHydrozoaNativeScript: NativeScript = {
        NativeScript(native.script.toCbor)
    }
}
extension (native: NativeScript) {
    def toScalusNativeScript: Native = {
        Cbor.decode(native.bytes).to[Native].value
    }
}

extension (network: HNetwork) {
    def toScalus: Network = {
        if network.networkId == 1
        then Mainnet
        else Testnet
    }
}

extension (ti: TransactionInput) {
    def toHydrozoa[L <: AnyLayer]: UtxoId[L] = UtxoId(
      txId = TxId(ti.transactionId.toHex),
      outputIx = TxIx(ti.index)
    )
}

extension (to: TransactionOutput) {
    def toHydrozoa[L <: AnyLayer]: Output[L] = Output(
      address = AddressBech[L](
        to.asInstanceOf[Babbage].address.asInstanceOf[ShelleyAddress].toBech32.get
      ),
      coins = BigInt(to.value.coin.value),
      tokens = to.value.assets.toHydrozoa,
      mbInlineDatum = to
          .asInstanceOf[Babbage]
          .datumOption
          .map(d => ByteString.fromArray(Cbor.encode(d).toByteArray).toHex)
    )
}

// REVIEW (Peter, 2025-07-11): This was adapted from Claude on 2025-07-11
// I don't really understand what its doing.
extension (tx: TxL1) {
    def toScalus: Transaction = {
        given OriginalCborByteArray = OriginalCborByteArray(tx.bytes)
        Cbor.decode(tx.bytes).to[Transaction].value
    }
}

// Uses the bloxbean backend to query a utxo into a scalus TransactionOutput
def bloxToScalusUtxoQuery(
    backendService: BackendService,
    input: TransactionInput
): Either[String, TransactionOutput] = {
    backendService.getUtxoService
        .getTxOutput(input.transactionId.toHex, input.index)
        .toEither match {
        case Left(err) => Left("[bloxToScalusUtxoQuery]: Querying failed: " ++ err)
        case Right(utxo) =>
            Right({

                println(utxo.getAddress)
                val outAddress = Address.fromBech32(utxo.getAddress)
                val outVal: Value = utxo.toValue.toLedgerValue
                val outDat = scala
                    .Option(utxo.getInlineDatum)
                    .map(hex =>
                        hex |> HexUtil.decodeHexString
                            |> PlutusData.deserialize
                            |> Interop.toScalusData
                            |> DatumOption.Inline.apply
                    )

                TransactionOutput(
                  address = outAddress,
                  value = outVal,
                  datumOption = outDat
                )
            })
    }
}

/** Convert scalus.ledger.api.v1.Address to scalus.cardano.address.Address .
  *
  * This function converts between the simplified address representation used in Plutus script
  * contexts and the comprehensive address representation used in the domain model.
  */

// TODO: Needs tests
def v1AddressToLedger(address: v1.Address, network: Network): ShelleyAddress = {

    val paymentPart: ShelleyPaymentPart = address.credential match {
        case ledger.api.v1.Credential.PubKeyCredential(v1hash) =>
            ShelleyPaymentPart.Key(AddrKeyHash(v1hash.hash))
        case ledger.api.v1.Credential.ScriptCredential(v1hash) =>
            ShelleyPaymentPart.Script(Hash(v1hash))
    }

    val delegationPart: ShelleyDelegationPart = address.stakingCredential match {
        case prelude.Option.None => Null
        case prelude.Option.Some(sc) =>
            sc match {
                case sh: StakingHash =>
                    sh match {
                        case ledger.api.v1.StakingCredential.StakingHash(v1Hash) =>
                            v1Hash match {
                                case ledger.api.v1.Credential.PubKeyCredential(v1Key) =>
                                    ShelleyDelegationPart.Key(Hash(v1Key.hash))
                                case ledger.api.v1.Credential.ScriptCredential(v1Script) =>
                                    ShelleyDelegationPart.Script(Hash(v1Script))
                            }
                    }
                case ledger.api.v1.StakingCredential.StakingPtr(a, b, c) =>
                    ShelleyDelegationPart.Pointer(Pointer(Slot(a.toLong), b.toLong, c.toLong))
            }
    }
    ShelleyAddress(network = network, payment = paymentPart, delegation = delegationPart)
}

extension [L <: AnyLayer](us: UtxoSet[L]) {
    def toScalus: UTxO = us.utxoMap.map((k, v) => (k.toScalus, v.toScalus))
}

def toV3UTxO(utxo: UTxO): Map[v3.TxOutRef, v3.TxOut] = {
    utxo.map((ti, to) =>
        (
          v3.TxOutRef(
            id = v3.TxId(ByteString.fromArray(ti.transactionId.bytes)),
            idx = BigInt(ti.index)
          ),
          LedgerToPlutusTranslation.getTxOutV2(Sized(to))
        )
    )
}

def toHUTxO[L <: AnyLayer](utxo: UTxO): UtxoSet[L] = {
    UtxoSet(map = utxo.map((ti, to) => (ti.toHydrozoa[L], to.toHydrozoa[L])))
}

extension (txor: v3.TxOutRef) {
    def toScalusLedger: TransactionInput =
        TransactionInput(transactionId = Hash(txor.id.hash), index = txor.idx.toInt)
    def toHydrozoa[L <: AnyLayer]: UtxoId[L] =
        UtxoId[L](txId = TxId(txor.id.hash.toHex), outputIx = TxIx(txor.idx.toInt))
}

// FIXME: This isn't a full translation. We don't care about delegation, so we drop them.
extension (addr: v3.Address) {
    def toScalusLedger: Address =
        Shelley(
          ShelleyAddress(
            network = networkL1static.toScalus,
            payment = addr.credential match {
                case PubKeyCredential(pkc) =>
                    ShelleyPaymentPart.Key(Hash(ByteString.fromArray(pkc.hash.bytes)))
                case ScriptCredential(sc) =>
                    ShelleyPaymentPart.Script(Hash(ByteString.fromArray(sc.bytes)))
            },
            delegation = ShelleyDelegationPart.Null
          )
        )
}

def csToPolicyId(cs: v3.CurrencySymbol): PolicyId = {
    Hash(ByteString.fromArray(cs.bytes))
}

def tnToAssetName(tn: v3.TokenName): AssetName = AssetName.fromHex(tn.toHex)

def listToMap[A, B](lop: prelude.List[(A, B)]): Map[A, B] = {
    lop.foldLeft(Map.empty)((m, ab) => m.updated(ab._1, ab._2))
}

extension (v: v3.Value) {
    def toScalusLedger: Value = {
        val coins: Coin = Coin(v.toList.head._2.toList.head._2.toLong)
        val ma0: prelude.List[(PolicyId, prelude.List[(AssetName, Long)])] =
            v.toList.tail.map((cs, assocMap) =>
                (csToPolicyId(cs), assocMap.toList.map((tn, bi) => (tnToAssetName(tn), bi.toLong)))
            )
        val ma1 = listToMap(ma0.map(x => (x._1, listToMap(x._2))))

        Value(coin = coins, multiAsset = ma1)
    }
}

extension (to: v3.TxOut) {
    def toScalusLedger: TransactionOutput = {
        Babbage(
          address = to.address.toScalusLedger,
          value = to.value.toScalusLedger,
          datumOption = to.datum match {
              case NoOutputDatum  => None
              case OutputDatum(d) => Some(Inline(d))
              case _              => throw new RuntimeException("invalid datum")
          },
          scriptRef = to.referenceScript match {
              case Option.None => None
              // The v3TxOut contains a script hash, but the ledger type contains the actual script.
              // We can't recover the full script from the hash, so we throw.
              // Maybe in the future we can pass in the script separately and ensure the hashes match
              case Option.Some(sr) =>
                  throw new RuntimeException("v3 TxOut has a reference script hash, but can't")
          }
        )

    }

}

// Adaptor from old ledger to new ledger "state"
// FIXME: This is a dummy value. The slot config and protocol params should be passed in
def contextAndStateFromV3UTxO(v3utxo: Map[v3.TxOutRef, v3.TxOut]): (Context, State) = {
    val cs = CertState(
      vstate = VotingState(Map.empty),
      pstate = PoolsState(),
      dstate = DelegationState(
        rewards = Map.empty,
        deposits = Map.empty,
        stakePools = Map.empty,
        dreps = Map.empty
      )
    )

    (
      Context(fee = Coin(0L), env = UtxoEnv.default),
      State(utxo = v3utxo.map((k, v) => (k.toScalusLedger, v.toScalusLedger)), certState = cs)
    )
}
