// These types are temporary bridging between hydrozoa types and scalus types.
// Eventually, we want to move exclusively to the scalus types
// TODO: Not tests for this module yet

package hydrozoa.infra.transitionary

import com.bloxbean.cardano.client.transaction.spec.{
    Transaction as BBTx,
    TransactionInput as BBTxIn,
    TransactionOutput as BBTxOut
}
import co.nstant.in.cbor.CborDecoder
import co.nstant.in.cbor.model.DataItem
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.plutus.spec.{CostMdls, PlutusData}
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.infra.{Piper, toEither}
import hydrozoa.{
    AddressBech,
    AnyLevel,
    NativeScript,
    Output,
    TokenName,
    Tokens,
    Tx,
    TxAny,
    TxL1,
    UtxoId,
    Network as HNetwork,
    PolicyId as HPolicyId
}
import io.bullet.borer.Cbor
import scalus.bloxbean.Interop
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.Address.Shelley
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.address.ShelleyDelegationPart.{Key, Null}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.BloxbeanToLedgerTranslation.toLedgerValue
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.Transaction.given
import scalus.ledger.api
import scalus.ledger.api.v1
import scalus.ledger.api.v1.StakingCredential.StakingHash
import scalus.{ledger, prelude}

import java.io.ByteArrayInputStream

val emptyTxBody: TransactionBody = TransactionBody(
  inputs = Set.empty,
  outputs = IndexedSeq.empty,
  fee = Coin(0)
)

extension [L <: AnyLevel](addr: Address) {
    def toHydrozoa: AddressBech[L] = addr match {
        case Shelley(s) => AddressBech(s.toBech32.get)
        case _          => throw RuntimeException("only shelley addresses allowed")
    }
}

// From Claude sonnet 4, July 17th 2025
extension (cm: CostModels) {
    def toBB: CostMdls = {
        // Step 1: Encode to CBOR bytes using Borer
        val cborBytes = Cbor.encode(cm).toByteArray

        // Step 2: Parse with co.nstant.in.cbor
        val inputStream = new ByteArrayInputStream(cborBytes)
        val decoder = new CborDecoder(inputStream)
        val dataItems: java.util.List[DataItem] = decoder.decode()

        // Get the first (and typically only) DataItem
        val dataItem: DataItem = dataItems.get(0)
        CostMdls.deserialize(dataItem)
    }

}

// Don't know if this will work
extension (tx: Transaction) {
    def toBB: BBTx = {
        BBTx.deserialize(Cbor.encode(tx).toByteArray)
    }
}

extension[L <: AnyLevel] (txIn: UtxoId[L]) {
    def toBB: BBTxIn = {
        BBTxIn(txIn.txId.hash, txIn.outputIx.ix)
    }
}

extension [L <: AnyLevel](utxo: UtxoId[L]) {
    def toScalus: TransactionInput =
        TransactionInput(
          transactionId = Hash(ByteString.fromHex(utxo.txId.hash)),
          index = utxo.outputIx.ix
        )
}

extension [L <: hydrozoa.AnyLevel](address: AddressBech[L]) {
    def toScalus: Address = Address.fromBech32(address.bech32)
}

extension (p: HPolicyId) {
    def toScalus: PolicyId = {
        Hash(ByteString.fromHex(p.policyId))
    }
}

extension (tn: TokenName) {
    // Token Name comes prepended with a 0x; we drop it
    def toScalus: AssetName = {
        AssetName(ByteString.fromHex(tn.tokenNameHex.drop(2)))
    }
}

def htokensToMultiAsset(tokens: Tokens): MultiAsset = {
    tokens.map((cs, tnAndQ) => (cs.toScalus, tnAndQ.map((tn, q) => (tn.toScalus, q.toLong))))
}

extension [L <: hydrozoa.AnyLevel](output: Output[L]) {
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

// REVIEW (Peter, 2025-07-11): This was adapted from Claude on 2025-07-11
// I don't really understand what its doing.
extension (tx: TxL1) {
    def toScalus: Transaction = {
        given OriginalCborByteArray = OriginalCborByteArray(tx.bytes)
        Cbor.decode(tx.bytes).to[Transaction].value
    }
}

// Uses the bloxbean backend to query a utxo into a scalus TransactionOutput
def bloxToScalusUtxoQuery[L <: AnyLevel](
    backendService: BackendService,
    utxoId: UtxoId[L]
): Either[String, TransactionOutput] = {
    backendService.getUtxoService
        .getTxOutput(utxoId.txId.hash, utxoId.outputIx.ix.intValue)
        .toEither match {
        case Left(err) => Left("[bloxToScalusUtxoQuery]: Querying failed: " ++ err)
        case Right(utxo) =>
            Right({

                println(utxo.getAddress)
                val outAddress = Address.fromBech32(utxo.getAddress)
                val outVal: Value = utxo.toValue.toLedgerValue
                val outDat = Option(utxo.getInlineDatum).map(hex =>
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
