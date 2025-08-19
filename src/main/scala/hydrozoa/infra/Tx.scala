package hydrozoa.infra

/** This package (partially) re-exposes the API of scalus's types on the L1/L2 tagged hydrozoa
  * types. Note that there are currently both bloxbean and scalus types imported. Where conflicts
  * occur, we import the scalus types with the `S` prefix; i.e. `SValue` comes from Scalus, `Value`
  * comes from bloxbean.
  */

import co.nstant.in.cbor.model.{
    Map,
    UnsignedInteger,
    Array as CborArray,
    ByteString as CborByteString
}
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.transaction.spec.script.{
    ScriptAll,
    NativeScript as BBNativeScript
}
import com.bloxbean.cardano.client.transaction.spec.{TransactionOutput, Value}
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.*
import hydrozoa.l1.multisig.tx.{MultisigTx, MultisigTxTag}
import io.bullet.borer.Cbor
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{
    AssetName,
    Blake2b_224,
    Hash,
    HashPurpose,
    MultiAsset,
    OriginalCborByteArray,
    Sized,
    TransactionInput,
    VKeyWitness,
    Transaction as STransaction,
    TransactionOutput as STransactionOutput,
    Value as SValue
}
import scalus.ledger.api.Timelock.AllOf

import java.math.BigInteger
import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

// TODO: make an API
// We should expose more of it, probably add more tagged types, and unifying the naming of the functions

def serializeTxHex[T <: MultisigTxTag, L <: AnyLayer](tx: MultisigTx[T] | TxAny): String =
    ByteString.fromArray(Cbor.encode(tx.asInstanceOf[STransaction]).toByteArray).toHex

def deserializeTxHex[L <: AnyLayer](hex: String): Tx[L] = Tx[L]({
    val bytes = HexUtil.decodeHexString(hex)
    given OriginalCborByteArray = OriginalCborByteArray(bytes)

    Cbor.decode(bytes).to[STransaction].value
})

// Pure function to add a key witness to a transaction.
def addWitness[L <: AnyLayer](tx: Tx[L], wit: VKeyWitness): Tx[L] =
    val txBytes = TransactionBytes(Cbor.encode(tx.untagged).toByteArray)
    val witnessSetDI = CborSerializationUtil.deserialize(txBytes.getTxWitnessBytes)
    val witnessSetMap = witnessSetDI.asInstanceOf[Map]

    val vkWitnessArrayDI = witnessSetMap.get(UnsignedInteger(0))

    val vkWitnessArray: CborArray =
        if (vkWitnessArrayDI != null) vkWitnessArrayDI.asInstanceOf[CborArray]
        else new CborArray

    if (vkWitnessArrayDI == null)
        witnessSetMap.put(new UnsignedInteger(0), vkWitnessArray): Unit

    val vkeyWitness = new CborArray
    vkeyWitness.add(CborByteString(wit.vkey.bytes))
    vkeyWitness.add(CborByteString(wit.signature.bytes))

    vkWitnessArray.add(vkeyWitness)

    val txWitnessBytes = CborSerializationUtil.serialize(witnessSetMap, false)
    val txBytesSigned = txBytes.withNewWitnessSetBytes(txWitnessBytes).getTxBytes
    given OriginalCborByteArray = OriginalCborByteArray(txBytesSigned)
    Tx(Cbor.decode(txBytesSigned).to[STransaction].value)
end addWitness

// A variant for multisig functions.
def addWitnessMultisig[T <: MultisigTxTag](tx: MultisigTx[T], wit: VKeyWitness): MultisigTx[T] =
    MultisigTx(addWitness(tx.untagged, wit))

/** @param tx
  * @param address
  * @return
  *   Index, value, and datum iff tx has exactly one output to address specified.
  */
def onlyOutputToAddress(
    tx: TxAny,
    address: AddressL1
): Either[(NoMatch | TooManyMatches | NonBabbageMatch | NoInlineDatum), (Integer, SValue, Data)] =
    val outputs = tx.body.value.outputs
    outputs.filter(output => output.value.address == address) match
        case IndexedSeq(elem: Sized[STransactionOutput]) =>
            elem.value match
                case b: Babbage if b.datumOption.isDefined =>
                    b.datumOption.get match {
                        case i: Inline =>
                            Right(
                              (
                                outputs.indexOf(Sized(b.asInstanceOf[STransactionOutput])),
                                b.value,
                                i.data
                              )
                            )
                        case _ => Left(NoInlineDatum())
                    }
                case _ => Left(NonBabbageMatch())
        case i: IndexedSeq[Any] if i.isEmpty => Left(NoMatch())
        case _                               => Left(TooManyMatches())

final class NoMatch
final class TooManyMatches
final class NonBabbageMatch
final class NoInlineDatum

// WARNING/FIXME: Partial for compatibility reasons, will throw if address is not Shelley
def toBloxBeanTransactionOutput[L <: AnyLayer](output: Output[L]): TransactionOutput =
    TransactionOutput.builder
        .address(output.address.asInstanceOf[ShelleyAddress].toBech32.get)
        .value(Value.builder.coin(BigInteger.valueOf(output.value.coin.value)).build)
        .build

def txInputs[L <: AnyLayer](tx: Tx[L]): Seq[UtxoId[L]] =
    val inputs = tx.body.value.inputs
    inputs.map(i => UtxoId(i)).toSeq

def valueTokens[L <: AnyLayer](tokens: Value): MultiAsset = MultiAsset({
    SortedMap.from(
      tokens.toMap.asScala.toMap.map((k, v) =>
          Hash[Blake2b_224, HashPurpose.ScriptHash](ByteString.fromHex(k))
              -> SortedMap.from(
                v.asScala.toMap.map((k, v) =>
                    AssetName(ByteString.fromHex(k.drop(2))) -> v.longValue()
                )
              )
      )
    )
})
def txOutputs[L <: AnyLayer](tx: Tx[L]): Seq[(UtxoId[L], Output[L])] =
    val outputs = tx.body.value.outputs
    outputs.zipWithIndex
        .map((o, ix) =>
            val utxoId = UtxoId[L](TransactionInput(transactionId = tx.id, index = ix))
            (utxoId, Output[L](o.value.asInstanceOf[Babbage]))
        )

/** This is an ad-hoc implementation, it won't be correct fot other cases. Returns the number of
  * top-level scripts in ScriptAll native script as if they all were `ScriptPubkey` (i.e. require
  * one signatory).
  *
  * @param nativeScript
  *   native script, see the comment above
  * @return
  *   number of signatories required for a native script
  */
def numberOfSignatories(nativeScript: BBNativeScript): Int =
    nativeScript match
        case scriptAll: ScriptAll => scriptAll.getScripts.size()
        case _                    => 0

def numberOfSignatories(nativeScript: Native): Int =
    nativeScript.script match
        case scriptAll: AllOf => scriptAll.scripts.size
        case _                => 0

def extractVoteTokenNameFromFallbackTx(fallbackTx: TxL1): AssetName =
    val mint = fallbackTx.body.value.mint.get.assets.toList
    assert(mint.size == 1)
    val tokens = mint.head._2.toList
    assert(tokens.size == 1)
    tokens.head._1
