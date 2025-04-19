package hydrozoa.infra

import co.nstant.in.cbor.model.{Array, ByteString, Map, UnsignedInteger}
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.common.model.Network as BBNetwork
import com.bloxbean.cardano.client.spec.Era
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import com.bloxbean.cardano.client.transaction.util.TransactionUtil.getTxHash
import com.bloxbean.cardano.client.transaction.spec.script.{ScriptAll, NativeScript as BBNativeScript}
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.*
import hydrozoa.l1.multisig.tx.{MultisigTx, MultisigTxTag, toL1Tx}
import hydrozoa.l2.ledger.{SimpleGenesis, SimpleTransaction, SimpleWithdrawal}
import scalus.bloxbean.Interop
import scalus.builtin.Data

import java.math.BigInteger
import scala.jdk.CollectionConverters.*

// TODO: make an API

def txHash[T <: MultisigTxTag, L <: AnyLevel](tx: MultisigTx[T] | Tx[L]): TxId = TxId(
  getTxHash(getAnyTxBytes(tx))
)

def serializeTxHex[T <: MultisigTxTag, L <: AnyLevel](tx: MultisigTx[T] | Tx[L]): String =
    HexUtil.encodeHexString(getAnyTxBytes(tx))

def getAnyTxBytes[L <: AnyLevel, T <: MultisigTxTag](tx: MultisigTx[T] | Tx[L]) =
    tx match
        case multisig: MultisigTx[T] => multisig.toL1Tx.bytes
        case tx: Tx[L]               => tx.bytes

def deserializeTxHex[L <: AnyLevel](hex: String): Tx[L] = Tx[L](HexUtil.decodeHexString(hex))

// Pure function to add a key witness to a transaction.
def addWitness[L <: AnyLevel](tx: Tx[L], wit: TxKeyWitness): Tx[L] =
    val txBytes = TransactionBytes(tx.bytes)
    val witnessSetDI = CborSerializationUtil.deserialize(txBytes.getTxWitnessBytes)
    val witnessSetMap = witnessSetDI.asInstanceOf[Map]

    val vkWitnessArrayDI = witnessSetMap.get(UnsignedInteger(0))

    val vkWitnessArray: Array =
        if (vkWitnessArrayDI != null) vkWitnessArrayDI.asInstanceOf[Array]
        else new Array

    if (vkWitnessArrayDI == null)
        witnessSetMap.put(new UnsignedInteger(0), vkWitnessArray)

    val vkeyWitness = new Array
    vkeyWitness.add(ByteString(wit.vkey))
    vkeyWitness.add(ByteString(wit.signature))

    vkWitnessArray.add(vkeyWitness)

    val txWitnessBytes = CborSerializationUtil.serialize(witnessSetMap, false)
    txBytes.withNewWitnessSetBytes(txWitnessBytes).getTxBytes |> Tx.apply
end addWitness

// A variant for multisig functions.
def addWitnessMultisig[T <: MultisigTxTag](tx: MultisigTx[T], wit: TxKeyWitness): MultisigTx[T] =
    MultisigTx(addWitness(tx.toL1Tx, wit))

/** @param tx
  * @param address
  * @return
  *   Index and ada amount iff tx has exactly one output to address specified. TODO: should be value
  */
def onlyOutputToAddress(
    tx: TxAny,
    address: AddressBechL1
): Either[(NoMatch | TooManyMatches), (TxIx, BigInt, Data)] =
    val outputs = Transaction.deserialize(tx.bytes).getBody.getOutputs.asScala.toList
    outputs.filter(output => output.getAddress == address.bech32) match
        case List(elem) =>
            Right(
              (
                TxIx(outputs.indexOf(elem).toChar),
                elem.getValue.getCoin.longValue(),
                Interop.toScalusData(
                  elem.getInlineDatum
                ) // FIXME: how does it indicate it's optional? Use Option.apply
              )
            )
        case Nil => Left(NoMatch())
        case _   => Left(TooManyMatches())

final class NoMatch
final class TooManyMatches

def txFees(tx: TxAny): Long = Transaction.deserialize(tx.bytes).getBody.getFee.longValue()

//def outputDatum(tx: TxAny, index: TxIx): Data =
//    val tx_ = Transaction.deserialize(tx.bytes)
//    val output = tx_.getBody.getOutputs.get(index.ix.intValue)
//    val datum = output.getInlineDatum
//    Interop.toScalusData(datum)

def toBloxBeanTransactionOutput[L <: AnyLevel](output: Output[L]): TransactionOutput =
    TransactionOutput.builder
        .address(output.address.bech32)
        .value(Value.builder.coin(BigInteger.valueOf(output.coins.longValue)).build)
        .build

// ----------------------------------------------------------------------------
// Cardano L2 transactions for the simplified ledger
// ----------------------------------------------------------------------------

def mkCardanoTxForL2Genesis(genesis: SimpleGenesis): TxL2 =

    val virtualOutputs = genesis.outputs.map { output =>
        TransactionOutput.builder
            .address(output.address.bech32)
            .value(Value.builder.coin(output.coins.bigInteger).build)
            .build
    }

    val body = TransactionBody.builder
        .outputs(virtualOutputs.asJava)
        .build

    val tx = Transaction.builder.era(Era.Conway).body(body).build
    TxL2(tx.serialize)

/** @param simpleTx
  * @return
  *   Virtual L2 transaction that spends L1 deposit utxos and produces L2 genesis utxos.
  */
def mkCardanoTxForL2Transaction(simpleTx: SimpleTransaction): TxL2 =

    val virtualInputs = simpleTx.inputs.map { input =>
        TransactionInput.builder
            .transactionId(input._1.hash)
            .index(input._2.ix.intValue)
            .build
    }

    val virtualOutputs = simpleTx.outputs.map { output =>
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

/** @param withdrawal
  * @return
  */
def mkCardanoTxForL2Withdrawal(withdrawal: SimpleWithdrawal): TxL2 =

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

def txInputs[L <: AnyLevel](tx: Tx[L]): Seq[UtxoId[L]] =
    val inputs = Transaction.deserialize(tx.bytes).getBody.getInputs.asScala
    inputs.map(i => UtxoId(TxId(i.getTransactionId), TxIx(i.getIndex.toChar))).toSeq

def txOutputs[L <: AnyLevel](tx: Tx[L]): Seq[(UtxoId[L], Output[L])] =
    val outputs = Transaction.deserialize(tx.bytes).getBody.getOutputs.asScala
    val txId = txHash(tx)
    outputs.zipWithIndex
        .map((o, ix) =>
            val utxoId = UtxoId[L](TxId(txId.hash), TxIx(ix.toChar))
            val utxo = Output[L](AddressBechL1(o.getAddress), o.getValue.getCoin.longValue())
            (utxoId, utxo)
        )
        .toSeq

extension (n: Network) {
    def toBloxbean: BBNetwork = BBNetwork(n.networkId, n.protocolMagic)
}

/**
 * This is an ad-hoc implementation, it won't be correct fot other cases.
 * Returns the number of top-level scripts in ScriptAll native script
 * as if they all were `ScriptPubkey` (i.e. require one signatory).
 *
 * @param nativeScript
 * native script, see the comment above
 * @return
 * number of signatories required for a native script
 */
def numberOfSignatories(nativeScript: BBNativeScript): Int =
    nativeScript match
        case scriptAll: ScriptAll => scriptAll.getScripts.size()
        case _ => 0