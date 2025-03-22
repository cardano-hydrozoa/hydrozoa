package hydrozoa.infra

import co.nstant.in.cbor.model.{Array, ByteString, Map, UnsignedInteger}
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.common.model.Network as BBNetwork
import com.bloxbean.cardano.client.crypto.*
import com.bloxbean.cardano.client.crypto.bip32.{HdKeyGenerator, HdKeyPair}
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import com.bloxbean.cardano.client.spec.Era
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import com.bloxbean.cardano.client.transaction.util.TransactionUtil.getTxHash
import com.bloxbean.cardano.client.util.HexUtil

import hydrozoa.*
import hydrozoa.l1.multisig.tx.{MultisigTx, MultisigTxTag, toL1Tx}
import hydrozoa.l2.ledger.{SimpleGenesis, SimpleTransaction, SimpleWithdrawal}
import scalus.bloxbean.Interop
import scalus.builtin.Data
import scalus.ledger.api.v1

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

// Pure function to create a transaction key witness with a HD key.
// TODO: handle exceptions
def createTxKeyWitness[T <: MultisigTxTag](tx: MultisigTx[T], pair: HdKeyPair): TxKeyWitness = {

    // See TransactionSigner

    val txBytes = TransactionBytes(tx.toL1Tx.bytes)
    val txnBodyHash = Blake2bUtil.blake2bHash256(txBytes.getTxBodyBytes)
    val signingProvider = CryptoConfiguration.INSTANCE.getSigningProvider
    val signature = signingProvider.signExtended(
      txnBodyHash,
      pair.getPrivateKey.getKeyData,
      pair.getPublicKey.getKeyData
    )
    TxKeyWitness(signature, pair.getPublicKey.getKeyData)
}

// Pure function to create a transaction key witness with a peer node's key.
// TODO: handle exceptions
def createTxKeyWitness[T <: MultisigTxTag](
    tx: MultisigTx[T],
    participantKey: ParticipantSecretKey
): TxKeyWitness = {

    // See TransactionSigner

    val secretKey = SecretKey.create(participantKey.bytes)
    val txBytes = TransactionBytes(tx.toL1Tx.bytes)
    val txnBodyHash = Blake2bUtil.blake2bHash256(txBytes.getTxBodyBytes)
    val signingProvider = CryptoConfiguration.INSTANCE.getSigningProvider

    val (signature, vKey) =
        if (secretKey.getBytes.length == 64) { // extended pvt key (most prob for regular account)
            // check for public key
            val vBytes = HdKeyGenerator.getPublicKey(secretKey.getBytes)
            val vKey = VerificationKey.create(vBytes)
            val sig = signingProvider.signExtended(txnBodyHash, secretKey.getBytes, vBytes)
            (sig, vKey)
        } else {
            val sig = signingProvider.sign(txnBodyHash, secretKey.getBytes)
            val vKey = KeyGenUtil.getPublicKeyFromPrivateKey(secretKey)
            (sig, vKey)
        }
    TxKeyWitness(signature, vKey.getBytes)
}

// Pure function to add a key witness to a transaction.
def addWitness[T <: MultisigTxTag](tx: MultisigTx[T], wit: TxKeyWitness): MultisigTx[T] = {
    val txBytes = TransactionBytes(tx.toL1Tx.bytes)
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
    MultisigTx(TxL1(txBytes.withNewWitnessSetBytes(txWitnessBytes).getTxBytes))
}

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
                TxIx(outputs.indexOf(elem)),
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

def outputDatum(tx: TxAny, index: TxIx): Data =
    val tx_ = Transaction.deserialize(tx.bytes)
    val output = tx_.getBody.getOutputs.get(index.ix.intValue)
    val datum = output.getInlineDatum
    Interop.toScalusData(datum)

// TODO: unused
//def txInputs[L <: AnyLevel](tx: TxAny): Set[OutputRef[L]] =
//    val tx_ = Transaction.deserialize(tx.bytes)
//    tx_.getBody.getInputs.asScala
//        .map(ti => OutputRef[L](TxId(ti.getTransactionId), TxIx(ti.getIndex)))
//        .toSet

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
    inputs.map(i => UtxoId(TxId(i.getTransactionId), TxIx(i.getIndex))).toSeq

def txOutputs[L <: AnyLevel](tx: Tx[L]): Seq[(UtxoId[L], Output[L])] =
    val outputs = Transaction.deserialize(tx.bytes).getBody.getOutputs.asScala
    val txId = txHash(tx)
    outputs.zipWithIndex
        .map((o, ix) =>
            val utxoId = UtxoId[L](TxId(txId.hash), TxIx(ix))
            val utxo = Output[L](AddressBechL1(o.getAddress), o.getValue.getCoin.longValue())
            (utxoId, utxo)
        )
        .toSeq

extension (n: Network) {
    def toBloxbean: BBNetwork = BBNetwork(n.networkId, n.protocolMagic)
}
