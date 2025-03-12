package hydrozoa.infra

import co.nstant.in.cbor.model.{Array, ByteString, Map, UnsignedInteger}
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.crypto.*
import com.bloxbean.cardano.client.crypto.bip32.{HdKeyGenerator, HdKeyPair}
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import com.bloxbean.cardano.client.transaction.util.TransactionUtil.getTxHash
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.*
import scalus.bloxbean.Interop
import scalus.builtin.Data

import scala.jdk.CollectionConverters.*

// TODO: make an API

def txHash(tx: L1Tx): TxId = TxId(getTxHash(tx.bytes))

// TODO: generalize fot both L1 and L2
def serializeTxHex(tx: L1Tx): String = HexUtil.encodeHexString(tx.bytes)

// TODO: generalize fot both L1 and L2
def deserializeTxHex(hex: String): L1Tx = L1Tx(HexUtil.decodeHexString(hex))

// Pure function to create a transaction key witness with a HD key.
// TODO: handle exceptions
def createTxKeyWitness(tx: L1Tx, pair: HdKeyPair): TxKeyWitness = {

    // See TransactionSigner

    val txBytes = TransactionBytes(tx.bytes)
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
def createTxKeyWitness(tx: L1Tx, participantKey: ParticipantSecretKey): TxKeyWitness = {

    // See TransactionSigner

    val secretKey = SecretKey.create(participantKey.bytes)
    val txBytes = TransactionBytes(tx.bytes)
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
def addWitness(tx: L1Tx, wit: TxKeyWitness): L1Tx = {
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
    L1Tx(txBytes.withNewWitnessSetBytes(txWitnessBytes).getTxBytes)
}

/** @param tx
  * @param address
  * @return
  *   Index and ada amount (should be value).
  */
def onlyAddressOutput(tx: L1Tx, address: AddressBechL1): Option[(TxIx, BigInt)] =
    val tx_ = Transaction.deserialize(tx.bytes)
    val outputs = tx_.getBody.getOutputs.asScala
    outputs.indexWhere(output => output.getAddress == address.bech32) match
        case -1 => None
        case i  => Some((TxIx(i), BigInt.apply(outputs.apply(i).getValue.getCoin.longValue())))

def outputDatum(tx: L1Tx, index: TxIx): Data =
    val tx_ = Transaction.deserialize(tx.bytes)
    val output = tx_.getBody.getOutputs.get(index.ix.intValue)
    val datum = output.getInlineDatum
    Interop.toScalusData(datum)

def txInputsRef(tx: L1Tx): Set[(TxId, TxIx)] =
    val tx_ = Transaction.deserialize(tx.bytes)
    tx_.getBody.getInputs.asScala.map(ti => (TxId(ti.getTransactionId), TxIx(ti.getIndex))).toSet
