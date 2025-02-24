package hydrozoa.head

import co.nstant.in.cbor.model.{Array, ByteString, Map, UnsignedInteger}
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.crypto.*
import com.bloxbean.cardano.client.crypto.bip32.HdKeyGenerator
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import com.bloxbean.cardano.client.transaction.util.TransactionBytes

// FIXME: make an API

// Pure function to sign a transaction
// FIXME: handle exceptions
// FIXME: use Hydrozoa own type for the secret key
def signTx(tx: L1Tx, participantKey: ParticipantSecretKey): TxKeyWitness = {

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

// Pure function to add a key witness to a transaction
def addWitness(tx: L1Tx, wit: TxKeyWitness): L1Tx = {
    val txBytes = TransactionBytes(tx.bytes)
    val witnessSetDI = CborSerializationUtil.deserialize(txBytes.getTxWitnessBytes)
    val witnessSetMap = witnessSetDI.asInstanceOf[Map]

    val vkWitnessArrayDI = witnessSetMap.get(UnsignedInteger(0))

    val vkWitnessArray: Array =
        if (vkWitnessArrayDI != null) vkWitnessArrayDI.asInstanceOf[Array]
        else new Array

    if (vkWitnessArrayDI != null)
        witnessSetMap.put(new UnsignedInteger(0), vkWitnessArray)

    val vkeyWitness = new Array
    vkeyWitness.add(ByteString(wit.vkey))
    vkeyWitness.add(ByteString(wit.signature))

    vkWitnessArray.add(vkeyWitness)

    val txWitnessBytes = CborSerializationUtil.serialize(witnessSetMap, false)
    L1Tx(txBytes.withNewWitnessSetBytes(txWitnessBytes).getTxBytes)
}

def genNodeKey(): (ParticipantSecretKey, ParticipantVerificationKey) = {
    val keys = KeyGenUtil.generateKey
    (ParticipantSecretKey(keys.getSkey.getBytes), ParticipantVerificationKey(keys.getVkey.getBytes))
}
