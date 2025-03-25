package hydrozoa.infra

import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.crypto.bip32.key.{HdPrivateKey, HdPublicKey}
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import hydrozoa.l1.multisig.tx.{MultisigTx, MultisigTxTag, toL1Tx}
import hydrozoa.{PeerKeyVault, PeerPrivateKeyBytes, PeerPublicKeyBytes, TxKeyWitness}

object HDPeerKeyVaultBB extends PeerKeyVault:

    override type PeerPublicKey = HdPublicKey
    override type PeerPrivateKey = HdPrivateKey

    override def peerPublicKeyBytes(publicKey: PeerPublicKey): PeerPublicKeyBytes =
        PeerPublicKeyBytes(publicKey.getKeyData)

    override def peerPrivateKeyBytes(privateKey: PeerPrivateKey): PeerPrivateKeyBytes =
        PeerPrivateKeyBytes(privateKey.getKeyData)

    override def createTxKeyWitness[T <: MultisigTxTag](
        tx: MultisigTx[T],
        publicKey: PeerPublicKey,
        privateKey: PeerPrivateKey
    ): TxKeyWitness =
        // See BloxBean's TransactionSigner
        val txBytes = TransactionBytes(tx.toL1Tx.bytes)
        val txnBodyHash = Blake2bUtil.blake2bHash256(txBytes.getTxBodyBytes)
        val signingProvider = CryptoConfiguration.INSTANCE.getSigningProvider
        val signature = signingProvider.signExtended(txnBodyHash, privateKey.getKeyData)
        TxKeyWitness(signature, publicKey.getKeyData)
