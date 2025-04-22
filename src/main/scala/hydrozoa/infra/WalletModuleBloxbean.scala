package hydrozoa.infra

import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.crypto.bip32.key.{HdPrivateKey, HdPublicKey}
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import hydrozoa.l1.multisig.tx.{MultisigTx, MultisigTxTag, toL1Tx}
import hydrozoa.{WalletModule, SigningKeyBytes, VerificationKeyBytes, TxKeyWitness}

object WalletModuleBloxbean extends WalletModule:

    override type VerificationKey = HdPublicKey
    override type SigningKey = HdPrivateKey

    override def exportVerificationKeyBytes(verificationKey: VerificationKey): VerificationKeyBytes =
        VerificationKeyBytes(verificationKey.getKeyData)

    override def createTxKeyWitness[T <: MultisigTxTag](
        tx: MultisigTx[T],
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): TxKeyWitness =
        // See BloxBean's TransactionSigner.class
        val txBytes = TransactionBytes(tx.toL1Tx.bytes)
        val txnBodyHash = Blake2bUtil.blake2bHash256(txBytes.getTxBodyBytes)
        val signingProvider = CryptoConfiguration.INSTANCE.getSigningProvider
        val signature = signingProvider.signExtended(txnBodyHash, signingKey.getKeyData)
        TxKeyWitness(signature, verificationKey.getKeyData)
