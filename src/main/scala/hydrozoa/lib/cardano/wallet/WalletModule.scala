package hydrozoa.lib.cardano.wallet

import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.crypto.api.SigningProvider
import com.bloxbean.cardano.client.crypto.bip32.key.{HdPrivateKey, HdPublicKey}
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import scala.language.implicitConversions
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.JVMPlatformSpecific.signEd25519
import scalus.cardano.ledger.{Transaction, VKeyWitness}
import scalus.crypto.ed25519.{SigningKey as ScalusSigningKey, VerificationKey as ScalusVerificationKey}

/*
Cardano adopted BIP-32: Hierarchical Deterministic Wallets in the form of Ed25529-BIP32:
https://github.com/input-output-hk/adrestia/raw/bdf00e4e7791d610d273d227be877bc6dd0dbcfb/user-guide/static/Ed25519_BIP.pdf

Java implementation (copied to BB):
https://github.com/semuxproject/semux-core/tree/master/src/main/java/org/semux/crypto/bip32

bouncycastle/scalus: doesn't support Ed25529-BIP32, only vanilla Ed25529.

BIP-32 overview:

Seed (32 bytes)
    ↓ (SHA-512 hash)
Extended Key (64 bytes)
    ↓
[scalar (32) | prefix (32)] - both parts are used during signing, seed has some bits changed.


Regular Verification Key (vk):
  [public_key (32)] = 32 bytes
    ↓
Extended Verification Key (xvk):
  [public_key (32) | chaincode (32)] = 64 bytes
    ↓
Extended Signing Key (xsk):
  [private_key (64) | public_key (32) | chaincode (32)] = 128 bytes

// For signature verification only first 32 bytes of xvk are used:
val vkeyForVerification = extendedVkey.take(32)  // First 32 bytes

The public key portion is identical whether it's:
 * Standalone (32 bytes)
 * Inside an extended verification key (first 32 of 64 bytes)
 * Inside an extended signing key (bytes 64-95 of 128 bytes)

 */

trait WalletModule:

    type VerificationKey
    type SigningKey

    def exportVerificationKey(publicKey: VerificationKey): ScalusVerificationKey

    def signTx(
        tx: Transaction,
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): VKeyWitness

    def signMsg(
        msg: IArray[Byte],
        signingKey: SigningKey
    ): IArray[Byte]

object WalletModule {
    // TODO: this may be moved to test
    object BloxBean extends WalletModule:

        protected val signingProvider: SigningProvider =
            CryptoConfiguration.INSTANCE.getSigningProvider

        override type VerificationKey = HdPublicKey
        override type SigningKey = HdPrivateKey

        override def exportVerificationKey(
            verificationKey: VerificationKey
        ): ScalusVerificationKey =
            ScalusVerificationKey.unsafeFromByteString(
              ByteString.fromArray(verificationKey.getKeyData)
            )

        override def signTx(
            tx: Transaction,
            verificationKey: VerificationKey,
            signingKey: SigningKey
        ): VKeyWitness =
            // See BloxBean's TransactionSigner.class
            val txBytes = TransactionBytes(tx.toCbor)
            val txnBodyHash = Blake2bUtil.blake2bHash256(txBytes.getTxBodyBytes)
            val signature = signingProvider.signExtended(txnBodyHash, signingKey.getKeyData)
            VKeyWitness(
              signature = ByteString.fromArray(signature),
              vkey = ByteString.fromArray(verificationKey.getKeyData)
            )

        override def signMsg(
            msg: IArray[Byte],
            signingKey: SigningKey
        ): IArray[Byte] =
            val signature = signingProvider.signExtended(
              IArray.genericWrapArray(msg).toArray,
              signingKey.getKeyData
            )
            IArray.from(signature)

    object Scalus extends WalletModule:
        override type VerificationKey = ScalusVerificationKey
        override type SigningKey = ScalusSigningKey

        override def exportVerificationKey(publicKey: VerificationKey): ScalusVerificationKey =
            publicKey

        override def signTx(
            tx: Transaction,
            verificationKey: VerificationKey,
            signingKey: SigningKey
        ): VKeyWitness = VKeyWitness(verificationKey, signEd25519(signingKey, tx.id))

        override def signMsg(
            msg: IArray[Byte],
            signingKey: SigningKey
        ): IArray[Byte] = {
            val msgBs = ByteString.fromArray(IArray.genericWrapArray(msg).toArray)
            IArray.from(signEd25519(signingKey, msgBs).bytes)
        }
}
