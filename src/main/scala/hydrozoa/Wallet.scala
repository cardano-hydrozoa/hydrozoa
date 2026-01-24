package hydrozoa

import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.crypto.api.SigningProvider
import com.bloxbean.cardano.client.crypto.bip32.key.{HdPrivateKey, HdPublicKey}
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import hydrozoa.multisig.protocol.types.AckBlock.HeaderSignature
import scala.language.implicitConversions
import scalus.builtin.ByteString
import scalus.builtin.JVMPlatformSpecific.signEd25519
import scalus.cardano.ledger.{Transaction, VKeyWitness}

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

// TODO: use opaque type?
case class WalletId(name: String)

class Wallet(
    name: String,
    walletModule: WalletModule,
    verificationKey: walletModule.VerificationKey,
    signingKey: walletModule.SigningKey
):
    private lazy val verificationKeysBytes =
        walletModule.exportVerificationKeyBytes(verificationKey)

    def exportVerificationKeyBytes: VerificationKeyBytes = verificationKeysBytes

    // TODO: it might be useful to have a method that returns the signature only
    def signTx(tx: Transaction): VKeyWitness =
        walletModule.signTx(tx, verificationKey, signingKey)

    // TODO: use more abstract return type
    def signMsg(msg: IArray[Byte]): HeaderSignature =
        walletModule.signMsg(msg, signingKey)

    def getWalletId: WalletId = WalletId(getName)

    def getName: String = name

trait WalletModule:

    type VerificationKey
    type SigningKey

    def exportVerificationKeyBytes(publicKey: VerificationKey): VerificationKeyBytes

    def signTx(
        tx: Transaction,
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): VKeyWitness

    def signMsg(
        msg: IArray[Byte],
        signingKey: SigningKey
    ): HeaderSignature

// TODO: this may be moved to test
object WalletModuleBloxbean extends WalletModule:

    protected val signingProvider: SigningProvider = CryptoConfiguration.INSTANCE.getSigningProvider

    override type VerificationKey = HdPublicKey
    override type SigningKey = HdPrivateKey

    override def exportVerificationKeyBytes(
        verificationKey: VerificationKey
    ): VerificationKeyBytes =
        VerificationKeyBytes(ByteString.fromArray(verificationKey.getKeyData))

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
    ): HeaderSignature =
        val signature = signingProvider.signExtended(
          IArray.genericWrapArray(msg).toArray,
          signingKey.getKeyData
        )
        HeaderSignature(IArray.from(signature))

object WalletModuleScalus extends WalletModule:
    override type VerificationKey = ByteString
    override type SigningKey = ByteString

    override def exportVerificationKeyBytes(publicKey: VerificationKey): VerificationKeyBytes =
        VerificationKeyBytes(publicKey)

    override def signTx(
        tx: Transaction,
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): VKeyWitness = VKeyWitness(verificationKey, signEd25519(signingKey, tx.id))

    override def signMsg(
        msg: IArray[Byte],
        signingKey: SigningKey
    ): HeaderSignature = {
        val msgBs = ByteString.fromArray(IArray.genericWrapArray(msg).toArray)
        HeaderSignature.apply(IArray.from(signEd25519(signingKey, msgBs).bytes))
    }
