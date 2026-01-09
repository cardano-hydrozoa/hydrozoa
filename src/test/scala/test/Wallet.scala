package test

import co.nstant.in.cbor.model.{Map, UnsignedInteger, Array as CborArray, ByteString as CborByteString}
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.crypto.api.SigningProvider
import com.bloxbean.cardano.client.crypto.bip32.key.{HdPrivateKey, HdPublicKey}
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import hydrozoa.multisig.protocol.types.AckBlock.HeaderSignature
import hydrozoa.{*, given}
import io.bullet.borer.Cbor
import scalus.builtin.ByteString
import scalus.cardano.ledger.{OriginalCborByteArray, Transaction, VKeyWitness}

import scala.language.implicitConversions

case class WalletId(name: String)

trait WalletModule:

    type VerificationKey
    type SigningKey

    def exportVerificationKeyBytes(publicKey: VerificationKey): VerificationKeyBytes

    def createTxKeyWitness(
        tx: Transaction,
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): VKeyWitness

    def createHeaderSignature(
        msg: IArray[Byte],
        signingKey: SigningKey
    ): HeaderSignature

    def validateHeaderSignature(
        msg: IArray[Byte],
        vk: VerificationKey,
        sig: HeaderSignature
    ): Boolean

class Wallet(
    name: String,
    walletModule: WalletModule,
    verificationKey: walletModule.VerificationKey,
    signingKey: walletModule.SigningKey
):
    private lazy val verificationKeysBytes =
        walletModule.exportVerificationKeyBytes(verificationKey)

    def exportVerificationKeyBytes: VerificationKeyBytes = verificationKeysBytes

    def createTxKeyWitness(tx: Transaction): VKeyWitness =
        walletModule.createTxKeyWitness(tx, verificationKey, signingKey)

    def getWalletId: WalletId = WalletId(getName)

    def getName: String = name

    def createHeaderSignature(msg: IArray[Byte]): HeaderSignature =
        walletModule.createHeaderSignature(msg, signingKey)

    def validateHeaderSignature(
        msg: IArray[Byte],
        sig: HeaderSignature
    ): Boolean =
        walletModule.validateHeaderSignature(msg, verificationKey, sig)

object WalletModuleBloxbean extends WalletModule:

    protected val signingProvider: SigningProvider = CryptoConfiguration.INSTANCE.getSigningProvider

    override type VerificationKey = HdPublicKey
    override type SigningKey = HdPrivateKey

    override def exportVerificationKeyBytes(
        verificationKey: VerificationKey
    ): VerificationKeyBytes =
        VerificationKeyBytes(ByteString.fromArray(verificationKey.getKeyData))

    override def createTxKeyWitness(
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

    override def createHeaderSignature(
        msg: IArray[Byte],
        signingKey: SigningKey
    ): HeaderSignature =
        val signature = signingProvider.signExtended(
          IArray.genericWrapArray(msg).toArray,
          signingKey.getKeyData
        )
        HeaderSignature(IArray.from(signature))

    override def validateHeaderSignature(
        msg: IArray[Byte],
        vk: HdPublicKey,
        sig: HeaderSignature
    ): Boolean =
        signingProvider.verify(
          IArray.genericWrapArray(sig.untagged).toArray,
          IArray.genericWrapArray(msg).toArray,
          vk.getKeyData
        )
