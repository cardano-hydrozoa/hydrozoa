package test

import co.nstant.in.cbor.model.{Map, UnsignedInteger, Array as CborArray, ByteString as CborByteString}
import com.bloxbean.cardano.client.common.cbor.CborSerializationUtil
import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.crypto.api.SigningProvider
import com.bloxbean.cardano.client.crypto.bip32.key.{HdPrivateKey, HdPublicKey}
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import hydrozoa.multisig.protocol.types.AckBlock.Ed25519Signature
import hydrozoa.{*, given}
import io.bullet.borer.Cbor
import scalus.builtin.ByteString
import scalus.cardano.ledger.{OriginalCborByteArray, Transaction, VKeyWitness}

import scala.language.implicitConversions

case class WalletId(name: String)

// Pure function to add a key witness to a transaction.
def addWitness(tx: Transaction, wit: VKeyWitness): Transaction =
    val txBytes = TransactionBytes(tx.toCbor)
    val witnessSetDI = CborSerializationUtil.deserialize(txBytes.getTxWitnessBytes)
    val witnessSetMap = witnessSetDI.asInstanceOf[Map]

    val vkWitnessArrayDI = witnessSetMap.get(UnsignedInteger(0))

    val vkWitnessArray: CborArray =
        if vkWitnessArrayDI != null then vkWitnessArrayDI.asInstanceOf[CborArray]
        else new CborArray

    if vkWitnessArrayDI == null then witnessSetMap.put(new UnsignedInteger(0), vkWitnessArray): Unit

    val vkeyWitness = new CborArray
    vkeyWitness.add(CborByteString(wit.vkey.bytes))
    vkeyWitness.add(CborByteString(wit.signature.bytes))

    vkWitnessArray.add(vkeyWitness)

    val txWitnessBytes = CborSerializationUtil.serialize(witnessSetMap, false)
    val txBytesSigned = txBytes.withNewWitnessSetBytes(txWitnessBytes).getTxBytes
    given OriginalCborByteArray = OriginalCborByteArray(txBytesSigned)
    Cbor.decode(txBytesSigned).to[Transaction].value

trait WalletModule:

    type VerificationKey
    type SigningKey

    def exportVerificationKeyBytes(publicKey: VerificationKey): VerificationKeyBytes

    def createTxKeyWitness(
        tx: Transaction,
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): VKeyWitness

    def createEd25519Signature(
        msg: IArray[Byte],
        signingKey: SigningKey
    ): Ed25519Signature

    def validateEd25519Signature(
        msg: IArray[Byte],
        vk: VerificationKey,
        sig: Ed25519Signature
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

    def createEd25519Signature(msg: IArray[Byte]): Ed25519Signature =
        walletModule.createEd25519Signature(msg, signingKey)

    def validateEd25519Signature(
        msg: IArray[Byte],
        sig: Ed25519Signature
    ): Boolean =
        walletModule.validateEd25519Signature(msg, verificationKey, sig)

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

    override def createEd25519Signature(
        msg: IArray[Byte],
        signingKey: SigningKey
    ): Ed25519Signature =
        val signature = signingProvider.signExtended(
          IArray.genericWrapArray(msg).toArray,
          signingKey.getKeyData
        )
        Ed25519Signature(IArray.from(signature))

    override def validateEd25519Signature(
        msg: IArray[Byte],
        vk: HdPublicKey,
        sig: Ed25519Signature
    ): Boolean =
        signingProvider.verify(
          IArray.genericWrapArray(sig.untagged).toArray,
          IArray.genericWrapArray(msg).toArray,
          vk.getKeyData
        )
