package hydrozoa.infra

import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.crypto.bip32.key.{HdPrivateKey, HdPublicKey}
import com.bloxbean.cardano.client.crypto.config.CryptoConfiguration
import com.bloxbean.cardano.client.transaction.util.TransactionBytes
import hydrozoa.*
import hydrozoa.l1.multisig.tx.{MultisigTx, MultisigTxTag}
import scalus.builtin.ByteString
import scalus.cardano.ledger.VKeyWitness

import scala.language.implicitConversions

object WalletModuleBloxbean extends WalletModule:

    override type VerificationKey = HdPublicKey
    override type SigningKey = HdPrivateKey

    override def exportVerificationKeyBytes(
        verificationKey: VerificationKey
    ): VerificationKeyBytes =
        VerificationKeyBytes(ByteString.fromArray(verificationKey.getKeyData))

    override def createTxKeyWitness[L <: AnyLayer](
        tx: Tx[L],
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): VKeyWitness =
        // See BloxBean's TransactionSigner.class
        val txBytes = TransactionBytes(tx.toCbor)
        val txnBodyHash = Blake2bUtil.blake2bHash256(txBytes.getTxBodyBytes)
        val signingProvider = CryptoConfiguration.INSTANCE.getSigningProvider
        val signature = signingProvider.signExtended(txnBodyHash, signingKey.getKeyData)
        VKeyWitness(
          signature = ByteString.fromArray(signature),
          vkey = ByteString.fromArray(verificationKey.getKeyData)
        )

    override def createEd25519Signature(
        msg: IArray[Byte],
        signingKey: SigningKey
    ): Ed25519Signature =
        val signingProvider = CryptoConfiguration.INSTANCE.getSigningProvider
        val signature = signingProvider.signExtended(
          IArray.genericWrapArray(msg).toArray,
          signingKey.getKeyData
        )
        Ed25519Signature(IArray.from(signature))
