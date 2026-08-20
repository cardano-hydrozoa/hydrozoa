package hydrozoa.lib.cardano.wallet

import scalus.cardano.ledger.{Transaction, VKeyWitness}
import scalus.crypto.ed25519.{SigningKey as ScalusSigningKey, VerificationKey as ScalusVerificationKey}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.JVMPlatformSpecific.signEd25519

/** A signing backend: the key types plus the Ed25519 signing primitives a
  * [[hydrozoa.multisig.consensus.peer.PeerWallet]] builds on.
  *
  * Cardano verifies witnesses with vanilla Ed25519 over the tx-body hash (`tx.id`), and the only
  * backend is [[WalletModule.Scalus]] — 32-byte vanilla Ed25519 keys that round-trip through the
  * private-config JSON codec. (A former BloxBean / Ed25519-BIP32 backend was removed: its 128-byte
  * extended signing keys cannot be expressed in the 32-byte codec, so serialized configs carried a
  * dummy key and were not runnable.)
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
