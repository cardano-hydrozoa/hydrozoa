package hydrozoa

import hydrozoa.node.state.WalletId
import scalus.cardano.ledger.VKeyWitness

trait WalletModule:

    type VerificationKey
    type SigningKey

    def exportVerificationKeyBytes(publicKey: VerificationKey): VerificationKeyBytes

    def createTxKeyWitness[L <: AnyLayer](
        tx: Tx[L],
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): VKeyWitness

    def createEd25519Signature(
        msg: IArray[Byte],
        signingKey: SigningKey
    ): Ed25519Signature

class Wallet(
    name: String,
    walletModule: WalletModule,
    verificationKey: walletModule.VerificationKey,
    signingKey: walletModule.SigningKey
):
    private lazy val verificationKeysBytes =
        walletModule.exportVerificationKeyBytes(verificationKey)

    def exportVerificationKeyBytes: VerificationKeyBytes = verificationKeysBytes

    def createTxKeyWitness[L <: AnyLayer](tx: Tx[L]): VKeyWitness =
        walletModule.createTxKeyWitness(tx, verificationKey, signingKey)

    def getWalletId: WalletId = WalletId(getName)

    def getName: String = name

    def createEd25519Signature(msg: IArray[Byte]): Ed25519Signature =
        walletModule.createEd25519Signature(msg, signingKey)
