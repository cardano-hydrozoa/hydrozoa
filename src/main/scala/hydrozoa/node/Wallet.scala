package hydrozoa

import hydrozoa.l1.multisig.tx.{MultisigTx, MultisigTxTag}
import hydrozoa.node.state.WalletId

trait WalletModule:

    type VerificationKey
    type SigningKey

    def exportVerificationKeyBytes(publicKey: VerificationKey): VerificationKeyBytes

    def createTxKeyWitness[T <: MultisigTxTag](
        tx: MultisigTx[T],
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): TxKeyWitness

class Wallet(
    name: String,
    walletModule: WalletModule,
    verificationKey: walletModule.VerificationKey,
    signingKey: walletModule.SigningKey
):
    def getName: String = name
    def exportVerificationKeyBytes: VerificationKeyBytes = walletModule.exportVerificationKeyBytes(verificationKey)
    def createTxKeyWitness[T <: MultisigTxTag](tx: MultisigTx[T]): TxKeyWitness =
        walletModule.createTxKeyWitness(tx, verificationKey, signingKey)

    def getWalletId: WalletId = WalletId(getName)
