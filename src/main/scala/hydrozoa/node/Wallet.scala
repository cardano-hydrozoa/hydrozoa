package hydrozoa

import hydrozoa.l1.multisig.tx.{MultisigTx, MultisigTxTag, toL1Tx}
import hydrozoa.node.state.WalletId

trait WalletModule:

    type VerificationKey
    type SigningKey

    def exportVerificationKeyBytes(publicKey: VerificationKey): VerificationKeyBytes

    def createTxKeyWitness[L <: AnyLevel](
        tx: Tx[L],
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): TxKeyWitness

class Wallet(
    name: String,
    walletModule: WalletModule,
    verificationKey: walletModule.VerificationKey,
    signingKey: walletModule.SigningKey
):
    private lazy val verificationKeysBytes = walletModule.exportVerificationKeyBytes(verificationKey)
    def getName: String = name
    def exportVerificationKeyBytes: VerificationKeyBytes = verificationKeysBytes
    def createTxKeyWitness[L <: AnyLevel](tx: Tx[L]): TxKeyWitness =
        walletModule.createTxKeyWitness(tx, verificationKey, signingKey)
    def getWalletId: WalletId = WalletId(getName)
