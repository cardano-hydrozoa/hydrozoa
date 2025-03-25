package hydrozoa

import hydrozoa.l1.multisig.tx.{MultisigTx, MultisigTxTag}
import hydrozoa.node.state.PeerInfo

trait PeerKeyVault:

    type PeerPublicKey

    type PeerPrivateKey

    def peerPublicKeyBytes(publicKey: PeerPublicKey): PeerPublicKeyBytes

    def peerPrivateKeyBytes(privateKey: PeerPrivateKey): PeerPrivateKeyBytes

    def createTxKeyWitness[T <: MultisigTxTag](
        tx: MultisigTx[T],
        publicKey: PeerPublicKey,
        privateKey: PeerPrivateKey
    ): TxKeyWitness

class Peer(
    name: String,
    keyVault: PeerKeyVault,
    publicKey: keyVault.PeerPublicKey,
    privateKey: keyVault.PeerPrivateKey
):
    def getName: String = name
    def getKeyVault: PeerKeyVault = keyVault
    def getPublicKey: PeerPublicKeyBytes = keyVault.peerPublicKeyBytes(publicKey)
    def getPrivateKey: PeerPrivateKeyBytes = keyVault.peerPrivateKeyBytes(privateKey)
    def createTxKeyWitness[T <: MultisigTxTag](tx: MultisigTx[T]): TxKeyWitness =
        keyVault.createTxKeyWitness(tx, publicKey, privateKey)

    def mkPeerInfo: PeerInfo = PeerInfo(getName)
