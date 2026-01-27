package hydrozoa.multisig.consensus.peer

import hydrozoa.VerificationKeyBytes
import hydrozoa.lib.cardano.wallet.*
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.dapp.tx.TxSignature
import scala.language.implicitConversions
import scalus.cardano.ledger.{Transaction, VKeyWitness}

final class PeerWallet(
    peerNumber: PeerNumber,
    walletModule: WalletModule,
    verificationKey: walletModule.VerificationKey,
    signingKey: walletModule.SigningKey
):
    private lazy val verificationKeysBytes =
        walletModule.exportVerificationKeyBytes(verificationKey)

    def exportVerificationKeyBytes: VerificationKeyBytes = verificationKeysBytes

    def mkVKeyWitness(tx: Transaction): VKeyWitness =
        walletModule.signTx(tx, verificationKey, signingKey)

    def mkTxSignature(tx: Transaction): TxSignature =
        TxSignature(mkVKeyWitness(tx))

    def mkMinorHeaderSignature(
        headerSerialized: BlockHeader.Minor.Onchain.Serialized
    ): BlockHeader.Minor.HeaderSignature =
        BlockHeader.Minor.HeaderSignature(walletModule.signMsg(headerSerialized, signingKey))

    def getPeerNum: PeerNumber = peerNumber
