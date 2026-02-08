package hydrozoa.multisig.consensus.peer

import hydrozoa.VerificationKeyBytes
import hydrozoa.lib.cardano.wallet.*
import hydrozoa.multisig.consensus.ack.{AckBlock, AckId, AckNumber}
import hydrozoa.multisig.ledger.block.{Block, BlockHeader}
import hydrozoa.multisig.ledger.dapp.tx.TxSignature
import scala.language.implicitConversions
import scalus.cardano.ledger.{Transaction, VKeyWitness}

final class HeadPeerWallet(
    peerNum: HeadPeerNumber,
    walletModule: WalletModule,
    verificationKey: walletModule.VerificationKey,
    signingKey: walletModule.SigningKey
) {
    def getPeerNum: HeadPeerNumber = peerNum

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

    def mkAcks(
        blockNext: Block.Unsigned.Next,
        finalizationRequested: Boolean
    ): List[AckBlock] = blockNext match {
        case block: Block.Unsigned.Minor =>
            List(mkAckMinor(block, finalizationRequested))
        case block: Block.Unsigned.Major =>
            List(mkAckMajor1(block, finalizationRequested), mkAckMajor2(block))
        case block: Block.Unsigned.Final =>
            List(mkAckFinal1(block), mkAckFinal2(block))
    }

    def mkAckMinor(
        block: Block.Unsigned.Minor,
        finalizationRequested: Boolean
    ): AckBlock.Minor = {
        import block.*
        AckBlock.Minor(
          ackId = AckId(peerNum, AckNumber.neededToConfirm(header)),
          blockNum = blockNum,
          header = mkMinorHeaderSignature(headerSerialized),
          postDatedRefundTxs = postDatedRefundTxs.map(_.tx).map(mkTxSignature),
          finalizationRequested = finalizationRequested
        )
    }

    def mkAckMajor1(
        block: Block.Unsigned.Major,
        finalizationRequested: Boolean
    ): AckBlock.Major1 = {
        import block.*
        AckBlock.Major1(
          ackId = AckId(peerNum, AckNumber.neededToConfirm(header).decrement),
          blockNum = blockNum,
          fallbackTx = mkTxSignature(fallbackTx.tx),
          rolloutTxs = rolloutTxs.map(_.tx).map(mkTxSignature),
          postDatedRefundTxs = postDatedRefundTxs.map(_.tx).map(mkTxSignature),
          finalizationRequested = finalizationRequested
        )
    }

    def mkAckMajor2(block: Block.Unsigned.Major): AckBlock.Major2 = {
        import block.*
        AckBlock.Major2(
          ackId = AckId(peerNum, AckNumber.neededToConfirm(header)),
          blockNum = blockNum,
          settlementTx = mkTxSignature(settlementTx.tx)
        )
    }

    def mkAckFinal1(block: Block.Unsigned.Final): AckBlock.Final1 = {
        import block.*
        AckBlock.Final1(
          ackId = AckId(peerNum, AckNumber.neededToConfirm(header).decrement),
          blockNum = blockNum,
          rolloutTxs = rolloutTxs.map(_.tx).map(mkTxSignature)
        )
    }

    def mkAckFinal2(block: Block.Unsigned.Final): AckBlock.Final2 = {
        import block.*
        AckBlock.Final2(
          ackId = AckId(peerNum, AckNumber.neededToConfirm(header)),
          blockNum = blockNum,
          finalizationTx = mkTxSignature(finalizationTx.tx)
        )
    }
}
