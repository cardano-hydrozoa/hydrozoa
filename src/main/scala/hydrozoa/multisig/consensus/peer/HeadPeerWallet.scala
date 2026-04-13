package hydrozoa.multisig.consensus.peer

import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.cardano.wallet.*
import hydrozoa.multisig.consensus.ack.{AckBlock, AckId, AckNumber}
import hydrozoa.multisig.ledger.block.{Block, BlockHeader}
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import scala.language.implicitConversions
import scalus.cardano.ledger.{Transaction, VKeyWitness}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}

final class HeadPeerWallet(
    peerNum: HeadPeerNumber,
    walletModule: WalletModule,
    verificationKey: walletModule.VerificationKey,
    signingKey: walletModule.SigningKey
) {
    // extensional equality
    override def equals(obj: Any): Boolean = obj match {
        case other: HeadPeerWallet =>
            this.getPeerNum == other.getPeerNum
            && this.exportVerificationKey == other.exportVerificationKey
            && (this.signMsg(IArray.from(Seq(0.toByte))) sameElements other.signMsg(
              IArray.from(Seq(0.toByte))
            ))
        case _ => false
    }

    def signMsg(msg: IArray[Byte]): IArray[Byte] = walletModule.signMsg(msg, signingKey)

    def getPeerNum: HeadPeerNumber = peerNum

    private lazy val verificationKeysBytes =
        walletModule.exportVerificationKey(verificationKey)

    def exportVerificationKey: VerificationKey = verificationKeysBytes

    def mkVKeyWitness(tx: Transaction): VKeyWitness =
        walletModule.signTx(tx, verificationKey, signingKey)

    def mkTxSignature(tx: Transaction): TxSignature =
        TxSignature(mkVKeyWitness(tx))

    // TODO: do we want to keep in tests only?
    def signTx(txUnsigned: Transaction): Transaction =
        val keyWitness = mkVKeyWitness(txUnsigned)
        txUnsigned.attachVKeyWitnesses(List(keyWitness))

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

object HeadPeerWallet:
    def scalusWallet(
        peerNum: HeadPeerNumber,
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): HeadPeerWallet = HeadPeerWallet(
      peerNum = peerNum,
      walletModule = WalletModule.Scalus,
      verificationKey = verificationKey,
      signingKey = signingKey
    )
