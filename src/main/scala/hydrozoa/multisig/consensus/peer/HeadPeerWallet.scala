package hydrozoa.multisig.consensus.peer

import cats.data.Validated.{Invalid, Valid}
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.dummySigningKey
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.cardano.wallet.*
import hydrozoa.multisig.consensus.ack.SoftAck
import hydrozoa.multisig.consensus.peer.HeadPeerNumber.given
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockHeader}
import hydrozoa.multisig.ledger.l1.tx.{Tx, TxSignature}
import io.circe.*
import io.circe.syntax.*
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

    def signTx[A <: Tx[A]](txUnsigned: A): A = {
        val vKeyWitness = mkVKeyWitness(txUnsigned.tx)
        txUnsigned.addSignatures(Set(vKeyWitness)) match {
            case Valid(tx) => tx
            // This should only happen if the public key and private key don't match
            case Invalid(e) => throw e.head
        }
    }

    /** Sign arbitrary canonical bytes as a `HeaderSignature`. Used for two distinct sign targets:
      *
      *   - Soft-ack over the fast-cycle brief identity ([[BlockHeader.SignedDigest.Serialized]]).
      *   - Hard-ack signature over the standalone evacuation commitment
      *     ([[hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment.Onchain.Serialized]]).
      *
      * Both opaque types provide an implicit conversion to `IArray[Byte]`, so callers pass either.
      */
    def mkHeaderSignature(
        headerSerialized: IArray[Byte]
    ): BlockHeader.HeaderSignature =
        BlockHeader.Minor.HeaderSignature(walletModule.signMsg(headerSerialized, signingKey))

    /** Sign the block brief's canonical signing bytes to produce this peer's soft ack. One ack per
      * peer per block, regardless of block type. See `consensus/fast-consensus` in the spec.
      */
    def mkSoftAck(
        brief: BlockBrief.Next,
        finalizationRequested: Boolean
    ): SoftAck = SoftAck(
      peerNum = peerNum,
      blockNum = brief.blockNum,
      header = mkHeaderSignature(brief.header.signingBytes),
      finalizationRequested = finalizationRequested
    )

    // Hard-ack signing material is produced by StackComposer (which walks the partition-indexed
    // StackEffects and applies the shared PartitionEffects.unlock rule). The wallet only exposes
    // the signing primitives above — mkTxSignature / mkVKeyWitness / mkHeaderSignature — plus
    // exportVerificationKey for the Initial individual-witness iff rule
    // (StackEffects.spendsFromIndividualAddress).
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

    /** We can't directly access the signing key of a head peer wallet, so we use a "dummy"
      * all-zeros signing key instead
      * @return
      */
    given dummyHeadPeerWalletEncoder: Encoder[HeadPeerWallet] = new Encoder[HeadPeerWallet] {
        override def apply(w: HeadPeerWallet): Json = Json.obj(
          "peerNum" -> w.getPeerNum.asJson,
          "verificationKey" -> w.exportVerificationKey.asJson(using given_Encoder_VerificationKey),
          "signingKey" -> dummySigningKey.asJson
        )
    }

    given headPeerWalletDecoder: Decoder[HeadPeerWallet] = Decoder.instance(c =>
        for {
            peerNum <- c.downField("peerNum").as[HeadPeerNumber]
            vkey <- c.downField("verificationKey").as[VerificationKey]
            skey <- c.downField("signingKey").as[SigningKey]
            w = HeadPeerWallet.scalusWallet(
              peerNum = peerNum,
              verificationKey = vkey,
              signingKey = skey
            )
        } yield w
    )
