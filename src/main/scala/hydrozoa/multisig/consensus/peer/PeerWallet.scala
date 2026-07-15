package hydrozoa.multisig.consensus.peer

import cats.data.Validated.{Invalid, Valid}
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.dummySigningKey
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.cardano.wallet.*
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.l1.tx.{EnrichedTx, TxSignature}
import io.circe.*
import io.circe.syntax.*
import scala.language.implicitConversions
import scalus.cardano.ledger.{Transaction, VKeyWitness}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}

/** A peer's signing wallet (ed25519, via a [[WalletModule]]). Head and coil peers share this
  * exactly: it holds only the keys and the signing primitives. Peer identity — which peer this is,
  * and whether it authors soft acks — lives in the config identity layer
  * ([[hydrozoa.config.node.owninfo.OwnPeerPrivate]]), not on the wallet.
  */
final class PeerWallet(
    walletModule: WalletModule,
    verificationKey: walletModule.VerificationKey,
    signingKey: walletModule.SigningKey
) {
    // extensional equality
    override def equals(obj: Any): Boolean = obj match {
        case other: PeerWallet =>
            this.exportVerificationKey == other.exportVerificationKey
            && (this.signMsg(IArray.from(Seq(0.toByte))) sameElements other.signMsg(
              IArray.from(Seq(0.toByte))
            ))
        case _ => false
    }

    def signMsg(msg: IArray[Byte]): IArray[Byte] = walletModule.signMsg(msg, signingKey)

    /** Sign `payload` as a CIP-30 `signData()` COSE_Sign1 with this peer's key, returning the
      * `(coseKey, coseSignature)` CBOR-hex pair.
      */
    def signCoseCip30(payload: Array[Byte]): Cip30SignedData =
        walletModule.signCoseCip30(payload, verificationKey, signingKey)

    private lazy val verificationKeysBytes =
        walletModule.exportVerificationKey(verificationKey)

    def exportVerificationKey: VerificationKey = verificationKeysBytes

    def mkVKeyWitness(tx: Transaction): VKeyWitness =
        walletModule.signTx(tx, verificationKey, signingKey)

    def mkTxSignature(tx: Transaction): TxSignature =
        TxSignature(mkVKeyWitness(tx))

    def signTx(txUnsigned: Transaction): Transaction =
        txUnsigned.attachVKeyWitnesses(List(mkVKeyWitness(txUnsigned)))

    def signTx[A <: EnrichedTx[A]](txUnsigned: A): A =
        txUnsigned.addSignatures(Set(mkVKeyWitness(txUnsigned.tx))) match {
            case Valid(tx) => tx
            // This should only happen if the public key and private key don't match
            case Invalid(e) => throw e.head
        }

    /** Sign arbitrary canonical bytes as a `HeaderSignature`. Used for two distinct sign targets:
      *
      *   - Soft-ack over the fast-cycle brief identity ([[BlockHeader.SignedDigest.Serialized]]).
      *   - Hard-ack signature over the standalone evacuation commitment
      *     ([[hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment.Onchain.Serialized]]).
      *
      * Both opaque types provide an implicit conversion to `IArray[Byte]`, so callers pass either.
      *
      * TODO: See
      * https://linear.app/gummiworm-labs/issue/GUM-141/sec-commitment-signatures-dedicated-type-rename-peerwalletmksignature
      */
    def mkHeaderSignature(
        headerSerialized: IArray[Byte]
    ): BlockHeader.HeaderSignature =
        BlockHeader.Minor.HeaderSignature(walletModule.signMsg(headerSerialized, signingKey))
}

object PeerWallet:
    def scalusWallet(
        verificationKey: VerificationKey,
        signingKey: SigningKey
    ): PeerWallet = PeerWallet(
      walletModule = WalletModule.Scalus,
      verificationKey = verificationKey,
      signingKey = signingKey
    )

    /** We can't directly access the signing key of a peer wallet, so we serialize a "dummy"
      * all-zeros signing key in its place.
      */
    given dummyPeerWalletEncoder: Encoder[PeerWallet] = new Encoder[PeerWallet] {
        override def apply(w: PeerWallet): Json = Json.obj(
          "verificationKey" -> w.exportVerificationKey.asJson(using given_Encoder_VerificationKey),
          "signingKey" -> dummySigningKey.asJson
        )
    }

    given peerWalletDecoder: Decoder[PeerWallet] = Decoder.instance(c =>
        for {
            vkey <- c.downField("verificationKey").as[VerificationKey]
            skey <- c.downField("signingKey").as[SigningKey]
        } yield PeerWallet.scalusWallet(verificationKey = vkey, signingKey = skey)
    )
