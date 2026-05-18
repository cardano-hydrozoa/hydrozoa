package hydrozoa.multisig.consensus.peer

import cats.data.Validated.{Invalid, Valid}
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.dummySigningKey
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.cardano.wallet.*
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, SoftAck, StackEffectsSigningInputs}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber.given
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockHeader}
import hydrozoa.multisig.ledger.l1.tx.{Tx, TxSignature}
import hydrozoa.multisig.ledger.stack.StackNumber
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

    def mkHeaderSignature(
        headerSerialized: BlockHeader.Minor.Onchain.Serialized
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

    // ============================================================
    // Hard-ack signing — slow consensus (see consensus/slow-consensus in the spec)
    //
    // The wallet is a pure mapper: it signs exactly the material the caller hands it and
    // never inspects a Stack / StackEffects. The StackComposer does all the walking and
    // hands over a HardAck.SigningInputs.* struct.
    // ============================================================

    /** Round-1 hard ack for the initial (stack 0) flow: a single signature over the locally derived
      * fallback tx body.
      */
    def mkHardAckRound1Initial(
        stackNum: StackNumber,
        hardAckNum: HardAckNumber,
        in: HardAck.SigningInputs.Round1Initial
    ): HardAck = HardAck(
      ackId = HardAckId(peerNum, hardAckNum),
      stackNum = stackNum,
      payload = HardAck.Round1Payload.Initial(fallbackSig = mkTxSignature(in.fallback))
    )

    /** Round-2 hard ack for the initial (stack 0) flow. Two distinct witness roles, both produced
      * here (collected across peers during stack-0 consensus):
      *   - `initTxSig` — this peer's contribution to the head **multisig** native script (minting
      *     the head tokens); always present.
      *   - `individualWitnesses` — a plain `VKeyWitness` for the init tx, attached **iff** the init
      *     tx actually spends an input at this peer's own individual address. If it does not, the
      *     list MUST stay empty: Cardano L1 rejects a tx carrying a vkey witness it does not need.
      *     The predicate is the shared, deterministic
      *     [[StackEffectsSigningInputs.spendsFromIndividualAddress]] (the verifier enforces the
      *     same iff rule on remote peers).
      */
    def mkHardAckRound2Initial(
        stackNum: StackNumber,
        hardAckNum: HardAckNumber,
        in: HardAck.SigningInputs.Round2Initial
    ): HardAck = HardAck(
      ackId = HardAckId(peerNum, hardAckNum),
      stackNum = stackNum,
      payload = HardAck.Round2Payload.Initial(
        initTxSig = mkTxSignature(in.initTx.tx),
        individualWitnesses =
            if StackEffectsSigningInputs
                    .spendsFromIndividualAddress(in.initTx, exportVerificationKey)
            then List(mkVKeyWitness(in.initTx.tx))
            else Nil
      )
    )

    /** Round-1 hard ack for a regular (non-initial) stack. The caller has already removed the
      * round-2 unlock (first settlement / finalization) from `in`. Each tx body is signed with
      * `mkTxSignature`; each evac-commit *header* (KZG lives on the block header) is signed with
      * `mkHeaderSignature`.
      */
    def mkHardAckRound1Regular(
        stackNum: StackNumber,
        hardAckNum: HardAckNumber,
        in: HardAck.SigningInputs.Round1Regular
    ): HardAck = HardAck(
      ackId = HardAckId(peerNum, hardAckNum),
      stackNum = stackNum,
      payload = HardAck.Round1Payload.Regular(
        settlements = in.settlements.view.mapValues(mkTxSignature).toMap,
        fallbacks = in.fallbacks.view.mapValues(mkTxSignature).toMap,
        rollouts = in.rollouts.view.mapValues(mkTxSignature).toMap,
        refunds = in.refunds.view.mapValues(mkTxSignature).toMap,
        evacCommit = in.evacCommit.map((bn, bytes) => (bn, mkHeaderSignature(bytes))),
        finalization = in.finalization.map(mkTxSignature)
      )
    )

    /** Round-2 hard ack for a regular stack: signs the first settlement / finalization (the unlock
      * tx body). The unlock tx body is known at stack close, so the signature is produced upfront —
      * the SlowConsensusActor withholds outbound broadcast until local round-1 confirmation.
      */
    def mkHardAckRound2Regular(
        stackNum: StackNumber,
        hardAckNum: HardAckNumber,
        in: HardAck.SigningInputs.Round2Regular
    ): HardAck = HardAck(
      ackId = HardAckId(peerNum, hardAckNum),
      stackNum = stackNum,
      payload = HardAck.Round2Payload.Regular(firstUnlockSig = mkTxSignature(in.unlock))
    )

    /** Sole-round hard ack for a 1-phase minor-only stack: signs every refund tx body + the single
      * standalone evac commitment's block header.
      */
    def mkHardAckSole(
        stackNum: StackNumber,
        hardAckNum: HardAckNumber,
        in: HardAck.SigningInputs.Sole
    ): HardAck = HardAck(
      ackId = HardAckId(peerNum, hardAckNum),
      stackNum = stackNum,
      payload = HardAck.SolePayload(
        refunds = in.refunds.view.mapValues(mkTxSignature).toMap,
        evacCommit = (in.evacCommit._1, mkHeaderSignature(in.evacCommit._2))
      )
    )
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
