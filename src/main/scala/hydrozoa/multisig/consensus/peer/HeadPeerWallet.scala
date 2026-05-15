package hydrozoa.multisig.consensus.peer

import cats.data.Validated.{Invalid, Valid}
import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.dummySigningKey
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.lib.cardano.wallet.*
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckId, HardAckNumber, SoftAck}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber.given
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockHeader}
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, Tx, TxSignature}
import hydrozoa.multisig.ledger.stack.{Stack, StackEffects, StackNumber}
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
    // ============================================================

    /** Round-1 hard ack for a regular (non-initial) stack containing settlement or finalization.
      * Signs every per-effect tx body EXCEPT the first settlement/finalization (the round-2
      * unlock). Per partition position is mapped to list-index for now — single-partition stacks
      * are the common case; multi-partition exercises the same code paths with multiple map
      * entries.
      *
      * "First unlock" = the head of `settlements`; or `finalization` if `settlements` is empty. The
      * caller (StackComposer) must use this method only for stacks where the unlock exists, i.e.
      * [[StackEffects.Regular]] containing at least one settlement OR a finalization.
      */
    def mkHardAckRound1Regular(
        stack: Stack.Unsigned,
        hardAckNum: HardAckNumber,
        finalizationRequested: Boolean
    ): HardAck = {
        val effects = stack.effects match {
            case r: StackEffects.Regular => r
            case _: StackEffects.Initial =>
                throw new IllegalArgumentException(
                  "mkHardAckRound1Regular requires StackEffects.Regular"
                )
        }
        val firstUnlockIsSettlement = effects.settlements.nonEmpty

        // Skip the first settlement (it's the round-2 unlock) when present.
        val settlementsToSign = effects.settlements.zipWithIndex.collect {
            case (tx, i) if !(firstUnlockIsSettlement && i == 0) => (i, mkTxSignature(tx.tx))
        }.toMap
        val fallbacks = effects.fallbacks.zipWithIndex.map { case (tx, i) =>
            (i, mkTxSignature(tx.tx))
        }.toMap
        val rollouts = effects.rollouts.zipWithIndex.map { case (tx, i) =>
            ((0, i), mkTxSignature(tx.tx))
        }.toMap
        val refunds = effects.refunds.zipWithIndex.map { case (tx, i) =>
            ((0, i), mkTxSignature(tx.tx))
        }.toMap
        val evacCommits = effects.evacCommits.zipWithIndex.map { case (tx, i) =>
            (i, mkTxSignature(tx.tx))
        }.toMap
        // Skip the finalization only if it's the unlock (i.e. no settlements precede it).
        val finalization =
            if firstUnlockIsSettlement then
                effects.finalization.map(tx => 0 -> mkTxSignature(tx.tx)).toMap
            else Map.empty[Int, TxSignature]

        HardAck(
          ackId = HardAckId(peerNum, hardAckNum),
          stackNum = stack.brief.stackNum,
          payload = HardAck.Round1Payload.Regular(
            settlements = settlementsToSign,
            fallbacks = fallbacks,
            rollouts = rollouts,
            refunds = refunds,
            evacCommits = evacCommits,
            finalization = finalization
          ),
          finalizationRequested = finalizationRequested
        )
    }

    /** Round-1 hard ack for the initial (stack 0) flow: a single signature over the locally derived
      * fallback tx body.
      */
    def mkHardAckRound1Initial(
        stackNum: StackNumber,
        fallbackTx: FallbackTx,
        hardAckNum: HardAckNumber,
        finalizationRequested: Boolean
    ): HardAck = HardAck(
      ackId = HardAckId(peerNum, hardAckNum),
      stackNum = stackNum,
      payload = HardAck.Round1Payload.Initial(
        fallbackSig = mkTxSignature(fallbackTx.tx)
      ),
      finalizationRequested = finalizationRequested
    )

    /** Round-2 hard ack for a regular stack: signs the first settlement / finalization (the unlock
      * tx body). The unlock tx body is known at stack close, so the signature can be produced
      * upfront — the SlowConsensusActor withholds outbound broadcast until local round-1
      * confirmation.
      */
    def mkHardAckRound2Regular(
        stack: Stack.Round1Confirmed,
        hardAckNum: HardAckNumber,
        finalizationRequested: Boolean
    ): HardAck = {
        val effects = stack.unsigned.effects match {
            case r: StackEffects.Regular => r
            case _: StackEffects.Initial =>
                throw new IllegalArgumentException(
                  "mkHardAckRound2Regular requires StackEffects.Regular"
                )
        }
        val unlockSig: TxSignature = effects.settlements.headOption match {
            case Some(s) => mkTxSignature(s.tx)
            case None =>
                effects.finalization match {
                    case Some(f) => mkTxSignature(f.tx)
                    case None =>
                        throw new IllegalArgumentException(
                          "mkHardAckRound2Regular: stack has no settlement and no finalization"
                        )
                }
        }
        HardAck(
          ackId = HardAckId(peerNum, hardAckNum),
          stackNum = stack.unsigned.brief.stackNum,
          payload = HardAck.Round2Payload.Regular(firstUnlockSig = unlockSig),
          finalizationRequested = finalizationRequested
        )
    }

    /** Round-2 hard ack for the initial (stack 0) flow: signs the exogenous init tx body and
      * attaches the peer's individual key witnesses for any utxos the peer is funding from its
      * individual address (operator-supplied funding).
      */
    def mkHardAckRound2Initial(
        stack: Stack.Round1Confirmed,
        individualWitnesses: List[scalus.cardano.ledger.VKeyWitness],
        hardAckNum: HardAckNumber,
        finalizationRequested: Boolean
    ): HardAck = {
        val effects = stack.unsigned.effects match {
            case i: StackEffects.Initial => i
            case _: StackEffects.Regular =>
                throw new IllegalArgumentException(
                  "mkHardAckRound2Initial requires StackEffects.Initial (stack 0)"
                )
        }
        HardAck(
          ackId = HardAckId(peerNum, hardAckNum),
          stackNum = stack.unsigned.brief.stackNum,
          payload = HardAck.Round2Payload.Initial(
            initTxSig = mkTxSignature(effects.initializationTx.tx),
            individualWitnesses = individualWitnesses
          ),
          finalizationRequested = finalizationRequested
        )
    }

    /** Sole-round hard ack for a 1-phase minor-only stack: signs every effect (refund txs + the
      * partition's last evac commit). Used only when the stack has no settlement / no finalization
      * — i.e. the only effects are refunds and standalone evac commits.
      */
    def mkHardAckSole(
        stack: Stack.Unsigned,
        hardAckNum: HardAckNumber,
        finalizationRequested: Boolean
    ): HardAck = {
        val effects = stack.effects match {
            case r: StackEffects.Regular => r
            case _: StackEffects.Initial =>
                throw new IllegalArgumentException(
                  "mkHardAckSole requires StackEffects.Regular (minor-only)"
                )
        }
        val refunds = effects.refunds.zipWithIndex.map { case (tx, i) =>
            ((0, i), mkTxSignature(tx.tx))
        }.toMap
        val evacCommits = effects.evacCommits.zipWithIndex.map { case (tx, i) =>
            (i, mkTxSignature(tx.tx))
        }.toMap
        HardAck(
          ackId = HardAckId(peerNum, hardAckNum),
          stackNum = stack.brief.stackNum,
          payload = HardAck.SolePayload(refunds = refunds, evacCommits = evacCommits),
          finalizationRequested = finalizationRequested
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
