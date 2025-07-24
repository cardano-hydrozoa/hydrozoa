package hydrozoa.l2.consensus.network

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, readFromArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import hydrozoa.*
import hydrozoa.l1.multisig.tx.PostDatedRefundTx
import hydrozoa.l2.block.{Block, BlockBody, BlockHeader, BlockTypeL2}
import hydrozoa.l2.consensus.ConsensusDispatcher
import hydrozoa.l2.ledger.*
import hydrozoa.node.TestPeer
import hydrozoa.node.state.WalletId
import ox.channels.ActorRef
import scalus.builtin.ByteString
import scalus.cardano.ledger.{Transaction, TransactionInput, TransactionOutput}
import sttp.tapir.Schema
import sttp.tapir.generic.auto.*

import scala.collection.mutable

// FIXME: revise return types?
trait HeadPeerNetwork {

    def setDispatcher(dispatcherRef: ActorRef[ConsensusDispatcher]): Unit

    /** @return
      *   set iof verification keys for all known peers
      */
    def reqVerificationKeys(): Map[WalletId, VerificationKeyBytes]

    def reqInit(req: ReqInit): TxId

    def reqRefundLater(req: ReqRefundLater): PostDatedRefundTx

    def reqEventL2(req: ReqEventL2): Unit

    def reqMinor(req: ReqMinor): Unit

    def reqMajor(req: ReqMajor): Unit

    def reqFinal(req: ReqFinal): Unit
}

/** ------------------------------------------------------------------------------------------
  * Consensus messages TODO: move out?
  * ------------------------------------------------------------------------------------------
  */

sealed trait Msg

sealed trait Req extends Msg:
    type ackType <: Ack
    type resultType

sealed trait Ack extends Msg

/** ------------------------------------------------------------------------------------------
  * ReqVerKey
  * ------------------------------------------------------------------------------------------
  */
case class ReqVerKey() extends Req:
    type ackType = AckVerKey
    type resultType = Map[WalletId, VerificationKeyBytes]

given reqVerKeyCodec: JsonValueCodec[ReqVerKey] =
    JsonCodecMaker.make

given reqVerKeySchema: Schema[ReqVerKey] =
    Schema.derived[ReqVerKey]

// ReqVerKey aux schemas
given walletIdSchema: Schema[WalletId] =
    Schema.derived[WalletId]

/** ------------------------------------------------------------------------------------------
  * AckVerKey
  * ------------------------------------------------------------------------------------------
  */

case class AckVerKey(
    peer: WalletId,
    verKey: VerificationKeyBytes
) extends Ack

given ackVerKeyCodec: JsonValueCodec[AckVerKey] =
    JsonCodecMaker.make

given ackVerKeySchema: Schema[AckVerKey] =
    Schema.derived[AckVerKey]

// AckVerKey aux schemas
given verificationKeyBytesSchema: Schema[VerificationKeyBytes] =
    Schema.derived[VerificationKeyBytes]

/** ------------------------------------------------------------------------------------------
  * ReqInit
  * ------------------------------------------------------------------------------------------
  */

case class ReqInit(
    initiator: WalletId,
    otherHeadPeers: Set[WalletId],
    seedUtxoId: UtxoIdL1,
    treasuryCoins: Long
) extends Req:
    type ackType = AckInit
    type resultType = TxId

given reqInitCodec: JsonValueCodec[ReqInit] =
    JsonCodecMaker.make

given reqInitSchema: Schema[ReqInit] =
    Schema.derived[ReqInit]

// ReqInit aux schemas
given utxoIdL1Schema: Schema[UtxoIdL1] =
    Schema.derived[UtxoIdL1]

given txIdSchema: Schema[TxId] =
    Schema.derived[TxId]

given txIxSchema: Schema[TxIx] =
    Schema.derived[TxIx]

given txKeyWitnessSchema: Schema[TxKeyWitness] =
    Schema.derived[TxKeyWitness]

/** ------------------------------------------------------------------------------------------
  * AckInit
  * ------------------------------------------------------------------------------------------
  */

case class AckInit(peer: WalletId, signature: TxKeyWitness) extends Ack

given ackInitCodec: JsonValueCodec[AckInit] =
    JsonCodecMaker.make

given ackInitSchema: Schema[AckInit] =
    Schema.derived[AckInit]

/** ------------------------------------------------------------------------------------------
  * ReqRefundLater
  * ------------------------------------------------------------------------------------------
  */

case class ReqRefundLater(depositTx: TxL1, index: TxIx) extends Req:
    type ackType = AckRefundLater
    type resultType = PostDatedRefundTx

given reqRefundLaterCodec: JsonValueCodec[ReqRefundLater] =
    JsonCodecMaker.make

given reqRefundLaterSchema: Schema[ReqRefundLater] =
    Schema.derived[ReqRefundLater]

given txL1Schema: Schema[TxL1] =
    Schema.derived[TxL1]

/** ------------------------------------------------------------------------------------------
  * AckRefundLater
  * ------------------------------------------------------------------------------------------
  */

case class AckRefundLater(peer: WalletId, signature: TxKeyWitness) extends Ack

given ackRefundLaterCodec: JsonValueCodec[AckRefundLater] =
    JsonCodecMaker.make

given ackRefundLaterSchema: Schema[AckRefundLater] =
    Schema.derived[AckRefundLater]

/** ------------------------------------------------------------------------------------------
  * ReqEventL2
  * ------------------------------------------------------------------------------------------
  */

case class ReqEventL2(eventL2: L2Event) extends Req:
    type ackType = AckUnit
    type resultType = Unit

given reqEventL2Codec: JsonValueCodec[L2Event] =
    JsonCodecMaker.make

given reqEventL2Schema: Schema[ReqEventL2] =
    Schema.derived[ReqEventL2]

/* ReqEventK2 aux schemas */

given nonGenesisL2Schema: Schema[L2Event] =
    Schema.derived[L2Event]

given l2EventTransactionSchema: Schema[L2EventTransaction] =
    Schema.derived[L2EventTransaction]

given utxoIdL2Schema: Schema[UtxoIdL2] =
    Schema.derived[UtxoIdL2]

given simpleOutputSchema: Schema[OutputNoTokens[L2]] =
    Schema.derived[OutputNoTokens[L2]]

given addressBechL2Schema: Schema[AddressBechL2] =
    Schema.derived[AddressBechL2]

given l2EventWithdrawalSchema: Schema[L2EventWithdrawal] =
    Schema.derived[L2EventWithdrawal]

/** ------------------------------------------------------------------------------------------
  * AckUnit
  * ------------------------------------------------------------------------------------------
  */

private case class AckUnit() extends Ack

object AckUnit extends AckUnit

/** ------------------------------------------------------------------------------------------
  * ReqMinor
  * ------------------------------------------------------------------------------------------
  */

case class ReqMinor(block: Block) extends Req:
    type ackType = AckMinor
    type resultType = Unit

given reqMinorCodec: JsonValueCodec[ReqMinor] =
    JsonCodecMaker.make

given reqMinorSchema: Schema[ReqMinor] =
    Schema.derived[ReqMinor]

/* ReqMinor aux codecs */

given blockCodec: JsonValueCodec[Block] =
    JsonCodecMaker.make

/* ReqMinor aux schemas */

given blockSchema: Schema[Block] =
    Schema.derived[Block]

given blockHeaderSchema: Schema[BlockHeader] =
    Schema.derived[BlockHeader]

given blockTypeL2Schema: Schema[BlockTypeL2] =
    Schema.derived[BlockTypeL2]

given blockBodySchema: Schema[BlockBody] =
    Schema.derived[BlockBody]

given txIdNonGenesisL2EventLabelPairSchema: Schema[(TxId, L2EventLabel)] =
    Schema.derived[(TxId, L2EventLabel)]

given nonGenesisL2EventLabelSchema: Schema[L2EventLabel] =
    Schema.derived[L2EventLabel]

/** ------------------------------------------------------------------------------------------
  * AckMinor
  * ------------------------------------------------------------------------------------------
  */

case class AckMinor(
    peer: WalletId,
    signature: Ed25519SignatureHex,
    nextBlockFinal: Boolean
) extends Ack

given ackMinorCodec: JsonValueCodec[AckMinor] =
    JsonCodecMaker.make

given ackMinorSchema: Schema[AckMinor] =
    Schema.derived[AckMinor]

given ed25519SignatureHexSchema: Schema[Ed25519SignatureHex] =
    Schema.derived[Ed25519SignatureHex]

/** ------------------------------------------------------------------------------------------
  * ReqMajor
  * ------------------------------------------------------------------------------------------
  */

case class ReqMajor(block: Block) extends Req:
    type ackType = AckMajor | AckMajor2
    type resultType = Unit

given reqMajorCodec: JsonValueCodec[ReqMajor] =
    JsonCodecMaker.make

given reqMajorSchema: Schema[ReqMajor] =
    Schema.derived[ReqMajor]

/** ------------------------------------------------------------------------------------------
  * AckMajor, AckMajor2
  * ------------------------------------------------------------------------------------------
  */

case class AckMajor(
    peer: WalletId,
    rollouts: Seq[TxKeyWitness],
    postDatedTransition: TxKeyWitness
) extends Ack

given ackMajorCodec: JsonValueCodec[AckMajor] =
    JsonCodecMaker.make

given ackMajorSchema: Schema[AckMajor] =
    Schema.derived[AckMajor]

case class AckMajor2(
    peer: WalletId,
    settlement: TxKeyWitness,
    nextBlockFinal: Boolean
) extends Ack

given ackMajor2Codec: JsonValueCodec[AckMajor2] =
    JsonCodecMaker.make

given ackMajor2Schema: Schema[AckMajor2] =
    Schema.derived[AckMajor2]

/** ------------------------------------------------------------------------------------------
  * ReqFinal
  * ------------------------------------------------------------------------------------------
  */

case class ReqFinal(block: Block) extends Req:
    type ackType = AckFinal | AckFinal2
    type resultType = Unit

given reqFinalCodec: JsonValueCodec[ReqFinal] =
    JsonCodecMaker.make

given reqFinalSchema: Schema[ReqFinal] =
    Schema.derived[ReqFinal]

/** ------------------------------------------------------------------------------------------
  * AckFinal, AckFinal2
  * ------------------------------------------------------------------------------------------
  */

case class AckFinal(
    peer: WalletId,
    rollouts: Seq[TxKeyWitness]
) extends Ack

given ackFinalCodec: JsonValueCodec[AckFinal] =
    JsonCodecMaker.make

given ackFinalSchema: Schema[AckFinal] =
    Schema.derived[AckFinal]

case class AckFinal2(
    peer: WalletId,
    finalization: TxKeyWitness
) extends Ack

given ackFinal2Codec: JsonValueCodec[AckFinal2] =
    JsonCodecMaker.make

given ackFinal2Schema: Schema[AckFinal2] =
    Schema.derived[AckFinal2]

// FIXME: remove, currently used in ReqAux
given testPeerSchema: Schema[TestPeer] =
    Schema.derived[TestPeer]

/////////////////////////////////////////
// Scala schemas/json codecs; should be supereded by CIP-0116 compliant instances

// N.B.: From the docs:
//   "As a fallback, you can also always use Schema.string[T] or Schema.binary[T], however this will provide only basic
//   documentation, and won’t perform any validation."
// Given that these are bytestrings, I think the binary schema is probably sufficient.
given byteStringSchema: Schema[ByteString] =
    Schema.binary[ByteString]

// N.B.: adapted from Claude Sonnet 4, 2025-07-24
given byteStringCodec: JsonValueCodec[ByteString] = new JsonValueCodec[ByteString] {
    override def decodeValue(in: JsonReader, default: ByteString): ByteString = {
        val bytes = in.readBase64AsBytes(null)
        ByteString.fromArray(bytes) 
    }

    override def encodeValue(x: ByteString, out: JsonWriter): Unit = {
        out.writeBase64Val(x.bytes, false) 
    }

    override val nullValue: ByteString = ByteString.empty
}