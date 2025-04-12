package hydrozoa.l2.consensus.network

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import hydrozoa.*
import hydrozoa.l1.multisig.tx.{MultisigTx, MultisigTxTag, PostDatedRefundTx}
import hydrozoa.l2.block.{Block, BlockHeader}
import hydrozoa.l2.consensus.network.transport.IncomingDispatcher
import hydrozoa.l2.ledger.{
    NonGenesisL2,
    SimpleOutput,
    SimpleTransaction,
    SimpleWithdrawal,
    UtxosSet
}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.Alice
import hydrozoa.node.state.WalletId
import ox.channels.ActorRef
import sttp.tapir.Schema

import scala.collection.mutable

// FIXME: revise return types?
trait HeadPeerNetwork {

    def setDispatcherActorRef(dispatcherRef: ActorRef[IncomingDispatcher]): Unit

    /** @return
      *   set iof verification keys for all known peers
      */
    def reqVerificationKeys(): Map[WalletId, VerificationKeyBytes]

    def reqInit(req: ReqInit): TxId

    def reqRefundLater(req: ReqRefundLater): PostDatedRefundTx

    def reqEventL2(req: ReqEventL2): Unit

    def reqMinor(block: Block): Set[AckMinor]

    // FIXME: remove utxosWithdrawn once we have block validation
    def reqMajor(block: Block, utxosWithdrawn: UtxosSet): Set[AckMajorCombined]

    // FIXME: remove utxosWithdrawn once we have block validation
    def reqFinal(block: Block, utxosWithdrawn: UtxosSet): Set[AckFinalCombined]
}

sealed trait Msg

sealed trait Req extends Msg:
    type ackType <: Ack
    type resultType

sealed trait Ack extends Msg

case class ReqVerKey() extends Req:
    type ackType = AckVerKey
    type resultType = Map[WalletId, VerificationKeyBytes]

given reqVerKeyCodec: JsonValueCodec[ReqVerKey] =
    JsonCodecMaker.make

given reqVerKeySchema: Schema[ReqVerKey] =
    Schema.derived[ReqVerKey]

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

case class ReqRefundLater(depositTx: TxL1, index: TxIx) extends Req:
    type ackType = AckRefundLater
    type resultType = PostDatedRefundTx

given reqRefundLaterCodec: JsonValueCodec[ReqRefundLater] =
    JsonCodecMaker.make

given reqRefundLaterSchema: Schema[ReqRefundLater] =
    Schema.derived[ReqRefundLater]

given txL1Schema: Schema[TxL1] =
    Schema.derived[TxL1]

case class ReqEventL2(eventL2: NonGenesisL2) extends Req:
    type ackType = AckUnit
    type resultType = Int

private class AckUnit extends Ack

object AckUnit extends AckUnit

given reqEventL2Codec: JsonValueCodec[NonGenesisL2] =
    JsonCodecMaker.make

given reqEventL2Schema: Schema[ReqEventL2] =
    Schema.derived[ReqEventL2]

given nonGenesisL2Schema: Schema[NonGenesisL2] =
    Schema.derived[NonGenesisL2]

given simpleTransactionSchema: Schema[SimpleTransaction] =
    Schema.derived[SimpleTransaction]

given utxoIdL2Schema: Schema[UtxoIdL2] =
    Schema.derived[UtxoIdL2]

given simpleOutputSchema: Schema[SimpleOutput] =
    Schema.derived[SimpleOutput]

given addressBechL2Schema: Schema[AddressBechL2] =
    Schema.derived[AddressBechL2]

given simpleWithdrawalSchema: Schema[SimpleWithdrawal] =
    Schema.derived[SimpleWithdrawal]

case class AckVerKey(
    peer: WalletId,
    verKey: VerificationKeyBytes
) extends Ack

given ackVerKeyCodec: JsonValueCodec[AckVerKey] =
    JsonCodecMaker.make

given verificationKeyBytesSchema: Schema[VerificationKeyBytes] =
    Schema.derived[VerificationKeyBytes]

given ackVerKeySchema: Schema[AckVerKey] =
    Schema.derived[AckVerKey]

case class AckInit(peer: WalletId, signature: TxKeyWitness) extends Ack

given ackInitCodec: JsonValueCodec[AckInit] =
    JsonCodecMaker.make

given ackInitSchema: Schema[AckInit] =
    Schema.derived[AckInit]

case class AckRefundLater(peer: WalletId, signature: TxKeyWitness) extends Ack

given ackRefundLaterCodec: JsonValueCodec[AckRefundLater] =
    JsonCodecMaker.make

given ackRefundLaterSchema: Schema[AckRefundLater] =
    Schema.derived[AckRefundLater]

// additional schemas
given walletIdSchema: Schema[WalletId] =
    Schema.derived[WalletId]

// FIXME: remove
given testPeerSchema: Schema[TestPeer] =
    Schema.derived[TestPeer]

given utxoIdL1Schema: Schema[UtxoIdL1] =
    Schema.derived[UtxoIdL1]

given txIdSchema: Schema[TxId] =
    Schema.derived[TxId]

given txIxSchema: Schema[TxIx] =
    Schema.derived[TxIx]

given txKeyWitnessSchema: Schema[TxKeyWitness] =
    Schema.derived[TxKeyWitness]

// old types to be upgraded

case class AckMinor(
    blockHeader: BlockHeader,
    signature: Unit,
    nextBlockFinal: Boolean
)

case class AckMajorCombined(
    blockHeader: BlockHeader,
    rollouts: Set[TxKeyWitness],
    settlement: TxKeyWitness,
    nextBlockFinal: Boolean
)

case class AckFinalCombined(
    blockHeader: BlockHeader,
    rollouts: Set[TxKeyWitness],
    finalization: TxKeyWitness
)
