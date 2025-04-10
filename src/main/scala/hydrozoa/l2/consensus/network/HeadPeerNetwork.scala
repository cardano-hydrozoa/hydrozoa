package hydrozoa.l2.consensus.network

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import hydrozoa.*
import hydrozoa.l1.multisig.tx.DepositTx
import hydrozoa.l2.block.{Block, BlockHeader}
import hydrozoa.l2.consensus.network.transport.IncomingDispatcher
import hydrozoa.l2.ledger.UtxosSet
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.Alice
import hydrozoa.node.state.WalletId
import ox.channels.ActorRef
import sttp.tapir.Schema

import scala.collection.mutable

// FIXME: revise return types?
trait HeadPeerNetwork {

    def setDispatcherActorRef(dispatcherRef: ActorRef[IncomingDispatcher]): Unit

    /** @param peers
      *   peers we are looking for
      * @return
      *   verification keys for peers provided
      */
    def reqVerificationKeys(peers: Set[WalletId]): Set[VerificationKeyBytes]

    def announceOwnVerificationKey(key: VerificationKeyBytes): Unit

    def reqInit(peers: Set[WalletId], req: ReqInit): Set[TxKeyWitness]

    def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness]

    def reqMinor(block: Block): Set[AckMinor]

    // FIXME: remove utxosWithdrawn once we have block validation
    def reqMajor(block: Block, utxosWithdrawn: UtxosSet): Set[AckMajorCombined]

    // FIXME: remove utxosWithdrawn once we have block validation
    def reqFinal(block: Block, utxosWithdrawn: UtxosSet): Set[AckFinalCombined]
}

sealed trait Msg

sealed trait Req extends Msg:
    type ackType <: Ack

trait Ack extends Msg

case class ReqVerKey() extends Req:
    type ackType = AckVerKey

given reqVerKeyCodec: JsonValueCodec[ReqVerKey] =
    JsonCodecMaker.make

given reqVerKeySchema: Schema[ReqVerKey] =
    Schema.derived[ReqVerKey]

case class ReqInit(seedUtxoId: UtxoIdL1, coins: Long) extends Req:
    type ackType = AckInit

given reqInitCodec: JsonValueCodec[ReqInit] =
    JsonCodecMaker.make

given reqInitSchema: Schema[ReqInit] =
    Schema.derived[ReqInit]

case class AckVerKey(
    peer: TestPeer,
    verKey: VerificationKeyBytes
) extends Ack

given ackVerKeyCodec: JsonValueCodec[AckVerKey] =
    JsonCodecMaker.make

given verificationKeyBytesSchema: Schema[VerificationKeyBytes] =
    Schema.derived[VerificationKeyBytes]

given ackVerKeySchema: Schema[AckVerKey] =
    Schema.derived[AckVerKey]

case class AckInit(peer: TestPeer, initTxId: TxId, signature: TxKeyWitness) extends Ack

given ackInitCodec: JsonValueCodec[AckInit] =
    JsonCodecMaker.make

given ackInitSchema: Schema[AckInit] =
    Schema.derived[AckInit]

// additional schemas
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
case class ReqRefundLater(depositTx: DepositTx, index: TxIx)

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
