package hydrozoa.l2.consensus.network

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.typesafe.scalalogging.Logger
import hydrozoa.l2.block.Block
import hydrozoa.l2.consensus.network.transport.HeadPeerNetworkTransportWS
import hydrozoa.l2.ledger.UtxosSet
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.Alice
import hydrozoa.node.state.WalletId
import hydrozoa.{TxKeyWitness, VerificationKeyBytes, Wallet}
import ox.{either, timeout}
import sttp.tapir.Schema

import scala.collection.mutable
import scala.concurrent.duration
import scala.concurrent.duration.DurationInt

class HeadPeerNetworkWS(
    ownPeer: TestPeer,
    transport: HeadPeerNetworkTransportWS
) extends HeadPeerNetwork:

    private val log = Logger(getClass)

    override def reqVerificationKeys(peers: Set[WalletId]): Set[VerificationKeyBytes] =

        val source = transport.broadcastAndCollect(ReqVerKey())

        def handleResponses: Set[VerificationKeyBytes] = {
            val responses: mutable.Map[TestPeer, VerificationKeyBytes] = mutable.Map.empty

            while responses.size < peers.size do
                val next = source.receive()
                responses.put(next.peer, next.verKey)

            responses.values.toSet
        }

        val ret: Either[Throwable, Set[VerificationKeyBytes]] =
            either.catching(timeout(10.second)(handleResponses))

        ret match
            case Left(throwable) =>
                log.error(s"Timeout while collecting verification keys: $throwable")
                throw IllegalStateException(throwable)
            case Right(responses) => responses

    private def requireHeadPeersAreKnown(headPeers: Set[WalletId]): Unit = ???
    override def reqInit(peers: Set[WalletId], req: ReqInit): Set[TxKeyWitness] = ???
    private def getOtherPeersWallets: Set[Wallet] = ???
    override def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness] = ???
    override def reqMinor(block: Block): Set[AckMinor] = ???
    override def reqMajor(block: Block, utxosWithdrawn: UtxosSet): Set[AckMajorCombined] = ???
    override def reqFinal(block: Block, utxosWithdrawn: UtxosSet): Set[AckFinalCombined] = ???

sealed trait Msg

sealed trait Req extends Msg:
    type ackType <: Ack

trait Ack extends Msg

case class ReqVerKey() extends Req:
    type ackType = AckVerKey

given reqVerKeyCodec: JsonValueCodec[ReqVerKey] =
    JsonCodecMaker.make

given testPeerSchema: Schema[TestPeer] =
    Schema.derived[TestPeer]

given reqVerKeySchema: Schema[ReqVerKey] =
    Schema.derived[ReqVerKey]

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
