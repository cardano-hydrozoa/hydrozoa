package hydrozoa.multisig.consensus

import cats.Monad
import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockHeader, BlockNumber, BlockStatus, BlockType, BlockVersion}
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.TransactionInput

object BlockWeaverNew {
    export BlockWeaverDataStructures.*

    type Config = CardanoNetwork.Section & OwnHeadPeerPublic.Section

    final case class Connections(
        jointLedger: JointLedger.Handle,
        tracer: hydrozoa.lib.tracing.ProtocolTracer = hydrozoa.lib.tracing.ProtocolTracer.noop,
    )

    type Handle = ActorRef[IO, Request]
    type Request = PreStart.type | UserRequestWithId | BlockBrief.Next | BlockConfirmed |
        PollResults | FinalizationLocallyTriggered.type

    type BlockConfirmed = BlockHeader.Section & Block.Fields.HasFinalizationRequested &
        BlockStatus.MultiSigned & BlockType.Next

    case object PreStart

    case object FinalizationLocallyTriggered {
        trait HasFinalizationLocallyTriggered {
            def finalizationLocallyTriggered: this.type
        }
    }

    sealed trait State {
        def connections: Connections
        def pollResults: PollResults

        def receiveTotal(req: Request): IO[Unit]
    }

    object State {

    }
}

private object BlockWeaverDataStructures {
    /** Simple immutable mempool implementation. Duplicate ledger request IDs are NOT allowed and a
     * runtime exception is thrown since this should never happen. Other components, particularly
     * the peer liaison is in charge or maintaining the integrity of the stream of messages.
     *
     * @param requests
     * map to store requests
     * @param receivingOrder
     * vector to store order of request ids
     */
    final case class Mempool(
        requests: Map[RequestId, UserRequestWithId] = Map.empty,
        receivingOrder: Vector[RequestId] = Vector.empty
    ) {

        /** Throws if a duplicate is detected.
         *
         * @param request
         * an request to add
         * @return
         * an updated mempool
         */
        def add(
            request: UserRequestWithId
        ): Mempool = {

            val requestId = request.requestId

            require(
                !requests.contains(requestId),
                s"panic - duplicate event id in the pool: $requestId"
            )

            copy(
                requests = requests + (requestId -> request),
                receivingOrder = receivingOrder :+ requestId
            )
        }

        // Remove event - returns new state
        def remove(id: RequestId): Mempool = {
            require(
                requests.contains(id),
                "panic - an attempt to remove a missing event from the mempool"
            )
            copy(
                requests = requests - id,
                receivingOrder = receivingOrder.filterNot(_ == id)
            )
        }

        // Find by ID
        def findById(id: RequestId): Option[UserRequestWithId] = requests.get(id)

        // Iterate in insertion order
        def inOrder: Iterator[UserRequestWithId] =
            receivingOrder.iterator.flatMap(requests.get)

        def isEmpty: Boolean = requests.isEmpty
    }

    object Mempool {
        val empty: Mempool = Mempool()

        def apply(events: Seq[UserRequestWithId]): Mempool =
            events.foldLeft(Mempool.empty)((mempool, request) => mempool.add(request))
    }

    /** So-called "poll results" from the Cardano Liaison, i.e., a set of all utxos ids found at the
     * multisig head address.
     *
     * @param utxos
     * all utxos found
     */
    final case class PollResults(utxos: Set[TransactionInput])

    object PollResults:
        val empty: PollResults = PollResults(Set.empty)
}