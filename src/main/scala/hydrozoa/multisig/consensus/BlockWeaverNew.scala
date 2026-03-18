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
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockHeader, BlockNumber, BlockStatus, BlockType, BlockVersion}
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.TransactionInput

final case class BlockWeaverNew(
    config: BlockWeaverNew.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | BlockWeaverNew.Connections
) extends Actor[IO, BlockWeaverNew.Request] {
    import BlockWeaverNew.*

    private val loggerIO = Logging.loggerIO("BlockWeaver")

    override def preStart: IO[Unit] = for {
        _ <- context.self ! BlockWeaverNew.PreStart
        _ <- context.become(receive)
    } yield ()

    private def become(state: BlockWeaverNew.State.Reactive): IO[Unit] =
        context.become(
          PartialFunction.fromFunction(req =>
              for {
                  // Handle the request using the current state's handler
                  mNewState <- state.react(config)(req)
                  // If the handler returns a new state, become that state.
                  // Otherwise, stop the actor.
                  _ <- mNewState.fold(context.self.stop)(newState => become(newState))
              } yield ()
          )
        )

    override def receive: Receive[IO, BlockWeaverNew.Request] = PartialFunction.fromFunction {
        case PreStart =>
            for {
                connections <- initializeConnections
                startingState <- State.start(config, connections, loggerIO)
                _ <- become(startingState)
            } yield ()
        case x =>
            val msg = s"Unexpected message received before PreStart: $x"
            loggerIO.error(msg) >> IO.raiseError(RuntimeException(msg))
    }

    private def initializeConnections: IO[BlockWeaverNew.Connections] = pendingConnections match {
        case pc: MultisigRegimeManager.PendingConnections =>
            for {
                c <- pc.get
            } yield BlockWeaverNew.Connections(
              jointLedger = c.jointLedger,
              tracer = c.tracer,
            )
        case c: BlockWeaverNew.Connections => IO.pure(c)
    }
}

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
        def logger: Logger[IO]
        def pollResults: PollResults
    }

    object State {
        def start(config: Config, connections: Connections, logger: Logger[IO]): IO[Idle | Leader] =
            Idle(connections, logger, PollResults.empty, Mempool.empty).act(config).map(_.get)

        private def continue[S <: State.Reactive](state: S): IO[Some[S]] =
            IO.pure(Some(state))
        private def stop(): IO[None.type] = IO.pure(None)

        sealed trait Caching extends State {
            def mempool: Mempool
        }

        /** An active state can immediately transition into another state, without waiting for a new
          * message.
          */
        sealed trait Active extends State {
            def act(config: Config): IO[Option[State.Reactive]]
        }

        /** A reactive state can receive a message, reacting by transitioning to another state. */
        sealed trait Reactive extends State {
            def react(config: Config)(req: Request): IO[Option[State.Reactive]]
        }

        final case class Idle private[State] (
            override val connections: Connections,
            override val logger: Logger[IO],
            override val pollResults: PollResults,
            override val mempool: Mempool,
        ) extends Active,
              Caching,
              Reactive {
            private def idle(newMempool: Mempool): Idle =
                Idle(this, newMempool)

            private def leader(blockNumber: BlockNumber, startedBlock: Leader.StartedBlock) =
                Leader(this, blockNumber, startedBlock)

            private def follower(newMempool: Mempool): Follower =
                Follower(this, newMempool)

            override def act(config: Config): IO[Some[Idle | Leader]] =
                if config.ownHeadPeerId.isLeader(BlockNumber.zero)
                then continue(leader(BlockNumber.zero, Leader.StartedBlock.NotStarted))
                else ???

            override def react(config: Config)(req: Request): IO[Option[Idle | FollowerAwaiting]] =
                req match {
                    case ur: UserRequestWithId =>
                        continue(idle(mempool.add(ur)))

                }
        }

        final case class Follower private (
            override val connections: Connections,
            override val logger: Logger[IO],
            override val pollResults: PollResults,
            override val mempool: Mempool,
        ) extends Active,
              Caching {
            def act(config: Config): IO[Some[Idle | FollowerAwaiting]] = ???
        }

        final case class FollowerAwaiting private (
            override val connections: Connections,
            override val logger: Logger[IO],
            override val pollResults: PollResults,
            override val mempool: Mempool,
            reproducingBlockBrief: BlockBrief.Next,
            awaitingRequestId: RequestId,
        ) extends Caching,
              Reactive {
            override def react(config: Config)(req: Request): IO[Option[State.Reactive]] = ???
        }

        final case class Leader private (
            override val connections: Connections,
            override val logger: Logger[IO],
            override val pollResults: PollResults,
            leadingBlockNumber: BlockNumber,
            startedBlock: Leader.StartedBlock
        ) extends Reactive {
            override def react(config: Config)(req: Request): IO[Option[State.Reactive]] = ???
        }

        final case class LeaderAwaiting private (
            override val connections: Connections,
            override val logger: Logger[IO],
            override val pollResults: PollResults,
            previousBlockBriefConfirmed: BlockBrief.NonFinal
        ) extends Reactive {
            override def react(config: Config)(req: Request): IO[Option[State.Reactive]] = ???
        }

        object Idle {
            private[State] def apply(state: State, mempool: Mempool): Idle =
                import state.*
                Idle(connections, logger, pollResults, mempool)
        }

        object Follower {
            private[State] def apply(state: State, mempool: Mempool): Follower =
                import state.*
                Follower(connections, logger, pollResults, mempool)
        }

        object FollowerAwaiting {
            private[State] def apply(
                state: State,
                mempool: Mempool,
                reproducingBlockBrief: BlockBrief.Next,
                awaitingRequestId: RequestId
            ): FollowerAwaiting =
                import state.*
                FollowerAwaiting(
                  connections,
                  logger,
                  pollResults,
                  mempool,
                  reproducingBlockBrief,
                  awaitingRequestId
                )
        }

        object Leader {
            private[State] def apply(
                state: State,
                blockNumber: BlockNumber,
                startedBlock: StartedBlock
            ): Leader =
                import state.*
                Leader(connections, logger, pollResults, blockNumber, startedBlock)

            enum StartedBlock:
                case Started, NotStarted
        }

        object LeaderAwaiting {
            private[State] def apply(
                state: State,
                previousBlockBriefConfirmed: BlockBrief.NonFinal
            ): LeaderAwaiting =
                import state.*
                LeaderAwaiting(connections, logger, pollResults, previousBlockBriefConfirmed)
        }

    }
}

private object BlockWeaverDataStructures {

    /** Simple immutable mempool implementation. Duplicate ledger request IDs are NOT allowed and a
      * runtime exception is thrown since this should never happen. Other components, particularly
      * the peer liaison is in charge or maintaining the integrity of the stream of messages.
      *
      * @param requests
      *   map to store requests
      * @param receivingOrder
      *   vector to store order of request ids
      */
    final case class Mempool(
        requests: Map[RequestId, UserRequestWithId] = Map.empty,
        receivingOrder: Vector[RequestId] = Vector.empty
    ) {

        /** Throws if a duplicate is detected.
          *
          * @param request
          *   an request to add
          * @return
          *   an updated mempool
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
      *   all utxos found
      */
    final case class PollResults(utxos: Set[TransactionInput])

    object PollResults:
        val empty: PollResults = PollResults(Set.empty)
}
