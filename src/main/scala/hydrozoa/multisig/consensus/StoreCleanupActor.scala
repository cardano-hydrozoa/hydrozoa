package hydrozoa.multisig.consensus

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.retention.PrunableAcks
import hydrozoa.multisig.persistence.{AckRetention, Cf, Markers, Persistence, WriteBatch}
import scala.util.control.NonFatal

/** Deletes what consensus no longer needs from the durable store.
  *
  * Cleanup is its own actor rather than a delete riding each confirmation's `WriteBatch`, because
  * pruning is not always immediate: with an archiver attached an ack survives until the archive has
  * copied it, so some deletion is always deferred past the confirmation that made it redundant. One
  * owner doing all of it beats a fast path plus a catch-up path, and it keeps deletion off the
  * consensus actors, which have no business carrying cleaning duties.
  *
  * **Driven by hard confirmation.** A stack confirms every few minutes — frequent enough to keep
  * the store bounded, rare enough that a pass batches usefully, and it lands on a mark that already
  * means something: the hard-confirmation floor, below which §5.1 forbids deleting at all.
  *
  * **Best-effort.** Retaining more than necessary is always safe, so a failed pass is reported and
  * the next one retries from the same floor. Taking the node down over a cleanup failure would
  * trade a bounded disk problem for an unbounded availability one.
  *
  * See `docs/spec/persistence-and-crash-recovery.md` §5.1 (the retention floor) and §7
  * (confirmation-driven ack-pruning).
  */
object StoreCleanupActor:
    type Config = HeadConfig.Section & OwnPeerPublic.Section

    /** What the cleanup actor is told. */
    enum Request:
        /** A stack hard-confirmed, so what it covers is now a candidate for deletion.
          *
          * Carries the stack's last block so the fast side is pruned in the same pass: by the time
          * a stack hard-confirms, every block in it is long soft-confirmed and each block's
          * soft-acks were made redundant by the `SoftConfirmation` that subsumed them.
          */
        case StackHardConfirmed(stackNum: StackNumber, lastBlockNum: BlockNumber)

    type Handle = ActorRef[IO, Request]

    def apply(
        config: Config,
        persistence: Persistence[IO],
        ackRetention: AckRetention,
        tracer: ContraTracer[IO, StoreCleanupActorEvent]
    ): IO[StoreCleanupActor] =
        IO.pure(new StoreCleanupActor(config, persistence, ackRetention, tracer))

end StoreCleanupActor

class StoreCleanupActor(
    config: StoreCleanupActor.Config,
    persistence: Persistence[IO],
    ackRetention: AckRetention,
    tracer: ContraTracer[IO, StoreCleanupActorEvent]
) extends Actor[IO, StoreCleanupActor.Request]:
    import StoreCleanupActor.*

    private given CardanoNetwork.Section = config

    /** Families whose highest key is a recovery marker, so a prune must leave one entry behind. */
    private val protectedFamilies: Set[Cf] = Markers.markerFamilies(config.ownPeerId)

    /** Every author with a hard-ack journal on this node: each head peer, and each coil peer whose
      * acks this node stores (a hub keeps a receive copy per coil it hubs).
      */
    private val hardAckAuthors: List[PeerId] =
        config.headPeerNums.toList.map(PeerId.Head(_)) ++
            config.coilPeers.coilPeerNumbers.map(PeerId.Coil(_))

    override def receive: Receive[IO, Request] = { case Request.StackHardConfirmed(stack, block) =>
        pruneAcks(stack, block).handleErrorWith { case NonFatal(e) =>
            tracer.traceWith(StoreCleanupActorEvent.PruneFailed(stack, e))
        }
    }

    /** Delete the ack signatures the confirmations at or below this stack have already subsumed.
      *
      * Both sides in one batch. This is not the whole retention story — the spines, the request
      * journals and the spine-indexed families are a separate pass — but the ack families churn
      * hardest, so they are where an unbounded store shows first.
      */
    private def pruneAcks(stack: StackNumber, lastBlock: BlockNumber): IO[Unit] = for {
        soft <- PrunableAcks.softAcks(
          persistence.backend,
          config.headPeerNums.toList,
          lastBlock,
          ackRetention
        )
        hard <- PrunableAcks.hardAcks(
          persistence.backend,
          hardAckAuthors,
          stack,
          ackRetention,
          protectedFamilies
        )
        keys = soft ++ hard
        _ <-
            if keys.isEmpty then tracer.traceWith(StoreCleanupActorEvent.NothingToPrune(stack))
            else
                persistence.write(keys.foldLeft(WriteBatch.start)((b, k) => b.delete(k))) *>
                    tracer.traceWith(
                      StoreCleanupActorEvent.AcksPruned(stack, lastBlock, soft.size, hard.size)
                    )
    } yield ()

end StoreCleanupActor
