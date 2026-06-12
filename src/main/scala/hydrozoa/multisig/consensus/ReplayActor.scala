package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.recovery.{ArrivalOrderedMerge, LaneScan, RawLaneEntry, ReplayCursors}
import hydrozoa.multisig.persistence.{LaneKey, Markers, Persistence, StoreKey}
import scalus.cardano.address.ShelleyAddress

/** The boot-time replay seam (R3 §8 step 3). Not a long-lived cats-actors `Actor`: it is the
  * one-shot routine `MultisigRegimeManager` runs **inline** after spawning the consensus actors and
  * **before** completing the start barrier (`pendingConnections`). That ordering is load-bearing
  * (Plan A): each consensus actor enqueues a `PreStart` message then blocks on the barrier inside
  * that handler, so every message [[replay]] sends now queues in the actor's mailbox *behind*
  * `PreStart` and drains in order once the barrier opens — recover-then-replay, deterministically.
  * A spawned actor would race the barrier; an inline routine cannot.
  *
  * What it does, in order:
  *
  *   1. Sample L1 directly (`CardanoBackend.utxosAt(treasuryAddress)`) and seed BlockWeaver's first
  *      [[PollResults]], so deposit decisions proceed immediately rather than waiting on
  *      CardanoLiaison's poll cadence (§5.5).
  *   2. Reconstruct the in-flight acked-but-unconfirmed stack — at most one (single-flight; the §9
  *      "crash mid-stack" case) — from its persisted `Stack.Unsigned` + this peer's hard-acks, and
  *      hand it to `SlowConsensusActor` so it re-forms its cell and re-aggregates. StackComposer
  *      stays out of the acked band (its recover treats acked stacks as closed).
  *   3. Derive the `2 + 3N` [[ReplayCursors]], scan every lane from its floor, total-order the tail
  *      by arrival stamp ([[ArrivalOrderedMerge]]), and route each entry into the reading actor's
  *      mailbox (Request → BlockWeaver, SoftAck → FastConsensusActor, HardAck → SlowConsensusActor,
  *      Block spine → FCA at `softConfirmed+1` and BlockWeaver at `softAcked+1`, Stack spine →
  *      StackComposer at `hardAcked+1`).
  *
  * Inbound is not restored from PeerLiaisonHeadToHead (it forwards, holds no inbound queue); this
  * is the single place that re-feeds the consensus actors from the persisted lane tail.
  */
object ReplayActor:

    /** The consensus actors the replay tail is routed into. */
    final case class Targets(
        blockWeaver: BlockWeaver.Handle,
        fastConsensusActor: FastConsensusActor.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        stackComposer: StackComposer.Handle
    )

    /** Run the boot replay (see the object docstring). Pure over the store + a one-shot L1 read;
      * all effects are mailbox sends to `targets`. `peers` is every head peer (own included); `own`
      * and `treasuryAddress` come from the node config.
      */
    def replay(
        persistence: Persistence[IO],
        cardanoBackend: CardanoBackend[IO],
        targets: Targets,
        own: HeadPeerNumber,
        peers: List[HeadPeerNumber],
        treasuryAddress: ShelleyAddress
    )(using CardanoNetwork.Section): IO[Unit] =
        val backend = persistence.backend
        for
            markers <- Markers.derive(backend, own)
            hardAckedStack <- markers.hardAcked.traverse(n =>
                persistence.getOrFail(LaneKey.HardAck(own, n)).map(_.payload.stackNum)
            )
            // Fail-safe (CR6/CR7): refuse to start on an inconsistent store (confirmed > acked).
            _ <- validateInvariants(markers, hardAckedStack)
            // 1. First L1 sample → BlockWeaver, so deposit decisions don't wait on the poll tick.
            _ <- seedFirstPollResults(cardanoBackend, treasuryAddress, targets.blockWeaver)
            // 2. The in-flight acked-but-unconfirmed stack (≤1) → reconstructed handoff to SCA.
            _ <- reconstructInflightHandoff(persistence, own, markers.hardConfirmed, hardAckedStack)
                .flatMap(_.traverse_(targets.slowConsensusActor ! _))
            // 3. Derive cursors, scan all lanes, total-order, route the tail into mailboxes.
            highWater <- recoverHighWater(persistence, markers.softAcked)
            cursors = ReplayCursors.derive(markers, peers, highWater, hardAckedStack)
            perLane <- LaneScan.scanLanes(backend, cursors)
            _ <- ArrivalOrderedMerge.merge(perLane).traverse_(route(_, cursors, targets))
        yield ()

    /** Boot-time consistency fail-safe (CR6/CR7): a confirmed mark must never exceed its acked mark
      * (`confirmed ≤ acked` — we confirm only what we have acked). A violation means a torn or
      * regressed store, so refuse to start — the raise fails `MultisigRegimeManager.preStartLocal`.
      * (The recover seams already fail-safe on a missing snapshot via `getOrFail`; this catches the
      * marker-vs-marker inconsistency before any replay is fed.)
      */
    private def validateInvariants(
        markers: Markers,
        hardAckedStack: Option[StackNumber]
    ): IO[Unit] =
        val softOk = (markers.softConfirmed, markers.softAcked) match
            case (Some(confirmed), Some(acked)) =>
                Ordering[BlockNumber].lteq(confirmed, acked.blockNum)
            case _ => true
        val hardOk = (markers.hardConfirmed, hardAckedStack) match
            case (Some(confirmed), Some(acked)) => Ordering[StackNumber].lteq(confirmed, acked)
            case _                              => true
        IO.raiseUnless(softOk && hardOk)(
          new IllegalStateException(
            s"Recovery refused: store inconsistency (confirmed > acked). markers=$markers, " +
                s"hardAckedStack=$hardAckedStack"
          )
        )

    /** Read L1 directly and send BlockWeaver the first [[PollResults]] (§5.5). */
    private def seedFirstPollResults(
        cardanoBackend: CardanoBackend[IO],
        treasuryAddress: ShelleyAddress,
        blockWeaver: BlockWeaver.Handle
    ): IO[Unit] =
        cardanoBackend.utxosAt(treasuryAddress).flatMap {
            case Left(err) =>
                IO.raiseError(new RuntimeException(s"ReplayActor L1 sample failed: $err"))
            case Right(utxos) => blockWeaver ! PollResults(utxos.keySet)
        }

    /** Reconstruct the handoff for the in-flight stack — the last own-acked stack, **iff** it is
      * not yet hard-confirmed (`hardConfirmed < hardAckedStack`). Its `Stack.Unsigned` was
      * persisted before the handoff (StackComposer), so SCA's cell re-forms from it + this peer's
      * hard-acks; `None` when the acked stack already confirmed (no in-flight cell to rebuild).
      * Stack 0 never appears here — bootstrap persists no own hard-ack, so it is re-bootstrapped,
      * not reconstructed.
      */
    private def reconstructInflightHandoff(
        persistence: Persistence[IO],
        own: HeadPeerNumber,
        hardConfirmed: Option[StackNumber],
        hardAckedStack: Option[StackNumber]
    )(using CardanoNetwork.Section): IO[Option[SlowConsensusActor.StackHandoff]] =
        hardAckedStack match
            case Some(stackNum) if hardConfirmed.forall(Ordering[StackNumber].lt(_, stackNum)) =>
                for
                    unsigned <- persistence.getOrFail(StoreKey.UnsignedStack(stackNum))
                    ownAcks <- ownHardAcksForStack(persistence, own, stackNum)
                yield Some(SlowConsensusActor.StackHandoff(unsigned, ownAcks))
            case _ => IO.pure(None)

    /** This peer's persisted hard-acks for `stackNum` (round-1 + round-2, or the sole ack) — the
      * own-keyed HardAck lane scanned and filtered by the ack's `stackNum`.
      */
    private def ownHardAcksForStack(
        persistence: Persistence[IO],
        own: HeadPeerNumber,
        stackNum: StackNumber
    )(using CardanoNetwork.Section): IO[List[HardAck]] =
        val k = LaneKey.HardAck(own, HardAckNumber.zero)
        LaneScan
            .scan(persistence.backend, k)
            .map(_.map(e => k.decodeValue(e.framed).payload).filter(_.stackNum == stackNum))

    /** The per-peer request high-water at `softAcked` (the RequestLane resume floor source, §5.3),
      * or empty for a cold store.
      */
    private def recoverHighWater(
        persistence: Persistence[IO],
        softAcked: Option[SoftAckNumber]
    )(using CardanoNetwork.Section): IO[Map[HeadPeerNumber, RequestNumber]] =
        softAcked match
            case None      => IO.pure(Map.empty)
            case Some(ack) => persistence.getOrFail(StoreKey.RequestHighWater(ack.blockNum))

    /** Route one decoded lane entry into the reading actor's mailbox. Spines fan out to two roles,
      * sliced by the ledger floor (the aggregator already gets everything scanned from its lower
      * floor): blocks to FastConsensusActor always (scanned from `softConfirmed+1`) and to
      * BlockWeaver only `≥ softAcked+1`; stacks to StackComposer only `≥ hardAcked+1` (the acked
      * band's single stack is handled by the reconstructed handoff, not its brief).
      */
    private def route(entry: RawLaneEntry, cursors: ReplayCursors, targets: Targets)(using
        CardanoNetwork.Section
    ): IO[Unit] =
        entry.key match
            case k: LaneKey.Request =>
                targets.blockWeaver ! k.decodeValue(entry.framed).payload
            case k: LaneKey.SoftAck =>
                targets.fastConsensusActor ! k.decodeValue(entry.framed).payload
            case k: LaneKey.HardAck =>
                targets.slowConsensusActor ! k.decodeValue(entry.framed).payload
            case k: LaneKey.Block =>
                val brief = k.decodeValue(entry.framed).payload
                val toLedger =
                    if Ordering[BlockNumber].gteq(brief.blockNum, cursors.blockSpineForLedger.num)
                    then targets.blockWeaver ! brief
                    else IO.unit
                (targets.fastConsensusActor ! brief) >> toLedger
            case k: LaneKey.Stack =>
                val brief = k.decodeValue(entry.framed).payload
                if Ordering[StackNumber].gteq(brief.stackNum, cursors.stackSpineForComposer.num)
                then targets.stackComposer ! brief
                else IO.unit
            // Coil lanes are not replayed: replay is head-shaped (`ReplayCursors.scanLanes` never
            // includes them); coil-peer recovery is a separate workstream.
            case _: LaneKey.CoilHardAck | _: LaneKey.HubHardAck => IO.unit
