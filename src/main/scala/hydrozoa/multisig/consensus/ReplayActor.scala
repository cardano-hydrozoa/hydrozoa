package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.recovery.{ArrivalOrderedMerge, JournalScan, RawJournalEntry, ReplayCursors}
import hydrozoa.multisig.persistence.{JournalKey, Markers, Persistence, StoreKey}
import scalus.cardano.address.ShelleyAddress

/** The boot-time replay seam (R3 §8 step 3). Not a long-lived cats-actors `Actor`: it is the
  * one-shot routine `HeadMultisigRegimeManager` runs **inline** after spawning the consensus actors
  * and **before** completing the start barrier (`pendingConnections`). That ordering is
  * load-bearing (Plan A): each consensus actor enqueues a `PreStart` message then blocks on the
  * barrier inside that handler, so every message [[replay]] sends now queues in the actor's mailbox
  * *behind* `PreStart` and drains in order once the barrier opens — recover-then-replay,
  * deterministically. A spawned actor would race the barrier; an inline routine cannot.
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
  *   3. Derive the [[ReplayCursors]], scan every lane from its floor, total-order the tail by
  *      arrival stamp ([[ArrivalOrderedMerge]]), and route each entry into the reading actor's
  *      mailbox (Request → BlockWeaver, SoftAck → FastConsensusActor, HardAck / HubHardAck →
  *      SlowConsensusActor, Block spine → FCA at `softConfirmed+1` and BlockWeaver at
  *      `fastBlockMark+1`, Stack spine → StackComposer at `hardAcked+1`).
  *   4. (Hub only) re-feed `CoilAckSequencer` the received-but-unstamped coil-ack gap — for each
  *      hubbed coil, the coil `HardAck` tail above its `CoilStampMark` floor — so the `HubHardAck`
  *      spine is rebuilt through the normal stamp path (see [[replayCoilAckGap]]).
  *
  * Inbound is not restored from PeerLiaisonHeadToHead (it forwards, holds no inbound queue); this
  * is the single place that re-feeds the consensus actors from the persisted lane tail.
  */
object ReplayActor:

    /** The consensus actors the replay tail is routed into. `coilAckSequencer` is present only on a
      * hub — the boundary the received-but-unstamped coil-ack gap is re-fed to (see [[replay]]).
      */
    final case class Targets(
        blockWeaver: BlockWeaver.Handle,
        fastConsensusActor: FastConsensusActor.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        stackComposer: StackComposer.Handle,
        coilAckSequencer: Option[CoilAckSequencer.Handle] = None
    )

    /** Run the boot replay (see the object docstring), for either peer type. Pure over the store +
      * a one-shot L1 read; all effects are mailbox sends to `targets`. The fast side and the slow
      * side are now common to both peer types: the slow-side own-ack journal is the one
      * `PeerId`-keyed `HardAck` journal, so `own: PeerId` flows straight into
      * `JournalKey.HardAck(own, n)` with no peer-type branch (§10 Q10). `peers` is every head peer
      * (own included on a head peer), `hubs` every hub head peer (their `HubHardAck` journals carry
      * the coil quorum SCA aggregates, read by both peer types), `coils` the hubbed coils whose ack
      * gap a hub re-feeds (`Nil` off a hub). `own` and `treasuryAddress` come from the node config.
      */
    def replay(
        persistence: Persistence[IO],
        cardanoBackend: CardanoBackend[IO],
        targets: Targets,
        own: PeerId,
        peers: List[HeadPeerNumber],
        hubs: List[HeadPeerNumber],
        coils: List[CoilPeerNumber],
        treasuryAddress: ShelleyAddress
    )(using CardanoNetwork.Section): IO[Unit] =
        val backend = persistence.backend
        for
            markers <- Markers.derive(backend, own)
            // The slow-side own-ack journal is the one `PeerId`-keyed `HardAck` journal — both the
            // acked stack (the StackComposer/aggregator floor) and the in-flight handoff's own acks
            // come from it, for either peer type (§10 Q10).
            ownAck <- recoverOwnAck(persistence, own, markers.hardConfirmed, markers.hardAcked)
            (hardAckedStack, inflight) = ownAck
            // Fail-safe (CR6/CR7): refuse to start on an inconsistent store (confirmed > acked).
            _ <- validateInvariants(
              markers.softConfirmed,
              markers.hardConfirmed,
              markers.fastBlockMark,
              hardAckedStack
            )
            // 1. First L1 sample → BlockWeaver, so deposit decisions don't wait on the poll tick.
            _ <- seedFirstPollResults(cardanoBackend, treasuryAddress, targets.blockWeaver)
            // 2. The in-flight acked-but-unconfirmed stack (≤1) → reconstructed handoff to SCA.
            _ <- inflight.traverse_(targets.slowConsensusActor ! _)
            // 3. Derive cursors, scan all lanes, total-order, route the tail into mailboxes.
            highWater <- recoverHighWater(persistence, markers.fastBlockMark)
            cursors = ReplayCursors.derive(
              markers,
              peers,
              hubs,
              highWater,
              hardAckedStack,
              own
            )
            perJournal <- JournalScan.scanJournals(backend, cursors)
            _ <- ArrivalOrderedMerge
                .merge(perJournal)
                .traverse_(
                  route(
                    _,
                    cursors.blockSpineForLedger.num,
                    cursors.stackSpineForComposer.num,
                    targets
                  )
                )
            // 4. (Hub only) re-feed the received-but-unstamped coil-ack gap to CoilAckSequencer:
            //    PeerLiaisonHubToCoil persisted each inbound ack (coil HardAck, CR8) before advancing
            //    its receive cursor, but a crash between that and the stamp can leave a tail the coil
            //    will never re-serve. Replaying it through the normal stamp path rebuilds the
            //    HubHardAck spine — the sequencer itself is a thin counter and does not scan.
            //    `coilAckSequencer` is present only on a hub, so this is a no-op off a hub.
            _ <- targets.coilAckSequencer.traverse_(replayCoilAckGap(persistence, coils, _))
        yield ()

    /** Re-feed each coil's received-but-unstamped hard-ack tail to `CoilAckSequencer`. The gap
      * floor per coil peer is its `CoilStampMark` (the highest already stamped onto `HubHardAck`);
      * the tail is the coil's `HardAck` entries above it (the hub's per-coil-peer receive copy),
      * which the sequencer stamps via its normal `HardAck` path. The scan stays partitioned per
      * coil peer — each coil's journal is its own CF and its `CoilStampMark` is its own floor.
      */
    private def replayCoilAckGap(
        persistence: Persistence[IO],
        coils: List[CoilPeerNumber],
        coilAckSequencer: CoilAckSequencer.Handle
    )(using CardanoNetwork.Section): IO[Unit] =
        persistence.get(StoreKey.CoilStampMark).map(_.getOrElse(Map.empty)).flatMap { marks =>
            coils.traverse_ { coil =>
                val from = marks.get(coil).fold(HardAckNumber.zero)(_.increment)
                val k = JournalKey.HardAck(PeerId.Coil(coil), from)
                JournalScan
                    .scan(persistence.backend, k)
                    .flatMap(_.traverse_(e => coilAckSequencer ! k.decodeValue(e.framed).payload))
            }
        }

    /** Read this peer's own slow-side anchors: the acked stack (StackComposer/aggregator floor,
      * unpacked from the last own `HardAck` value — the `HardAckNumber → StackNumber` gap, §10 Q9)
      * and the in-flight handoff. `hardAcked` is the already-derived [[Markers]] hard-ack counter,
      * so this avoids re-scanning the own `HardAck` CF. The own ack journal is the one
      * `PeerId`-keyed `HardAck` journal, so `own: PeerId` works for both peer types (§10 Q10).
      */
    private def recoverOwnAck(
        persistence: Persistence[IO],
        own: PeerId,
        hardConfirmed: Option[StackNumber],
        hardAcked: Option[HardAckNumber]
    )(using
        CardanoNetwork.Section
    ): IO[(Option[StackNumber], Option[SlowConsensusActor.StackHandoff])] =
        for
            hardAckedStack <- hardAcked.traverse(n =>
                persistence.getOrFail(JournalKey.HardAck(own, n)).map(_.payload.stackNum)
            )
            inflight <- reconstructInflightHandoff(persistence, own, hardConfirmed, hardAckedStack)
        yield (hardAckedStack, inflight)

    /** Boot-time consistency fail-safe (CR6/CR7): a confirmed mark must never exceed its acked mark
      * (`confirmed ≤ acked` — we confirm only what we have acked). A violation means a torn or
      * regressed store, so refuse to start — the raise fails
      * `HeadMultisigRegimeManager.preStartLocal`. (The recover seams already fail-safe on a missing
      * snapshot via `getOrFail`; this catches the marker-vs-marker inconsistency before any replay
      * is fed.) Shared by both peer types: the fast arm checks `softConfirmed ≤ fastBlockMark`, the
      * slow arm `hardConfirmed ≤ hardAckedStack`.
      */
    private def validateInvariants(
        softConfirmed: Option[BlockNumber],
        hardConfirmed: Option[StackNumber],
        fastBlockMark: Option[BlockNumber],
        hardAckedStack: Option[StackNumber]
    ): IO[Unit] =
        val softOk = (softConfirmed, fastBlockMark) match
            case (Some(confirmed), Some(acked)) => Ordering[BlockNumber].lteq(confirmed, acked)
            case _                              => true
        val hardOk = (hardConfirmed, hardAckedStack) match
            case (Some(confirmed), Some(acked)) => Ordering[StackNumber].lteq(confirmed, acked)
            case _                              => true
        IO.raiseUnless(softOk && hardOk)(
          new IllegalStateException(
            "Recovery refused: store inconsistency (confirmed > acked). " +
                s"softConfirmed=$softConfirmed hardConfirmed=$hardConfirmed " +
                s"fastBlockMark=$fastBlockMark, hardAckedStack=$hardAckedStack"
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
        own: PeerId,
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
      * own-keyed `HardAck` lane scanned and filtered by the ack's `stackNum`. `own: PeerId`, so it
      * serves both peer types.
      */
    private def ownHardAcksForStack(
        persistence: Persistence[IO],
        own: PeerId,
        stackNum: StackNumber
    )(using CardanoNetwork.Section): IO[List[HardAck]] =
        val k = JournalKey.HardAck(own, HardAckNumber.zero)
        JournalScan
            .scan(persistence.backend, k)
            .map(_.map(e => k.decodeValue(e.framed).payload).filter(_.stackNum == stackNum))

    /** The per-peer request high-water at `fastBlockMark` (the fast anchor; the RequestLane resume
      * floor source, §5.3), or empty for a cold store. Both peer types anchor the request
      * high-water on `max(BlockResult)`.
      */
    private def recoverHighWater(
        persistence: Persistence[IO],
        fastBlockMark: Option[BlockNumber]
    )(using @scala.annotation.unused section: CardanoNetwork.Section): IO[Map[HeadPeerNumber, RequestNumber]] =
        fastBlockMark match
            case None    => IO.pure(Map.empty)
            case Some(b) => persistence.getOrFail(StoreKey.RequestHighWater(b))

    /** Route one decoded lane entry into the reading actor's mailbox. Journal-agnostic (shared by
      * both peer types in [[replay]]): spines fan out to two roles, sliced by the ledger floor (the
      * aggregator already gets everything scanned from its lower floor): blocks to
      * FastConsensusActor always (scanned from `softConfirmed+1`) and to BlockWeaver only
      * `≥ blockLedgerFloor`; stacks to StackComposer only `≥ stackComposerFloor` (the acked band's
      * single stack is handled by the reconstructed handoff, not its brief). The hard-ack journals
      * route to `SlowConsensusActor`: a `HardAck` entry carries a `HardAck` directly (a head peer's
      * own head hard-acks, or — on the coil path — a coil peer's own coil hard-acks), `HubHardAck`
      * carries a `HardAckWithId` whose `.ack` is the underlying `HardAck`. `HubHardAck` is scanned
      * by every peer (head and coil) for the coil quorum.
      */
    private def route(
        entry: RawJournalEntry,
        blockLedgerFloor: BlockNumber,
        stackComposerFloor: StackNumber,
        targets: Targets
    )(using CardanoNetwork.Section): IO[Unit] =
        entry.key match
            case k: JournalKey.Request =>
                targets.blockWeaver ! k.decodeValue(entry.framed).payload
            case k: JournalKey.SoftAck =>
                targets.fastConsensusActor ! k.decodeValue(entry.framed).payload
            case k: JournalKey.HardAck =>
                targets.slowConsensusActor ! k.decodeValue(entry.framed).payload
            case k: JournalKey.Block =>
                val brief = k.decodeValue(entry.framed).payload
                val toLedger =
                    if Ordering[BlockNumber].gteq(brief.blockNum, blockLedgerFloor)
                    then targets.blockWeaver ! brief
                    else IO.unit
                (targets.fastConsensusActor ! brief) >> toLedger
            case k: JournalKey.Stack =>
                val brief = k.decodeValue(entry.framed).payload
                if Ordering[StackNumber].gteq(brief.stackNum, stackComposerFloor)
                then targets.stackComposer ! brief
                else IO.unit
            case k: JournalKey.HubHardAck =>
                targets.slowConsensusActor ! k.decodeValue(entry.framed).payload.ack
