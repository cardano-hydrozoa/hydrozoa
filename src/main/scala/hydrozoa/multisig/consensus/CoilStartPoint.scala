package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.multisig.consensus.ack.{HardAckNumber, HubHardAckNumber, SoftAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.{Join, Population}
import hydrozoa.multisig.consensus.liaison.BatchNumber
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.l1.tx.SettlementTx
import hydrozoa.multisig.ledger.l2.L2Ledger
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects, StackNumber, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.{Cf, JournalKey, Persistence, StoreKey}
import java.nio.ByteBuffer

/** What a hub decides when a coil peer connects (GUM-312): where that coil should start, or that it
  * should not be seeded at all.
  */
enum StartPoint:

    /** Seed this coil at the enclosed start point. */
    case Offer(offer: Join.Offer)

    /** Leave this coil to walk forward over the population lanes — the ticket's **warm reconnect**.
      * It keeps its own cursors and the hub serves it from where it already is.
      *
      * ⛔ This is the common outcome, not the rare one. A hub produces continuously, so a
      * reconnecting coil is essentially ALWAYS behind; the question is never "is it behind" but "by
      * more than `coilCatchUpStacks`". Seeding is the exception, for a coil far enough back that
      * replaying is not worth it.
      */
    case CatchUp

    /** The hub has no start point it could offer. The coil does what it does today — bootstrap
      * stack 0 from config and catch up over the population — which is correct in both of these
      * cases and cheap, because both mean the head has little history.
      */
    case Unavailable(reason: StartPoint.Reason)

object StartPoint:

    enum Reason:
        /** The head has not hard-confirmed anything past stack 0, so there is nothing to seed from.
          * Every peer derives stack 0 from shared config anyway.
          */
        case HeadAtStackZero

        /** The head has hard-confirmed stacks but has never made a **major**, so no settlement
          * exists and there is no certified treasury to hand over.
          *
          * ⚠️ **A known, deliberate limitation.** A coil that connects in this window is not seeded
          * and catches up from stack 0 instead. That is cheap while the window is short — which it
          * normally is, since a head reaches its first major early — but it widens if majors are
          * configured sparsely and the head produces many minor-only stacks in between.
          *
          * The alternative was to make `Join.Offer.settlement` optional and let the coil take the
          * treasury from `config.initializationTx` when no settlement exists yet. That is sound,
          * and strictly *more* trustworthy (config-derived rather than donor-supplied) — but it
          * adds a second path through `JoinOfferVerifier` that only this narrow window would ever
          * exercise, and a barely-tested branch in verification is worse than not having the
          * capability. Revisit if a head is seen with a long minor-only run before its first major.
          */
        case NoMajorYet(latestStack: StackNumber)

    /** Decide what to offer a coil peer that has just connected.
      *
      * The hub chooses; the coil's `connected` marks are a **hint** used for one thing only —
      * telling a warm reconnect (already current, [[StartPoint.NotNeeded]]) from a coil that needs
      * seeding. Everything the coil then adopts is computed here, so a coil that lies about its
      * marks gets seeded rather than believed.
      */
    def decide(
        coil: PeerId.Coil,
        connected: Join.Connected,
        persistence: Persistence[IO],
        ledger: L2Ledger[IO]
    )(using config: Config): IO[StartPoint] =
        latestHardConfirmed(persistence).flatMap {
            case None =>
                IO.pure(Unavailable(Reason.HeadAtStackZero))
            case Some(stack) if stack == StackNumber.zero =>
                IO.pure(Unavailable(Reason.HeadAtStackZero))
            case Some(stack) if withinCatchUp(connected.stack, stack) =>
                IO.pure(CatchUp)
            case Some(stack) =>
                buildOffer(coil, stack, persistence, ledger)
        }

    /** Everything the hub reads to assemble an offer, plus the catch-up threshold. */
    type Config = HeadPeers.Section & CardanoNetwork.Section & HeadConfig.Bootstrap.Section &
        NodeOperationMultisigConfig.Section

    /** Is this coil close enough behind to walk forward over the lanes instead of being seeded?
      *
      * A coil reporting **no** stack at all is never within catch-up: it holds nothing to walk
      * forward from. That is the cold store the whole exchange exists for.
      */
    private def withinCatchUp(coilStack: Option[StackNumber], latest: StackNumber)(using
        config: Config
    ): Boolean =
        coilStack.exists(s => (latest: Int) - (s: Int) <= config.coilCatchUpStacks)

    /** The latest stack this hub has hard-confirmed — the only start point it offers.
      * `lastKey(Cf.HardConfirmation)`, the same derivation `Markers.hardConfirmed` uses.
      */
    private def latestHardConfirmed(persistence: Persistence[IO]): IO[Option[StackNumber]] =
        persistence.backend
            .lastKey(Cf.HardConfirmation)
            .map(_.map(bytes => StackNumber(ByteBuffer.wrap(bytes).getInt)))

    private def buildOffer(
        coil: PeerId.Coil,
        stack: StackNumber,
        persistence: Persistence[IO],
        ledger: L2Ledger[IO]
    )(using config: Config): IO[StartPoint] =
        for {
            effects <- persistence.getOrFail(StoreKey.HardConfirmation(stack)).map(_.payload)
            brief <- persistence.getOrFail(JournalKey.Stack(stack)).map(_.payload)
            block = brief.lastBlockNum
            settlementAndSec <- certificateFor(effects, stack, persistence)
            result <- settlementAndSec match {
                case None => IO.pure(Unavailable(Reason.NoMajorYet(stack)))
                case Some((settlement, sec)) =>
                    for {
                        commandNumber <- persistence.getOrFail(StoreKey.L2CommandNumber(block))
                        exported <- ledger.exportStateAt(commandNumber).value
                        cursors <- cursorsAt(stack, block, persistence)
                        ownHardAck <- ownHardAckStart(coil, stack, persistence)
                        offer <- exported match {
                            case Left(e) => IO.raiseError(e)
                            case Right(state) =>
                                IO.pure(
                                  Offer(
                                    Join.Offer(
                                      startStack = stack,
                                      cursors = cursors,
                                      ownHardAck = ownHardAck,
                                      settlement = settlement,
                                      sec = sec,
                                      state = state
                                    )
                                  )
                                )
                        }
                    } yield offer
            }
        } yield result

    /** The two signed artifacts for this start point.
      *
      * The stack's **last** partition is the one the start point sits in: a `Major` carries its own
      * settlement and needs no SEC beside it, a `Minor` carries a mandatory SEC and sends us back
      * for the latest major's settlement — a minor touches no L1 and rotates no treasury.
      *
      * `None` when no major exists anywhere at or below `stack` — see [[Reason.NoMajorYet]].
      */
    private def certificateFor(
        effects: StackEffects.HardConfirmed,
        stack: StackNumber,
        persistence: Persistence[IO]
    ): IO[Option[(SettlementTx, Option[StandaloneEvacuationCommitment.MultiSigned])]] =
        effects match {
            case _: StackEffects.HardConfirmed.Initial =>
                IO.pure(None)
            case StackEffects.HardConfirmed.Regular(partitions) =>
                partitions.last match {
                    case major: PartitionEffects.Major[?] =>
                        IO.pure(Some((major.settlement, None)))
                    case minor: PartitionEffects.Minor[
                          StandaloneEvacuationCommitment.MultiSigned
                        ] =>
                        latestSettlementAtOrBelow(stack, persistence).map(
                          _.map(settlement => (settlement, Some(minor.sec)))
                        )
                    case _: PartitionEffects.Final =>
                        // A final stack finalizes the head; nothing joins at that anchor.
                        IO.pure(None)
                }
        }

    /** Walk stacks down from `stack` for the newest settlement — the treasury a minor start point
      * inherits. Bounded by retention: a store pruned below its last major has none to find, which
      * reads as [[Reason.NoMajorYet]] and sends the coil down the bootstrap path.
      */
    private def latestSettlementAtOrBelow(
        stack: StackNumber,
        persistence: Persistence[IO]
    ): IO[Option[SettlementTx]] =
        def go(n: Int): IO[Option[SettlementTx]] =
            if n < 0 then IO.pure(None)
            else
                persistence.get(StoreKey.HardConfirmation(StackNumber(n))).flatMap {
                    case Some(t) =>
                        settlementIn(t.payload) match {
                            case Some(s) => IO.pure(Some(s))
                            case None    => go(n - 1)
                        }
                    case None => go(n - 1)
                }
        go(stack: Int)

    /** The newest settlement inside one stack's effects, if it has one. */
    private def settlementIn(effects: StackEffects.HardConfirmed): Option[SettlementTx] =
        effects match {
            case _: StackEffects.HardConfirmed.Initial => None
            case StackEffects.HardConfirmed.Regular(partitions) =>
                partitions.toList.reverse.collectFirst { case m: PartitionEffects.Major[?] =>
                    m.settlement
                }
        }

    /** Every population cursor the coil adopts, assembled from the start point.
      *
      * Mostly lookup rather than arithmetic, because a cursor read from the journal a lane actually
      * serves cannot drift from it. The one arithmetic case is safe: `SoftAck.ackNum` IS the block
      * number (one soft-ack per block), so the two coincide by construction.
      */
    private def cursorsAt(
        stack: StackNumber,
        block: BlockNumber,
        persistence: Persistence[IO]
    )(using config: Config): IO[Population.Get] =
        val peers = config.headPeerNums.toList
        for {
            highWater <- persistence.getOrFail(StoreKey.RequestHighWater(block))
            headHardAcks <- peers.traverse(h =>
                firstAckAfter(PeerId.Head(h), stack, persistence).map(h -> _)
            )
            coilHardAcks <- peers.traverse(h => hubHardAckStart(h, persistence).map(h -> _))
        } yield Population.Get(
          batchNum = BatchNumber.zero,
          block = block.increment,
          stack = stack.increment,
          requests = peers.map(h => h -> highWater.getOrElse(h, RequestNumber(0))).toMap,
          softAcks = peers.map(h => h -> SoftAckNumber((block: Int) + 1)).toMap,
          headHardAcks = headHardAcks.toMap,
          coilHardAcks = coilHardAcks.toMap
        )

    /** The index of `peer`'s first hard-ack covering a stack **after** `stack`.
      *
      * A lookup and not arithmetic: a stack yields one ack per peer when it is sole (minor-only)
      * and two when it is 2-phase, so an ack index cannot be computed from a stack number. Scanning
      * the peer's own journal is what keeps this aligned with what the lane will actually serve.
      */
    private def firstAckAfter(
        peer: PeerId,
        stack: StackNumber,
        persistence: Persistence[IO]
    ): IO[HardAckNumber] =
        persistence.backend
            .cursor(Cf.HardAck(peer), Array.emptyByteArray)
            .use { c =>
                def go: IO[Option[HardAckNumber]] =
                    c.next.flatMap {
                        case None => IO.pure(None)
                        case Some((key, _)) =>
                            val ackNum = HardAckNumber(ByteBuffer.wrap(key).getInt)
                            persistence
                                .getOrFail(JournalKey.HardAck(peer, ackNum))
                                .flatMap(v =>
                                    if Ordering[StackNumber].gt(v.payload.stackNum, stack) then
                                        IO.pure(Some(ackNum))
                                    else go
                                )
                    }
                go
            }
            .flatMap {
                case Some(n) => IO.pure(n)
                // No ack past the start point yet: the next one this peer makes is the next index.
                case None =>
                    persistence.backend
                        .lastKey(Cf.HardAck(peer))
                        .map(
                          _.fold(HardAckNumber.zero)(b =>
                              HardAckNumber(ByteBuffer.wrap(b).getInt).increment
                          )
                        )
            }

    /** Where the coil starts pulling `hub`'s re-sequenced coil-ack lane: past everything the hub
      * has sequenced so far, since a seeded coil needs none of it.
      */
    private def hubHardAckStart(
        hub: HeadPeerNumber,
        persistence: Persistence[IO]
    ): IO[HubHardAckNumber] =
        persistence.backend
            .lastKey(Cf.HubHardAck(hub))
            .map(
              _.fold(HubHardAckNumber.zero)(b =>
                  HubHardAckNumber(ByteBuffer.wrap(b).getInt).increment
              )
            )

    /** The first own-hard-ack index the hub will ask this coil for.
      *
      * ⛔ The hub moves its own inbound cursor **forward** to here and never back. The acks below it
      * are never produced and never requested — which is exactly what lets a coil with no history
      * connect at all, instead of being asked for acks it cannot reconstruct and must not re-sign.
      */
    private def ownHardAckStart(
        coil: PeerId.Coil,
        stack: StackNumber,
        persistence: Persistence[IO]
    ): IO[HardAckNumber] =
        firstAckAfter(coil, stack, persistence)
