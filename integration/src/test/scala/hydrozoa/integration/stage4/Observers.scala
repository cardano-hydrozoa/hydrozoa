package hydrozoa.integration.stage4

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import hydrozoa.integration.harness.MultiPeerHeadHarness.Event as HEvent
import hydrozoa.integration.stage4.EffectsLanded.BlockExpectation
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.CommonChildEvent
import hydrozoa.multisig.consensus.{CardanoLiaisonEvent, SlowConsensusActorEvent}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedgerEvent
import scalus.cardano.ledger.TransactionHash

/** Test-side [[ContraTracer]] observers that ride alongside the harness's slf4j sinks (combined
  * via the `ContraTracer` monoid) so stage4 can populate per-peer Refs and fire shared cross-peer
  * Deferreds from the same event stream the logger sees.
  *
  * Each observer is typed at the harness's [[HEvent]] level — its pattern-match arms route on
  * `Event.Head(peerNum, _)` vs `Event.Coil(coilNum, _)`. Events that the actor never emits
  * (`BriefProduced` on a coil, for instance) simply don't get an arm. Combining the observers
  * with `|+|` produces the full set of stage4 capture sinks.
  */
private[stage4] object Observers {

    /** Capture `SlowConsensusActorEvent.StackHardConfirmed` from any peer or coil. Head emits go
      * into per-peer captures and drive the cross-peer `slowCoverageSignal` once
      * `slowCoverageTarget` is armed by `beforeFinalize`. Coil emits go into per-coil captures
      * (no cross-peer barrier — followers participate only via the head set).
      */
    def captureStackHardConfirmed(
        captures: Map[hydrozoa.multisig.consensus.peer.HeadPeerNumber, PerPeerCaptures],
        coilCaptures: Map[hydrozoa.multisig.consensus.peer.CoilPeerNumber, PerCoilCaptures],
        slowCoverageSignal: Deferred[IO, Unit],
        slowCoverageTarget: Deferred[IO, Set[Int]],
    ): ContraTracer[IO, HEvent] =
        ContraTracer.emit[IO, HEvent] {
            case HEvent.Head(
                  peerNum,
                  CommonChildEvent.SlowConsensusActor(
                    SlowConsensusActorEvent.StackHardConfirmed(stack)
                  ),
                ) =>
                for {
                    _           <- captures(peerNum).stacks.update(_ :+ stack)
                    maybeTarget <- slowCoverageTarget.tryGet
                    _ <- maybeTarget match {
                        case None             => IO.unit
                        case Some(targetNums) =>
                            for {
                                allPeersStacks <- captures.values.toList.traverse(_.stacks.get)
                                allCovered = targetNums.isEmpty ||
                                    allPeersStacks.forall { peerStacks =>
                                        targetNums.forall { bn =>
                                            peerStacks.exists { s =>
                                                (s.brief.firstBlockNum: Int) <= bn &&
                                                bn <= (s.brief.lastBlockNum: Int)
                                            }
                                        }
                                    }
                                _ <-
                                    if allCovered then slowCoverageSignal.complete(()).void
                                    else IO.unit
                            } yield ()
                    }
                } yield ()
            case HEvent.Coil(
                  coilNum,
                  CommonChildEvent.SlowConsensusActor(
                    SlowConsensusActorEvent.StackHardConfirmed(stack)
                  ),
                ) =>
                coilCaptures(coilNum).stacks.update(_ :+ stack)
            case _ => IO.unit
        }

    /** Capture `CardanoLiaisonEvent.TxSubmitting` from head OR coil. Coil peers don't normally
      * submit L1 transactions, but the arm is wired defensively (e.g. for an init-tx race).
      */
    def captureTxSubmitting(
        landedRef: Ref[IO, Set[TransactionHash]],
        effectsLandedSignal: Deferred[IO, Unit],
        effectsLandedTarget: Deferred[IO, List[BlockExpectation]],
    ): ContraTracer[IO, HEvent] =
        ContraTracer.emit[IO, HEvent] {
            case HEvent.Head(_, CommonChildEvent.CardanoLiaison(CardanoLiaisonEvent.TxSubmitting(txId))) =>
                onTxLanded(txId, landedRef, effectsLandedSignal, effectsLandedTarget)
            case HEvent.Coil(_, CommonChildEvent.CardanoLiaison(CardanoLiaisonEvent.TxSubmitting(txId))) =>
                onTxLanded(txId, landedRef, effectsLandedSignal, effectsLandedTarget)
            case _ => IO.unit
        }

    /** Capture `CardanoLiaisonEvent.FallbackToRuleBasedDispatched` from head OR coil. Idempotent:
      * subsequent fallback dispatches try-complete the same deferred and are no-ops.
      */
    def captureFallbackEntered(
        fallbackEnteredSignal: Deferred[IO, TransactionHash],
    ): ContraTracer[IO, HEvent] =
        ContraTracer.emit[IO, HEvent] {
            case HEvent.Head(_, CommonChildEvent.CardanoLiaison(CardanoLiaisonEvent.FallbackToRuleBasedDispatched(txId))) =>
                fallbackEnteredSignal.complete(txId).void
            case HEvent.Coil(_, CommonChildEvent.CardanoLiaison(CardanoLiaisonEvent.FallbackToRuleBasedDispatched(txId))) =>
                fallbackEnteredSignal.complete(txId).void
            case _ => IO.unit
        }

    /** Capture `JointLedgerEvent.BriefProduced` from any head peer (coil peers don't produce
      * briefs). Drives the per-peer `blockBriefs` capture and fires `fastSettlementSignal` once
      * `fastSettlementTarget` is armed.
      */
    def captureBriefProduced(
        captures: Map[hydrozoa.multisig.consensus.peer.HeadPeerNumber, PerPeerCaptures],
        fastSettlementSignal: Deferred[IO, Unit],
        fastSettlementTarget: Deferred[IO, Set[RequestId]],
    ): ContraTracer[IO, HEvent] =
        ContraTracer.emit[IO, HEvent] {
            case HEvent.Head(peerNum, CommonChildEvent.JointLedger(JointLedgerEvent.BriefProduced(b))) =>
                for {
                    _           <- captures(peerNum).blockBriefs.update(_ :+ b)
                    maybeTarget <- fastSettlementTarget.tryGet
                    _ <- maybeTarget match {
                        case None            => IO.unit
                        case Some(submitted) =>
                            for {
                                briefs <- captures(peerNum).blockBriefs.get
                                seen = briefs
                                    .flatMap(br =>
                                        br.events.map(_._1) ++
                                            br.depositsAbsorbed ++
                                            br.depositsRefunded
                                    )
                                    .toSet
                                _ <-
                                    if submitted.forall(seen.contains)
                                    then fastSettlementSignal.complete(()).void
                                    else IO.unit
                            } yield ()
                    }
                } yield ()
            case _ => IO.unit
        }

    private def onTxLanded(
        txId: TransactionHash,
        landedRef: Ref[IO, Set[TransactionHash]],
        effectsLandedSignal: Deferred[IO, Unit],
        effectsLandedTarget: Deferred[IO, List[BlockExpectation]],
    ): IO[Unit] =
        for {
            _           <- landedRef.update(_ + txId)
            maybeTarget <- effectsLandedTarget.tryGet
            _ <- maybeTarget match {
                case None       => IO.unit
                case Some(exps) =>
                    for {
                        landed <- landedRef.get
                        _ <-
                            if EffectsLanded.isComplete(landed, exps) then
                                effectsLandedSignal.complete(()).void
                            else IO.unit
                    } yield ()
            }
        } yield ()
}
