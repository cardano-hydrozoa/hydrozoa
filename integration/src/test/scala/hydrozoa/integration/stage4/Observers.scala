package hydrozoa.integration.stage4

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import hydrozoa.integration.stage4.EffectsLanded.BlockExpectation
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.{CommonChildEvent, HeadMultisigRegimeManagerEvent}
import hydrozoa.multisig.consensus.{CardanoLiaisonEvent, SlowConsensusActorEvent}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedgerEvent
import hydrozoa.multisig.ledger.stack.Stack
import scalus.cardano.ledger.TransactionHash

/** Test-side [[ContraTracer]] observers that ride alongside an HMRM's slf4j sink (combined via the
  * `ContraTracer` monoid) so stage4 can populate per-peer Refs and fire shared cross-peer
  * Deferreds from the same event stream the logger sees.
  *
  * Each observer matches one HMRM event variant and is a no-op for everything else; combining
  * them with `|+|` produces the full set of stage4 capture sinks.
  */
private[stage4] object Observers {

    /** Capture `SlowConsensusActorEvent.StackHardConfirmed` for the given peer: append to the
      * per-peer stacks Ref, then (once [[slowCoverageTarget]] is populated by `beforeFinalize`)
      * fire [[slowCoverageSignal]] when every targeted block number is covered by some
      * hard-confirmed stack on *every* peer.
      */
    def captureStackHardConfirmed(
        peerNum: HeadPeerNumber,
        captures: Map[HeadPeerNumber, PerPeerCaptures],
        slowCoverageSignal: Deferred[IO, Unit],
        slowCoverageTarget: Deferred[IO, Set[Int]],
    ): ContraTracer[IO, HeadMultisigRegimeManagerEvent] =
        ContraTracer.emit[IO, HeadMultisigRegimeManagerEvent] {
            case CommonChildEvent.SlowConsensusActor(SlowConsensusActorEvent.StackHardConfirmed(stack)) =>
                for {
                    _           <- captures(peerNum).stacks.update(_ :+ stack)
                    maybeTarget <- slowCoverageTarget.tryGet
                    _ <- maybeTarget match {
                        case None => IO.unit
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
            case _ => IO.unit
        }

    /** Coil-side counterpart of [[captureStackHardConfirmed]]: appends each hard-confirmed stack
      * to the per-coil Ref. No cross-peer barrier (followers participate in `slowCoverageSignal`
      * only via the head set); `propCoilParticipation` reads `coilStacks` post-run.
      */
    def captureCoilStackHardConfirmed(
        stacksRef: Ref[IO, Vector[Stack.HardConfirmed]],
    ): ContraTracer[IO, HeadMultisigRegimeManagerEvent] =
        ContraTracer.emit[IO, HeadMultisigRegimeManagerEvent] {
            case CommonChildEvent.SlowConsensusActor(SlowConsensusActorEvent.StackHardConfirmed(stack)) =>
                stacksRef.update(_ :+ stack)
            case _ => IO.unit
        }

    /** Capture `CardanoLiaisonEvent.TxSubmitting` from any peer: append the tx hash to the
      * shared cross-peer landed set, then (once [[effectsLandedTarget]] is populated by
      * `beforeFinalize` with the backbone's [[BlockExpectation]]s) fire [[effectsLandedSignal]]
      * the moment the same condition `propEffectsLanded` would accept is met.
      *
      * Wired on every head peer; the `Set` collapses identical hashes submitted in parallel.
      * Coil peers don't submit L1 transactions, so they don't contribute here.
      */
    def captureTxSubmitting(
        landedRef: Ref[IO, Set[TransactionHash]],
        effectsLandedSignal: Deferred[IO, Unit],
        effectsLandedTarget: Deferred[IO, List[BlockExpectation]],
    ): ContraTracer[IO, HeadMultisigRegimeManagerEvent] =
        ContraTracer.emit[IO, HeadMultisigRegimeManagerEvent] {
            case CommonChildEvent.CardanoLiaison(CardanoLiaisonEvent.TxSubmitting(txId)) =>
                for {
                    _           <- landedRef.update(_ + txId)
                    maybeTarget <- effectsLandedTarget.tryGet
                    _ <- maybeTarget match {
                        case None => IO.unit
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
            case _ => IO.unit
        }

    /** Capture `CardanoLiaisonEvent.FallbackToRuleBasedDispatched` from any peer: complete
      * [[fallbackEnteredSignal]] with the fallback tx hash the first time any peer's CL
      * successfully submits a `FallbackToRuleBased` action. Drives the `beforeFinalize` race
      * that short-circuits scenarios drifting outside the modeled happy-path regime.
      *
      * Idempotent: subsequent fallback dispatches (other peers, retries) try-complete the same
      * deferred and are no-ops.
      */
    def captureFallbackEntered(
        fallbackEnteredSignal: Deferred[IO, TransactionHash],
    ): ContraTracer[IO, HeadMultisigRegimeManagerEvent] =
        ContraTracer.emit[IO, HeadMultisigRegimeManagerEvent] {
            case CommonChildEvent.CardanoLiaison(CardanoLiaisonEvent.FallbackToRuleBasedDispatched(txId)) =>
                fallbackEnteredSignal.complete(txId).void
            case _ => IO.unit
        }

    /** Capture `JointLedgerEvent.BriefProduced` for the given peer: append to the per-peer
      * blockBriefs Ref, then (once [[fastSettlementTarget]] is populated) fire
      * [[fastSettlementSignal]] when every submitted RequestId has been observed in some brief.
      */
    def captureBriefProduced(
        peerNum: HeadPeerNumber,
        captures: Map[HeadPeerNumber, PerPeerCaptures],
        fastSettlementSignal: Deferred[IO, Unit],
        fastSettlementTarget: Deferred[IO, Set[RequestId]],
    ): ContraTracer[IO, HeadMultisigRegimeManagerEvent] =
        ContraTracer.emit[IO, HeadMultisigRegimeManagerEvent] {
            case CommonChildEvent.JointLedger(JointLedgerEvent.BriefProduced(b)) =>
                for {
                    _           <- captures(peerNum).blockBriefs.update(_ :+ b)
                    maybeTarget <- fastSettlementTarget.tryGet
                    _ <- maybeTarget match {
                        case None => IO.unit
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
}
