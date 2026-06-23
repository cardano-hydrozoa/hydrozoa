package hydrozoa.integration.stage4

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManagerEvent
import hydrozoa.multisig.consensus.SlowConsensusActorEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedgerEvent
import hydrozoa.multisig.ledger.stack.Stack

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
        stacksMap: Map[HeadPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
        slowCoverageSignal: Deferred[IO, Unit],
        slowCoverageTarget: Deferred[IO, Set[Int]],
    ): ContraTracer[IO, HeadMultisigRegimeManagerEvent] =
        ContraTracer.emit[IO, HeadMultisigRegimeManagerEvent] {
            case HeadMultisigRegimeManagerEvent
                    .SlowConsensusActor(SlowConsensusActorEvent.StackHardConfirmed(stack)) =>
                for {
                    _           <- stacksMap(peerNum).update(_ :+ stack)
                    maybeTarget <- slowCoverageTarget.tryGet
                    _ <- maybeTarget match {
                        case None => IO.unit
                        case Some(targetNums) =>
                            for {
                                allPeersStacks <- stacksMap.values.toList.traverse(_.get)
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
            case HeadMultisigRegimeManagerEvent
                    .SlowConsensusActor(SlowConsensusActorEvent.StackHardConfirmed(stack)) =>
                stacksRef.update(_ :+ stack)
            case _ => IO.unit
        }

    /** Capture `JointLedgerEvent.BriefProduced` for the given peer: append to the per-peer
      * blockBriefs Ref, then (once [[fastSettlementTarget]] is populated) fire
      * [[fastSettlementSignal]] when every submitted RequestId has been observed in some brief.
      */
    def captureBriefProduced(
        peerNum: HeadPeerNumber,
        blockBriefsMap: Map[HeadPeerNumber, Ref[IO, Vector[BlockBrief.Intermediate]]],
        fastSettlementSignal: Deferred[IO, Unit],
        fastSettlementTarget: Deferred[IO, Set[RequestId]],
    ): ContraTracer[IO, HeadMultisigRegimeManagerEvent] =
        ContraTracer.emit[IO, HeadMultisigRegimeManagerEvent] {
            case HeadMultisigRegimeManagerEvent.JointLedger(JointLedgerEvent.BriefProduced(b)) =>
                for {
                    _           <- blockBriefsMap(peerNum).update(_ :+ b)
                    maybeTarget <- fastSettlementTarget.tryGet
                    _ <- maybeTarget match {
                        case None => IO.unit
                        case Some(submitted) =>
                            for {
                                briefs <- blockBriefsMap(peerNum).get
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
