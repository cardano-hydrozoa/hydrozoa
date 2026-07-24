package hydrozoa.integration.stage4

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import hydrozoa.integration.harness.MultiPeerHeadHarness.Event as HEvent
import hydrozoa.integration.harness.{Capture, Signal}
import hydrozoa.integration.stage4.EffectsLanded.BlockExpectation
import hydrozoa.multisig.CommonChildEvent
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import hydrozoa.multisig.consensus.{CardanoLiaisonEvent, SlowConsensusActorEvent}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedgerEvent
import scalus.cardano.ledger.TransactionHash

/** Stage4 plugin builders — each function returns a typed [[Capture]] (Refs + writer arm) or
  * [[Signal]] (Deferred + predicate arm) that the [[Stage4Suite.sutResource]] folds into the
  * harness's root tracer via [[Plugin.tracerOf]].
  *
  * Captures own state Refs; signals layer on top by closing over a target Deferred (armed in
  * `beforeFinalize`) and/or another capture's state.
  */
private[stage4] object Stage4Plugins {

    /** Capture briefs (`JL.BriefProduced`) and hard-confirmed stacks (`SCA.StackHardConfirmed`)
      * per head peer.
      */
    def perPeerCaptures(
        peers: Seq[HeadPeerNumber]
    ): IO[Capture[Map[HeadPeerNumber, PerPeerCaptures]]] =
        PerPeerCaptures.makeMap(peers).map { state =>
            Capture.make(state) { s =>
                {
                    case HEvent.Head(
                          p,
                          CommonChildEvent.JointLedger(JointLedgerEvent.BriefProduced(b)),
                        ) =>
                        s(p).blockBriefs.update(_ :+ b)
                    case HEvent.Head(
                          p,
                          CommonChildEvent.SlowConsensusActor(
                            SlowConsensusActorEvent.StackHardConfirmed(st)
                          ),
                        ) =>
                        s(p).stacks.update(_ :+ st)
                    case _ => IO.unit
                }
            }
        }

    /** Capture hard-confirmed stacks per coil peer follower (`SCA.StackHardConfirmed` on the coil
      * side). No-op for a pure-head run.
      */
    def perCoilCaptures(
        coils: Seq[CoilPeerNumber]
    ): IO[Capture[Map[CoilPeerNumber, PerCoilCaptures]]] =
        PerCoilCaptures.makeMap(coils).map { state =>
            Capture.make(state) { s =>
                {
                    case HEvent.Coil(
                          c,
                          CommonChildEvent.SlowConsensusActor(
                            SlowConsensusActorEvent.StackHardConfirmed(st)
                          ),
                        ) =>
                        s(c).stacks.update(_ :+ st)
                    case _ => IO.unit
                }
            }
        }

    /** Capture L1 tx hashes from `CardanoLiaisonEvent.TxSubmitting` across head and coil sides.
      * All head peers submit the same backbone txs in parallel; the `Set` collapses duplicates.
      */
    def effectsLandedCapture: IO[Capture[Ref[IO, Set[TransactionHash]]]] =
        Ref[IO].of(Set.empty[TransactionHash]).map { ref =>
            Capture.make(ref) { r =>
                {
                    case HEvent.Head(
                          _,
                          CommonChildEvent.CardanoLiaison(CardanoLiaisonEvent.TxSubmitting(t)),
                        ) =>
                        r.update(_ + t)
                    case HEvent.Coil(
                          _,
                          CommonChildEvent.CardanoLiaison(CardanoLiaisonEvent.TxSubmitting(t)),
                        ) =>
                        r.update(_ + t)
                    case _ => IO.unit
                }
            }
        }

    /** Fast-cycle drain signal: armed externally with the submitted reqId set; fires once any
      * peer's local briefs cover every submitted id (events or absorbed/refunded deposits).
      * Per-peer "first to cover" semantics — matches the original observer.
      */
    def fastSettlementSignal(
        perPeer: Capture[Map[HeadPeerNumber, PerPeerCaptures]],
        target: Deferred[IO, Set[RequestId]],
    ): IO[Signal[Unit]] =
        Signal.make[Unit] {
            case HEvent.Head(
                  p,
                  CommonChildEvent.JointLedger(JointLedgerEvent.BriefProduced(_)),
                ) =>
                target.tryGet.flatMap {
                    case None            => IO.pure(None)
                    case Some(submitted) =>
                        perPeer.state(p).blockBriefs.get.map { briefs =>
                            val seen = briefs
                                .flatMap(br =>
                                    br.requests.map(_._1) ++
                                        br.depositsAbsorbed ++
                                        br.depositsRejected
                                )
                                .toSet
                            Option.when(submitted.forall(seen.contains))(())
                        }
                }
            case _ => IO.pure(None)
        }

    /** Slow-cycle coverage signal: armed externally with the block-number set that must be
      * covered; fires once every head peer's hard-confirmed stacks span every block num. No coil
      * arm — followers participate only via the head set (see [[propCoilParticipation]]).
      */
    def slowCoverageSignal(
        perPeer: Capture[Map[HeadPeerNumber, PerPeerCaptures]],
        target: Deferred[IO, Set[Int]],
    ): IO[Signal[Unit]] =
        Signal.make[Unit] {
            case HEvent.Head(
                  _,
                  CommonChildEvent.SlowConsensusActor(
                    SlowConsensusActorEvent.StackHardConfirmed(_)
                  ),
                ) =>
                target.tryGet.flatMap {
                    case None             => IO.pure(None)
                    case Some(targetNums) =>
                        perPeer.state.values.toList.traverse(_.stacks.get).map { allPeersStacks =>
                            val allCovered = targetNums.isEmpty ||
                                allPeersStacks.forall { peerStacks =>
                                    targetNums.forall { bn =>
                                        peerStacks.exists { s =>
                                            (s.brief.firstBlockNum: Int) <= bn &&
                                            bn <= (s.brief.lastBlockNum: Int)
                                        }
                                    }
                                }
                            Option.when(allCovered)(())
                        }
                }
            case _ => IO.pure(None)
        }

    /** Effects-landed signal: armed externally with the backbone expectations derived from the
      * canonical hard-confirmed stacks; fires once observed `TxSubmitting` hashes cover every
      * expectation.
      */
    def effectsLandedSignal(
        landed: Capture[Ref[IO, Set[TransactionHash]]],
        target: Deferred[IO, List[BlockExpectation]],
    ): IO[Signal[Unit]] =
        Signal.make[Unit] {
            case HEvent.Head(
                  _,
                  CommonChildEvent.CardanoLiaison(CardanoLiaisonEvent.TxSubmitting(_)),
                ) | HEvent.Coil(
                  _,
                  CommonChildEvent.CardanoLiaison(CardanoLiaisonEvent.TxSubmitting(_)),
                ) =>
                target.tryGet.flatMap {
                    case None       => IO.pure(None)
                    case Some(exps) =>
                        landed.state.get.map { l =>
                            Option.when(EffectsLanded.isComplete(l, exps))(())
                        }
                }
            case _ => IO.pure(None)
        }

    /** Fallback-entered signal: fires on the first `FallbackToRuleBasedDispatched` from head or
      * coil. No target — fires directly off the event.
      */
    def fallbackEnteredSignal: IO[Signal[TransactionHash]] =
        Signal.make[TransactionHash] {
            case HEvent.Head(
                  _,
                  CommonChildEvent.CardanoLiaison(
                    CardanoLiaisonEvent.FallbackToRuleBasedDispatched(txId)
                  ),
                ) =>
                IO.pure(Some(txId))
            case HEvent.Coil(
                  _,
                  CommonChildEvent.CardanoLiaison(
                    CardanoLiaisonEvent.FallbackToRuleBasedDispatched(txId)
                  ),
                ) =>
                IO.pure(Some(txId))
            case _ => IO.pure(None)
        }
}
