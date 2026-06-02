package hydrozoa.multisig.consensus

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.effect.IO
import cats.implicits.*
import hydrozoa.multisig.consensus.SlowConsensusActor.Cell
import hydrozoa.multisig.consensus.ack.HardAck
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.l1.tx.{RefundTx, Tx, TxSignature}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects, StandaloneEvacuationCommitment}
import scalus.cardano.ledger.{Transaction, TransactionHash, VKeyWitness}
import scalus.crypto.ed25519.VerificationKey

/** Aggregates a saturated stack's per-peer hard-ack signatures into the multisigned
  * [[StackEffects.HardConfirmed]]: per-effect tx-body sigs become `VKeyWitness`es keyed by tx hash
  * ([[WitnessMap]]), and each partition's SEC header sigs are collected separately. The signer
  * population is driven entirely by its inputs — the keys of the `vkeys` map for tx witnesses, and
  * the `signers` list for SEC header sigs — so the caller fixes which peers (all head peers plus a
  * chosen `coilQuorum` coil peers) contribute.
  */
final class HardAckAggregator() {

    /** This peer-set's multisig witnesses for each effect, keyed by tx body hash. */
    type WitnessMap = Map[TransactionHash, Set[VKeyWitness]]

    import WitnessMap.*

    /** Aggregate every collected per-peer tx-body signature of a saturated cell into a
      * [[WitnessMap]] (`VKeyWitness`es keyed by effect tx hash): round-1 partition slots + the
      * round-2 unlock for a 2-phase stack, or the refund sigs for a sole (minor-only) stack.
      */
    def aggregateTxSignatures(
        cell: Cell.WaitingRound2 | Cell.WaitingSole,
        vkeys: Map[PeerId, VerificationKey]
    ): WitnessMap =
        cell match {
            case c: Cell.WaitingRound2 =>
                vkeys.keySet.foldLeft(Map.empty: WitnessMap) { (m, peer) =>
                    val vk = vkeys(peer)
                    val afterR1 = (regularPartitions(c.unsigned), c.round1.get(peer)) match {
                        case (Some(parts), Some(r: HardAck.Round1Payload.Regular)) =>
                            parts.toList
                                .zip(HardAck.Round1Payload.Regular.asSlots(r).toList)
                                .foldLeft(m) { case (acc, (e, sg)) =>
                                    addPartitionWitnesses(acc, vk, e, sg)
                                }
                        case (None, Some(r: HardAck.Round1Payload.Initial)) =>
                            c.unsigned.effects match {
                                case i: StackEffects.Unsigned.Initial =>
                                    m.add(i.fallbackTx.tx.id, mkWitness(vk, r.fallbackSig))
                                case _ => m
                            }
                        case _ => m
                    }
                    (regularPartitions(c.unsigned), c.round2.get(peer)) match {
                        case (Some(parts), Some(r: HardAck.Round2Payload.Regular)) =>
                            PartitionEffects.unlock(parts) match {
                                case Some(u) =>
                                    afterR1.add(
                                      PartitionEffects.unlockTxOf(parts, u).id,
                                      mkWitness(vk, r.firstUnlockSig)
                                    )
                                case None => afterR1
                            }
                        case (None, Some(r: HardAck.Round2Payload.Initial)) =>
                            c.unsigned.effects match {
                                case i: StackEffects.Unsigned.Initial =>
                                    val txId = i.initializationTx.tx.id
                                    afterR1
                                        .add(txId, mkWitness(vk, r.initTxSig))
                                        .addOptional(txId, vk, r.individualSig)
                                case _ => afterR1
                            }
                        case _ => afterR1
                    }
                }
            case c: Cell.WaitingSole =>
                vkeys.keySet.foldLeft(Map.empty: WitnessMap) { (m, peer) =>
                    (regularPartitions(c.unsigned), c.sole.get(peer)) match {
                        case (Some(parts), Some(p)) =>
                            parts.head match {
                                case PartitionEffects.Minor(_, refunds) =>
                                    m.addZip(refunds.map(_.tx), vkeys(peer), p.refunds)
                                case _ => m
                            }
                        case _ => m
                    }
                }
        }

    /** Per-partition SEC header signatures across the `signers`, in their given order, aligned to
      * the effects' partition list. A partition with no SEC contributes an empty list; a present
      * SEC has one header sig per signer (verification already enforced presence + count). Empty
      * list overall for an Initial stack (no partitions).
      */
    def collectSecSignatures(
        cell: Cell.WaitingRound2 | Cell.WaitingSole,
        signers: List[PeerId]
    ): List[List[BlockHeader.HeaderSignature]] =
        regularPartitions(cell.unsigned) match {
            case None => Nil
            case Some(parts) =>
                val peerSigParts: List[Option[NonEmptyList[HardAck.Round1Payload.PartitionSigs]]] =
                    signers.map { peer =>
                        cell match {
                            case c: Cell.WaitingRound2 =>
                                c.round1.get(peer).collect {
                                    case r: HardAck.Round1Payload.Regular =>
                                        HardAck.Round1Payload.Regular.asSlots(r)
                                }
                            case c: Cell.WaitingSole =>
                                c.sole
                                    .get(peer)
                                    .map(p =>
                                        NonEmptyList.one(
                                          HardAck.Round1Payload.PartitionSigs
                                              .Minor(p.sec, p.refunds)
                                        )
                                    )
                        }
                    }
                parts.toList.indices.toList.map { i =>
                    peerSigParts.flatMap {
                        case Some(sps) =>
                            sps.toList.lift(i).flatMap {
                                case HardAck.Round1Payload.PartitionSigs.MajorComplete(
                                      _,
                                      _,
                                      _,
                                      _,
                                      sec
                                    ) =>
                                    sec
                                case HardAck.Round1Payload.PartitionSigs.MajorPartial(
                                      _,
                                      _,
                                      _,
                                      sec
                                    ) =>
                                    sec
                                case HardAck.Round1Payload.PartitionSigs.Minor(sec, _) => Some(sec)
                                case _: HardAck.Round1Payload.PartitionSigs.FinalComplete => None
                                case _: HardAck.Round1Payload.PartitionSigs.FinalPartial  => None
                            }
                        case None => None
                    }
                }
        }

    /** Attach the aggregated tx-body witnesses (`wmap`) and per-partition SEC header sigs (`evac`)
      * onto the unsigned effects, producing the L1-submittable / dispute-usable multisigned
      * [[StackEffects.HardConfirmed]].
      */
    def attachWitnesses(
        unsigned: Stack.Unsigned,
        wmap: WitnessMap,
        evac: List[List[BlockHeader.HeaderSignature]]
    ): IO[StackEffects.HardConfirmed] = unsigned.effects match {
        case r: StackEffects.Unsigned.Regular =>
            r.partitions.toList
                .zip(evac)
                .traverse { (pe, secSigs) =>
                    pe match {
                        case PartitionEffects.Major(
                              settlement,
                              fallback,
                              rollouts,
                              refunds,
                              sec
                            ) =>
                            for {
                                st <- signOne(settlement, wmap)
                                fb <- signOne(fallback, wmap)
                                ro <- rollouts.traverse(signOne(_, wmap))
                                rf <- refunds.traverse { case pd: RefundTx.PostDated =>
                                    signOne(pd, wmap).widen[RefundTx]
                                }
                            } yield PartitionEffects.Major(
                              st,
                              fb,
                              ro,
                              rf,
                              sec.map(StandaloneEvacuationCommitment.MultiSigned(_, secSigs))
                            ): PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]
                        case PartitionEffects.Final(finalization, rollouts) =>
                            for {
                                fi <- signOne(finalization, wmap)
                                ro <- rollouts.traverse(signOne(_, wmap))
                            } yield PartitionEffects.Final(fi, ro): PartitionEffects[
                              StandaloneEvacuationCommitment.MultiSigned
                            ]
                        case PartitionEffects.Minor(sec, refunds) =>
                            refunds
                                .traverse { case pd: RefundTx.PostDated =>
                                    signOne(pd, wmap).widen[RefundTx]
                                }
                                .map(rf =>
                                    PartitionEffects.Minor(
                                      StandaloneEvacuationCommitment.MultiSigned(sec, secSigs),
                                      rf
                                    ): PartitionEffects[
                                      StandaloneEvacuationCommitment.MultiSigned
                                    ]
                                )
                    }
                }
                .map(list => StackEffects.HardConfirmed.Regular(NonEmptyList.fromListUnsafe(list)))
        case i: StackEffects.Unsigned.Initial =>
            for {
                it <- signOne(i.initializationTx, wmap)
                fb <- signOne(i.fallbackTx, wmap)
            } yield StackEffects.HardConfirmed.Initial(
              initializationTx = it,
              fallbackTx = fb
            )
    }

    private def signOne[A <: Tx[A]](a: A, wmap: WitnessMap): IO[A] =
        wmap.get(a.tx.id).filter(_.nonEmpty) match {
            case None => IO.pure(a)
            case Some(ws) =>
                a.addSignatures(ws) match {
                    case Valid(signed) => IO.pure(signed)
                    case Invalid(e)    => IO.raiseError(e.head)
                }
        }

    /** Witness-map construction primitives: build `VKeyWitness`es, insert them, and fold one
      * partition's tx-body sigs in. Brought into this class via `import WitnessMap.*`.
      */
    private object WitnessMap {
        def mkWitness(vk: VerificationKey, sig: TxSignature): VKeyWitness =
            VKeyWitness(vk, sig)

        extension (m: WitnessMap)
            def add(h: TransactionHash, w: VKeyWitness): WitnessMap =
                m.updated(h, m.getOrElse(h, Set.empty) + w)
            def addOptional(
                h: TransactionHash,
                vk: VerificationKey,
                s: Option[TxSignature]
            ): WitnessMap = s.fold(m)(sg => m.add(h, mkWitness(vk, sg)))
            def addZip(
                txs: List[Transaction],
                vk: VerificationKey,
                sigs: List[TxSignature]
            ): WitnessMap =
                txs.zip(sigs).foldLeft(m) { case (acc, (tx, sg)) =>
                    acc.add(tx.id, mkWitness(vk, sg))
                }

        def regularPartitions(
            u: Stack.Unsigned
        ): Option[NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment]]] =
            u.effects match {
                case r: StackEffects.Unsigned.Regular => Some(r.partitions)
                case _: StackEffects.Unsigned.Initial => None
            }

        /** Fold one partition's tx-body sigs into the witness map (SEC header sigs are handled
          * separately by [[collectSecSignatures]] — not tx witnesses).
          */
        def addPartitionWitnesses(
            m: WitnessMap,
            vk: VerificationKey,
            e: PartitionEffects[StandaloneEvacuationCommitment],
            s: HardAck.Round1Payload.PartitionSigs
        ): WitnessMap = (e, s) match {
            case (
                  PartitionEffects.Major(settlement, fallback, rollouts, refunds, _),
                  HardAck.Round1Payload.PartitionSigs.MajorComplete(
                    sSettlement,
                    sFallback,
                    sRollouts,
                    sRefunds,
                    _
                  )
                ) =>
                m.add(settlement.tx.id, mkWitness(vk, sSettlement))
                    .add(fallback.tx.id, mkWitness(vk, sFallback))
                    .addZip(rollouts.map(_.tx), vk, sRollouts)
                    .addZip(refunds.map(_.tx), vk, sRefunds)
            case (
                  PartitionEffects.Major(_, fallback, rollouts, refunds, _),
                  HardAck.Round1Payload.PartitionSigs.MajorPartial(
                    sFallback,
                    sRollouts,
                    sRefunds,
                    _
                  )
                ) =>
                // Settlement sig is deferred to Round2Payload.Regular.firstUnlockSig — not here.
                m.add(fallback.tx.id, mkWitness(vk, sFallback))
                    .addZip(rollouts.map(_.tx), vk, sRollouts)
                    .addZip(refunds.map(_.tx), vk, sRefunds)
            case (
                  PartitionEffects.Final(finalization, rollouts),
                  HardAck.Round1Payload.PartitionSigs.FinalComplete(sFinalization, sRollouts)
                ) =>
                m.add(finalization.tx.id, mkWitness(vk, sFinalization))
                    .addZip(rollouts.map(_.tx), vk, sRollouts)
            case (
                  PartitionEffects.Final(_, rollouts),
                  HardAck.Round1Payload.PartitionSigs.FinalPartial(sRollouts)
                ) =>
                // Finalization sig is deferred to Round2Payload.Regular.firstUnlockSig — not here.
                m.addZip(rollouts.map(_.tx), vk, sRollouts)
            case (
                  PartitionEffects.Minor(_, refunds),
                  HardAck.Round1Payload.PartitionSigs.Minor(_, sRefunds)
                ) =>
                m.addZip(refunds.map(_.tx), vk, sRefunds)
            case _ => m // verification already rejected kind mismatches
        }
    }
}
