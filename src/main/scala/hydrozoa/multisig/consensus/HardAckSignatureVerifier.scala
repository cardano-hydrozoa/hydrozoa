package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.implicits.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.multisig.consensus.SlowConsensusActor.CellError
import hydrozoa.multisig.consensus.ack.HardAck
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.l1.tx.TxSignature
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects, StandaloneEvacuationCommitment}
import scala.util.control.NonFatal
import scalus.cardano.ledger.Transaction
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.{ByteString, platform}

/** Verifies the **signatures** carried in a peer's hard-ack against the locally-derived,
  * partition-indexed effect bodies: each one is checked as a valid Ed25519 signature over the
  * corresponding tx body hash (or evac-commitment header), with the signer's vkey resolved from
  * `config` — head or coil.
  *
  * This owns only the second of three layers that together make a hard-confirmed effect tx valid:
  *   1. **Body validity** (balanced, well-formed, scripts, fee) is established at *build time*,
  *      when each peer deterministically derives the effect txs (the builders run every validator
  *      except signature presence / validity). Hard-acks carry only signatures, never tx bodies, so
  *      there is no foreign body to re-validate here.
  *   2. **Signature validity** — THIS class: every collected signature is correct over the local
  *      body. It does not re-check ledger validity; that is layer 1's job.
  *   3. **Witness completeness** — that enough valid signatures are collected to satisfy the head
  *      multisig script (all head peers + `coilQuorum` coil peers) — is enforced by
  *      [[SlowConsensusActor]]'s saturation, not here.
  *
  * Each signature is checked against THIS peer's own deterministically-derived body, so a signature
  * that fails to verify means the signer derived different effects — a critical, unrecoverable
  * consensus break: [[SlowConsensusActor]] raises and halts, and the rule-based fallback takes
  * over. Pure given `config` + its inputs.
  */
final class HardAckSignatureVerifier(config: HeadConfig.Bootstrap.Section) {

    /** Resolve a peer's verification key from `config` — head or coil — or fail with
      * [[CellError.UnknownPeer]].
      */
    def resolvePeerVKey(peer: PeerId): IO[VerificationKey] = {
        val vk = peer match {
            case PeerId.Head(n) => config.headPeerVKey(n)
            case PeerId.Coil(n) => config.coilPeerVKey(n)
        }
        vk.liftTo[IO](CellError.UnknownPeer(peer))
    }

    /** Verify a peer's round-1 ack against the locally-derived effects: each Regular partition's
      * round-1 slot (all its effects except the withheld unlock), or — for the Initial stack — the
      * fallback-tx signature.
      */
    def verify2PhaseRound1(
        unsigned: Stack.Unsigned,
        peer: PeerId,
        p: HardAck.Payload.Round1
    ): IO[Unit] = (unsigned.effects, p) match {
        case (r: StackEffects.Unsigned.Regular, pr: HardAck.Round1Payload.Regular) =>
            resolvePeerVKey(peer).flatMap(vk => verifyRound1Regular(vk, r, pr))
        case (i: StackEffects.Unsigned.Initial, pi: HardAck.Round1Payload.Initial) =>
            resolvePeerVKey(peer).flatMap(vk => verifyTx(vk, i.fallbackTx.tx, pi.fallbackSig))
        case _ =>
            IO.raiseError(CellError.EffectsPayloadMismatch("round-1", p.roundLabel))
    }

    /** Verify a peer's round-2 ack: the unlock tx's signature for a Regular stack (the first
      * settlement / finalization), or the Initial stack's init-tx plus its iff-gated individual
      * signature.
      */
    def verify2PhaseRound2(
        unsigned: Stack.Unsigned,
        peer: PeerId,
        p: HardAck.Payload.Round2
    ): IO[Unit] = (unsigned.effects, p) match {
        case (r: StackEffects.Unsigned.Regular, pr: HardAck.Round2Payload.Regular) =>
            resolvePeerVKey(peer).flatMap { vk =>
                PartitionEffects.unlock(r.partitions) match {
                    case Some(u) =>
                        verifyTx(
                          vk,
                          PartitionEffects.unlockTxOf(r.partitions, u),
                          pr.firstUnlockSig
                        )
                    case None =>
                        IO.raiseError(
                          CellError.KeysetMismatch("round-2 unlock", "present", "absent")
                        )
                }
            }
        case (i: StackEffects.Unsigned.Initial, pi: HardAck.Round2Payload.Initial) =>
            verifyRound2Initial(i, peer, pi)
        case _ =>
            IO.raiseError(CellError.EffectsPayloadMismatch("round-2", p.roundLabel))
    }

    /** Verify a peer's sole (1-phase) ack for a minor-only stack: the latest minor's
      * evac-commitment header signature plus its post-dated refund signatures.
      */
    def verifySole(
        unsigned: Stack.Unsigned,
        peer: PeerId,
        p: HardAck.SolePayload
    ): IO[Unit] = unsigned.effects match {
        case r: StackEffects.Unsigned.Regular =>
            r.partitions.head match {
                case PartitionEffects.Minor(sec, refunds) if r.partitions.tail.isEmpty =>
                    resolvePeerVKey(peer).flatMap(vk =>
                        verifyHeader(vk, sec.header, p.sec) >>
                            verifyTxList(vk, "sole.refunds", refunds.map(_.tx), p.refunds)
                    )
                case _ =>
                    IO.raiseError(
                      CellError.KeysetMismatch("sole shape", "single Minor partition", "other")
                    )
            }
        case _: StackEffects.Unsigned.Initial =>
            IO.raiseError(CellError.EffectsPayloadMismatch("sole", p.roundLabel))
    }

    private def verifyRound1Regular(
        vk: VerificationKey,
        r: StackEffects.Unsigned.Regular,
        p: HardAck.Round1Payload.Regular
    ): IO[Unit] = {
        val unlock = PartitionEffects.unlock(r.partitions)
        val es = r.partitions.toList
        val ss = HardAck.Round1Payload.Regular.asSlots(p).toList
        IO.raiseUnless(es.length == ss.length)(
          CellError.KeysetMismatch("round1.partitions", es.length.toString, ss.length.toString)
        ) >> es.zip(ss).zipWithIndex.traverse_ { case ((e, sg), i) =>
            verifyPartition(
              vk,
              e,
              sg,
              isSettlementUnlock = unlock.contains(PartitionEffects.Unlock.Settlement(i)),
              isFinalizationUnlock = unlock.contains(PartitionEffects.Unlock.Finalization(i))
            )
        }
    }

    /** Initial stack round 2. `initTxSig` is this peer's head-multisig contribution over the init
      * tx body (always required). `individualSig` must satisfy the iff rule via the SAME shared
      * deterministic predicate the signer used ([[StackEffects.spendsFromIndividualAddress]]):
      * present iff the peer funds an init input. The signature is verified against the peer's known
      * head key `vk` (which is also its individual-funding key), so a peer can only ever contribute
      * a witness under its own key — there is no foreign-key witness to reject.
      */
    private def verifyRound2Initial(
        i: StackEffects.Unsigned.Initial,
        peer: PeerId,
        p: HardAck.Round2Payload.Initial
    ): IO[Unit] = for {
        vk <- resolvePeerVKey(peer)
        initTx = i.initializationTx
        _ <- verifyTx(vk, initTx.tx, p.initTxSig)
        expected = StackEffects.spendsFromIndividualAddress(initTx, vk)
        _ <- (expected, p.individualSig) match {
            case (false, None) => IO.unit
            case (false, Some(_)) =>
                IO.raiseError(
                  CellError.KeysetMismatch(
                    "round2Initial.individualSig (peer funds no init input)",
                    "absent",
                    "present"
                  )
                )
            case (true, None) =>
                IO.raiseError(
                  CellError.KeysetMismatch(
                    "round2Initial.individualSig (peer funds an init input)",
                    "present",
                    "absent"
                  )
                )
            case (true, Some(sig)) =>
                verifyTx(vk, initTx.tx, sig)
        }
    } yield ()

    private def verifyPartition(
        vk: VerificationKey,
        e: PartitionEffects[StandaloneEvacuationCommitment],
        s: HardAck.Round1Payload.PartitionSigs,
        isSettlementUnlock: Boolean,
        isFinalizationUnlock: Boolean
    ): IO[Unit] = (e, s) match {
        case (
              PartitionEffects.Major(settlement, fallback, rollouts, refunds, sec),
              HardAck.Round1Payload.PartitionSigs.MajorComplete(
                sSettlement,
                sFallback,
                sRollouts,
                sRefunds,
                sSec
              )
            ) =>
            for {
                _ <- IO.raiseWhen(isSettlementUnlock)(
                  CellError.KeysetMismatch(
                    "partition.settlement (unlock ⇒ MajorUnlock slot)",
                    "MajorUnlock",
                    "Major"
                  )
                )
                _ <- verifyTx(vk, settlement.tx, sSettlement)
                _ <- verifyTx(vk, fallback.tx, sFallback)
                _ <- verifyTxList(vk, "partition.rollouts", rollouts.map(_.tx), sRollouts)
                _ <- verifyTxList(vk, "partition.refunds", refunds.map(_.tx), sRefunds)
                _ <- verifySecOpt(vk, sec, sSec)
            } yield ()
        case (
              PartitionEffects.Major(_, fallback, rollouts, refunds, sec),
              HardAck.Round1Payload.PartitionSigs.MajorPartial(sFallback, sRollouts, sRefunds, sSec)
            ) =>
            for {
                _ <- IO.raiseUnless(isSettlementUnlock)(
                  CellError.KeysetMismatch(
                    "partition.settlement (non-unlock ⇒ Major slot)",
                    "Major",
                    "MajorUnlock"
                  )
                )
                _ <- verifyTx(vk, fallback.tx, sFallback)
                _ <- verifyTxList(vk, "partition.rollouts", rollouts.map(_.tx), sRollouts)
                _ <- verifyTxList(vk, "partition.refunds", refunds.map(_.tx), sRefunds)
                _ <- verifySecOpt(vk, sec, sSec)
            } yield ()
        case (
              PartitionEffects.Final(finalization, rollouts),
              HardAck.Round1Payload.PartitionSigs.FinalComplete(sFinalization, sRollouts)
            ) =>
            for {
                _ <- IO.raiseWhen(isFinalizationUnlock)(
                  CellError.KeysetMismatch(
                    "partition.finalization (unlock ⇒ FinalUnlock slot)",
                    "FinalUnlock",
                    "Final"
                  )
                )
                _ <- verifyTx(vk, finalization.tx, sFinalization)
                _ <- verifyTxList(vk, "partition.rollouts", rollouts.map(_.tx), sRollouts)
            } yield ()
        case (
              PartitionEffects.Final(_, rollouts),
              HardAck.Round1Payload.PartitionSigs.FinalPartial(sRollouts)
            ) =>
            for {
                _ <- IO.raiseUnless(isFinalizationUnlock)(
                  CellError.KeysetMismatch(
                    "partition.finalization (non-unlock ⇒ Final slot)",
                    "Final",
                    "FinalUnlock"
                  )
                )
                _ <- verifyTxList(vk, "partition.rollouts", rollouts.map(_.tx), sRollouts)
            } yield ()
        case (
              PartitionEffects.Minor(sec, refunds),
              HardAck.Round1Payload.PartitionSigs.Minor(sSec, sRefunds)
            ) =>
            verifyHeader(vk, sec.header, sSec) >>
                verifyTxList(vk, "partition.refunds", refunds.map(_.tx), sRefunds)
        case _ =>
            IO.raiseError(
              CellError.KeysetMismatch(
                "partition kind",
                e.getClass.getSimpleName,
                s.getClass.getSimpleName
              )
            )
    }

    private def verifySecOpt(
        vk: VerificationKey,
        eSec: Option[StandaloneEvacuationCommitment],
        sSec: Option[BlockHeader.HeaderSignature]
    ): IO[Unit] = (eSec, sSec) match {
        case (None, None)            => IO.unit
        case (Some(sec), Some(hsig)) => verifyHeader(vk, sec.header, hsig)
        case (e, s) =>
            IO.raiseError(
              CellError.KeysetMismatch(
                "partition.sec presence",
                e.isDefined.toString,
                s.isDefined.toString
              )
            )
    }

    private def verifyTxList(
        vk: VerificationKey,
        label: String,
        txs: List[Transaction],
        sigs: List[TxSignature]
    ): IO[Unit] =
        IO.raiseUnless(txs.length == sigs.length)(
          CellError.KeysetMismatch(label, txs.length.toString, sigs.length.toString)
        ) >> txs.zip(sigs).traverse_ { case (tx, sg) => verifyTx(vk, tx, sg) }

    private def verifyHeader(
        vk: VerificationKey,
        msg: ByteString,
        sig: BlockHeader.HeaderSignature
    ): IO[Unit] =
        IO.delay(platform.verifyEd25519Signature(vk, msg, sig))
            .handleErrorWith {
                case NonFatal(_) => IO.pure(false)
                case e           => IO.raiseError(e)
            }
            .flatMap(ok => IO.raiseUnless(ok)(CellError.BadSignature("evac-commit header")))

    private def verifyTx(vk: VerificationKey, t: Transaction, sig: TxSignature): IO[Unit] =
        IO.delay(platform.verifyEd25519Signature(vk, t.id, sig))
            .handleErrorWith {
                case NonFatal(_) => IO.pure(false)
                case e           => IO.raiseError(e)
            }
            .flatMap(ok => IO.raiseUnless(ok)(CellError.BadSignature(s"tx ${t.id}")))
}
