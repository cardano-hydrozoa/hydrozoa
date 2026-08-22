package hydrozoa.rulebased.evacuator

import cats.data.EitherT
import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{EvacuateRedeemer, TreasuryRedeemer}
import scalus.cardano.ledger.{AssetName, PolicyId, TransactionHash}
import scalus.uplc.builtin.Data.fromData

/** Rebuilds the set of evacuations still owed, by replaying the regime's own history.
  *
  * The treasury datum carries `evacuationActive`, a KZG commitment, which is not invertible — so
  * "what is still owed?" cannot be read off chain state. It has to be derived: start from the full
  * evacuation map (whose preimage only the head's operators hold) and subtract every key that a
  * landed `Evacuate` already paid out.
  *
  * The subtraction is checked, not assumed: [[reconstruct]] recomputes the residual commitment and
  * compares it to the treasury's `evacuationActive`. A match proves the reconstruction is exactly
  * the set the validator will accept membership proofs against; a mismatch means the map preimage
  * and the chain disagree, and building against it would produce txs that fail on-chain.
  */
object OutstandingSet {

    /** A reconstruction that has been checked against the on-chain commitment. */
    final case class Reconstructed(
        outstanding: EvacuationMap,
        evacuatedCount: Int,
        txsReplayed: Int
    )

    enum Error extends RuntimeException:
        case Backend(wrapped: CardanoBackend.Error)
        case NotASubset(strayKeys: Int)
        case CommitmentMismatch(computed: String, onChain: String)

        override def toString: String = getMessage

        override def getMessage: String = this match
            case Backend(wrapped) => s"Backend error while replaying evacuation history: $wrapped"
            case NotASubset(strayKeys) =>
                s"$strayKeys evacuated key(s) are absent from the evacuation map — " +
                    "the map preimage does not belong to this head"
            case CommitmentMismatch(computed, onChain) =>
                "Residual commitment does not match the treasury datum. " +
                    s"computed=$computed onChain=$onChain"

    /** Replay every `Evacuate` since the fallback and subtract what it paid out.
      *
      * One backend walk, at startup — not per tick. The bot then tracks its own submissions
      * locally, and only comes back here when its bookkeeping and the chain disagree (a lost race,
      * a partial landing, a rollback), which is exactly when the local record is untrustworthy.
      *
      * @param fullMap
      *   the head's complete evacuation map, from its persistence store
      * @param beacon
      *   the treasury beacon, which marks the continuing chain
      * @param fallbackTx
      *   the anchor: the tx that drove the head into the rule-based regime
      * @param onChainCommitment
      *   the live treasury datum's `evacuationActive`, used as the acceptance check
      */
    def reconstruct(
        backend: CardanoBackend[IO],
        fullMap: EvacuationMap,
        beacon: (PolicyId, AssetName),
        fallbackTx: TransactionHash,
        onChainCommitment: String
    )(using CardanoNetwork.Section): IO[Either[Error, Reconstructed]] =
        (for {
            continuing <- EitherT(backend.lastContinuingTxs(beacon, fallbackTx))
                .leftMap(Error.Backend(_))

            // Each continuing tx carries the redeemer that spent the previous treasury output.
            // Only `Evacuate` pays anything out; Resolve/Tally/Vote advance the regime without
            // touching the outstanding set, so they contribute no keys.
            evacuatedKeys = continuing.flatMap { tx =>
                fromData[TreasuryRedeemer](tx.spendingRedeemer) match {
                    case TreasuryRedeemer.Evacuate(r: EvacuateRedeemer) =>
                        r.evacuationKeys.asScala
                    case _ => Seq.empty
                }
            }

            stray = evacuatedKeys.filterNot(fullMap.evacuationMap.contains)
            _ <- EitherT.cond[IO](stray.isEmpty, (), Error.NotASubset(stray.size))

            outstanding = fullMap.removedAll(evacuatedKeys)

            // The acceptance check. The residual commitment is what the validator will verify
            // membership proofs against, so if ours does not equal the datum's, every tx we build
            // would be rejected — better to find that out here than one submission later.
            computed = outstanding.kzgCommitment.toHex
            _ <- EitherT.cond[IO](
              computed == onChainCommitment,
              (),
              Error.CommitmentMismatch(computed, onChainCommitment)
            )
        } yield Reconstructed(
          outstanding = outstanding,
          evacuatedCount = evacuatedKeys.size,
          txsReplayed = continuing.size
        )).value
}
