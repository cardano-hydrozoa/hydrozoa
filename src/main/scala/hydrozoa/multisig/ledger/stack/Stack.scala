package hydrozoa.multisig.ledger.stack

import hydrozoa.config.node.operation.multisig.RateLimits
import hydrozoa.multisig.consensus.limiter.LimiterTimestamp
import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/** A closed slow-consensus stack at one of two signing stages.
  *
  *   - [[Stack.Unsigned]] — composed locally; per-effect bodies are derived but no hard-acks are
  *     aggregated yet.
  *   - [[Stack.HardConfirmed]] — every required round is saturated (round 1 + round 2 for 2-phase
  *     stacks; the sole round for minor-only ones). The collected per-peer hard-ack signatures have
  *     been aggregated into `VKeyWitness`es and attached onto the effect tx bodies:
  *     [[HardConfirmed.effects]] is the **multisigned** effect set, submittable on L1 as is. The
  *     raw acks have served their purpose at this point — they are verified and aggregated by
  *     [[hydrozoa.multisig.consensus.SlowConsensusActor]], not carried further.
  *
  * The structural split (genesis init+fallback vs regular partition-indexed effects) is carried by
  * the wrapped [[StackEffects]], not by the `Stack` wrapper itself:
  *
  *   - stack 0 wraps [[StackEffects.Unsigned.Initial]] / [[StackEffects.HardConfirmed.Initial]] —
  *     exogenous from `HeadConfig`; every peer derives it locally at boot, so nothing is
  *     wire-broadcast. Only the hard-acks flow across peers (via PeerLiaison's hard-ack lane). It
  *     still carries a (synthetic) [[StackBrief]] — zero block range, `creationEndTime` set to the
  *     initial block's end-time — so the rate limiter has a uniform `creationEndTime` to consult on
  *     every stack.
  *   - stack 1+ wraps [[StackEffects.Unsigned.Regular]] / [[StackEffects.HardConfirmed.Regular]],
  *     driven by the leader's wire-broadcast [[StackBrief]] (block range + creation end-time) so
  *     followers know which `BlockResult`s to fold into their local effect derivation.
  */
sealed trait Stack {
    def stackNum: StackNumber
}

object Stack:
    /** @param brief
      *   the stack's composition (the only thing wire-broadcast)
      * @param effects
      *   the locally-derived, partition-indexed effects. `BlockResult`s are a construction-only
      *   input (consumed by partitioning + derivation in `StackComposer.mkUnsigned`) and are NOT
      *   retained — every datum the slow side still needs (incl. each SEC's minor header bytes) is
      *   carried on `effects` itself (PR #446 review).
      */
    final case class Unsigned(
        brief: StackBrief,
        effects: StackEffects.Unsigned
    ) extends Stack {
        override def stackNum: StackNumber = brief.stackNum
    }

    /** @param brief
      *   the stack-composition announcement; carries `creationEndTime` consulted by the rate
      *   limiter (see below). The unsigned-side effects ([[Stack.Unsigned.effects]]) are NOT
      *   retained — once hard-confirmed, all needed effect data is on the multisigned `effects`.
      * @param effects
      *   the partition-indexed effects with every head peer's signature aggregated in: tx bodies
      *   carry the multisig `VKeyWitness`es; each partition's standalone evac commitment is a
      *   [[StandaloneEvacuationCommitment.MultiSigned]] (the dormant record + all peers' header
      *   signatures). Tx bodies are L1-submittable as is; the SEC is dispute-usable.
      *
      * Also the signal sent by [[hydrozoa.multisig.consensus.SlowConsensusActor]] to
      * [[hydrozoa.multisig.consensus.StackComposer]] to unblock the next stack close. The
      * [[hydrozoa.multisig.consensus.limiter.Limiter]] sitting on that lane reads the brief's
      * [[StackBrief.creationEndTime]] to throttle stack-production rate.
      */
    final case class HardConfirmed(
        brief: StackBrief,
        effects: StackEffects.HardConfirmed
    ) extends Stack,
          LimiterTimestamp {
        override def stackNum: StackNumber = brief.stackNum

        override def limiterTimestamp: Instant = brief.creationEndTime.instant

        override def minPeriod(using cfg: RateLimits.Section): FiniteDuration =
            cfg.hardStackMinPeriod
    }
