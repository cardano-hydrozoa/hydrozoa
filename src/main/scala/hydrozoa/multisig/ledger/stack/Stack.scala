package hydrozoa.multisig.ledger.stack

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
  * Two structural variants:
  *
  *   - `Initial` (stack 0, exogenous from `HeadConfig`): the genesis init+fallback pair. No
  *     [[StackBrief]] — every peer derives this stack locally from shared `HeadConfig`; nothing
  *     needs to be wire-broadcast from the leader. Only the hard-acks flow across peers (via
  *     PeerLiaison's hard-ack lane).
  *   - `Regular` (stack 1+): driven by a wire-broadcast [[StackBrief]] (block range + creation
  *     end-time) so followers know which `BlockResult`s to fold into their local effect derivation.
  */
sealed trait Stack {
    def stackNum: StackNumber
}

object Stack:
    sealed trait Unsigned extends Stack {
        def effects: StackEffects.Unsigned
    }

    object Unsigned:
        /** Stack 0. Built by each peer locally at boot from `HeadConfig.initialBlock.effects`; not
          * wire-broadcast. The slow side runs its hard-ack flow over these unsigned txs to produce
          * a [[HardConfirmed.Initial]].
          */
        final case class Initial(
            effects: StackEffects.Unsigned.Initial
        ) extends Unsigned {
            override val stackNum: StackNumber = StackNumber.zero
        }

        /** Stack 1+. The leader's [[StackBrief]] is the wire-broadcast announcement.
          *
          * @param brief
          *   the stack's wire-broadcast announcement (block range + creation end-time).
          * @param effects
          *   the locally-derived, partition-indexed effects. `BlockResult`s are a construction-only
          *   input (consumed by partitioning + derivation in `StackComposer.mkUnsigned`) and are
          *   NOT retained — every datum the slow side still needs (incl. each SEC's minor header
          *   bytes) is carried on `effects` itself (PR #446 review).
          */
        final case class Regular(
            brief: StackBrief,
            effects: StackEffects.Unsigned.Regular
        ) extends Unsigned {
            override def stackNum: StackNumber = brief.stackNum
        }

    sealed trait HardConfirmed extends Stack {
        def effects: StackEffects.HardConfirmed
    }

    object HardConfirmed:
        /** Stack 0, hard-confirmed. */
        final case class Initial(
            effects: StackEffects.HardConfirmed.Initial
        ) extends HardConfirmed {
            override val stackNum: StackNumber = StackNumber.zero
        }

        /** Stack 1+, hard-confirmed.
          *
          * @param brief
          *   the stack's brief (carried from the unsigned form).
          * @param effects
          *   the partition-indexed effects with every head peer's signature aggregated in: tx
          *   bodies carry the multisig `VKeyWitness`es; each partition's standalone evac commitment
          *   is a [[StandaloneEvacuationCommitment.MultiSigned]] (the dormant record + all peers'
          *   header signatures). Tx bodies are L1-submittable as is; the SEC is dispute-usable.
          */
        final case class Regular(
            brief: StackBrief,
            effects: StackEffects.HardConfirmed.Regular
        ) extends HardConfirmed {
            override def stackNum: StackNumber = brief.stackNum
        }
