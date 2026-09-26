package hydrozoa.multisig.metrics

/** The [[hydrozoa.multisig.consensus.StackComposer]]'s current phase, reported to [[PeerMetrics]].
  *
  * `tryProgress` re-evaluates these on every inbound event and returns `IO.unit` for all of them
  * but [[Deriving]] — silently. A composer blocked on a peer therefore looks exactly like one with
  * nothing to do, which is what this gauge exists to separate.
  */
enum StackComposerPhase:
    /** The single-flight gate: stack N+1 cannot close until N hard-confirms. Applies to **both**
      * roles — the check precedes the leader/follower branch — and covers the interval after this
      * peer closed a stack and is awaiting its confirmation.
      */
    case WaitingForPreviousHardConfirmation

    /** A block is stack-eligible only once both halves arrive: the `BlockResult` from JointLedger
      * and the `Block.SoftConfirmed` from FastConsensusActor. They are tracked per block, so at any
      * moment different blocks may be missing different halves — hence one phase rather than two.
      */
    case WaitingForBlockResultsOrSoftConfirmations

    /** Follower only: no `StackBrief` yet for the stack this peer would close next. */
    case WaitingForStackBrief

    /** Building effects. The partition counts in [[ComposerStats]] track progress; a large stack
      * holds hundreds, and the derivation runs to completion inside one actor message.
      */
    case Deriving
