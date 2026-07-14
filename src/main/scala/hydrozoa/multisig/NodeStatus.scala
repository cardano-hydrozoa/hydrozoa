package hydrozoa.multisig

/** Node lifecycle status backing the user-facing server's readiness endpoint (`GET /ready`).
  *
  * Tracks the head lifecycle as seen by this node: it starts at [[NodeStatus.Initializing]],
  * becomes [[NodeStatus.Active]] once stack 0 hard-confirms (the head exists on L1), and ends in
  * one of the terminal statuses — [[NodeStatus.Finalized]] on head finalization or
  * [[NodeStatus.HandedOffToRuleBased]] on the multisig→rule-based handoff. Only
  * [[NodeStatus.Active]] means the node should receive user traffic.
  */
enum NodeStatus:
    /** Stack 0 is not hard-confirmed yet: the head is not on L1 and nothing is submittable. */
    case Initializing

    /** The head is open on L1; the node serves user traffic. */
    case Active

    /** The head has been finalized; the node takes no further user traffic. Terminal. */
    case Finalized

    /** The multisig regime handed off to the rule-based regime; the request-processing actors are
      * stopped and the node takes no further user traffic. Terminal.
      */
    case HandedOffToRuleBased

object NodeStatus:
    extension (current: NodeStatus)
        /** Monotone advance: move to `next` only if it is further along the lifecycle. Earlier or
          * equally advanced statuses are ignored, so a terminal status sticks (first writer wins)
          * and independent writers cannot regress the status.
          */
        def advanceTo(next: NodeStatus): NodeStatus =
            if rank(next) > rank(current) then next else current

    private def rank(status: NodeStatus): Int = status match
        case NodeStatus.Initializing         => 0
        case NodeStatus.Active               => 1
        case NodeStatus.Finalized            => 2
        case NodeStatus.HandedOffToRuleBased => 2
