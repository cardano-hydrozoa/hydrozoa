package hydrozoa.multisig.consensus

import hydrozoa.multisig.ledger.stack.StackNumber

/** A node-local notification the [[SlowConsensusActor]] sends up a coil peer's
  * [[liaison.PeerLiaisonCoilToHub]] when a stack hard-confirms: that stack's number.
  *
  * The slow-side counterpart of [[SoftConfirmedHighWater]], and sent only on a coil peer — it
  * anchors the stack, head-hard-ack and coil-hard-ack pull ceilings against the hub, which serves
  * the whole population and can therefore run arbitrarily far ahead of one coil peer. A head peer
  * needs none of this: N-of-N confirmation already gates every mesh lane.
  *
  * The number alone rather than the `Stack.HardConfirmed` it came from: the liaison bounds pulls,
  * it does not inspect stacks. See docs/spec/liaison-backpressure.md.
  */
final case class HardConfirmedHighWater(stackNum: StackNumber)
