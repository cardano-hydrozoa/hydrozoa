package hydrozoa.l2.consensus

import hydrozoa.{Network, ParticipantVerificationKey, UDiffTimeMilli}

/** @param l2NetworkId
  * @param participants
  * @param l2ConsensusParams
  * @param minimalDepositWindow
  *   minimal window that should fit into deadline
  */
case class HeadParams(
//    l2NetworkId: Network,
//    participants: Set[ParticipantVerificationKey],
    l2ConsensusParams: L2ConsensusParams,
    minimalDepositWindow: UDiffTimeMilli
)

object HeadParams:
    def default = HeadParams(L2ConsensusParams.default, UDiffTimeMilli(10_000))

/** L2 consensus parameters - their hash is used to guarantee all nodes are running with the same
  * parameters set.
  *
  * @param depositMarginMaturity
  *   how long a deposit should persist on L1 before its collecting into the head (seconds)
  * @param depositMarginExpiry
  *   collection won't be run if it's too close to the deadline (seconds)
  */
case class L2ConsensusParams(
                                depositMarginMaturity: UDiffTimeMilli,
                                depositMarginExpiry: UDiffTimeMilli
)

object L2ConsensusParams:
    def default = L2ConsensusParams(UDiffTimeMilli(1_000), UDiffTimeMilli(5_000))
