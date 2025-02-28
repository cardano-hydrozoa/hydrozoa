package hydrozoa.l2.consensus

import hydrozoa.{Network, ParticipantVerificationKey, UDiffTime}

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
    minimalDepositWindow: UDiffTime
)

object HeadParams:
    def default = HeadParams(L2ConsensusParams.default, UDiffTime(10))

/** L2 consensus parameters - their hash is used to guarantee all nodes are running with the same
  * parameters set.
  *
  * @param depositMarginMaturity
  *   how long a deposit should persist on L1 before its collecting into the head (seconds)
  * @param depositMarginExpiry
  *   collection won't be run if it's too close to the deadline (seconds)
  */
case class L2ConsensusParams(
    depositMarginMaturity: UDiffTime,
    depositMarginExpiry: UDiffTime
)

object L2ConsensusParams:
    def default = L2ConsensusParams(UDiffTime(1), UDiffTime(5))
