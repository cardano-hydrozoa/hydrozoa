package hydrozoa.l2.consensus

import hydrozoa.{Network, ParticipantVerificationKey}

case class HeadParams(
    networkId: Network,
    participants: Set[ParticipantVerificationKey]
)
