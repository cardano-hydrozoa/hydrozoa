package hydrozoa.head

import com.bloxbean.cardano.client.common.model.Network

case class HeadParams(networkId: Network, participants: Set[ParticipantVerificationKey])
